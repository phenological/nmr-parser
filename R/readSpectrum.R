#' read a spectrum (processed) from a Bruker expno folder
#'
#' @param expno - the path to the expNo folder
#' @param procs - the name of the folder with experiments
#' @param options - options (uncalibrate, eretic, fromTo, length.out)
#' @return a vector with spectra (real part and x axis)
#' @importFrom signal interp1
#' @importFrom data.table data.table
#' @export
readSpectrum <- function(expno, procs = TRUE, options = list()){
  file1r <- file.path(expno, "pdata", "1", "1r")
  file1i <- file.path(expno, "pdata", "1", "1i")



  if (is.logical(procs) && isTRUE(procs)) {
    fileProcs <- file.path(expno, "pdata", "1", "procs")
  } else {
    fileProcs <- procs
  }
  fileAcqus <- file.path(expno, "acqus")

  # checking that file is not empty
  if (file.exists(fileProcs)) {
    if (is.null(readParam(fileProcs, "NC_proc"))) {
      cat(crayon::yellow("readSpectrum >> empty procs file for", expno, "                 \n"))
      return(NULL)
    }
  } else {
    cat(crayon::yellow("readSpectrum >> procs file not found for", expno, "               \n"))
    return(NULL)
  }

  if (file.exists(fileAcqus)) {
    if (is.null(readParam(fileAcqus, "BF1"))) {
      cat(crayon::yellow("readSpectrum >> empty acqus for", expno, "                      \n"))
      return(NULL)
    }
  } else {
    cat(crayon::yellow("readSpectrum >> acqus file not found for", expno, "               \n"))
    return(NULL)
  }

  if (file.exists(file1r)) {
    if ("im" %in% names(options)) {
      im <- options$im
    } else {
      im = FALSE
    }

    if (im) {
      if (!file.exists(file1i)) {
        cat(crayon::yellow("readSpectrum >> imaginary data not found for", expno, "       \n"))
        return(NULL)
      }
    }

    if ("uncalibrate" %in% names(options)) {
      uncalibrate <- options$uncalibrate
    } else {
      uncalibrate <- FALSE
    }

    # reading important parameters
    if(readParam(fileProcs, "BYTORDP") == 0) {
      endian <- "little"
    } else {
      endian = "big"
    }

    nc <- readParam(fileProcs, "NC_proc")
    size <- readParam(fileProcs, "FTSIZE") # it should be equivalent to use SI
    sf <-readParam(fileProcs, "SF") # SF is equal to acqus/BF1
    sw_p <- readParam(fileProcs, "SW_p")
    offset <- readParam(fileProcs, "OFFSET")

    # read additional information for output
    phc0 <- readParam(fileProcs, "PHC0")
    phc1 <- readParam(fileProcs, "PHC1")

    bf1 <- readParam(fileAcqus, "BF1")

    # checking for empty params
    params <- c(endian, nc, size, sf, sw_p, offset, phc0, phc1, bf1)
    fi <- is.na(params)
    if (sum(fi) > 0) {
      cat(crayon::yellow("readSpectrum >> empty parameter for", expno, "                    \n"))
      return(NULL)
    }

    if (phc1 != 0) {
      cat(crayon::yellow("readSpectrum >> phc1 is expected to be 0 in IVDr experiments. Found:",
                         phc1, "                    \r"))
    }

    # removing SR (useful for JEDI experiments)
    sw <- sw_p / sf # SW_p is equal to acqus/SW_h
    SR_p <- (sf - bf1) * 1e6 / sf
    SR <- (sf - bf1) * 1e6
    if (uncalibrate) {
      # a negative SR value means an uncalibrated signal on the right of 0
      offset <- offset + SR_p

      cat(crayon::blue("readSpectrum >> calibration (SR) removed:",
                       SR_p, "ppm", SR, "Hz",
                       "\n"))
    }

    # computing increment, ppm axis and reading spectra
    y <- read1r(file1r, size, nc, endian)
    y <- rev(y)
    inc <- sw / (length(y) - 1) # ok
    x <- seq(from = offset, to = (offset - sw), by = -inc)
    x <- rev(x)

    # reading imaginary data if necessary
    if (im) {
      yi <- read1r(file1i, size, nc, endian)
      yi <- rev(yi)
      # check for length
      if (length(yi) != length(y)) {
        cat(crayon::yellow("readSpectrum >> Im and Re have different dimensions", expno, "      \n"))
        return(NULL)
      }
    }

    # applying eretic correction if provided
    if ("eretic" %in% names(options)) {
      y <- y / options$eretic
      if (im) {
        yi <- yi / options$eretic
      }

      cat(crayon::blue("readSpectrum >> spectra corrected for eretic:",
                       options$eretic,
                       "\r"))
    }

    # if upper and lower bounds are provided the spectra is extrapolated to fit
    # those boundaries. If no length.out is provided, them similar length is
    # used.
    if ("fromTo" %in% names(options)) {
      from <- options$fromTo[1]
      to <- options$fromTo[2]

      if (from > to) {
        cat(crayon::blue("readSpectrum >> from should be smaller than to\n"))
      }

      if ("length.out" %in% names(options)) {
        length.out <- options$length.out
        trimmedSize <- length.out
      } else {
        fi <- x > from & x < to
        trimmedSize <- sum(fi)
        length.out <- sum(fi)
      }

      newX <- seq(from,
                  to,
                  length.out = length.out)

      if (length(x) == length(y)) {
        y <- interp1(x = x,
                     y = y,
                     xi = newX,
                     method = "spline")
      } else {

        cat(crayon::red("readSpectrum >> x and y are of different length\n"))
        cat(crayon::blue("readSpectrum >>", expno, "\n"))
        cat(crayon::blue("readSpectrum >> length(x)", length(x), "\n"))
        cat(crayon::blue("readSpectrum >> length(y)", length(y), "\n"))
        return(NULL)

      }


      if (im) {
        yi <- interp1(x = x,
                      y = yi,
                      xi = newX,
                      method = "spline")
      }

      x <- newX

      cat(crayon::blue("readSpectrum >> spectra in common grid (from:",
                       from,
                       "to:",
                       to,
                       "dim:",
                       length.out,
                       "orig.size:",
                       size,
                       "trimmed.size:",
                       trimmedSize,
                       ")\r"))
    }

    info <- c(SF = sf,
              PHC0 = phc0,
              PHC1 = phc1,
              SR = SR)
    if (im) {
      spec <- list(info  = info,
                   spec = data.table(x, y, yi))
    } else {
      spec <- list(info  = info,
                   spec = data.table(x, y))
    }

    return(spec)

  } else {
    cat(crayon::yellow("readSpectrum >> data not found for", expno, "                       \n"))
  }
}
