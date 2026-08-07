#' read a spectrum (processed) from a Bruker expno folder
#'
#' @param expno - the path to the expNo folder
#' @param procno - chose procno to read from
#' @param procs - the name of the folder with experiments
#' @param options - options (uncalibrate, eretic, fromTo, length.out, im,
#'   verbose). verbose = FALSE keeps the routine notes quiet, failures are
#'   always reported
#' @return a vector with spectra (real part and x axis)
#' @importFrom signal interp1
#' @importFrom data.table data.table
#' @export
readSpectrum <- function(expno, procno = 1, procs = TRUE, options = list()){
  file1r <- file.path(expno, "pdata", procno, "1r")
  file1i <- file.path(expno, "pdata", procno, "1i")



  if (is.logical(procs) && isTRUE(procs)) {
    fileProcs <- file.path(expno, "pdata", procno, "procs")
  } else {
    fileProcs <- procs
  }
  fileAcqus <- file.path(expno, "acqus")

  # The routine notes below describe what was done rather than what went
  # wrong, and there is one set per spectrum, so a rack of 1000 prints
  # thousands of lines all saying the same thing. readExperiment() turns
  # them off and reports its own tally; a direct call still narrates itself.
  # Failures are never silenced.
  verbose <- !isFALSE(options$verbose)

  # checking that file is not empty
  if (file.exists(fileProcs)) {
    if (is.null(readParam(fileProcs, "NC_proc"))) {
      message(crayon::yellow(paste("readSpectrum >> empty procs file for", expno)))
      return(NULL)
    }
  } else {
    message(crayon::yellow(paste("readSpectrum >> procs file not found for", expno)))
    return(NULL)
  }

  if (file.exists(fileAcqus)) {
    if (is.null(readParam(fileAcqus, "BF1"))) {
      message(crayon::yellow(paste("readSpectrum >> empty acqus for", expno)))
      return(NULL)
    }
  } else {
    message(crayon::yellow(paste("readSpectrum >> acqus file not found for", expno)))
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
        message(crayon::yellow(paste("readSpectrum >> imaginary data not found for", expno)))
        return(NULL)
      }
    }

    if ("uncalibrate" %in% names(options)) {
      uncalibrate <- options$uncalibrate
    } else {
      uncalibrate <- FALSE
    }

    # reading important parameters
    bytordp <- readParam(fileProcs, "BYTORDP")
    nc <- readParam(fileProcs, "NC_proc")
    size <- readParam(fileProcs, "FTSIZE") # it should be equivalent to use SI
    sf <-readParam(fileProcs, "SF") # SF is equal to acqus/BF1
    sw_p <- readParam(fileProcs, "SW_p")
    offset <- readParam(fileProcs, "OFFSET")

    # read additional information for output
    phc0 <- readParam(fileProcs, "PHC0")
    phc1 <- readParam(fileProcs, "PHC1")

    bf1 <- readParam(fileAcqus, "BF1")

    # checking for empty params. readParam() returns NULL for a parameter it
    # cannot find and c() drops NULL without trace, so the missing ones have
    # to be counted while they still have a name attached. Reaching the
    # arithmetic below with one of them NULL does not fail here, it fails
    # further down with 'argument is of length zero' or "'to' must be of
    # length 1", which aborts the whole readExperiment run over one file
    params <- list(BYTORDP = bytordp,
                   NC_proc = nc,
                   FTSIZE = size,
                   SF = sf,
                   SW_p = sw_p,
                   OFFSET = offset,
                   PHC0 = phc0,
                   PHC1 = phc1,
                   BF1 = bf1)

    fi <- vapply(params, function(x) length(x) != 1 || is.na(x), logical(1))
    if (sum(fi) > 0) {
      message(crayon::yellow(paste("readSpectrum >> empty parameter",
                         paste(names(params)[fi], collapse = ", "),
                         "for", expno)))
      return(NULL)
    }

    if (bytordp == 0) {
      endian <- "little"
    } else {
      endian = "big"
    }

    if (phc1 != 0) {
      message(crayon::yellow(paste("readSpectrum >> phc1 is expected to be 0 in IVDr experiments. Found:",
                         phc1)))
    }

    # removing SR (useful for JEDI experiments)
    sw <- sw_p / sf # SW_p is equal to acqus/SW_h
    SR_p <- (sf - bf1) * 1e6 / sf
    SR <- (sf - bf1) * 1e6
    if (uncalibrate) {
      # a negative SR value means an uncalibrated signal on the right of 0
      offset <- offset + SR_p

      if (verbose) {
        message(crayon::blue(paste("readSpectrum >> calibration (SR) removed:",
                                   SR_p, "ppm", SR, "Hz")))
      }
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
        message(crayon::yellow(paste("readSpectrum >> Im and Re have different dimensions", expno)))
        return(NULL)
      }
    }

    # applying eretic correction if provided
    if ("eretic" %in% names(options)) {
      ereticFactor <- options$eretic
      y <- y / ereticFactor
      if (im) {
        yi <- yi / ereticFactor
      }

      if (verbose) {
        message(crayon::blue(paste("readSpectrum >> spectra corrected for eretic:",
                                   ereticFactor)))
      }
    } else {
      # NA rather than NULL: c() drops NULL, so info below would come back
      # one element shorter and callers reading it by position would shift
      ereticFactor <- NA
    }

    # if upper and lower bounds are provided the spectra is extrapolated to fit
    # those boundaries. If no length.out is provided, them similar length is
    # used.
    if ("fromTo" %in% names(options)) {
      from <- options$fromTo[1]
      to <- options$fromTo[2]

      if (from > to) {
        message(crayon::blue(paste("readSpectrum >> from should be smaller than to")))
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

        message(crayon::red(paste0("readSpectrum >> x and y are of different length (",
                                   length(x), " and ", length(y), ") for ", expno)))
        return(NULL)

      }


      if (im) {
        yi <- interp1(x = x,
                      y = yi,
                      xi = newX,
                      method = "spline")
      }

      x <- newX

      if (verbose) {
        message(crayon::blue(paste0("readSpectrum >> common grid ", from, " to ", to,
                                    " ppm, ", length.out, " points (from ", size,
                                    ", ", trimmedSize, " in range)")))
      }
    }

    info <- c(SF = sf,
              PHC0 = phc0,
              PHC1 = phc1,
              SR = SR,
              ereticFactor = ereticFactor)

    if (uncalibrate) {
      info <- c(info, uncalibrated = 1)
    } else {
      info <- c(info, uncalibrated = 0)
    }

    if (im) {
      spec <- list(info  = info,
                   spec = data.table(x, y, yi))
    } else {
      spec <- list(info  = info,
                   spec = data.table(x, y))
    }

    return(spec)

  } else {
    message(crayon::yellow(paste("readSpectrum >> data not found for", expno)))
  }
}
