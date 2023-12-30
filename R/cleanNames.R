#' function to clean names for importation into databases
#' @param names - a name or an array of
#' @return clean name(s)
#' @examples
#' originalID <- c("ddd.aaa", "ddd uuu", "ddd+aaa", "ddd*yyy", "ddd#dd", "ddd_fff")
#' originalID <-c(originalID, "ddd$ddd", "ddd@ddd", "dd_aa", "dad*")
#' cleanNames(originalID)
#' @export
cleanNames <- function(names) {
  gsub("\\\\", "", names)
  # first we remove trailing spaces
  names <-gsub("\\s+$", "", names)
  # second we remove spaces at the beginning of each lines
  names <-gsub("^\\s+", "", names)
  # third we remove double spaces
  names <-gsub("\\s+", " ", names)

  # last we curate from other weird characters
  names <- tolower(names)
  make.unique(names, sep = "#")
  names <-gsub("[*]$", "-S", names)
  names <-gsub("[*]", "T", names)
  names <-gsub("[+]", "P", names)
  # we remove all except # for replicates
  names <-gsub("[^A-Za-z0-9\\W#]", "-", names)
  # names <-gsub("[\\W_]", "-", names)
  # names <-gsub("[\\W ]", "-", names)
  names <- gsub("-+", "-", names)
  names <- gsub("^-", "", names)
  return(names)
}
