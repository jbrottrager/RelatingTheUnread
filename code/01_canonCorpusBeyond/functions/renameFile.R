################################## renameFile() ###################################
#------------------------------------------------------------------------------#

### Last updated: 2023-09-21 ###################################################

#------------------------------------------------------------------------------#
## Function -------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# renameFile()
#
# Renames a file and creates, if necessary, a new directory.
# @param from path to the original file
# @param to path the the new file

renameFile <- function(from, to) {
  if (is.character(from) == FALSE) {
    message("The provided path is not a character!")
  }
  if (is.character(to) == FALSE) {
    message("The provided path is not a character!")
  }
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) {
    dir.create(todir, recursive = TRUE, showWarnings = FALSE)
  }
  file.rename(from = from,  to = to)
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#