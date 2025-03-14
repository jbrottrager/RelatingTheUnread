################### SCRAPING TEXT FROM PROJECT GUTENBERG.DE ####################
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#

dissolveSpecialCharactersFilenames <- function(string) {
  string <- gsub("�", "ae", string)
  string <- gsub("�", "oe", string)
  string <- gsub("�", "ue", string)
  string <- gsub("�", "Ae", string)
  string <- gsub("�", "Oe", string)
  string <- gsub("�", "Ue", string)
  string <- gsub("(�|�|�|�)", "e", string)
  string <- gsub("(�|�|�|�)", "i", string)
  string <- gsub("�", "ss", string)
  string <- gsub("('|'|�|�|\\?|\\!|\\.|,|;|:|<|>)", "", string)
  string <- gsub('"', '', string)
  string <- gsub('-+', '-', string)
  string <- gsub("(/| )", "-", string)
  return(string)
}

dissolveSpecialCharacters <- function(string, regex_list) {
  for (i in 1:length(regex_list)) {
    string <- string %>%
      str_replace_all(regex_list[[i]][1], regex_list[[i]][2])
  }
  return(string)
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#