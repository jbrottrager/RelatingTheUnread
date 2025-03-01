################### SCRAPING TEXT FROM PROJECT GUTENBERG.DE ####################
#------------------------------------------------------------------------------#



#------------------------------------------------------------------------------#

dissolveSpecialCharactersFilenames <- function(string) {
  string <- gsub("ä", "ae", string)
  string <- gsub("ö", "oe", string)
  string <- gsub("ü", "ue", string)
  string <- gsub("Ä", "Ae", string)
  string <- gsub("Ö", "Oe", string)
  string <- gsub("Ü", "Ue", string)
  string <- gsub("(é|è|ê|ë)", "e", string)
  string <- gsub("(ï|ì|í|î)", "i", string)
  string <- gsub("ß", "ss", string)
  string <- gsub("('|'|»|«|\\?|\\!|\\.|,|;|:|<|>)", "", string)
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