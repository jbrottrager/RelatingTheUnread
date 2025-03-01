########################## SCRAPING GUTENBERG US LINKS ######################### 
#------------------------------------------------------------------------------#

### Last updated: 2023-10-21 ###################################################

# Creates a list of all Gutenberg-US text links.

#------------------------------------------------------------------------------#
## Required packages ----------------------------------------------------------#
#------------------------------------------------------------------------------#

library(xml2)
library(xslt)

#------------------------------------------------------------------------------#
## Scraping -------------------------------------------------------------------#
#------------------------------------------------------------------------------#

path <- "C:\\Users\\Brottrager\\Documents\\Diss\\corpora\\20240714\\GER\\0_scraping\\textgrid\\Digitale-Bibliothek-Literatur"
setwd(path)

files <- list.files(pattern = ".xml", full.names = TRUE)

new_dir <- "C:\\Users\\Brottrager\\Documents\\Diss\\corpora\\20240714\\GER\\0_scraping\\textgrid\\20240926_individual_texts"
if (dir.exists(new_dir) == FALSE) {
  dir.create(new_dir, recursive = TRUE)
}


for (j in 1:length(files)) {
  doc <- read_xml(files[j])
  
  ns <- c(tei = "http://www.tei-c.org/ns/1.0")
  
  # Find all <TEI> elements
  tei_elements <- xml_find_all(doc, "//tei:TEI", ns = ns)
  
  # Find all <TEI> elements
  tei_elements <- xml_find_all(doc, "//tei:TEI", ns = ns)
  
  
  for (i in seq_along(tei_elements)) {
    tei_element <- tei_elements[[i]]
    
    title <- xml_attr(tei_element, "n")
    
    if (grepl("Gedichte|Sagen|Dramen|Lyrik|Lieder|Bildergeschichten", title, ignore.case = TRUE)) {
      cat("Skipping:", title, "because wrong genre.\n")
      next  # Skip this iteration
    }
    
    safe_title <- gsub("[<>:\"/\\|?*,]", "_", title)  # Replace illegal characters with underscores
    safe_title <- gsub("_Literatur.+?_(.+)", "\\1", safe_title)
    
    file_name <- paste0(gsub(" ", "_", safe_title), ".txt")
    file_name <- gsub("__", "_", file_name)
    
    front_text <- xml_text(xml_find_all(tei_element, "tei:text/tei:front//text()", ns = ns))
    
    body_text <- xml_text(xml_find_all(tei_element, "tei:text/tei:body//text()", ns = ns))
    
    front_text_combined <- paste(front_text, collapse = " ")
    body_text_combined <- paste(body_text, collapse = " ")
    
    tryCatch({
      con <- file(paste0(new_dir, "//", file_name), encoding = "UTF-8")
      on.exit(close(con))  # Ensure the connection is closed when exiting this context
      
      # Save the texts to the file
      writeLines(c(front_text_combined, "", body_text_combined), con = con)
      
      cat("Saved:", file_name, "\n")
    }, error = function(e) {
      cat("Error saving file:", file_name, "\n", "Error message:", e$message, "\n")
    })
  }
  message("\n\n------------------------File ", j, " is done ------------------------n\n")
}





filelist <- dir("C:\\Users\\Brottrager\\Documents\\Diss\\corpora\\20240714\\GER\\0_scraping\\textgrid\\20240926_individual_texts",
                full.names = TRUE)
path <- "C:\\Users\\Brottrager\\Documents\\Diss\\corpora\\20240714\\GER\\0_scraping\\textgrid\\txt"

if (dir.exists(path) == FALSE) {
  dir.create(path, recursive = TRUE)
}

corpus <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\corpora\\20240714\\GER\\20240926_GER_corpus_list.csv", 
                   sep = ";", encoding = "UTF-8")
corpus_textgrid <- subset(corpus, text_source == "textgrid")
corpus_textgrid$scraped_id <- gsub(".+texts/(.+).xml", "\\1", corpus_textgrid$repoID)
corpus_textgrid$scraped_id <- gsub("-", "_", corpus_textgrid$scraped_id)

for (i in 1:length(filelist)){
  file_name <- filelist[i]
  short_name <- gsub(".+texts/(.+).txt", "\\1", filelist[i])
  short_name <- gsub("(Erzählung|Roman|Romane|Erzählungen)_", "", short_name)
  short_name <- gsub("(.+)\\..+", "\\1", short_name)
  short_name <- gsub("-", "_", short_name)
  message(short_name)
  if (short_name %in% corpus_textgrid$scraped_id) {
    message(short_name)
    filename <- paste0(corpus_textgrid$wikiname[which(corpus_textgrid$scraped_id == short_name)], "_", 
                       corpus_textgrid$wikiID[which(corpus_textgrid$scraped_id == short_name)])
    file.copy(from = file_name, to = paste0(path, "//", filename, ".txt"))
  }
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

