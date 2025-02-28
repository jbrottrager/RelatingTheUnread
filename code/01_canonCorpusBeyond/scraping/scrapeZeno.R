library(rvest)
library(purrr)


path <- "C:\\Users\\Brottrager\\Documents\\Diss\\corpora\\20240714\\GER\\0_scraping\\zeno\\txt"

if (dir.exists(path) == FALSE) {
  dir.create(path, recursive = TRUE)
}

corpus <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\corpora\\20240714\\GER\\20240926_GER_corpus_list.csv", 
                   sep = ";", encoding = "UTF-8")
corpus_zeno <- subset(corpus, text_source == "zeno")

scrape_link <- function(url) {
  page <- tryCatch(read_html(url), error = function(e) NULL)  # Handle errors if a link doesn't load
  
  if (!is.null(page)) {
    # Extract all text within <div class="zenoCOMain">, including nested text
    content <- page %>%
      html_nodes(xpath = "//div[contains(@class, 'zenoCOMain')]") %>% 
      html_text(trim = TRUE)           # Get all nested text
    return(content)
  }
  return(NULL)
}


for (i in 1:nrow(corpus_zeno)) {
  main_page <- read_html(corpus_zeno$repoID[i])
  
  links <- main_page %>%
    html_nodes("div.zenoTRNavBottom a") %>%  # Select <a> tags within div.zenoTRNavBottom
    html_attr("href") %>%                    # Extract href attribute
    na.omit() %>%                            # Remove NA values
    unique()                                 # Remove duplicate links
  
  links <- gsub("(.+)", "http://www.zeno.org/\\1", links)
  
  all_content <- map(links, scrape_link) %>% 
    compact() %>%
    unlist()
  
  if (is.null(all_content) || length(all_content) == 0) {
    next
  }
  
  filename <- paste0(corpus_zeno$wikiname[i], "_", 
                     corpus_zeno$wikiID[i])
  
  file_connection <- file(paste0(path, "//", filename, ".txt"), encoding = "UTF-8")
  writeLines(all_content, file_connection)
  close(file_connection)
  
  message("file ", i, " of ", nrow(corpus_zeno), " done!")
  
}


url <- "http://www.zeno.org/Literatur/M/Tieck,+Ludwig/Romane/Geschichte+des+Herrn+William+Lovell"
