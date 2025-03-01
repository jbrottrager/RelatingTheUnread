############################# SCRAPING GUTENBERG DE ############################ 
#------------------------------------------------------------------------------#

### Last updated: 2024-07-14 ###################################################

# 
#
#
#

#------------------------------------------------------------------------------#
## Required packages ----------------------------------------------------------#
#------------------------------------------------------------------------------#

library(rvest)
library(textutils)
library(stringr)
library(stringi)

source("C:\\Users\\Brottrager\\Documents\\Diss\\code\\relating-the-unread\\functions\\readUrl.R")
source("C:\\Users\\Brottrager\\Documents\\Diss\\code\\relating-the-unread\\functions\\renameFile.R")
source("C:\\Users\\Brottrager\\Documents\\Diss\\code\\relating-the-unread\\functions\\dissolveSpecialCharacters.R")


#------------------------------------------------------------------------------#
## Preparation ----------------------------------------------------------------#
#------------------------------------------------------------------------------#

date <- gsub("\\d\\d(\\d\\d)-(\\d\\d)-(\\d\\d)", "\\1\\2\\3", Sys.Date())
date <- "240714"

all_titles <- "https://www.projekt-gutenberg.org/info/texte/allworka.html"

path_corpora <- "C:\\Users\\Brottrager\\Documents\\Diss\\corpora\\20240714\\GER\\0_scraping"
setwd(path_corpora)

path_new <- paste0("C:\\Users\\Brottrager\\Documents\\Diss\\corpora\\20240714\\GER\\0_scraping\\", date, "_scraped_gutenberg")

indeces_done_dir <- paste0(path_corpora, "\\indeces_done")

if (dir.exists(indeces_done_dir) == FALSE) {
  dir.create(indeces_done_dir, recursive = TRUE)
}

all_titles_listed <- read_html(all_titles)
scraped_list <- html_nodes(all_titles_listed, xpath = '//dl//a[@href]')
scraped_list_regex <- html_attr(scraped_list, "href")
scraped_list_regex <- gsub("../..(/.+?/.+?/.+?.html)", 
                           "https://www.projekt-gutenberg.org\\1", 
                           scraped_list_regex)

writeLines(scraped_list_regex,
           paste0(path_corpora, "\\ger.txt"),
           sep = "\n", useBytes = FALSE)

skeleton <- "C:\\Users\\Brottrager\\Documents\\Diss\\corpora\\ger\\skeleton_upper.xml"
skeleton_xml <- readLines(skeleton)

skeleton_lower <- "</body>\n</text>\n</TEI>"

cleaned_up_dir <- paste0(path_new, "\\gutenberg_html")

if (dir.exists(cleaned_up_dir) == FALSE) {
  dir.create(cleaned_up_dir, recursive = TRUE)
}

replace_regex <- '(<em>|</em>|<i>|</i>|<b>|</b>|<sup>|</sup>|<sub>|</sub>|<tt>|</tt>|<br>|<a id=".+?" name=".+?" title=".+?">\\n?</a>|<small>|</small>|<p>( )*</p>)'

regex_list <- list(c("&", "&amp;"),c('<a name="page.+"></a>', ''),
                   c('<img alt=".+" src=".+" title=".+">', ''),
                   c('<meta name="author" content=".+">', ''),
                   c('<meta name="title" content=".+">', ''),
                   c('<img alt=".+" src=".+">'))

corpus_dir <-  paste0(path_new, "\\cleaned_up_gutenberg")

if (dir.exists(corpus_dir) == FALSE) {
  dir.create(corpus_dir, recursive = TRUE)
}

#------------------------------------------------------------------------------#
## Filtering ------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#start_at <- 1
#stop_at <- 100

start_at <- 1
stop_at <- 100
while (start_at <= length(scraped_list_regex)) {
  id <- 1
  nested_links <- list()
  error_urls_indexing <- c()
  for (i in start_at:stop_at) {
    message("Indexing for indeces from ", start_at, " to ", stop_at)
    base <- readUrl(scraped_list_regex[i])
    if (is.numeric(base)) {
      message("\n\nIndexing is paused.")
      error_urls_indexing <- append(error_urls_indexing, scraped_list_regex[i])
      Sys.sleep(print(sample(5:30)[1]))
      next()
    }
    index <- html_nodes(base, xpath = '//div/span/a[starts-with(@href, "index")]')
    if (length(index) != 0) {
      contents <- html_nodes(base, xpath = '//ul/li/a[@href]')
      attributes <- html_attr(contents, "href")
      attributes <- attributes[which(!startsWith(attributes, "#"))]
      base <- gsub('(https:.+)/.+html', '\\1', scraped_list_regex[i])
      for (j in 1:length(attributes)) {
        attributes[j] <- paste0(base, "/", attributes[j])
        print(attributes[j])
      }
    } else {
      attributes <- scraped_list_regex[i]
      print(attributes)
    }
    nested_links[[id]] <- attributes
    names(nested_links[[id]]) <- scraped_list_regex[i]
    message("Indexing for ", i, " of ", length(scraped_list_regex), " is done!")
    id <- id + 1
  }

  name <- paste0("links_indeces_", start_at, "-to-", stop_at)
  saveRDS(nested_links, paste0(name, ".rds"))
  writeLines(unlist(nested_links), paste0(name, ".txt"),
             sep = " ", useBytes = FALSE)
  
  start_at <- start_at + 100
  stop_at <- stop_at + 100
  
  if (start_at >= length(scraped_list_regex)) {
    stop()
  }
  if (stop_at > length(scraped_list_regex)) {
    stop_at <- length(scraped_list_regex)
  }
  
  name_errors <- paste0("errors_indeces_", start_at, "-to-", stop_at, ".rds")
  saveRDS(error_urls_indexing, name_errors)
  
  pause <- sample(3:60)[1]
  message("Pausing for ", pause, 
          " seconds, then starting the next iteration (indeces ", 
          start_at, " to ", stop_at, ").")
  Sys.sleep(pause)
}

message("Indexing is done!")


#------------------------------------------------------------------------------#
## Filtering ------------------------------------------------------------------#
#------------------------------------------------------------------------------#

lists <- list.files(path = path_corpora, pattern = "links.+.rds",
                    full.names = TRUE)

for (x in 9:length(lists)) {
  data <- readRDS(lists[x])
  error_urls_scraping <- c()
  for (j in 1:length(data)) {
    all_text <- vector()
    name <- gsub('.+org/(.+?)/(.+?)/.+html', '\\1_\\2', names(data[[j]][1]))
    name <- gsub('/', '', name)
    for (i in 1:length(data[[j]])){
      html_text <- readUrl(as.character(data[[j]][i]))
      if (is.numeric(html_text)) {
        message("\n\nScraping is paused, next iteration starts at ", j, ".")
        error_urls_scraping <- append(error_urls_scraping, names(data[[j]][1]))
        start_at <- i
        stop_at <- i + 100
        Sys.sleep(print(sample(2:10)[1]))
        next()
      }
      # title, author, text & headings
      scraped_text <- html_nodes(
        html_text, xpath = '//meta[@name="author"]|//meta[@name="title"]|//p|//h2|//h4|//h3')
      scraped_text <- HTMLdecode(scraped_text, named = TRUE, hex = TRUE, 
                                 decimal = TRUE)
      all_text <- append(all_text, scraped_text)
    }
    all_text_char <- as.character(all_text)
    Encoding(all_text_char) <- "UTF-8"
    writeLines(all_text_char,
               # Dateiname angeben, unter dem der Text gespeichert werden soll
               paste0(path_new, "\\gutenberg_html\\", name, ".txt"),
               sep = " ", useBytes = FALSE)
    
    cleaned_up <- all_text_char %>%
      str_replace_all(replace_regex, "") %>%
      str_replace_all("(«|»)", '"') %>%
      str_replace_all('<p class=".+?">', '<p>') %>%
      str_replace_all('<h3 class=".+?">', "<head>") %>%
      str_replace_all('h3', 'head') %>%
      str_replace_all("(</p>|</head>)", "\\1\n") %>%
      str_replace_all('<span class="tooltip" title=".+?">(.+?)</span>', "\\1")
    
    cleaned_up <- append(skeleton_xml, cleaned_up)
    cleaned_up <- append(cleaned_up, skeleton_lower)
    writeLines(cleaned_up, paste0(cleaned_up_dir, "\\", name, ".xml"))
    
    author <- grep('<meta name="author" content=".+">', cleaned_up, value = TRUE) 
    author <- gsub('.+content="(.+?)">\\n?', "\\1", author[1])
    author <- strsplit(author, " ")
    author <- paste(author[[1]][2], author[[1]][1], sep = "_")
    
    title <- grep('<meta name="title" content=".+">', cleaned_up, value = TRUE) 
    title <- gsub('.+content="(.+?)">\\n?', "\\1", title[1])
    
    filename <- paste0(author, "_", title)
    filename <- gsub(" ", "-", filename)
    filename <- gsub("\\t", "-", filename)
    filename <- dissolveSpecialCharactersFilenames(filename)
    if (nchar(filename) > 60) {
      filename <- strtrim(filename, 60)
    }
    filename <- paste0(corpus_dir, "\\", filename, ".xml")
    cleaned <- dissolveSpecialCharacters(cleaned_up, regex_list)
    writeLines(cleaned, filename)

    message("Scraping for ", j, " of ", length(data), " is done!")
    
  }
  name_errors <- paste0("errors_scraping_", x, ".rds")
  saveRDS(error_urls_scraping, name_errors)
  file_name <- gsub("(.+/)(.+?\\.rds)", "\\2", lists[x])
  renameFile(from = file_name,
             to = paste0(indeces_done_dir, "\\", file_name))
  pause <- sample(10:15)[1]
  message("Pausing for ", pause, " seconds, then starting with the next set
          of indeces.")
  Sys.sleep(pause) 
}

message("Scraping is done!")


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#