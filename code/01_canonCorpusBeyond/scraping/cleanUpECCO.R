########################### CLEANING UP ECCO SNAPSHOT ########################## 
#------------------------------------------------------------------------------#

### Last updated: 2023-10-13 ###################################################

# Cleans up the ECCO TCP snapshot and adapts the XML structure. 
# --> https://umich.app.box.com/s/7dc9b3b0f859a6b36bc2/folder/3109725483

#------------------------------------------------------------------------------#
## Required Packages ----------------------------------------------------------#
#------------------------------------------------------------------------------#

library(xml2)
library(xslt)

#------------------------------------------------------------------------------#
## Clean-up -------------------------------------------------------------------#
#------------------------------------------------------------------------------#

date <- gsub("\\d\\d(\\d\\d)-(\\d\\d)-(\\d\\d)", "\\1\\2\\3", Sys.Date())

path_output <- paste0("C:\\Users\\Brottrager\\Documents\\Diss\\corpora\\eng\\0_pre_text_pdf\\ecco\\", 
                      date, "_ecco_cleaned_up")

if (!isTRUE(dir.exists(path_output))) {
  dir.create(path_output, recursive = TRUE, showWarnings = FALSE)
}

path_xml <- "C:\\Users\\Brottrager\\Documents\\Diss\\corpora\\eng\\0_pre_text_pdf\\ecco\\P5_snapshot_201502\\P5_snapshot_201502\\Ecco_released_P5_201502"
xml_files <- list.files(path_xml, full.names = TRUE)

xslt <- read_xml("C:\\Users\\Brottrager\\Documents\\Diss\\code\\XSLT\\ecco_xslt_stylesheet.xsl")

for (i in 1:length(xml_files)) {
  xml <- read_xml(xml_files[i])
  title <- as.character(xml_find_all(xml, xpath = "/d1:TEI/d1:teiHeader/d1:fileDesc/d1:titleStmt/d1:title/text()", xml_ns(xml)))
  title <- gsub("(.+?)[\\.,;:].+", "\\1", title[1])
  title <- gsub("(\\[|\\]|\\*)", "", title)
  title <- gsub(" ", "-", title)
  date <- as.character(xml_find_all(xml, xpath = "/d1:TEI/d1:teiHeader/d1:fileDesc/d1:sourceDesc/d1:biblFull/d1:publicationStmt/d1:date/text()", xml_ns(xml)))
  date <- gsub("(.+) \\[.+\\]", "\\1", date)
  date <- gsub("(\\[|\\]|\\*|\\?)", "", date)
  date <- gsub("(\\d{4}).+", "\\1", date)
  title <- gsub('(\\[|\\]|\\*|\\?|")', "", title)
  author <- as.character(xml_find_all(xml, xpath = "/d1:TEI/d1:teiHeader/d1:fileDesc/d1:titleStmt/d1:author/text()", xml_ns(xml)))
  author <- gsub("(.+),? \\d+.+", "\\1", author)
  author <- gsub("(\\[|\\]|\\*|\\.|\\)|\\(|Mrs|Mr|, b\\.)", "", author)
  author <- gsub("(.+),? +(.+)", "\\1_\\2", author)
  author <- gsub("[, ]", "", author)
  update <- xml_xslt(xml, xslt)
  name <- paste0(author, "_", title, "_", date, ".xml")
  name <- gsub("ä", "ae", name)
  name <- gsub("ö", "oe", name)
  name <- gsub("ü", "ue", name)
  name <- gsub("(é|è|ê|ë)", "e", name)
  name <- gsub("ß", "ss", name)
  name <- gsub("('|»|«)", "ae", name)
  write_xml(update, paste0(path_output, "\\", name))
  message(i, " of ", length(xml_files), " done!")
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


