############## Figures: 2.3.2 Canon-Conscious Corpus Compilation ############### 
#------------------------------------------------------------------------------#

# --- Last edited: 2024-10-07 -------------------------------------------------#

#------------------------------------------------------------------------------#
## Required packages ----------------------------------------------------------#
#------------------------------------------------------------------------------#

library(ggplot2)
library(ggpubr)
library(broom)
library(ggridges)
library(ggExtra)
library(gridExtra)
library(ggthemes)
library(magrittr)

#------------------------------------------------------------------------------#
## Data import ----------------------------------------------------------------#
#------------------------------------------------------------------------------#

#### Figure 8: Two-dimensional temporal component of literary histories -------#

### Covered Years -------------------------------------------------------------#

## German ---------------------------------------------------------------------#

setwd("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\literary_histories\\GER")

covered_years_ger <- read.csv("covered_years.csv", sep = ";", encoding = "UTF-8")

covered_years_vec_ger <- c()

for (i in 1:nrow(covered_years_ger)) {
  count <- sum(covered_years_ger[i, -1], na.rm = TRUE)
  covered_years_vec_ger <- append(covered_years_vec_ger, rep.int(covered_years_ger[i, 1], count))
}

df_ger <- data.frame(years = covered_years_vec_ger)
df_ger$lang <- "GER"

## English --------------------------------------------------------------------#

setwd("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\literary_histories\\ENG")

covered_years_eng <- read.csv("covered_years.csv", sep = ";", encoding = "UTF-8")

covered_years_vec_eng <- c()

for (i in 1:nrow(covered_years_eng)) {
  count <- sum(covered_years_eng[i, -1], na.rm = TRUE)
  covered_years_vec_eng <- append(covered_years_vec_eng, rep.int(covered_years_eng[i, 1], count))
}

df_eng <- data.frame(years = covered_years_vec_eng)
df_eng$lang <- "ENG"

df <- rbind(df_ger, df_eng)

## Plotting -------------------------------------------------------------------#

p_coveredyears <- ggplot(df, aes(x = years)) +
                  geom_histogram(bins = 40) +
                  facet_grid(. ~ lang) +
                  theme_par() +
                  theme(plot.margin = unit(c(1,5,1,1), 'pt')) +
                  xlab("Years covered in literary histories") +
                  ylab("Num. of secondary sources")

ggsave("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\literary_histories\\coveredYears.png", dpi = 360,
              width = 10, height = 10, units = "cm")

### Publication Years ---------------------------------------------------------#

## German ---------------------------------------------------------------------#

setwd("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\literary_histories\\GER")

pub_years_ger <- read.csv("pub_years.csv", sep = ";", encoding = "UTF-8")

pub_years_vec_ger <- c()

for (i in 1:nrow(pub_years_ger)) {
  count <- sum(pub_years_ger[i, -1], na.rm = TRUE)
  pub_years_vec_ger <- append(pub_years_vec_ger, rep.int(pub_years_ger[i, 1], count))
}

df_ger <- data.frame(years = pub_years_vec_ger)
df_ger$lang <- "GER"

## English --------------------------------------------------------------------#

setwd("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\literary_histories\\ENG")

pub_years_eng <- read.csv("pub_years.csv", sep = ";", encoding = "UTF-8")

pub_years_vec_eng <- c()

for (i in 1:nrow(pub_years_eng)) {
  count <- sum(pub_years_eng[i, -1], na.rm = TRUE)
  pub_years_vec_eng <- append(pub_years_vec_eng, rep.int(pub_years_eng[i, 1], count))
}

df_eng <- data.frame(years = pub_years_vec_eng)
df_eng$lang <- "ENG"

df <- rbind(df_ger, df_eng)

## Plotting -------------------------------------------------------------------#

p_pubyears <- ggplot(df, aes(x = years)) +
              geom_histogram(bins = 40) +
              facet_grid(. ~ lang) +
              scale_colour_grey() +
              theme_par() +
              theme(plot.margin = unit(c(1,5,1,1), 'pt')) +
              xlab("Publication years of literary histories") +
              ylab("Num. of secondary sources")

ggsave("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\literary_histories\\pubYears.png", dpi = 360,
              width = 10, height = 10, units = "cm")

figure <- ggarrange(p_coveredyears, p_pubyears,
                    ncol = 1, nrow = 2, align = "hv")

ggsave("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\literary_histories\\yearsCombinedPlot.png", 
              figure, dpi = 360, width = 16, height = 16, units = "cm")

#### Figure 9: Distribution of texts over time --------------------------------#

corpus_list_eng <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\corpora\\ENG_corpus.csv", 
                            sep = ";", encoding = "UTF-8")
years_eng <- as.data.frame(corpus_list_eng$pub_year)
years_eng$lang <- "ENG"
names(years_eng) <- c("pub_year", "lang")

corpus_list_ger <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\corpora\\GER_corpus.csv", 
                            sep = ";", encoding = "UTF-8")
years_ger <- as.data.frame(corpus_list_ger$pub_year)
years_ger$lang <- "GER"
names(years_ger) <- c("pub_year", "lang")

d <- rbind(years_eng, years_ger)

p_pubyears <- ggplot(d, aes(x = pub_year)) +
              geom_histogram(bins = 150) +
              facet_grid(. ~ lang) +
              scale_colour_grey() +
              theme_par() +
              theme(plot.margin = unit(c(1,5,1,1), 'pt')) +
              xlab("Publication years of corpus texts") +
              ylab("Num. of texts") +
              xlim(1685,1920)


p_pubyears <- ggplot(d, aes(x = pub_year, group = lang, fill = lang)) +
              geom_density(alpha = 0.5) +
              scale_fill_manual(values=c("#040404", "#eae3e3"), name = NULL) +
              theme_par() +
              theme(plot.margin = unit(c(1,5,1,1), 'pt')) +
              xlab("Publication years of corpus texts") +
              ylab("Density of num. of texts") +
              xlim(1685,1920) 

ggsave("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\corpora\\publicationYearsCombinedPlot.png", 
              dpi = 360, width = 16, height = 8, units = "cm")


#### Figure 10: Distribution of mentions --------------------------------------#

### Mentions ------------------------------------------------------------------#

mentions_eng <- as.data.frame(corpus_list_eng$freq)
mentions_eng$lang <- "ENG"
names(mentions_eng) <- c("mentions", "lang")

length(which(mentions_eng$mentions <= 5))
length(which(mentions_eng$mentions >= 50))

mentions_ger <- as.data.frame(corpus_list_ger$freq)
mentions_ger$lang <- "GER"
names(mentions_ger) <- c("mentions", "lang")

length(which(mentions_ger$mentions <= 5))
length(which(mentions_ger$mentions >= 50))

d <- rbind(mentions_eng, mentions_ger)

p_mentions <- ggplot(d, aes(x = mentions)) +
  geom_histogram(binwidth = 1) +
  facet_grid(. ~ lang) +
  scale_fill_grey() +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt')) +
  xlab("Num. of mentions") +
  ylab("Num. of corpus texts") 


ggsave("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\corpora\\mentionsPlot.png", 
              dpi = 360, width = 16, height = 8, units = "cm")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
