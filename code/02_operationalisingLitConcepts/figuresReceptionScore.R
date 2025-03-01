####################### Figures: 5.1. Rolling Centroids ######################## 
#------------------------------------------------------------------------------#

# --- Last edited: 2024-12-02 -------------------------------------------------#

#------------------------------------------------------------------------------#
## Required packages ----------------------------------------------------------#
#------------------------------------------------------------------------------#

library(ggplot2)
library(ggpubr)
library(broom)
library(ggridges)
library(ggExtra)
library(gridExtra)
library(tidyr)
library(dplyr)
library(ggthemes)
library(magrittr)
library(reshape2)

#------------------------------------------------------------------------------#
## Data prep ------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### German --------------------------------------------------------------------#

setwd("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\scores\\reception_scores")

canonisation_scores_ger <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\scores\\canonisation_scores\\GER\\GER_canonisationScore.csv",
                                sep = ",", encoding = "UTF-8")
canonisation_scores_ger <- subset(canonisation_scores_ger, select = c("ID", "canonisation_score"))

meta_ger <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\scores\\canonisation_scores\\GER\\GER_corpus_meta.csv",
                     sep = ";", encoding = "UTF-8")
meta_ger$ID <- paste0(meta_ger$wikiname, "_", meta_ger$wikiID)
ids_ger <- subset(meta_ger, select = c(wikiname, ID))

circl_ger <- read.csv("circulatinglibraries\\GER\\GER_CIRCULATINGLIBRARIES.csv",
                       sep = ";", encoding = "UTF-8")
circl_ger <- merge(circl_ger, ids_ger, by = "wikiname", all = TRUE)
circl_ger_binary <- subset(circl_ger, select = c(ID, binary, sum_libraries))
names(circl_ger_binary) <- c("ID", "circl_binary", "circl_sum")

reviews_ger <- read.csv("reviews\\GER\\GER_REVIEWS.csv",
                        sep = ";", encoding = "UTF-8")
reviews_ger <- merge(reviews_ger, ids_ger, by = "wikiname", all = TRUE)
reviews_ger_binary <- subset(reviews_ger, select = c(ID, binary, sum_reviews))
names(reviews_ger_binary) <- c("ID", "reviews_binary", "reviews_sum")

data_ger <- merge(circl_ger_binary, reviews_ger_binary, by = "ID")

data_ger <- merge(data_ger, canonisation_scores_ger, by = "ID",
                  all.y = TRUE)

data_ger <- data_ger %>%
  mutate(
    class = case_when(
      circl_binary == 1 & reviews_binary == 1 ~ "Review & Catalogue",
      circl_binary == 0 & reviews_binary == 1 ~ "Review, No Catalogue",
      circl_binary == 1 & reviews_binary == 0 ~ "No Review, Catalogue",
      circl_binary == 0 & reviews_binary == 0 ~ "No Review & No Catalogue"
    )
  )

write.csv(data_ger, "C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\scores\\reception_scores\\reception_scores_classes_GER.csv")

data_ger_subset <- subset(data_ger, select = -c(circl_binary, circl_sum,
                                         reviews_binary, reviews_sum))

ggplot(data_ger_subset, aes(x = class, y = canonisation_score, group = class)) +
  geom_boxplot() +
  labs(title = "ENG",
       x = "Reception classes",
       y = "Canonisation scores"
  ) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))


df_summary_ger <- data_ger %>%
  group_by(class) %>%
  summarise(
    count = n(),
    percentage = (n() / nrow(data_ger)) * 100,  # Calculate percentage
    mean_score = mean(canonisation_score, na.rm = TRUE),  # Calculate mean score
    sd_score = sd(canonisation_score, na.rm = TRUE),  # Calculate standard deviation
    .groups = "drop"
  )


df_summary_ger <- df_summary_ger %>%
  mutate(
    x = case_when(
      class == "Review & Catalogue" ~ "Catalogue Entry",
      class == "Review, No Catalogue" ~ "No Catalogue Entry",
      class == "No Review, Catalogue" ~ "Catalogue Entry",
      class == "No Review & No Catalogue" ~ "No Catalogue Entry"
    ),
    y = case_when(
      class == "Review & Catalogue" ~ "Has Review",
      class == "Review, No Catalogue" ~ "Has Review",
      class == "No Review, Catalogue" ~ "No Review",
      class == "No Review & No Catalogue" ~ "No Review"
    )
  )

df_summary_ger$x <- factor(df_summary_ger$x, levels = c("No Catalogue Entry", "Catalogue Entry"))
df_summary_ger$y <- factor(df_summary_ger$y, levels = c("No Review", "Has Review"))

df_summary_ger$lang <- "GER"

### English -------------------------------------------------------------------#

canonisation_scores_eng <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\scores\\canonisation_scores\\ENG\\ENG_canonisationScore.csv",
                                    sep = ",")
canonisation_scores_eng <- subset(canonisation_scores_eng, select = c("ID", "canonisation_score"))

meta_eng <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\scores\\canonisation_scores\\ENG\\ENG_corpus_meta.csv",
                     sep = ";", encoding = "UTF-8")
meta_eng$ID <- paste0(meta_eng$wikiname, "_", meta_eng$wikiID)
ids_eng <- subset(meta_eng, select = c(wikiname, ID))

circl_eng <- read.csv("circulatinglibraries\\ENG\\ENG_CIRCULATINGLIBRARIES.csv",
                      sep = ";", encoding = "UTF-8")
circl_eng <- merge(circl_eng, ids_eng, by = "wikiname", all = TRUE)
circl_eng_binary <- subset(circl_eng, select = c(ID, binary, sum_libraries))
names(circl_eng_binary) <- c("ID", "circl_binary", "circl_sum")

reviews_eng <- read.csv("reviews\\ENG\\ENG_REVIEWS.csv",
                        sep = ";", encoding = "UTF-8")
reviews_eng <- merge(reviews_eng, ids_eng, by = "wikiname", all = TRUE)
reviews_eng_binary <- subset(reviews_eng, select = c(ID, binary, sum_reviews))
names(reviews_eng_binary) <- c("ID", "reviews_binary", "reviews_sum")

data_eng <- merge(circl_eng_binary, reviews_eng_binary, by = "ID")

data_eng <- merge(data_eng, canonisation_scores_eng, by = "ID")

data_eng <- data_eng %>%
  mutate(
    class = case_when(
      circl_binary == 1 & reviews_binary == 1 ~ "Review & Catalogue",
      circl_binary == 0 & reviews_binary == 1 ~ "Review, No Catalogue",
      circl_binary == 1 & reviews_binary == 0 ~ "No Review, Catalogue",
      circl_binary == 0 & reviews_binary == 0 ~ "No Review & No Catalogue"
    )
  )

write.csv(data_eng, "C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\scores\\reception_scores\\reception_scores_classes_ENG.csv")

df_summary_eng <- data_eng %>%
  group_by(class) %>%
  summarise(
    count = n(),
    percentage = (n() / nrow(data_ger)) * 100,  # Calculate percentage
    mean_score = mean(canonisation_score, na.rm = TRUE),  # Calculate mean score
    sd_score = sd(canonisation_score, na.rm = TRUE),  # Calculate standard deviation
    .groups = "drop"
  )

df_summary_eng <- df_summary_eng %>%
  mutate(
    x = case_when(
      class == "Review & Catalogue" ~ "Catalogue Entry",
      class == "Review, No Catalogue" ~ "No Catalogue Entry",
      class == "No Review, Catalogue" ~ "Catalogue Entry",
      class == "No Review & No Catalogue" ~ "No Catalogue Entry"
    ),
    y = case_when(
      class == "Review & Catalogue" ~ "Has Review",
      class == "Review, No Catalogue" ~ "Has Review",
      class == "No Review, Catalogue" ~ "No Review",
      class == "No Review & No Catalogue" ~ "No Review"
    )
  )

df_summary_eng$x <- factor(df_summary_eng$x, levels = c("No Catalogue Entry", "Catalogue Entry"))
df_summary_eng$y <- factor(df_summary_eng$y, levels = c("No Review", "Has Review"))

df_summary_eng$lang <- "ENG"

#------------------------------------------------------------------------------#
## Plotting -------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# Heatmap ---------------------------------------------------------------------#

df_summary <- rbind(df_summary_eng, df_summary_ger)

ggplot(df_summary, aes(x = x, y = y, fill = percentage)) +
  geom_tile(color = "black", linewidth = 1) +  
  facet_wrap(vars(lang), nrow = 1) +
  scale_fill_gradient(low = "gray100", high = "grey35") +  
  geom_text(aes(label = paste0(round(percentage, 1), "%")), colour = "black", size = 5) +  
  labs(
    fill = "Percentage"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10, colour = "black"),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),  
    axis.title.y = element_blank(),  
    legend.position = "none"
  )

ggsave("distReceptionScores.png", 
       dpi = 360, width = 16, height = 8, units = "cm")

# Boxplots --------------------------------------------------------------------#

subset_ger <- subset(data_ger, select = c("ID", "canonisation_score", "class"))
subset_ger$lang <- "GER"
subset_eng <- subset(data_eng, select = c("ID", "canonisation_score", "class"))
subset_eng$lang <- "ENG"

df_all <- rbind(subset_ger, subset_eng)


ggplot(df_all, aes(x = class, y = canonisation_score)) +
  geom_boxplot() +
  facet_wrap(vars(lang), nrow = 1) +
  labs(
    title = "Distribution of Scores by Class",
    x = "Class",
    y = "Score"
  ) +
  theme_par()

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#