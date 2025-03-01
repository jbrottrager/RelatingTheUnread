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
library(dplyr)
library(ggthemes)
library(magrittr)
library(stringr)
library(ggdist)  
library(ggridges)   
library(gghalves) 
library(ggbeeswarm)

#------------------------------------------------------------------------------#
## ANOVA ----------------------------------------------------------------------#
#------------------------------------------------------------------------------#

data_eng <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\scores\\reception_scores\\reception_scores_classes_ENG.csv",
                      sep = ",", row.names = 1)

data_eng$class <- factor(data_eng$class, levels =  c("No Review & No Catalogue", "No Review, Catalogue",
                                                     "Review, No Catalogue", "Review & Catalogue"))
data_eng$lang <- "ENG"

anova <- aov(canonisation_score ~ class, data = data_eng)
summary(anova)
tukey <- TukeyHSD(anova)
print(tukey)


data_ger <-  read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\scores\\reception_scores\\reception_scores_classes_GER.csv",
                      sep = ",", row.names = 1)

data_ger$class <- factor(data_ger$class, levels =  c("No Review & No Catalogue", "No Review, Catalogue",
                                                     "Review, No Catalogue", "Review & Catalogue"))

data_ger$lang <- "GER"


anova <- aov(canonisation_score ~ class, data = data_ger)
summary(anova)
tukey <- TukeyHSD(anova)
print(tukey)

combined_data <- rbind(data_eng, data_ger)

combined_data$class <- str_replace_all(combined_data$class, ", ", "\n")
combined_data$class <- str_replace_all(combined_data$class, " & ", "\n")
combined_data$class <- factor(combined_data$class, 
                              levels = c("No Review\nNo Catalogue", "No Review\nCatalogue",
                                         "Review\nNo Catalogue", "Review\nCatalogue"))

combined_data <- subset(combined_data, select = -c(circl_binary, circl_sum,
                                                   reviews_binary, reviews_sum))

ggplot(combined_data, aes(x = class, y = canonisation_score, group = class)) +
  geom_jitter(width = .25, alpha = 0.7, size = 2,
              shape = 21, fill = "white") +
  facet_wrap(vars(lang), nrow = 1, scales = "free") +
  labs(title = "ENG",
       x = "Reception classes",
       y = "Embedding scores"
  ) +
  geom_boxplot(outlier.shape = NA, alpha = 0) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\scores\\", 
              Sys.Date(), "_boxplots_canonisation_reception.png"), 
       dpi = 360, width = 24, height = 16, units = "cm")


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#