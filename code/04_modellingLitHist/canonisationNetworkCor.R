######################### Nearest canonised neighbours ######################### 
#------------------------------------------------------------------------------#

# --- Last edited: 2025-01-06 -------------------------------------------------#

#------------------------------------------------------------------------------#
## Required packages ----------------------------------------------------------#
#------------------------------------------------------------------------------#

library(ggplot2)
library(ggthemes)
library(reshape2)
library(stringr)
library(dplyr)
library(cowplot)

#------------------------------------------------------------------------------#
## Canonisation ---------------------------------------------------------------#
#------------------------------------------------------------------------------#

# English ---------------------------------------------------------------------#

nearest_neighbours <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\nearest_neighbours_3_decay=0.01_df_ENG.csv",
                               row.names = 1)

bins <- c(0, 0.25, 0.5, 0.75, 1.0)
labels <- c('Low (0-0.25)', 'Mid-Low (0.25-0.5)', 'Mid-High (0.5-0.75)', 'High (0.75-1.0)')

nearest_neighbours$ScoreRange <- cut(
  nearest_neighbours$CanonisationScore, 
  breaks = bins,        # Specifying the bin boundaries
  labels = labels,      # Assigning the labels to the bins
  include.lowest = TRUE # Ensures the lowest value is included in the first bin
)

nearest_neighbours$ScoreRange <- factor(nearest_neighbours$ScoreRange, 
                                        levels = c('Low (0-0.25)', 'Mid-Low (0.25-0.5)', 
                                                   'Mid-High (0.5-0.75)', 'High (0.75-1.0)'))

cor.test(nearest_neighbours$CanonisationScore, nearest_neighbours$SimilarityScore)

p1 <- ggplot(nearest_neighbours, aes(x = CanonisationScore, y = SimilarityScore)) +
  geom_point(alpha = 0.45) +
  labs(
    x = "Canonisation score",
    y = "Cosine similarity"
  ) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))


p2 <- ggplot(nearest_neighbours, aes(x = ScoreRange, y = SimilarityScore)) +
  geom_boxplot() +
  labs(
    x = "Canonisation score",
    y = "Cosine similarity"
  ) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

# German ----------------------------------------------------------------------#

nearest_neighbours <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\nearest_neighbours_3_decay=0.01_df_GER.csv",
                               row.names = 1, sep = ";")

bins <- c(0, 0.25, 0.5, 0.75, 1.0)
labels <- c('Low (0-0.25)', 'Mid-Low (0.25-0.5)', 'Mid-High (0.5-0.75)', 'High (0.75-1.0)')

nearest_neighbours$ScoreRange <- cut(
  nearest_neighbours$CanonisationScore, 
  breaks = bins,        # Specifying the bin boundaries
  labels = labels,      # Assigning the labels to the bins
  include.lowest = TRUE # Ensures the lowest value is included in the first bin
)

nearest_neighbours$ScoreRange <- factor(nearest_neighbours$ScoreRange, 
                                        levels = c('Low (0-0.25)', 'Mid-Low (0.25-0.5)', 
                                                   'Mid-High (0.5-0.75)', 'High (0.75-1.0)'))

cor.test(nearest_neighbours$CanonisationScore, nearest_neighbours$SimilarityScore)

p7 <- ggplot(nearest_neighbours, aes(x = CanonisationScore, y = SimilarityScore)) +
  geom_point(alpha = 0.45) +
  labs(
    x = "Canonisation score",
    y = "Cosine similarity"
  ) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

p8 <- ggplot(nearest_neighbours, aes(x = ScoreRange, y = SimilarityScore)) +
  geom_boxplot() +
  labs(
    x = "Canonisation score",
    y = "Cosine similarity"
  ) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))


#------------------------------------------------------------------------------#
## Save as png ----------------------------------------------------------------#
#------------------------------------------------------------------------------#

aligned <- align_plots(p1, p2,
                       p7, p8, align = "v")

ggdraw(aligned[[1]])

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\doc2vec\\ENG\\", 
              Sys.Date(), "_scatter_nearest_neighbours_noTimeLimit_df_canonisation=0.75_ENG.png"), 
       dpi = 360, width = 16, height = 16, units = "cm")

ggdraw(aligned[[2]])

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\doc2vec\\ENG\\", 
              Sys.Date(), "_boxplot_nearest_neighbours_noTimeLimit_df_canonisation=0.75_ENG.png"), 
       dpi = 360, width = 16, height = 16, units = "cm")

ggdraw(aligned[[3]])

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\doc2vec\\ENG\\", 
              Sys.Date(), "_scatter_nearest_neighbours_2_noTimeLimit_df_canonisation=0.75_MSQ_ENG.png"), 
       dpi = 360, width = 16, height = 16, units = "cm")

ggdraw(aligned[[4]])

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\doc2vec\\ENG\\", 
              Sys.Date(), "_boxplot_nearest_neighbours_2_noTimeLimit_df_canonisation=0.75_MSQ_ENG.png"), 
       dpi = 360, width = 16, height = 16, units = "cm")

ggdraw(aligned[[5]])

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\doc2vec\\ENG\\", 
              Sys.Date(), "_scatter_nearest_neighbours_5_df_ENG_indegree.png"), 
       dpi = 360, width = 16, height = 16, units = "cm")

ggdraw(aligned[[6]])

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\doc2vec\\ENG\\", 
              Sys.Date(), "_boxplot_nearest_neighbours_5_df_ENG_indegree.png"), 
       dpi = 360, width = 16, height = 16, units = "cm")

ggdraw(aligned[[7]])

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\doc2vec\\GER\\", 
              Sys.Date(), "_scatter_nearest_neighbours_noTimeLimit_df_canonisation=0.75_GER.png"), 
       dpi = 360, width = 16, height = 16, units = "cm")

ggdraw(aligned[[8]])

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\doc2vec\\GER\\", 
              Sys.Date(), "_boxplot_nearest_neighbours_noTimeLimit_df_canonisation=0.75_GER.png"), 
       dpi = 360, width = 16, height = 16, units = "cm")

ggdraw(aligned[[9]])

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\doc2vec\\GER\\", 
              Sys.Date(), "_scatter_nearest_neighbours_2_noTimeLimit_df_canonisation=0.75_MSQ_GER.png"), 
       dpi = 360, width = 16, height = 16, units = "cm")

ggdraw(aligned[[10]])

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\doc2vec\\GER\\", 
              Sys.Date(), "_boxplot_nearest_neighbours_2_noTimeLimit_df_canonisation=0.75_MSQ_GER.png"), 
       dpi = 360, width = 16, height = 16, units = "cm")

ggdraw(aligned[[11]])

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\doc2vec\\GER\\", 
              Sys.Date(), "_scatter_nearest_neighbours_5_df_GER_indegree.png"), 
       dpi = 360, width = 16, height = 16, units = "cm")

ggdraw(aligned[[12]])

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\doc2vec\\GER\\", 
              Sys.Date(), "_boxplot_nearest_neighbours_5_df_GER_indegree.png"), 
       dpi = 360, width = 16, height = 16, units = "cm")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#