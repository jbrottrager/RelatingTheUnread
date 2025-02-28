####################### Figures: 5.1. Rolling Centroids ######################## 
#------------------------------------------------------------------------------#

# --- Last edited: 2024-12-02 -------------------------------------------------#

#------------------------------------------------------------------------------#
## Required packages ----------------------------------------------------------#
#------------------------------------------------------------------------------#

# Load required libraries
library(ggplot2)
library(ggthemes)
library(cowplot)
library(pheatmap)
library(reshape2)

#------------------------------------------------------------------------------#
## Correlation described and detected -----------------------------------------#
#------------------------------------------------------------------------------#

setwd("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\comparing_systems")

diff_eng <- read.csv("ENG\\diff_matrix.csv")

melted_diff_eng <- melt(diff_eng)
melted_diff_eng$lang <- "ENG"
melted_diff_eng <- melted_diff_eng[melted_diff_eng$value != 0,]

diff_ger <- read.csv("GER\\diff_matrix.csv")

melted_diff_ger <- melt(diff_ger)
melted_diff_ger$lang <- "GER"
melted_diff_ger <- melted_diff_ger[melted_diff_ger$value != 0,]

ggplot() +
  geom_histogram(data = melted_diff_eng, aes(x = value, y = ..count.. / sum(..count..) * 100, fill = "black"), 
                 binwidth = 0.01, alpha = 1, colour = "black", size = 1) +
  geom_histogram(data = melted_diff_ger, aes(x = value, y = ..count.. / sum(..count..) * 100, fill = "white"), 
                 binwidth = 0.01, alpha = 0.6, colour = "black", size = 1) +
  labs(x = "Absolute differences", y = "Percentage (%)") +
  scale_fill_manual(name = "", values=c("black","white"),labels=c("ENG","GER")) +
  ylim(0, 6) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  theme_par() +
  theme(plot.margin = unit(c(1, 5, 1, 1), 'pt'),
        legend.position = c(0.9, 0.8))

ggsave("diff_percentages.png", dpi = 360, height = 10, width = 16, units = "cm")

df_eng <- read.csv("ENG\\canonisation_vs_diff.csv")
df_eng$lang <- "ENG"

cor(df_eng$canonisation_score, df_eng$avg_differences)
cor.test(df_eng$canonisation_score, df_eng$avg_differences)$p.value

df_ger <- read.csv("GER\\canonisation_vs_diff.csv")
df_ger$lang <- "GER"

cor(df_ger$canonisation_score, df_ger$avg_differences)
cor.test(df_ger$canonisation_score, df_ger$avg_differences)$p.value

df <- rbind(df_eng, df_ger)

ggplot(df, aes(x = canonisation_score, y = avg_differences)) +
  geom_point(alpha = 0.5) +
  facet_wrap(vars(lang), nrow = 2) +
  labs(
    x = "Canonisation score",
    y = "Average difference between \ndescribed and detected similarities"
  ) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

library(ggplot2)
library(ggrepel)

# Define the titles you want to highlight
highlighted_titles <- c("DIE_LEIDEN_DES_JUNGEN_WERTHERS_Q151883", 
                        "WAVERLEY_Q678251", 
                        "AGNES_GREY_Q517172",
                        "WILHELM_MEISTERS_WANDERJAHRE_Q478344")  # Replace with actual titles

ggplot(df, aes(x = canonisation_score, y = avg_differences)) +
  geom_point(alpha = 0.5) +  # Base points
  geom_point(
    data = df[df$ID %in% highlighted_titles, ], 
    aes(x = canonisation_score, y = avg_differences), 
    colour = "red", size = 3
  ) +  # Highlighted points
  geom_text_repel(
    data = df[df$ID %in% highlighted_titles, ], 
    aes(label = ID),
    size = 3,
    nudge_y = 0.02, # Adjust for readability
    segment.color = "grey50"
  ) +
  facet_wrap(vars(lang), nrow = 2) +
  labs(
    x = "Canonisation score",
    y = "Average difference between \ndescribed and detected similarities"
  ) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))


ggsave("described_vs_detected.png", dpi = 360, height = 16, width = 16, units = "cm")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#