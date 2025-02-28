##################### Modularity vs canonisation/reception ##################### 
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
library(vcd)
library(reshape2)
library(cowplot)

#------------------------------------------------------------------------------#
## Canonisation ---------------------------------------------------------------#
#------------------------------------------------------------------------------#

# English ---------------------------------------------------------------------#

setwd("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\networks")

modularity_ENG <- read.csv("doc2vec_adjacency_ENG_backbone_3-9_louvain_centrality.csv")
modularity_ENG$louvain <- factor(modularity_ENG$louvain, 
                                 levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
modularity_ENG$class <- factor(modularity_ENG$class, 
                                 levels =  c("No Review & No Catalogue", "No Review, Catalogue",
                                             "Review, No Catalogue", "Review & Catalogue"))

mean_canonisation_by_cluster <- modularity_ENG %>%
  group_by(louvain) %>%
  summarise(mean_canonisation_score = mean(canonisation_score, na.rm = TRUE))

print(mean_canonisation_by_cluster)

anova <- aov(canonisation_score ~ louvain, data = modularity_ENG)
summary(anova)
tukey <- TukeyHSD(anova)
print(tukey)

df <- as.data.frame(tukey$louvain)
df$louvain1 <- gsub("(\\d+)-(\\d+)", "\\1", rownames(df))
df$louvain2 <- gsub("(\\d+)-(\\d+)", "\\2", rownames(df))

modularity_ENG <- modularity_ENG %>%
  mutate(Score.Range = case_when(
    canonisation_score >= 0 & canonisation_score <= 0.25 ~ 'Low (0-0.25)',
    canonisation_score > 0.25 & canonisation_score <= 0.5 ~ 'Mid-Low (0.25-0.5)',
    canonisation_score > 0.5 & canonisation_score <= 0.75 ~ 'Mid-High (0.5-0.75)',
    canonisation_score > 0.75 & canonisation_score <= 1.0 ~ 'High (0.75-1.0)'
  ))

modularity_ENG$Score.Range <- factor(modularity_ENG$Score.Range, 
                                     levels = c('Low (0-0.25)', 'Mid-Low (0.25-0.5)',
                                                'Mid-High (0.5-0.75)', 'High (0.75-1.0)'))

tbl <- table(modularity_ENG$Score.Range, modularity_ENG$louvain)


# Perform the chi-square test
chisq <- chisq.test(modularity_ENG$Score.Range, modularity_ENG$louvain)

# Extract residuals and convert to data frame
residuals_df <- as.data.frame(as.table(chisq$residuals))

# Rename columns for easier understanding
colnames(residuals_df) <- c("Score.Range", "Louvain", "Residuals")

residuals_df <- residuals_df %>%
  mutate(annotation = case_when(
    Residuals > 2 ~ "+",  # Define what counts as "high" residuals
    Residuals < -2 ~ "-", # Define what counts as "low" residuals
    TRUE ~ ""            # No annotation for neutral residuals
  ))

# Create the heatmap with annotations
plot_eng_canonisation <- ggplot(residuals_df, aes(x = Score.Range, y = Louvain, fill = Residuals)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(midpoint = 0, low = "black", high = "black", mid = "white") +
  geom_text(aes(label = annotation), color = "white", size = 6) +  # Add text annotation
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),
        legend.position = "none",) +
  labs(x = "", y = "Cluster")

# German ----------------------------------------------------------------------#

setwd("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\networks")

modularity_GER <- read.csv("doc2vec_adjacency_GER_backbone_3-9_louvain_centrality.csv")
modularity_GER$louvain <- factor(modularity_GER$louvain, 
                                 levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))
modularity_GER$class <- factor(modularity_GER$class, 
                               levels =  c("No Review & No Catalogue", "No Review, Catalogue",
                                           "Review, No Catalogue", "Review & Catalogue"))

mean_canonisation_by_cluster <- modularity_GER %>%
  group_by(louvain) %>%
  summarise(mean_canonisation_score = mean(canonisation_score, na.rm = TRUE))

anova <- aov(canonisation_score ~ louvain, data = modularity_GER)
summary(anova)
tukey <- TukeyHSD(anova)
print(tukey)

options(scipen = 100, digits = 4)

df <- as.data.frame(tukey$louvain)
df$louvain1 <- gsub("(\\d+)-(\\d+)", "\\1", rownames(df))
df$louvain2 <- gsub("(\\d+)-(\\d+)", "\\2", rownames(df))

modularity_GER <- modularity_GER %>%
  mutate(Score.Range = case_when(
    canonisation_score >= 0 & canonisation_score <= 0.25 ~ 'Low (0-0.25)',
    canonisation_score > 0.25 & canonisation_score <= 0.5 ~ 'Mid-Low (0.25-0.5)',
    canonisation_score > 0.5 & canonisation_score <= 0.75 ~ 'Mid-High (0.5-0.75)',
    canonisation_score > 0.75 & canonisation_score <= 1.0 ~ 'High (0.75-1.0)'
  ))

modularity_GER$Score.Range <- factor(modularity_GER$Score.Range, 
                                     levels = c('Low (0-0.25)', 'Mid-Low (0.25-0.5)',
                                                'Mid-High (0.5-0.75)', 'High (0.75-1.0)'))

tbl <- table(modularity_GER$Score.Range, modularity_GER$louvain)

# Perform the chi-square test
chisq <- chisq.test(modularity_GER$Score.Range, modularity_GER$louvain)

# Extract residuals and convert to data frame
residuals_df <- as.data.frame(as.table(chisq$residuals))

# Rename columns for easier understanding
colnames(residuals_df) <- c("Score.Range", "Louvain", "Residuals")

residuals_df <- residuals_df %>%
  mutate(annotation = case_when(
    Residuals > 2 ~ "+",  # Define what counts as "high" residuals
    Residuals < -2 ~ "-", # Define what counts as "low" residuals
    TRUE ~ ""            # No annotation for neutral residuals
  ))

# Create the heatmap with annotations
plot_ger_canonisation <- ggplot(residuals_df, aes(x = Score.Range, y = Louvain, fill = Residuals)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(midpoint = 0, low = "black", high = "black", mid = "white") +
  geom_text(aes(label = annotation), color = "white", size = 6) +  # Add text annotation
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),
        legend.position = "none",) +
  labs(x = "", y = "Cluster")

#------------------------------------------------------------------------------#
## Reception ------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# English ---------------------------------------------------------------------#

tbl <- table(modularity_ENG$class, modularity_ENG$louvain)

# Perform the chi-square test
chisq <- chisq.test(modularity_ENG$class, modularity_ENG$louvain)

# Extract residuals and convert to data frame
residuals_df <- as.data.frame(as.table(chisq$residuals))

# Rename columns for easier understanding
colnames(residuals_df) <- c("Class", "Louvain", "Residuals")

residuals_df <- residuals_df %>%
  mutate(annotation = case_when(
    Residuals > 2 ~ "+",  # Define what counts as "high" residuals
    Residuals < -2 ~ "-", # Define what counts as "low" residuals
    TRUE ~ ""            # No annotation for neutral residuals
  ))

# Create the heatmap with annotations
plot_eng_reception <- ggplot(residuals_df, aes(x = Class, y = Louvain, fill = Residuals)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(midpoint = 0, low = "black", high = "black", mid = "white") +
  geom_text(aes(label = annotation), color = "white", size = 6) +  # Add text annotation
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),
        legend.position = "none",) +
  labs(x = "", y = "Cluster")

# German ----------------------------------------------------------------------#

tbl <- table(modularity_GER$class, modularity_GER$louvain)

# Compute Cramér's V
assoc_stats <- assocstats(tbl)

# Perform the chi-square test
chisq <- chisq.test(modularity_GER$class, modularity_GER$louvain)

# Extract residuals and convert to data frame
residuals_df <- as.data.frame(as.table(chisq$residuals))

# Rename columns for easier understanding
colnames(residuals_df) <- c("Class", "Louvain", "Residuals")

residuals_df <- residuals_df %>%
  mutate(annotation = case_when(
    Residuals > 2 ~ "+",  # Define what counts as "high" residuals
    Residuals < -2 ~ "-", # Define what counts as "low" residuals
    TRUE ~ ""            # No annotation for neutral residuals
  ))

# Create the heatmap with annotations
plot_ger_reception <- ggplot(residuals_df, aes(x = Class, y = Louvain, fill = Residuals)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(midpoint = 0, low = "black", high = "black", mid = "white") +
  geom_text(aes(label = annotation), color = "white", size = 6) +  # Add text annotation
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),
        legend.position = "none",) +
  labs(x = "", y = "Cluster")

#------------------------------------------------------------------------------#
## Save as png ----------------------------------------------------------------#
#------------------------------------------------------------------------------#

setwd("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\networks")

plot_grid(plot_eng_canonisation, plot_eng_reception, nrow = 2, align = "v")

ggsave("residuals_heatmap_ENG.png", 
       dpi = 360, height = 6, width = 16, units = "cm")

setwd("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\networks")

plot_grid(plot_ger_canonisation, plot_ger_reception, nrow = 2, align = "v")

ggsave("residuals_heatmap_GER.png", 
       dpi = 360, height = 6, width = 16, units = "cm")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
