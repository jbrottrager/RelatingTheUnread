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
library(broom)

#------------------------------------------------------------------------------#
## Canonisation ---------------------------------------------------------------#
#------------------------------------------------------------------------------#

# English ---------------------------------------------------------------------#

setwd("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist")

nearest_neighbours_ENG <- read.csv("ENG\\networks\\nearest_neighbours_3_decay=0.01_df_ENG_centrality.csv")

meta_ENG <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\corpora\\ENG_corpus.csv", sep = ";")
meta_ENG <- subset(meta_ENG, select = c(pub_year, wikiname, wikiID))
meta_ENG$Text <- paste0(meta_ENG$wikiname, "_", meta_ENG$wikiID)

data_ENG <- merge(nearest_neighbours_ENG, meta_ENG, by = "Text")
summary(data_ENG$indegree)

data_ENG$class <- factor(data_ENG$class, 
                         levels = c("No Review & No Catalogue", "No Review, Catalogue",
                                    "Review, No Catalogue", "Review & Catalogue"))
bins <- c(0, 0.25, 0.5, 0.75, 1.0)
labels <- c("Low (0-0.25)", "Mid-Low (0.25-0.5)", "Mid-High (0.5-0.75)", "High (0.75-1.0)")

data_ENG <- data_ENG %>%
  mutate(score_range = cut(canonisation_score, breaks = bins, labels = labels, include.lowest = TRUE))

data_ENG$score_range <- factor(data_ENG$score_range, 
                               levels = c("Low (0-0.25)", "Mid-Low (0.25-0.5)", 
                                          "Mid-High (0.5-0.75)", "High (0.75-1.0)"))

cor(data_ENG$pub_year, data_ENG$indegree)
cor.test(data_ENG$pub_year, data_ENG$indegree)$p.value

yearly_counts_ENG <- data_ENG %>%
  group_by(pub_year) %>%
  summarise(num_texts = n(), avg_indegree = mean(indegree), total_indegree = sum(indegree))

cor(yearly_counts_ENG$num_texts, yearly_counts_ENG$avg_indegree)
cor.test(yearly_counts_ENG$num_texts, yearly_counts_ENG$avg_indegree)$p.value
cor(yearly_counts_ENG$num_texts, yearly_counts_ENG$total_indegree)
cor.test(yearly_counts_ENG$num_texts, yearly_counts_ENG$total_indegree)$p.value

cor(data_ENG$canonisation_score, data_ENG$indegree)
cor.test(data_ENG$canonisation_score, data_ENG$indegree)

anova_result <- aov(indegree ~ score_range, data = data_ENG)
summary(anova_result)
TukeyHSD(anova_result)

anova_result <- aov(indegree ~ class, data = data_ENG)
summary(anova_result)
TukeyHSD(anova_result)

data_ENG %>%
  group_by(class) %>%
  summarise(avg_indegree = mean(indegree))

threshold <- quantile(data_ENG$indegree, 0.99)
top_5_percent <- data_ENG[data_ENG$indegree >= threshold, ]
summary(top_5_percent)

data_ENG$circl_binary <- factor(data_ENG$circl_binary, levels = c(0,1))
data_ENG$reviews_binary <- factor(data_ENG$reviews_binary, levels = c(0,1))

lm_multiple_result <- lm(indegree ~ canonisation_score + circl_binary + reviews_binary, data = data_ENG)
summary(lm_multiple_result)

coefs_ENG <- tidy(lm_multiple_result)
coefs_ENG$lang <- "ENG"

# German ----------------------------------------------------------------------#

nearest_neighbours_GER <- read.csv("GER\\networks\\nearest_neighbours_3_decay=0.01_df_GER_centrality.csv", encoding = "UTF-8")

meta_GER <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\corpora\\GER_corpus.csv", sep = ";")
meta_GER <- subset(meta_GER, select = c(pub_year, wikiname, wikiID))
meta_GER$Text <- paste0(meta_GER$wikiname, "_", meta_GER$wikiID)

data_GER <- merge(nearest_neighbours_GER, meta_GER, by = "Text")
summary(data_GER$indegree)

data_GER$class <- factor(data_GER$class, 
                         levels = c("No Review & No Catalogue", "No Review, Catalogue",
                                    "Review, No Catalogue", "Review & Catalogue"))
bins <- c(0, 0.25, 0.5, 0.75, 1.0)
labels <- c("Low (0-0.25)", "Mid-Low (0.25-0.5)", "Mid-High (0.5-0.75)", "High (0.75-1.0)")

data_GER <- data_GER %>%
  mutate(score_range = cut(canonisation_score, breaks = bins, labels = labels, include.lowest = TRUE))

data_GER$score_range <- factor(data_GER$score_range, 
                               levels = c("Low (0-0.25)", "Mid-Low (0.25-0.5)", 
                                          "Mid-High (0.5-0.75)", "High (0.75-1.0)"))

cor(data_GER$pub_year, data_GER$indegree)
cor.test(data_GER$pub_year, data_GER$indegree)$p.value

yearly_counts_GER <- data_GER %>%
  group_by(pub_year) %>%
  summarise(num_texts = n(), avg_indegree = mean(indegree), total_indegree = sum(indegree))

cor(yearly_counts_GER$num_texts, yearly_counts_GER$avg_indegree)
cor.test(yearly_counts_GER$num_texts, yearly_counts_GER$avg_indegree)$p.value
cor(yearly_counts_GER$num_texts, yearly_counts_GER$total_indegree)
cor.test(yearly_counts_GER$num_texts, yearly_counts_GER$total_indegree)$p.value

cor(data_GER$canonisation_score, data_GER$indegree)
cor.test(data_GER$canonisation_score, data_GER$indegree)

anova_result <- aov(indegree ~ score_range, data = data_GER)
summary(anova_result)
TukeyHSD(anova_result)

anova_result <- aov(indegree ~ class, data = data_GER)
summary(anova_result)
TukeyHSD(anova_result)

data_GER %>%
  group_by(class) %>%
  summarise(avg_indegree = mean(indegree))

threshold <- quantile(data_GER$indegree, 0.99)
top_5_percent <- data_GER[data_GER$indegree >= threshold, ]
summary(top_5_percent)

data_GER$circl_binary <- factor(data_GER$circl_binary, levels = c(0,1))
data_GER$reviews_binary <- factor(data_GER$reviews_binary, levels = c(0,1))

lm_multiple_result <- lm(indegree ~ canonisation_score + circl_binary + reviews_binary, data = data_GER)
summary(lm_multiple_result)

coefs_GER <- tidy(lm_multiple_result)
coefs_GER$lang <- "GER"


# Plotting --------------------------------------------------------------------#

yearly_counts_ENG$lang <- "ENG"
yearly_counts_GER$lang <- "GER"
yearly_counts <- rbind(yearly_counts_ENG, yearly_counts_GER)

ggplot(yearly_counts, aes(x = num_texts, y = total_indegree)) +
  geom_point() +
  facet_wrap(vars(lang), nrow = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey10") +
  labs(x = "Number of Texts Published", y = "Average Indegree", title = "Correlation Between Text Count and Indegree") +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

coefs <- rbind(coefs_ENG, coefs_GER)

ggplot(coefs, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(size = 3) +
  facet_grid(. ~ lang) +
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, 
                     xmax = estimate + 1.96 * std.error), height = 0.2) +
  theme_minimal() +
  labs(x = "Estimate", y = "Predictor", title = "Regression Coefficients") +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt')) +
  scale_y_discrete(labels = c(
    "(Intercept)" = "Intercept",
    "canonisation_score" = "Canonisation score",
    "circl_binary1" = "Has catalogue entry",
    "reviews_binary1" = "Has reviews"
  ))


data_ENG <- subset(data_ENG, select = c(Text, indegree, class, score_range, pub_year))
data_ENG$lang <- "ENG"
data_GER <- subset(data_GER, select = c(Text, indegree, class, score_range, pub_year))
data_GER$lang <- "GER"

data_all <- rbind(data_ENG, data_GER)

data_ENG$class <- str_replace_all(data_ENG$class, ", ", "\n")
data_ENG$class <- str_replace_all(data_ENG$class, " & ", "\n")
data_ENG$class <- factor(data_ENG$class, 
                         levels = c("No Review\nNo Catalogue", "No Review\nCatalogue",
                                         "Review\nNo Catalogue", "Review\nCatalogue"))
data_ENG$score_range <- str_replace_all(data_ENG$score_range, " ", "\n")
data_ENG$score_range <- factor(data_ENG$score_range, 
                         levels = c("Low\n(0-0.25)", "Mid-Low\n(0.25-0.5)",
                                    "Mid-High\n(0.5-0.75)", "High\n(0.75-1.0)"))

p1 <- ggplot(data_ENG, aes(x = score_range, y = indegree)) +
  geom_boxplot() +
  labs(
    x = "Canonisation",
    y = "Indegree"
  ) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

p2 <- ggplot(data_ENG, aes(x = class, y = indegree)) +
  geom_boxplot() +
  labs(
    x = "Reception",
    y = "Indegree"
  ) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

plot_grid(p1, p2)

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\", 
              Sys.Date(), "_boxplots_indegree_ENG.png"), 
       dpi = 360, width = 24, height = 16, units = "cm")

data_GER$class <- str_replace_all(data_GER$class, ", ", "\n")
data_GER$class <- str_replace_all(data_GER$class, " & ", "\n")
data_GER$class <- factor(data_GER$class, 
                         levels = c("No Review\nNo Catalogue", "No Review\nCatalogue",
                                    "Review\nNo Catalogue", "Review\nCatalogue"))
data_GER$score_range <- str_replace_all(data_GER$score_range, " ", "\n")
data_GER$score_range <- factor(data_GER$score_range, 
                               levels = c("Low\n(0-0.25)", "Mid-Low\n(0.25-0.5)",
                                          "Mid-High\n(0.5-0.75)", "High\n(0.75-1.0)"))

p1 <- ggplot(data_GER, aes(x = score_range, y = indegree)) +
  geom_boxplot() +
  labs(
    x = "Canonisation",
    y = "Indegree"
  ) +
  theme_par() +
  scale_y_continuous(breaks = c(0, 5, 10, 15)) +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

p2 <- ggplot(data_GER, aes(x = class, y = indegree)) +
  geom_boxplot() +
  labs(
    x = "Reception",
    y = "Indegree"
  ) +
  theme_par() +
  scale_y_continuous(breaks = c(0, 5, 10, 15)) +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

plot_grid(p1, p2)

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\", 
              Sys.Date(), "_boxplots_indegree_GER.png"), 
       dpi = 360, width = 24, height = 16, units = "cm")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
