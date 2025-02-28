############### Embedding Scores: Correlation and Visualisation ################ 
#------------------------------------------------------------------------------#

# --- Last edited: 2025-01-06 -------------------------------------------------#

#------------------------------------------------------------------------------#
## Required packages ----------------------------------------------------------#
#------------------------------------------------------------------------------#

library(ggplot2)
library(ggthemes)
library(reshape2)
library(stringr)

#------------------------------------------------------------------------------#
## Canonisation ---------------------------------------------------------------#
#------------------------------------------------------------------------------#

# English ---------------------------------------------------------------------#

embedding_scores <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\doc2vec_scores_ENG.csv",
                             sep = ",", row.names = 1)

canonisation_scores <-  read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\scores\\canonisation_scores\\ENG\\ENG_canonisationScore.csv",
                                 sep = ",", row.names = 1)

combined_data <- merge(embedding_scores, canonisation_scores, by.x = "text_id",
                       by.y = "ID")

cor.test(combined_data$canonisation_score, combined_data$intra_textual_variance)
cor.test(combined_data$canonisation_score, combined_data$stepwise_distance)
cor.test(combined_data$canonisation_score, combined_data$outlier_score)
cor.test(combined_data$canonisation_score, combined_data$overlap_score)

combined_melt <- melt(combined_data, id=c("text_id", "canonisation_score"))

labels <- list("intra_textual_variance" = "Intra-textual variance", 
               "stepwise_distance" = "Stepwise distance", 
               "outlier_score" = "Outlier score", 
               "overlap_score" = "Overlap score")

labeller <- function(variable, value) {
  return(labels[value])
}

ggplot(combined_melt, aes(x = canonisation_score, y = value, 
                          group = variable)) +
  geom_point(alpha = 0.45) + 
  facet_wrap(vars(variable), nrow = 2, scales = "free",
             labeller = labeller) +
  labs(
    x = "Canonisation score",
    y = "Embedding score"
  ) +
  ggtitle("ENG") +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\scatterplots_embeddingScores_ENG.png"), 
       dpi = 360, width = 16, height = 16, units = "cm")

# German ----------------------------------------------------------------------#

embedding_scores <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\doc2vec_scores_GER.csv",
                             sep = ",", row.names = 1)

canonisation_scores <-  read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\scores\\canonisation_scores\\GER\\GER_canonisationScore.csv",
                                 sep = ",", row.names = 1)

combined_data <- merge(embedding_scores, canonisation_scores, by.x = "text_id",
                       by.y = "ID")

cor.test(combined_data$canonisation_score, combined_data$intra_textual_variance)
cor.test(combined_data$canonisation_score, combined_data$stepwise_distance)
cor.test(combined_data$canonisation_score, combined_data$outlier_score)
cor.test(combined_data$canonisation_score, combined_data$overlap_score)

combined_melt <- melt(combined_data, id=c("text_id", "canonisation_score"))

labels <- list("intra_textual_variance" = "Intra-textual variance", 
               "stepwise_distance" = "Stepwise distance", 
               "outlier_score" = "Outlier score", 
               "overlap_score" = "Overlap score")

labeller <- function(variable, value) {
  return(labels[value])
}

ggplot(combined_melt, aes(x = canonisation_score, y = value, 
                          group = variable)) +
  geom_point(alpha = 0.45) + 
  facet_wrap(vars(variable), nrow = 2, scales = "free",
             labeller = labeller) +
  labs(
    x = "Canonisation score",
    y = "Embedding scores"
  ) +
  ggtitle("GER") +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\scatterplots_embeddingScores_GER.png"), 
       dpi = 360, width = 16, height = 16, units = "cm")


#------------------------------------------------------------------------------#
## Reception ------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# English ---------------------------------------------------------------------#

embedding_scores <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\doc2vec_scores_ENG.csv",
                             sep = ",", row.names = 1)

reception_scores <-  read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\scores\\reception_scores\\reception_scores_classes_ENG.csv",
                                 sep = ",", row.names = 1)

combined_data <- merge(embedding_scores, reception_scores, by.x = "text_id",
                       by = "ID")

combined_data$class <- str_replace_all(combined_data$class, ", ", "\n")
combined_data$class <- str_replace_all(combined_data$class, " & ", "\n")
combined_data$class <- factor(combined_data$class, 
                              levels = c("No Review\nNo Catalogue", "No Review\nCatalogue",
                                         "Review\nNo Catalogue", "Review\nCatalogue"))
combined_data$circl_binary <- factor(combined_data$circl_binary, levels = c(0, 1))
combined_data$reviews_binary <- factor(combined_data$reviews_binary, levels = c(0, 1))

anova_intra_textual_variance <- aov(intra_textual_variance ~ class, data = combined_data)
summary(anova_intra_textual_variance)
tukey_result_intra_textual_variance <- TukeyHSD(anova_intra_textual_variance)
print(tukey_result_intra_textual_variance)

anova_stepwise_distance <- aov(stepwise_distance ~ class, data = combined_data)
summary(anova_stepwise_distance)
tukey_result_stepwise_distance <- TukeyHSD(anova_stepwise_distance)
print(tukey_result_stepwise_distance)

anova_outlier_score <- aov(outlier_score ~ class, data = combined_data)
summary(anova_outlier_score)
tukey_result_outlier_score <- TukeyHSD(anova_outlier_score)
print(tukey_result_outlier_score)

anova_overlap_score <- aov(overlap_score ~ class, data = combined_data)
summary(anova_overlap_score)
tukey_result_overlap_score <- TukeyHSD(anova_overlap_score)
print(tukey_result_overlap_score)

## Binary 

anova_intra_textual_variance <- aov(intra_textual_variance ~ circl_binary, data = combined_data)
summary(anova_intra_textual_variance)
tukey_result_intra_textual_variance <- TukeyHSD(anova_intra_textual_variance)
print(tukey_result_intra_textual_variance)

anova_stepwise_distance <- aov(stepwise_distance ~ circl_binary, data = combined_data)
summary(anova_stepwise_distance)
tukey_result_stepwise_distance <- TukeyHSD(anova_stepwise_distance)
print(tukey_result_stepwise_distance)

anova_outlier_score <- aov(outlier_score ~ circl_binary, data = combined_data)
summary(anova_outlier_score)
tukey_result_outlier_score <- TukeyHSD(anova_outlier_score)
print(tukey_result_outlier_score)

anova_overlap_score <- aov(overlap_score ~ circl_binary, data = combined_data)
summary(anova_overlap_score)
tukey_result_overlap_score <- TukeyHSD(anova_overlap_score)
print(tukey_result_overlap_score)

anova_intra_textual_variance <- aov(intra_textual_variance ~ reviews_binary, data = combined_data)
summary(anova_intra_textual_variance)
tukey_result_intra_textual_variance <- TukeyHSD(anova_intra_textual_variance)
print(tukey_result_intra_textual_variance)

anova_stepwise_distance <- aov(stepwise_distance ~ reviews_binary, data = combined_data)
summary(anova_stepwise_distance)
tukey_result_stepwise_distance <- TukeyHSD(anova_stepwise_distance)
print(tukey_result_stepwise_distance)

anova_outlier_score <- aov(outlier_score ~ reviews_binary, data = combined_data)
summary(anova_outlier_score)
tukey_result_outlier_score <- TukeyHSD(anova_outlier_score)
print(tukey_result_outlier_score)

anova_overlap_score <- aov(overlap_score ~ reviews_binary, data = combined_data)
summary(anova_overlap_score)
tukey_result_overlap_score <- TukeyHSD(anova_overlap_score)
print(tukey_result_overlap_score)


binary_data <- subset(combined_data, select = -c(class, reviews_sum, circl_sum,
                                                 canonisation_score))

binary_data <- melt(binary_data, id=c("text_id", "reviews_binary", "circl_binary"))

ggplot(binary_data, aes(x = circl_binary, y = value, group = circl_binary)) +
  geom_boxplot() +
  facet_wrap(vars(variable), nrow = 2, scales = "free",
             labeller = labeller) +
  labs(title = "ENG",
       x = "Reception classes",
       y = "Embedding scores"
  ) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

ggplot(binary_data, aes(x = reviews_binary, y = value, group = reviews_binary)) +
  geom_boxplot() +
  facet_wrap(vars(variable), nrow = 2, scales = "free",
             labeller = labeller) +
  labs(title = "ENG",
       x = "Reception classes",
       y = "Embedding scores"
  ) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))


## All classes

combined_data <- subset(combined_data, select = -c(circl_binary, circl_sum,
                                                   reviews_binary, reviews_sum,
                                                   canonisation_score))
combined_melt <- melt(combined_data, id=c("text_id", "class"))

ggplot(combined_melt, aes(x = class, y = value, group = class)) +
  geom_boxplot() +
  facet_wrap(vars(variable), nrow = 2, scales = "free",
             labeller = labeller) +
  labs(title = "ENG",
       x = "Reception classes",
       y = "Embedding scores"
  ) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\ENG\\boxplots_embeddingScores_ENG.png"), 
       dpi = 360, width = 24, height = 16, units = "cm")

# German ----------------------------------------------------------------------#

embedding_scores <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\doc2vec_scores_GER.csv",
                             sep = ",", row.names = 1)

reception_scores <-  read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\data\\scores\\reception_scores\\reception_scores_classes_GER.csv",
                              sep = ",", row.names = 1)

combined_data <- merge(embedding_scores, reception_scores, by.x = "text_id",
                       by = "ID")

combined_data$class <- str_replace_all(combined_data$class, ", ", "\n")
combined_data$class <- str_replace_all(combined_data$class, " & ", "\n")
combined_data$class <- factor(combined_data$class, 
                              levels = c("No Review\nNo Catalogue", "No Review\nCatalogue",
                                         "Review\nNo Catalogue", "Review\nCatalogue"))
combined_data$circl_binary <- factor(combined_data$circl_binary, levels = c(0, 1))
combined_data$reviews_binary <- factor(combined_data$reviews_binary, levels = c(0, 1))

anova_intra_textual_variance <- aov(intra_textual_variance ~ class, data = combined_data)
summary(anova_intra_textual_variance)
tukey_result_intra_textual_variance <- TukeyHSD(anova_intra_textual_variance)
print(tukey_result_intra_textual_variance)

anova_stepwise_distance <- aov(stepwise_distance ~ class, data = combined_data)
summary(anova_stepwise_distance)
tukey_result_stepwise_distance <- TukeyHSD(anova_stepwise_distance)
print(tukey_result_stepwise_distance)

anova_outlier_score <- aov(outlier_score ~ class, data = combined_data)
summary(anova_outlier_score)
tukey_result_outlier_score <- TukeyHSD(anova_outlier_score)
print(tukey_result_outlier_score)

anova_overlap_score <- aov(overlap_score ~ class, data = combined_data)
summary(anova_overlap_score)
tukey_result_overlap_score <- TukeyHSD(anova_overlap_score)
print(tukey_result_overlap_score)

## Binary 

anova_intra_textual_variance <- aov(intra_textual_variance ~ circl_binary, data = combined_data)
summary(anova_intra_textual_variance)
tukey_result_intra_textual_variance <- TukeyHSD(anova_intra_textual_variance)
print(tukey_result_intra_textual_variance)

anova_stepwise_distance <- aov(stepwise_distance ~ circl_binary, data = combined_data)
summary(anova_stepwise_distance)
tukey_result_stepwise_distance <- TukeyHSD(anova_stepwise_distance)
print(tukey_result_stepwise_distance)

anova_outlier_score <- aov(outlier_score ~ circl_binary, data = combined_data)
summary(anova_outlier_score)
tukey_result_outlier_score <- TukeyHSD(anova_outlier_score)
print(tukey_result_outlier_score)

anova_overlap_score <- aov(overlap_score ~ circl_binary, data = combined_data)
summary(anova_overlap_score)
tukey_result_overlap_score <- TukeyHSD(anova_overlap_score)
print(tukey_result_overlap_score)

anova_intra_textual_variance <- aov(intra_textual_variance ~ reviews_binary, data = combined_data)
summary(anova_intra_textual_variance)
tukey_result_intra_textual_variance <- TukeyHSD(anova_intra_textual_variance)
print(tukey_result_intra_textual_variance)

anova_stepwise_distance <- aov(stepwise_distance ~ reviews_binary, data = combined_data)
summary(anova_stepwise_distance)
tukey_result_stepwise_distance <- TukeyHSD(anova_stepwise_distance)
print(tukey_result_stepwise_distance)

anova_outlier_score <- aov(outlier_score ~ reviews_binary, data = combined_data)
summary(anova_outlier_score)
tukey_result_outlier_score <- TukeyHSD(anova_outlier_score)
print(tukey_result_outlier_score)

anova_overlap_score <- aov(overlap_score ~ reviews_binary, data = combined_data)
summary(anova_overlap_score)
tukey_result_overlap_score <- TukeyHSD(anova_overlap_score)
print(tukey_result_overlap_score)

binary_data <- subset(combined_data, select = -c(class, reviews_sum, circl_sum,
                                                 canonisation_score))

binary_data <- melt(binary_data, id=c("text_id", "reviews_binary", "circl_binary"))

ggplot(binary_data, aes(x = circl_binary, y = value, group = circl_binary)) +
  geom_boxplot() +
  facet_wrap(vars(variable), nrow = 2, scales = "free",
             labeller = labeller) +
  labs(title = "GER",
       x = "Reception classes",
       y = "Embedding scores"
  ) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

ggplot(binary_data, aes(x = reviews_binary, y = value, group = reviews_binary)) +
  geom_boxplot() +
  facet_wrap(vars(variable), nrow = 2, scales = "free",
             labeller = labeller) +
  labs(title = "GER",
       x = "Reception classes",
       y = "Embedding scores"
  ) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))
        
## All classes

combined_data <- subset(combined_data, select = -c(circl_binary, circl_sum,
                                                   reviews_binary, reviews_sum,
                                                   canonisation_score))
combined_melt <- melt(combined_data, id=c("text_id", "class"))


ggplot(combined_melt, aes(x = class, y = value, group = class)) +
  geom_boxplot() +
  facet_wrap(vars(variable), nrow = 2, scales = "free",
             labeller = labeller) +
  labs(title = "GER",
       x = "Reception classes",
       y = "Embedding scores"
  ) +
  theme_par() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'))

ggsave(paste0("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist\\GER\\boxplots_embeddingScores_GER.png"), 
       dpi = 360, width = 24, height = 16, units = "cm")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
