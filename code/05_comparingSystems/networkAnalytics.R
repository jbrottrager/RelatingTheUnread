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
library(tidyr)
library(vcd)
library(reshape2)
library(cowplot)
library(MASS)
library(car)

#------------------------------------------------------------------------------#
## Centrality measures --------------------------------------------------------#
#------------------------------------------------------------------------------#

# Classic set-up --------------------------------------------------------------#

setwd("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\modelling_lithist")

centrality_ENG_classic <- read.csv("ENG\\networks\\doc2vec_adjacency_ENG_backbone_3-9_louvain_centrality.csv")
names(centrality_ENG_classic)[names(centrality_ENG_classic) == "id"] <- "Text"

centrality_ENG_classic$pagerank_norm <- (centrality_ENG_classic$pagerank - min(centrality_ENG_classic$pagerank)) / 
  (max(centrality_ENG_classic$pagerank) - min(centrality_ENG_classic$pagerank))
centrality_ENG_classic$indegree_norm <- (centrality_ENG_classic$indegree - min(centrality_ENG_classic$indegree)) / 
  (max(centrality_ENG_classic$indegree) - min(centrality_ENG_classic$indegree))

centrality_ENG_classic <- centrality_ENG_classic %>%
  mutate(Score.Range = case_when(
    canonisation_score >= 0 & canonisation_score <= 0.25 ~ 'Low (0-0.25)',
    canonisation_score > 0.25 & canonisation_score <= 0.5 ~ 'Mid-Low (0.25-0.5)',
    canonisation_score > 0.5 & canonisation_score <= 0.75 ~ 'Mid-High (0.5-0.75)',
    canonisation_score > 0.75 & canonisation_score <= 1.0 ~ 'High (0.75-1.0)'
  ))

centrality_ENG_classic$Score.Range <- factor(centrality_ENG_classic$Score.Range, 
                                             levels = c('Low (0-0.25)', 'Mid-Low (0.25-0.5)',
                                                'Mid-High (0.5-0.75)', 'High (0.75-1.0)'))
centrality_ENG_classic$lang <- "ENG"

centrality_GER_classic <- read.csv("GER\\networks\\doc2vec_adjacency_GER_backbone_3-9_louvain_centrality.csv")

centrality_GER_classic$pagerank_norm <- (centrality_GER_classic$pagerank - min(centrality_GER_classic$pagerank)) / 
  (max(centrality_GER_classic$pagerank) - min(centrality_GER_classic$pagerank))
centrality_GER_classic$indegree_norm <- (centrality_GER_classic$indegree - min(centrality_GER_classic$indegree)) / 
  (max(centrality_GER_classic$indegree) - min(centrality_GER_classic$indegree))

centrality_GER_classic <- centrality_GER_classic %>%
  mutate(Score.Range = case_when(
    canonisation_score >= 0 & canonisation_score <= 0.25 ~ 'Low (0-0.25)',
    canonisation_score > 0.25 & canonisation_score <= 0.5 ~ 'Mid-Low (0.25-0.5)',
    canonisation_score > 0.5 & canonisation_score <= 0.75 ~ 'Mid-High (0.5-0.75)',
    canonisation_score > 0.75 & canonisation_score <= 1.0 ~ 'High (0.75-1.0)'
  ))

centrality_GER_classic$Score.Range <- factor(centrality_GER_classic$Score.Range, 
                                             levels = c('Low (0-0.25)', 'Mid-Low (0.25-0.5)',
                                                'Mid-High (0.5-0.75)', 'High (0.75-1.0)'))
centrality_GER_classic$lang <- "GER"

p1 <- ggplot() +
  geom_histogram(data = centrality_ENG_classic, aes(x = indegree_norm, y = ..count.. / sum(..count..) * 100,
                                            fill = "black"), binwidth = 0.1, alpha = 1, colour = "black") +
  geom_histogram(data = centrality_GER_classic, aes(x = indegree_norm, y = ..count.. / sum(..count..) * 100,
                                            fill = "white", ), binwidth = 0.1, alpha = 0.6, colour = "black") +
  scale_fill_manual(name = "", values=c("black","white"),labels=c("ENG","GER")) +
  labs(x = "Normalised indegree centrality", y = "Percentage (%)") +
  theme_par() +
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 40, by = 10)) +  
  theme(plot.margin = unit(c(1, 5, 1, 1), 'pt'), 
        legend.position = c(0.9, 0.75))

p2 <- ggplot() +
  geom_histogram(data = centrality_ENG_classic, aes(x = pagerank_norm, y = ..count.. / sum(..count..) * 100), 
                 fill = "black", binwidth = 0.1, alpha = 1, colour = "black") +
  geom_histogram(data = centrality_GER_classic, aes(x = pagerank_norm, y = ..count.. / sum(..count..) * 100), 
                 fill = "white", binwidth = 0.1, alpha = 0.6, colour = "black") +
  labs(x = "Normalised PageRank centrality", y = "Percentage (%)") +
  theme_par() +
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 40, by = 10)) +  
  theme(plot.margin = unit(c(1, 5, 1, 1), 'pt'))

plot_grid(p1, p2, ncol = 1)

ggsave("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\comparing_systems\\networkAnalytics_classic_normalised.png", 
       dpi = 360, width = 16, height = 12, units = "cm")

threshold_value <- 0.25
percentage_ENG <- sum(centrality_ENG_classic$indegree_norm > threshold_value) / nrow(centrality_ENG_classic) * 100
percentage_ENG
percentage_GER <- sum(centrality_GER_classic$indegree_norm > threshold_value) / nrow(centrality_GER_classic) * 100
percentage_GER

# Timeseries ------------------------------------------------------------------#

centrality_ENG_time <- read.csv("ENG\\networks\\nearest_neighbours_3_decay=0.01_df_ENG_centrality.csv")
summary(centrality_ENG_time)

centrality_ENG_time$pagerank_norm <- (centrality_ENG_time$pagerank - min(centrality_ENG_time$pagerank)) / 
  (max(centrality_ENG_time$pagerank) - min(centrality_ENG_time$pagerank))
centrality_ENG_time$indegree_norm <- (centrality_ENG_time$indegree - min(centrality_ENG_time$indegree)) / 
  (max(centrality_ENG_time$indegree) - min(centrality_ENG_time$indegree))

centrality_ENG_time <- centrality_ENG_time %>%
  mutate(Score.Range = case_when(
    canonisation_score >= 0 & canonisation_score <= 0.25 ~ 'Low (0-0.25)',
    canonisation_score > 0.25 & canonisation_score <= 0.5 ~ 'Mid-Low (0.25-0.5)',
    canonisation_score > 0.5 & canonisation_score <= 0.75 ~ 'Mid-High (0.5-0.75)',
    canonisation_score > 0.75 & canonisation_score <= 1.0 ~ 'High (0.75-1.0)'
  ))

centrality_ENG_time$Score.Range <- factor(centrality_ENG_time$Score.Range, 
                                     levels = c('Low (0-0.25)', 'Mid-Low (0.25-0.5)',
                                                'Mid-High (0.5-0.75)', 'High (0.75-1.0)'))
centrality_ENG_time$lang <- "ENG"

centrality_GER_time <- read.csv("GER\\networks\\nearest_neighbours_3_decay=0.01_df_GER_centrality.csv")
summary(centrality_GER_time)

centrality_GER_time$pagerank_norm <- (centrality_GER_time$pagerank - min(centrality_GER_time$pagerank)) / 
  (max(centrality_GER_time$pagerank) - min(centrality_GER_time$pagerank))
centrality_GER_time$indegree_norm <- (centrality_GER_time$indegree - min(centrality_GER_time$indegree)) / 
  (max(centrality_GER_time$indegree) - min(centrality_GER_time$indegree))

centrality_GER_time <- centrality_GER_time %>%
  mutate(Score.Range = case_when(
    canonisation_score >= 0 & canonisation_score <= 0.25 ~ 'Low (0-0.25)',
    canonisation_score > 0.25 & canonisation_score <= 0.5 ~ 'Mid-Low (0.25-0.5)',
    canonisation_score > 0.5 & canonisation_score <= 0.75 ~ 'Mid-High (0.5-0.75)',
    canonisation_score > 0.75 & canonisation_score <= 1.0 ~ 'High (0.75-1.0)'
  ))

centrality_GER_time$Score.Range <- factor(centrality_GER_time$Score.Range, 
                                     levels = c('Low (0-0.25)', 'Mid-Low (0.25-0.5)',
                                                'Mid-High (0.5-0.75)', 'High (0.75-1.0)'))
centrality_GER_time$lang <- "GER"

p1 <- ggplot() +
  geom_histogram(data = centrality_ENG_time, aes(x = indegree_norm, y = ..count.. / sum(..count..) * 100,
                                            fill = "black"), binwidth = 0.1, alpha = 1, colour = "black") +
  geom_histogram(data = centrality_GER_time, aes(x = indegree_norm, y = ..count.. / sum(..count..) * 100,
                                            fill = "white"), binwidth = 0.1, alpha = 0.6, colour = "black") +
  scale_fill_manual(name = "", values=c("black","white"),labels=c("ENG","GER")) +
  labs(x = "Normalised indegree centrality", y = "Percentage (%)") +
  theme_par() +
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 50, by = 10)) +  
  theme(plot.margin = unit(c(1, 5, 1, 1), 'pt'), 
        legend.position = c(0.9, 0.75))

p2 <- ggplot() +
  geom_histogram(data = centrality_ENG_time, aes(x = pagerank_norm, y = ..count.. / sum(..count..) * 100), 
                 fill = "black", binwidth = 0.1, alpha = 1, colour = "black") +
  geom_histogram(data = centrality_GER_time, aes(x = pagerank_norm, y = ..count.. / sum(..count..) * 100), 
                 fill = "white", binwidth = 0.1, alpha = 0.6, colour = "black") +
  labs(x = "Normalised PageRank centrality", y = "Percentage (%)") +
  theme_par() +
  scale_y_continuous(limits = c(0, 55), breaks = seq(0, 50, by = 10)) +  
  theme(plot.margin = unit(c(1, 5, 1, 1), 'pt'))

plot_grid(p1, p2, ncol = 1)

ggsave("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\comparing_systems\\networkAnalytics_timeseries_normalised.png", 
       dpi = 360, width = 16, height = 12, units = "cm")

threshold_value <- 0.25
percentage_ENG <- sum(centrality_ENG_time$indegree_norm > threshold_value) / nrow(centrality_ENG_time) * 100
percentage_ENG
percentage_GER <- sum(centrality_GER_time$indegree_norm > threshold_value) / nrow(centrality_GER_time) * 100
percentage_GER

## Comparison -----------------------------------------------------------------#

setwd("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\analyses\\comparing_systems")

mean_sd_both_standard <- data.frame(
  Network = c("ENG", "GER", "ENG", "GER"),
  Metric = c("PageRank", "PageRank", "Indegree", "Indegree"),
  Mean = c(mean(centrality_ENG_classic$pagerank_norm), mean(centrality_GER_classic$pagerank_norm),
           mean(centrality_ENG_classic$indegree_norm), mean(centrality_GER_classic$indegree_norm)),
  SD = c(sd(centrality_ENG_classic$pagerank_norm), sd(centrality_GER_classic$pagerank_norm),
         sd(centrality_ENG_classic$indegree_norm), sd(centrality_GER_classic$indegree_norm)),
  var = c(var(centrality_ENG_classic$pagerank_norm), var(centrality_GER_classic$pagerank_norm),
          var(centrality_ENG_classic$indegree_norm), var(centrality_GER_classic$indegree_norm))
)


mean_sd_both_timeseries <- data.frame(
  Network = c("ENG", "GER", "ENG", "GER"),
  Metric = c("PageRank", "PageRank", "Indegree", "Indegree"),
  Mean = c(mean(centrality_ENG_time$pagerank_norm), mean(centrality_GER_time$pagerank_norm),
             mean(centrality_ENG_time$indegree_norm), mean(centrality_GER_time$indegree_norm)),
  SD = c(sd(centrality_ENG_time$pagerank_norm), sd(centrality_GER_time$pagerank_norm),
         sd(centrality_ENG_time$indegree_norm), sd(centrality_GER_time$indegree_norm)),
  var = c(var(centrality_ENG_time$pagerank_norm), var(centrality_GER_time$pagerank_norm),
          var(centrality_ENG_time$indegree_norm), var(centrality_GER_time$indegree_norm))
)

mean_sd_both_standard$mode <- "Time-agnostic network"
mean_sd_both_timeseries$mode <- "Time-sensitive network"

mean_sd_both <- rbind(mean_sd_both_standard, mean_sd_both_timeseries)
mean_sd_both$mode <- factor(mean_sd_both$mode, levels = c(unique(mean_sd_both$mode)))

ggplot(mean_sd_both, aes(x = Metric, y = Mean, color = Network)) +
  geom_point(size = 4, position = position_dodge(width = 0.75)) + 
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.4, 
                position = position_dodge(width = 0.75), size = 1) +  
  facet_wrap(vars(mode), nrow = 1, scales = "free") +
  labs(y = "Mean normalised value", x = "Metric") +
  ylim(-0.075, 0.5) +
  theme_minimal() +
  scale_color_manual(name = "", values = c("ENG" = "black", "GER" = "grey")) +
  theme_par() +
  theme(plot.margin = unit(c(1, 5, 1, 1), 'pt'),
        legend.position = c(0.9, 0.855))

ggsave("networkAnalytics_normalised_comp.png", 
       dpi = 360, width = 16, height = 10, units = "cm")

## Clustering -----------------------------------------------------------------#

### Elbow ---------------------------------------------------------------------#

set.seed(1)
centrality_ENG_classic_filtered <- centrality_ENG_classic[, c('indegree_norm', 'pagerank_norm')]

wss <- sapply(1:10, function(k) {
  kmeans(centrality_ENG_classic_filtered, centers = k, nstart = 25)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares", 
     main = "Elbow Method for Optimal Clusters")

set.seed(1)
centrality_GER_classic_filtered <- centrality_GER_classic[, c('indegree_norm', 'pagerank_norm')]

wss <- sapply(1:10, function(k) {
  kmeans(centrality_GER_classic_filtered, centers = k, nstart = 25)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares", 
     main = "Elbow Method for Optimal Clusters")

set.seed(1)
centrality_ENG_time_filtered <- centrality_ENG_time[, c('indegree_norm', 'pagerank_norm')]

wss <- sapply(1:10, function(k) {
  kmeans(centrality_ENG_time_filtered, centers = k, nstart = 25)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares", 
     main = "Elbow Method for Optimal Clusters")

set.seed(1)
centrality_GER_time_filtered <- centrality_GER_time[, c('indegree_norm', 'pagerank_norm')]

wss <- sapply(1:10, function(k) {
  kmeans(centrality_GER_time_filtered, centers = k, nstart = 25)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares", 
     main = "Elbow Method for Optimal Clusters")


### kmeans --------------------------------------------------------------------#

set.seed(1)
centrality_ENG_classic_filtered <- centrality_ENG_classic[, c('indegree_norm', 'pagerank_norm')]
kmeans_result <- kmeans(centrality_ENG_classic_filtered, centers = 4)
centrality_ENG_classic$Cluster <- as.factor(kmeans_result$cluster)
new_cluster_names <- c("Cluster 4", "Cluster 3", "Cluster 2", "Cluster 1")
centrality_ENG_classic$Cluster <- factor(centrality_ENG_classic$Cluster, 
                                      levels = unique(centrality_ENG_classic$Cluster),
                                      labels = new_cluster_names)
centrality_ENG_classic$Cluster <- factor(centrality_ENG_classic$Cluster, 
                                      levels = c("Cluster 1", "Cluster 2", "Cluster 3", 
                                                 "Cluster 4"))
info <- subset(centrality_ENG_classic, select = c("Text", "Cluster"))
write.csv(info, "ENG\\centrality_ENG_classic_clusters.csv")

set.seed(1)
centrality_GER_classic_filtered <- centrality_GER_classic[, c('indegree_norm', 'pagerank_norm')]
kmeans_result <- kmeans(centrality_GER_classic_filtered, centers = 4)
centrality_GER_classic$Cluster <- as.factor(kmeans_result$cluster)
new_cluster_names <- c("Cluster 4", "Cluster 2", "Cluster 3", "Cluster 1")
centrality_GER_classic$Cluster <- factor(centrality_GER_classic$Cluster, 
                                      levels = unique(centrality_GER_classic$Cluster),
                                      labels = new_cluster_names)
centrality_GER_classic$Cluster <- factor(centrality_GER_classic$Cluster, 
                                      levels = c("Cluster 1", "Cluster 2", "Cluster 3", 
                                                 "Cluster 4"))
info <- subset(centrality_GER_classic, select = c("id", "Cluster"))
write.csv(info, "GER\\centrality_GER_classic_clusters.csv")


centrality_ENG_classic %>%
  count(Cluster) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  filter(Cluster %in% c("Cluster 1", "Cluster 4"))

centrality_GER_classic %>%
  count(Cluster) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  filter(Cluster %in% c("Cluster 1", "Cluster 4"))

plot_eng_1 <- ggplot(centrality_ENG_classic, aes(x = indegree_norm, y = pagerank_norm, 
                                            fill = Cluster)) +
  geom_point(size = 1, alpha = 0.8, shape = 21, colour = "black") +
  labs(x = "", y = "normalised PageRank centrality") +
  scale_fill_manual(values = c("black", "white", "white", "grey40")) +  
  ggtitle("Time-agnostic model\nENG") +
  theme_par() +
  theme(plot.margin = unit(c(1, 5, 1, 1), 'pt'),
        legend.position = "none",
        plot.title = element_text(size = 10, face = "plain"))

plot_ger_1 <- ggplot(centrality_GER_classic, aes(x = indegree_norm, y = pagerank_norm, 
                                            fill = Cluster)) +
  geom_point(size = 1, alpha = 0.8, shape = 21, colour = "black") +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("black", "white", "white", "grey40")) + 
  ggtitle("Time-agnostic model\nGER") +
  theme_par() +
  theme(plot.margin = unit(c(1, 5, 1, 1), 'pt'),
        legend.position = "none",
        plot.title = element_text(size = 10, face = "plain"))

set.seed(1)
centrality_ENG_time_filtered <- centrality_ENG_time[, c('indegree_norm', 'pagerank_norm')]
kmeans_result <- kmeans(centrality_ENG_time_filtered, centers = 4)
centrality_ENG_time$Cluster <- as.factor(kmeans_result$cluster)
new_cluster_names <- c("Cluster 3", "Cluster 1", "Cluster 2", "Cluster 4")
centrality_ENG_time$Cluster <- factor(centrality_ENG_time$Cluster, 
                                      levels = unique(centrality_ENG_time$Cluster),
                                      labels = new_cluster_names)
centrality_ENG_time$Cluster <- factor(centrality_ENG_time$Cluster, 
                                      levels = c("Cluster 1", "Cluster 2", "Cluster 3", 
                                                 "Cluster 4"))
info <- subset(centrality_ENG_time, select = c("Text", "Cluster"))
write.csv(info, "ENG\\centrality_ENG_time_clusters.csv")

set.seed(1)
centrality_GER_time_filtered <- centrality_GER_time[, c('indegree_norm', 'pagerank_norm')]
kmeans_result <- kmeans(centrality_GER_time_filtered, centers = 4)
centrality_GER_time$Cluster <- as.factor(kmeans_result$cluster)
new_cluster_names <- c("Cluster 2", "Cluster 1", "Cluster 4", "Cluster 3")
centrality_GER_time$Cluster <- factor(centrality_GER_time$Cluster, 
                                      levels = unique(centrality_GER_time$Cluster),
                                      labels = new_cluster_names)
centrality_GER_time$Cluster <- factor(centrality_GER_time$Cluster, 
                                      levels = c("Cluster 1", "Cluster 2", "Cluster 3", 
                                                 "Cluster 4"))
info <- subset(centrality_GER_time, select = c("Text", "Cluster"))
write.csv(info, "GER\\centrality_GER_time_clusters.csv")
                                      
centrality_ENG_time %>%
  count(Cluster) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  filter(Cluster %in% c("Cluster 1", "Cluster 4"))

centrality_GER_time %>%
  count(Cluster) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  filter(Cluster %in% c("Cluster 1", "Cluster 4"))
                                      
plot_eng_2 <- ggplot(centrality_ENG_time, aes(x = indegree_norm, y = pagerank_norm, 
                                            fill = Cluster)) +
  geom_point(size = 1, alpha = 0.8, shape = 21, colour = "black") +
  labs(x = "normalised indegree centrality", y = "normalised PageRank centrality") +
  scale_fill_manual(values = c("black", "white", "white", "grey40")) + 
  ggtitle("Time-sensitive model\nENG") +
  theme_par() +
  theme(plot.margin = unit(c(1, 5, 1, 1), 'pt'),
        legend.position = "none",
        plot.title = element_text(size = 10, face = "plain"))

plot_ger_2 <- ggplot(centrality_GER_time, aes(x = indegree_norm, y = pagerank_norm, 
                                            fill = Cluster)) +
  geom_point(size = 1, alpha = 0.8, shape = 21, colour = "black") +
  labs(x = "normalised indegree centrality", y = "") +
  scale_fill_manual(values = c("black", "white", "white", "grey40")) + 
  ggtitle("Time-sensitive model\nGER") +
  theme_par() +
  theme(plot.margin = unit(c(1, 5, 1, 1), 'pt'),
        legend.position = "none",
        plot.title = element_text(size = 10, face = "plain"))

plot_grid(plot_eng_1, plot_ger_1, plot_eng_2, plot_ger_2, ncol = 2)

ggsave("networkAnalytics_cluster.png", 
       dpi = 360, width = 16, height = 16, units = "cm")

# Cluster // Canonisation -----------------------------------------------------#

centrality_ENG_classic_summary <- centrality_ENG_classic %>%
  group_by(Cluster) %>%
  summarise(
    avg_canonisation_score = mean(canonisation_score, na.rm = TRUE),
    sd_canonisation_score = sd(canonisation_score, na.rm = TRUE),
    predominant_class = names(sort(table(class), decreasing = TRUE))[1]
  )

centrality_ENG_classic_summary$lang <- "ENG"

centrality_GER_classic_summary <- centrality_GER_classic %>%
  group_by(Cluster) %>%
  summarise(
    avg_canonisation_score = mean(canonisation_score, na.rm = TRUE),
    sd_canonisation_score = sd(canonisation_score, na.rm = TRUE),
    predominant_class = names(sort(table(class), decreasing = TRUE))[1]
  )

centrality_GER_classic_summary$lang <- "GER"

centrality_classic_summary <- rbind(centrality_ENG_classic_summary, 
                                    centrality_GER_classic_summary)

centrality_classic_summary$Cluster <- factor(centrality_classic_summary$Cluster, 
                                             levels = c("Cluster 1", "Cluster 2",
                                                        "Cluster 3", "Cluster 4"))

centrality_classic_summary$mode <- "Time-agnostic model"

centrality_ENG_time_summary <- centrality_ENG_time %>%
  group_by(Cluster) %>%
  summarise(
    avg_canonisation_score = mean(canonisation_score, na.rm = TRUE),
    sd_canonisation_score = sd(canonisation_score, na.rm = TRUE),
    predominant_class = names(sort(table(class), decreasing = TRUE))[1]
  )

centrality_ENG_time_summary$lang <- "ENG"

centrality_GER_time_summary <- centrality_GER_time %>%
  group_by(Cluster) %>%
  summarise(
    avg_canonisation_score = mean(canonisation_score, na.rm = TRUE),
    sd_canonisation_score = sd(canonisation_score, na.rm = TRUE),
    predominant_class = names(sort(table(class), decreasing = TRUE))[1]
  )

centrality_GER_time_summary$lang <- "GER"

centrality_time_summary <- rbind(centrality_ENG_time_summary, 
                                 centrality_GER_time_summary)

centrality_time_summary$Cluster <- factor(centrality_time_summary$Cluster, 
                                             levels = c("Cluster 1", "Cluster 2",
                                                        "Cluster 3", "Cluster 4"))

centrality_time_summary$mode <- "Time-sensitive model"

centrality_summary <- rbind(centrality_classic_summary, centrality_time_summary)

ggplot(centrality_summary, aes(x = Cluster, y = avg_canonisation_score)) +
  geom_point(size = 4, position = position_dodge(width = 0.75), colour = "black") + 
  geom_errorbar(aes(ymin = avg_canonisation_score - sd_canonisation_score, 
                    ymax = avg_canonisation_score + sd_canonisation_score), 
                width = 0.4, position = position_dodge(width = 0.75), size = 1) +  
  facet_wrap(vars(mode, lang), nrow = 2) +
  scale_y_continuous(limits = c(-0.1, 0.8), breaks = seq(0, 1, by = 0.2)) +  
  labs(y = "Mean canonisation score", x = "") +
  theme_par() +
  theme(plot.margin = unit(c(1, 5, 1, 1), 'pt'))

ggsave("networkAnalytics_summary_comp.png", 
       dpi = 360, width = 20, height = 16, units = "cm")


## Heatmaps --------------------------------------------------------------------#

centrality_ENG_classic$Score.Range <- factor(centrality_ENG_classic$Score.Range, 
                                             levels = c('Low (0-0.25)', 'Mid-Low (0.25-0.5)',
                                                'Mid-High (0.5-0.75)', 'High (0.75-1.0)'))
chisq <- chisq.test(centrality_ENG_classic$Score.Range, centrality_ENG_classic$Cluster)
residuals_df <- as.data.frame(as.table(chisq$residuals))
colnames(residuals_df) <- c("Score.Range", "Cluster", "Residuals")

residuals_df_ENG_canonisation <- residuals_df %>%
  mutate(annotation = case_when(
    Residuals > 2 ~ "+", 
    Residuals < -2 ~ "-", 
    TRUE ~ ""            
  ))

residuals_df_ENG_canonisation$lang <- "ENG"
residuals_df_ENG_canonisation$category <- "Canonisation"

centrality_GER_classic$Score.Range <- factor(centrality_GER_classic$Score.Range, 
                                             levels = c('Low (0-0.25)', 'Mid-Low (0.25-0.5)',
                                                        'Mid-High (0.5-0.75)', 'High (0.75-1.0)'))
chisq <- chisq.test(centrality_GER_classic$Score.Range, centrality_GER_classic$Cluster)
residuals_df <- as.data.frame(as.table(chisq$residuals))
colnames(residuals_df) <- c("Score.Range", "Cluster", "Residuals")

residuals_df_GER_canonisation <- residuals_df %>%
  mutate(annotation = case_when(
    Residuals > 2 ~ "+",  
    Residuals < -2 ~ "-", 
    TRUE ~ ""            
  ))

residuals_df_GER_canonisation$lang <- "GER"
residuals_df_GER_canonisation$category <- "Canonisation"

residuals_df_canonisation <- rbind(residuals_df_ENG_canonisation, 
                                   residuals_df_GER_canonisation)

p1 <- ggplot(residuals_df_canonisation, aes(x = Score.Range, y = Cluster, fill = Residuals)) +
  geom_tile(color = "black") +
  facet_wrap(vars(lang), nrow = 1) +
  scale_fill_gradient2(midpoint = 0, low = "black", high = "black", mid = "white") +
  geom_text(aes(label = annotation), color = "white", size = 6) +  # Add text annotation
  coord_flip() +
  theme_minimal() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'),
        legend.position = "none") +
  labs(x = "", y = "")

#------------------------------------------------------------------------------#

centrality_ENG_classic$class <- factor(centrality_ENG_classic$class, 
                                       levels =  c("No Review & No Catalogue", "No Review, Catalogue",
                                           "Review, No Catalogue", "Review & Catalogue"))
chisq <- chisq.test(centrality_ENG_classic$class, centrality_ENG_classic$Cluster)
residuals_df <- as.data.frame(as.table(chisq$residuals))
colnames(residuals_df) <- c("Class", "Cluster", "Residuals")

residuals_df_ENG_reception <- residuals_df %>%
  mutate(annotation = case_when(
    Residuals > 2 ~ "+",  
    Residuals < -2 ~ "-", 
    TRUE ~ ""            
  ))

residuals_df_ENG_reception$lang <- "ENG"
residuals_df_ENG_reception$category <- "Reception"

centrality_GER_classic$class <- factor(centrality_GER_classic$class, 
                                       levels =  c("No Review & No Catalogue", "No Review, Catalogue",
                                                   "Review, No Catalogue", "Review & Catalogue"))

chisq <- chisq.test(centrality_GER_classic$class, centrality_GER_classic$Cluster)
residuals_df <- as.data.frame(as.table(chisq$residuals))
colnames(residuals_df) <- c("Class", "Cluster", "Residuals")

residuals_df_GER_reception <- residuals_df %>%
  mutate(annotation = case_when(
    Residuals > 2 ~ "+",  
    Residuals < -2 ~ "-", 
    TRUE ~ ""            
  ))

residuals_df_GER_reception$lang <- "GER"
residuals_df_GER_reception$category <- "Reception"

residuals_df_reception <- rbind(residuals_df_ENG_reception, 
                                residuals_df_GER_reception)

p2 <- ggplot(residuals_df_reception, aes(x = Class, y = Cluster, fill = Residuals)) +
  geom_tile(color = "black") +
  facet_wrap(vars(lang), nrow = 1) +
  scale_fill_gradient2(midpoint = 0, low = "black", high = "black", mid = "white") +
  geom_text(aes(label = annotation), color = "white", size = 6) +  # Add text annotation
  coord_flip() +
  theme_minimal() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'),
        legend.position = "none") +
  labs(x = "", y = "")


aligned <- align_plots(p1, p2, align = "v")

plot_grid(ggdraw(aligned[[1]]), ggdraw(aligned[[2]]), nrow = 2, align = "v")

ggsave("residuals_heatmap_classic.png", 
       dpi = 360, height = 6, width = 16, units = "cm")

#------------------------------------------------------------------------------#

centrality_ENG_time$Score.Range <- factor(centrality_ENG_time$Score.Range, 
                                             levels = c('Low (0-0.25)', 'Mid-Low (0.25-0.5)',
                                                        'Mid-High (0.5-0.75)', 'High (0.75-1.0)'))
chisq <- chisq.test(centrality_ENG_time$Score.Range, centrality_ENG_time$Cluster)
residuals_df <- as.data.frame(as.table(chisq$residuals))
colnames(residuals_df) <- c("Score.Range", "Cluster", "Residuals")

residuals_df_ENG_canonisation <- residuals_df %>%
  mutate(annotation = case_when(
    Residuals > 2 ~ "+", 
    Residuals < -2 ~ "-", 
    TRUE ~ ""            
  ))

residuals_df_ENG_canonisation$lang <- "ENG"
residuals_df_ENG_canonisation$category <- "Canonisation"

centrality_GER_time$Score.Range <- factor(centrality_GER_time$Score.Range, 
                                             levels = c('Low (0-0.25)', 'Mid-Low (0.25-0.5)',
                                                        'Mid-High (0.5-0.75)', 'High (0.75-1.0)'))
chisq <- chisq.test(centrality_GER_time$Score.Range, centrality_GER_time$Cluster)
residuals_df <- as.data.frame(as.table(chisq$residuals))
colnames(residuals_df) <- c("Score.Range", "Cluster", "Residuals")

residuals_df_GER_canonisation <- residuals_df %>%
  mutate(annotation = case_when(
    Residuals > 2 ~ "+",  
    Residuals < -2 ~ "-", 
    TRUE ~ ""            
  ))

residuals_df_GER_canonisation$lang <- "GER"
residuals_df_GER_canonisation$category <- "Canonisation"

residuals_df_canonisation <- rbind(residuals_df_ENG_canonisation, 
                                   residuals_df_GER_canonisation)

p1 <- ggplot(residuals_df_canonisation, aes(x = Score.Range, y = Cluster, fill = Residuals)) +
  geom_tile(color = "black") +
  facet_wrap(vars(lang), nrow = 1) +
  scale_fill_gradient2(midpoint = 0, low = "black", high = "black", mid = "white") +
  geom_text(aes(label = annotation), color = "white", size = 6) +  # Add text annotation
  coord_flip() +
  theme_minimal() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'),
        legend.position = "none") +
  labs(x = "", y = "")

#------------------------------------------------------------------------------#

centrality_ENG_time$class <- factor(centrality_ENG_time$class, 
                                       levels =  c("No Review & No Catalogue", "No Review, Catalogue",
                                                   "Review, No Catalogue", "Review & Catalogue"))
chisq <- chisq.test(centrality_ENG_time$class, centrality_ENG_time$Cluster)
residuals_df <- as.data.frame(as.table(chisq$residuals))
colnames(residuals_df) <- c("Class", "Cluster", "Residuals")

residuals_df_ENG_reception <- residuals_df %>%
  mutate(annotation = case_when(
    Residuals > 2 ~ "+",  
    Residuals < -2 ~ "-", 
    TRUE ~ ""            
  ))

residuals_df_ENG_reception$lang <- "ENG"
residuals_df_ENG_reception$category <- "Reception"

centrality_GER_time$class <- factor(centrality_GER_time$class, 
                                       levels =  c("No Review & No Catalogue", "No Review, Catalogue",
                                                   "Review, No Catalogue", "Review & Catalogue"))

chisq <- chisq.test(centrality_GER_time$class, centrality_GER_time$Cluster)
residuals_df <- as.data.frame(as.table(chisq$residuals))
colnames(residuals_df) <- c("Class", "Cluster", "Residuals")

residuals_df_GER_reception <- residuals_df %>%
  mutate(annotation = case_when(
    Residuals > 2 ~ "+",  
    Residuals < -2 ~ "-", 
    TRUE ~ ""            
  ))

residuals_df_GER_reception$lang <- "GER"
residuals_df_GER_reception$category <- "Reception"

residuals_df_reception <- rbind(residuals_df_ENG_reception, 
                                residuals_df_GER_reception)

p2 <- ggplot(residuals_df_reception, aes(x = Class, y = Cluster, fill = Residuals)) +
  geom_tile(color = "black") +
  facet_wrap(vars(lang), nrow = 1) +
  scale_fill_gradient2(midpoint = 0, low = "black", high = "black", mid = "white") +
  geom_text(aes(label = annotation), color = "white", size = 6) +  # Add text annotation
  coord_flip() +
  theme_minimal() +
  theme(plot.margin = unit(c(1,5,1,1), 'pt'),
        legend.position = "none") +
  labs(x = "", y = "")


aligned <- align_plots(p1, p2, align = "v")

plot_grid(ggdraw(aligned[[1]]), ggdraw(aligned[[2]]), nrow = 2, align = "v")

ggsave("residuals_heatmap_time.png", 
       dpi = 360, height = 6, width = 16, units = "cm")


# Cluster ---------------------------------------------------------------------#

centrality_ENG_classic_cluster1 <- subset(centrality_ENG_classic, 
                                          Cluster == "Cluster 1", 
                                          select = c("Text", "canonisation_score",
                                                     "Score.Range", "class"))
centrality_GER_classic_cluster1 <- subset(centrality_GER_classic, 
                                          Cluster == "Cluster 1", 
                                          select = c("id", "canonisation_score",
                                                     "Score.Range", "class"))
names(centrality_GER_classic_cluster1) <-  c("Text", "canonisation_score",
                                             "Score.Range", "class")

centrality_ENG_time_cluster1 <- subset(centrality_ENG_time, Cluster == "Cluster 1", 
                                       select = c("Text", "canonisation_score",
                                                  "Score.Range", "class"))
centrality_GER_time_cluster1 <- subset(centrality_GER_time, 
                                          Cluster == "Cluster 1", 
                                       select = c("Text", "canonisation_score",
                                                  "Score.Range", "class"))

intersect <- intersect(centrality_ENG_classic_cluster1, centrality_ENG_time_cluster1)
intersect$time_agnostic <- 1
intersect$time_sensitive <- 1

setdiff1 <- setdiff(centrality_ENG_classic_cluster1, centrality_ENG_time_cluster1)
setdiff1$time_agnostic <- 1
setdiff1$time_sensitive <- 0

setdiff2 <- setdiff(centrality_ENG_time_cluster1, centrality_ENG_classic_cluster1)
setdiff2$time_agnostic <- 0
setdiff2$time_sensitive <- 1

all <- rbind(intersect, setdiff1, setdiff2)

meta_ENG <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\corpora\\ENG_corpus.csv", sep = ";")
meta_ENG <- subset(meta_ENG, select = c(pub_year, wikiname, wikiID, author))
meta_ENG$Text <- paste0(meta_ENG$wikiname, "_", meta_ENG$wikiID)

all <- merge(all, meta_ENG, by = "Text", all.x = TRUE)

write.csv(all, "ENG\\centre_clusters_diff_ENG.csv")

intersect <- intersect(centrality_GER_classic_cluster1, centrality_GER_time_cluster1)
intersect$time_agnostic <- 1
intersect$time_sensitive <- 1

setdiff1 <- setdiff(centrality_GER_classic_cluster1, centrality_GER_time_cluster1)
setdiff1$time_agnostic <- 1
setdiff1$time_sensitive <- 0

setdiff2 <- setdiff(centrality_GER_time_cluster1, centrality_GER_classic_cluster1)
setdiff2$time_agnostic <- 0
setdiff2$time_sensitive <- 1

all <- rbind(intersect, setdiff1, setdiff2)

meta_GER <- read.csv("C:\\Users\\Brottrager\\Documents\\Diss\\RelatingTheUnread\\corpora\\GER_corpus.csv", sep = ";")
meta_GER <- subset(meta_GER, select = c(pub_year, wikiname, wikiID, author))
meta_GER$Text <- paste0(meta_GER$wikiname, "_", meta_GER$wikiID)

all <- merge(all, meta_GER, by = "Text", all.x = TRUE)

write.csv(all, "GER\\centre_clusters_diff_GER.csv")


centrality_ENG_df <- union(centrality_ENG_classic_cluster1, centrality_ENG_time_cluster1)
num_no_review_no_catalogue <- nrow(centrality_ENG_df[centrality_ENG_df$class == "No Review & No Catalogue", ])
total_rows <- nrow(centrality_ENG_df)
percentage <- (num_no_review_no_catalogue / total_rows) * 100
percentage
write.csv(centrality_ENG_df, "ENG//centrality_ENG_df.csv")

centrality_GER_df <- union(centrality_GER_classic_cluster1, centrality_GER_time_cluster1)
num_no_review_no_catalogue <- nrow(centrality_GER_df[centrality_GER_df$class == "No Review & No Catalogue", ])
total_rows <- nrow(centrality_GER_df)
percentage <- (num_no_review_no_catalogue / total_rows) * 100
percentage

write.csv(centrality_GER_df, "ENG//centrality_GER_df.csv")

centrality_ENG_classic_cluster4 <- subset(centrality_ENG_classic, 
                                          Cluster == "Cluster 4", 
                                          select = c("Text", "canonisation_score",
                                                     "Score.Range", "class"))
centrality_GER_classic_cluster4 <- subset(centrality_GER_classic, 
                                          Cluster == "Cluster 4", 
                                          select = c("id", "canonisation_score",
                                                     "Score.Range", "class"))

centrality_ENG_time_cluster4 <- subset(centrality_ENG_time, Cluster == "Cluster 4", 
                                       select = c("Text", "canonisation_score",
                                                  "Score.Range", "class"))
centrality_GER_time_cluster4 <- subset(centrality_GER_time, 
                                       Cluster == "Cluster 4", 
                                       select = c("Text", "canonisation_score",
                                                  "Score.Range", "class"))

periphery_ENG_set <- union(centrality_ENG_classic_cluster4$Text, 
                            centrality_ENG_classic_cluster4$Text)
periphery_GER_set <- union(centrality_GER_classic_cluster4$Text, 
                            centrality_GER_time_cluster4$Text)

periphery_ENG_df <- union(centrality_ENG_classic_cluster4, centrality_ENG_classic_cluster4)
periphery_GER_df <- union(centrality_GER_classic_cluster4, centrality_GER_time_cluster4)


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
