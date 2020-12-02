# Compute dwell time for Tommy's data
# 
# 
library(momentuHMM)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
path <- './R-Rocio/'
# 
# SOUTH GEORGIA
# 
load(paste0(path,"SG_M30_dir.RData"))

SG_data <- model$data
SG_data$states <- viterbi(model)

# dwell-time per state per ID
IDs <- unique(SG_data$ID)
dwell_df <- do.call(what = rbind.data.frame,lapply(IDs, function(x){
  ind <- which(levels(SG_data$ID)[SG_data$ID] == x)
  cons_counts <- rle(SG_data$states[ind]) # counts for consecutive states
  counts_df <- data.frame(state = cons_counts$values, obs = cons_counts$lengths)
  return(counts_df)
}))

dwell_df$duration <- dwell_df$obs * 25  # observations are each 25 minutes
dwell_df$state <- as.factor(dwell_df$state)

dwell_med <- dwell_df %>% 
  group_by(state) %>% 
  summarise(med = median(duration))

dwell_SG <- ggplot(data = dwell_df, aes(x = duration, color = state)) +
  geom_histogram(aes(y = stat(density)), binwidth = 25) +
  scale_color_discrete(labels = c(paste0("1 (median: ", dwell_med$med[1], " min)"),
                                  paste0("2 (median: ", dwell_med$med[2], " min)"),
                                  paste0("3 (median: ", dwell_med$med[3], " min)"))) +
  theme_light() +
  theme(legend.justification = c(0,0), legend.position = c(0.6,0.5)) +
  ggtitle("SG: duration per state")


# turning angle

angles_SG <- ggplot(SG_data, aes(x = angle, color = as.factor(states))) +
  geom_histogram(aes(y = stat(density)), binwidth = 0.1) +
  theme_light() +
  ggtitle("Crozet: angles per state")

Angles_SG <- SG_data %>% 
  group_by(as.factor(states)) %>% 
  summarise(median = median(angle, na.rm = TRUE),
            min = min(angle, na.rm = TRUE),
            max = max(angle, na.rm = TRUE),
            p5 = quantile(angle, probs = 0.05, na.rm = TRUE),
            p95 = quantile(angle, probs = 0.95, na.rm = TRUE),
            q1 = quantile(angle, probs = 0.25, na.rm = TRUE),
            q3 = quantile(angle, probs = 0.75, na.rm = TRUE))


# CROZET
# 
load(paste0(path,"Cro_M30_rel_dir2.RData"))

CRO_data <- model$data
CRO_data$states <- viterbi(model)

# dwell-time per state per ID
IDs <- unique(CRO_data$ID)
dwell_df <- do.call(what = rbind.data.frame,lapply(IDs, function(x){
  ind <- which(levels(CRO_data$ID)[CRO_data$ID] == x)
  cons_counts <- rle(CRO_data$states[ind]) # counts for consecutive states
  counts_df <- data.frame(state = cons_counts$values, obs = cons_counts$lengths)
  return(counts_df)
}))

dwell_df$duration <- dwell_df$obs * 15  # observations are each 15 minutes
dwell_df$state <- as.factor(dwell_df$state)

dwell_med <- dwell_df %>% 
  group_by(state) %>% 
  summarise(med = median(duration))

dwell_CRO <- ggplot(data = dwell_df, aes(x = duration, color = state)) +
  geom_histogram(aes(y = stat(density)), binwidth = 25) +
  scale_color_discrete(labels = c(paste0("1 (median: ", dwell_med$med[1], " min)"),
                                  paste0("2 (median: ", dwell_med$med[2], " min)"),
                                  paste0("3 (median: ", dwell_med$med[3], " min)"))) +
  theme_light() +
  theme(legend.justification = c(0,0), legend.position = c(0.6,0.5)) +
  ggtitle("CRO: duration per state")


# turning angle

angles_CRO <- ggplot(CRO_data, aes(x = angle, color = as.factor(states))) +
  geom_histogram(aes(y = stat(density)), binwidth = 0.1) +
  theme_light() +
  ggtitle("Crozet: angles per state")

Angles_CRO <- CRO_data %>% 
  group_by(as.factor(states)) %>% 
  summarise(median = median(angle, na.rm = TRUE),
            min = min(angle, na.rm = TRUE),
            max = max(angle, na.rm = TRUE),
            p5 = quantile(angle, probs = 0.05, na.rm = TRUE),
            p95 = quantile(angle, probs = 0.95, na.rm = TRUE),
            q1 = quantile(angle, probs = 0.25, na.rm = TRUE),
            q3 = quantile(angle, probs = 0.75, na.rm = TRUE))

# plots together
# 
dwell_SG / dwell_CRO

ggsave(filename = paste0(path, "duration_HMM.png"), height = 10, width = 7)
