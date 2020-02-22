#### 1. euclidean distance between rows ####

base_data <- read_csv("base_data.csv")

# define function get euclidean dist
row_distance <- function(x) {
  x <- as.matrix(x)
  dist <- c(NA, sqrt(rowSums((tail(x, -1) - head(x, -1))^2)))
  x <- cbind(x, distance = dist) %>% as_tibble()
  return(x)
}

# calculate distance
idx <- all_datasets %>% select(1:3)

data_dist <- base_data %>%
  # sclae variables
  mutate_at(vars(-type, -unit, -time), scale) %>%
  # get distance
  select(-1, -2, -3) %>%
  row_distance() %>%
  bind_cols(idx, .) %>%
  select(type, unit, time, distance, everything()) %>%
  # insert NA in first row of each units
  group_by(unit) %>%
  mutate(distance = ifelse(row_number(unit) == 1, NA, distance)) %>%
  ungroup


#### 2. visualization ####

# total shape of dist by time
data_dist %>%
  ggplot(aes(time, distance, group = unit)) +
  geom_line()


#### 2.1 same label distance plot ####

# Q: 같은 label을 갖고 있는 unit들의 distance 분포는 비슷하지 않을까?

# major label top 5
major_label <- data_dist %>% 
  group_by(label) %>%
  summarise(count = n()/60) %>%
  arrange(desc(count)) %>%
  select(label) %>%
  head(5) %>%
  unlist(use.names = FALSE)

# minor label top 5
minor_label <- data_dist %>% 
  group_by(label) %>%
  summarise(count = n()/60) %>%
  arrange(count) %>%
  select(label) %>% 
  head(5) %>%
  unlist(use.names = FALSE)

# make multiple plot
major_plot <- data_dist %>% 
  select(1:4) %>%
  filter(label %in% major_label) %>% 
  group_by(label) %>% 
  nest() %>%
  mutate(
    unit_count = data[[1]] %>% nrow %>% `/`(60),
    plot = pmap(list(data, label, unit_count),
                ~ggplot(data = ..1, aes(time, distance)) +
                  geom_line(aes(group = unit), color = col_pal[1]) +
                  geom_smooth(aes(group = 1), color = col_pal[3], size = 2, se = FALSE, method = 'loess') +
                  labs(
                    title = paste("Distance distribution of Label", ..2),
                    subtitle = paste(..3, "units belong to label", ..2))))

minor_plot <- data_dist %>% 
  select(1:4) %>%
  filter(label %in% minor_label) %>% 
  group_by(label) %>% 
  nest() %>%
  mutate(
    unit_count = data[[1]] %>% nrow %>% `/`(60),
    plot = pmap(list(data, label, unit_count),
                ~ggplot(data = ..1, aes(time, distance)) +
                  geom_line(aes(group = unit), color = col_pal[1]) +
                  geom_smooth(aes(group = 1), color = col_pal[3], size = 2, se = FALSE, method = 'loess') +
                  labs(
                    title = paste("Distance distribution of Label", ..2),
                    subtitle = paste(..3, "units belong to label", ..2))))

# print plots
major_plot$plot
minor_plot$plot

# A: 딱히 패턴이 보이는 것 같지는 않다.....ㅠㅠ


#### 2.2 kmeans clustering ####

# total plot을 보니 3, 4개 정도로 군집이 나뉘는듯 보인다
# 알고리즘을 돌리고 plotting을 해서 확인해보자

# k-means clustering
data_dist_clusters <- data_dist %>%
  select(1:4) %>%
  spread(key = "time", value = "distance") %>%
  {. ->> temp} %>%
  select(-1, -2, -3) %>%
  # add scale fun later
  kmeans(centers = 100) %>%
  `$`(cluster) %>%
  as_tibble %>% 
  rename(cluster = value) %>%
  bind_cols(., temp) %>%
  gather(key = "time", value = "distance", -1, -2, -3) %>%
  arrange(unit) %>%
  mutate(
    cluster = as.character(cluster),
    time = as.double(time)
  )

# join target label data
label_data <- read_csv("train_label.csv")
label_data <- label_data %>% rename(unit = id)

data_dist_clusters <- data_dist_clusters %>% 
  left_join(label_data, by = "unit")

# total plot group by clusters
data_dist_clusters %>%
  ggplot(aes(time, distance, group = unit)) +
  geom_line(aes(color = cluster), alpha = 0.5)

# label distribution of each cluster(count)
label_count <- data_dist_clusters %>%
  group_by(label) %>%
  summarise(count = n()/60) %>%
  arrange(desc(count))

data_dist_clusters %>%
  mutate(
    cluster = factor(cluster, levels = seq(1, length(unique(cluster)), 1)),
    label = factor(label, levels = label_count$label)
  ) %>%
  group_by(cluster, label) %>%
  summarise(count = n()/60) %>%
  mutate(tot_count = sum(count),
         label_ratio = round(count/tot_count, 3)) %>% 
  ggplot(aes(label, cluster)) +
  geom_tile(aes(fill = count)) +
  scale_fill_viridis() +
  labs(
    title = "Label distribution of each cluster",
    x = "Target Label (from left: Major / to right: Minor)",
    y = "Clusters"
  ) +
  theme(axis.text.x = element_text(size = 6, angle = 90),
        axis.text.y = element_text(size = 8),
        panel.grid.major = element_line(linetype = 1),
        panel.grid.minor = element_line(linetype = 1))

# label distribution of each cluster(ratio)
data_dist_clusters %>%
  mutate(
    cluster = factor(cluster, levels = seq(1, length(unique(cluster)), 1)),
    label = factor(label, levels = label_count$label)
  ) %>%
  group_by(cluster, label) %>%
  summarise(count = n()/60) %>%
  mutate(tot_count = sum(count),
         label_ratio = round(count/tot_count, 3)) %>% 
  ggplot(aes(label, cluster)) +
  geom_tile(aes(fill = label_ratio), alpha = 0.7) +
  scale_fill_viridis() +
  labs(
    title = "Label distribution of each cluster",
    x = "Target Label (from left: Major / to right: Minor)",
    y = "Clusters"
  ) +
  theme(axis.text.x = element_text(size = 6, angle = 90),
        axis.text.y = element_text(size = 8),
        panel.grid.major = element_line(linetype = 1),
        panel.grid.minor = element_line(linetype = 1))


#### 3. prediction using clustering ####

# get proportion of label by each cluster

target_distribution <- data_dist_clusters %>%
  filter(type == "train") %>% 
  group_by(cluster, label) %>%
  summarise(count = n()/60) %>%
  mutate(tot_count = sum(count),
         label_ratio = round(count/tot_count, 3)) %>%
  select(cluster, label, label_ratio) %>%
  spread(key = "label", value = "label_ratio") %>%
  mutate_at(vars(-cluster), ~replace(., is.na(.), 0))

output <- data_dist_clusters %>%
  filter(type == "test") %>%
  select(unit, cluster) %>%
  distinct() %>%
  left_join(target_distribution, by = "cluster") %>%
  select(-cluster) %>%
  rename(id = unit)

write_csv(output, "kmeans_prediction.csv")