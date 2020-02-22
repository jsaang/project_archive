#### 1. unique label prob calibration ####

# import best score submission
result <- read_csv("0205_3_soft_voting.csv")

result_long <- result %>% 
  gather(key = "label", value = "prob", -id) %>% 
  arrange(id)

# find unique labels
train_label <- read_csv("train_label.csv")

unique_labels <- train_label %>% 
  group_by(label) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  filter(count == 1) %>%
  select(label) %>% 
  unlist(use.names = FALSE)

# 1) unique label로 예측한 id를 찾는다
unique_labels_id <- result_long %>% 
  group_by(id) %>% 
  filter(prob == max(prob)) %>% 
  filter(label %in% unique_labels) %>% 
  select(id) %>% 
  unlist(use.names = FALSE)


# 2) 해당 id의 prob들을 mean prob vector와 가중평균하여 하향해준다
#    또는 max prob vector와 가중평균하여 상승시켜준다
weight = 0.5

result_calibrated <- result_long %>%
  group_by(id) %>% 
  mutate(max_prob = ifelse(prob == max(prob), 1, 0)) %>%
  ungroup %>% 
  mutate(new_prob = ifelse(id %in% unique_labels_id, weight * prob + (1-weight) * max_prob, prob)) %>%
  #mutate(mean_prob = 1/198) %>%
  #mutate(new_prob = ifelse(id %in% unique_labels_id, weight * prob + (1-weight) * mean_prob, prob)) %>%
  select(id, label, new_prob) %>%
  mutate(label = as.integer(label)) %>% 
  spread(key = label, value = new_prob)
  
# 3. 제출하여 성능향상이 있는지 확인하며 가중치를 조절해본다
write_csv(result_calibrated, "submission_calibrated_3.csv")

# case 1. unique label -> down prob : 점수 떨어짐
# case 2. unique label -> up prob : 점수 조금 오름(0.7865773436)

# unique label과 아닌 것들의 prob 분포도
result_long %>% 
  group_by(id) %>% 
  filter(prob == max(prob)) %>% 
  mutate(unique = ifelse(id %in% major_labels_id, TRUE, FALSE)) %>% 
  ggplot(aes(unique, prob)) +
  geom_point(position = "jitter") +
  geom_boxplot()

# unique label들의 경우 예측 확률이 거의 1에 근접한 애들과 0.5 미만인 애들로 나뉨
# 두 그룹에 대해서 확률 보정이 다르게 작용하는지 테스트!

# 그룹1, 2에 각각 확률 보정 적용
unique_labels_group1_id <- result_long %>% 
  group_by(id) %>% 
  filter(prob == max(prob)) %>% 
  filter(label %in% unique_labels) %>% 
  filter(prob >= 0.5) %>%
  select(id) %>% 
  unlist(use.names = FALSE)

unique_labels_group2_id <- result_long %>% 
  group_by(id) %>% 
  filter(prob == max(prob)) %>% 
  filter(label %in% unique_labels) %>% 
  filter(prob < 0.5) %>%
  select(id) %>% 
  unlist(use.names = FALSE)

result_calibrated <- result_long %>%
  group_by(id) %>% 
  mutate(max_prob = ifelse(prob == max(prob), 1, 0)) %>%
  ungroup %>% 
  mutate(new_prob = ifelse(id %in% unique_labels_group2_id, weight * prob + (1-weight) * max_prob, prob)) %>%
  #mutate(mean_prob = 1/198) %>%
  #mutate(new_prob = ifelse(id %in% unique_labels_id, weight * prob + (1-weight) * mean_prob, prob)) %>%
  select(id, label, new_prob) %>%
  mutate(label = as.integer(label)) %>% 
  spread(key = label, value = new_prob)

write_csv(result_calibrated, "submission_calibrated_group2.csv")

# 그룹 1(0.5이상)에만 확률 높임 보정 적용: 점수 상승 (0.7808011171)
# 그룹 2(0.5미만)에만 확률 높임 보정 적용: 점수 하락 (0.7933199696)


#### 2. well-predicted label prob calibration ####

# import one of validation result
result_val <- read_csv("confusionInfo.csv")


good_labels <- result_val %>% 
  group_by(true_label) %>% 
  summarise(true_prob = mean(pred),
            count = n()) %>% 
  arrange(desc(count), desc(true_prob)) %>% 
  filter(count == 4) %>%
  select(true_label) %>%
  unlist(use.names = FALSE)

good_labels_id <- result_long %>% 
  group_by(id) %>% 
  filter(prob == max(prob)) %>% 
  filter(label %in% good_labels) %>% 
  select(id) %>% 
  unlist(use.names = FALSE)

weight = 0.5

result_calibrated <- result_long %>%
  group_by(id) %>% 
  mutate(max_prob = ifelse(prob == max(prob), 1, 0)) %>%
  ungroup %>% 
  mutate(new_prob = ifelse(id %in% good_labels_id, weight * prob + (1-weight) * max_prob, prob)) %>%
  #mutate(mean_prob = 1/198) %>%
  #mutate(new_prob = ifelse(id %in% unique_labels_id, weight * prob + (1-weight) * mean_prob, prob)) %>%
  select(id, label, new_prob) %>%
  mutate(label = as.integer(label)) %>% 
  spread(key = label, value = new_prob)

write_csv(result_calibrated, "submission_calibrated_good.csv")

# good labels -> up prob (weight = 0.5) : 점수 상승 (0.7345620392)

# good label의 범위를 넓혀보자
good_labels <- result_val %>% 
  group_by(true_label) %>% 
  summarise(true_prob = mean(pred),
            count = n()) %>% 
  arrange(desc(count), desc(true_prob)) %>% 
  filter(count %in% c(3,4), true_prob == 1) %>%
  select(true_label) %>%
  unlist(use.names = FALSE)

good_labels_id <- result_long %>% 
  group_by(id) %>% 
  filter(prob == max(prob)) %>% 
  filter(label %in% good_labels) %>% 
  select(id) %>% 
  unlist(use.names = FALSE)

result_calibrated <- result_long %>%
  group_by(id) %>% 
  mutate(max_prob = ifelse(prob == max(prob), 1, 0)) %>%
  ungroup %>% 
  mutate(new_prob = ifelse(id %in% good_labels_id, weight * prob + (1-weight) * max_prob, prob)) %>%
  #mutate(mean_prob = 1/198) %>%
  #mutate(new_prob = ifelse(id %in% unique_labels_id, weight * prob + (1-weight) * mean_prob, prob)) %>%
  select(id, label, new_prob) %>%
  mutate(label = as.integer(label)) %>% 
  spread(key = label, value = new_prob)

write_csv(result_calibrated, "submission_calibrated_good2.csv")

# count 3까지 범위 넓혔더니 점수 떨어졌음 : 0.7361490975