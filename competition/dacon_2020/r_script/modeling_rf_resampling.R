set.seed(1234)

#### 1. Prepare Train & Test Set ####

# import base data (preprocessed train + test)
base_data <- read_csv("base_data.csv")

# use data after 10 seconds
base_data <- base_data %>% 
  filter(time >= 10) %>% 
  select(-time)

# split train & test set
train <- base_data %>% 
  filter(type == "train") %>% 
  select(-type)

test <- base_data %>% 
  filter(type == "test") %>% 
  select(-type)

# join label data
label_data <- read_csv("train_label.csv")
label_data <- label_data %>% rename(unit = id)

train <- train %>% 
  left_join(label_data, by = "unit") %>%
  select(label, unit, everything())

# remove minor labeled unit
minor_label_removed <- train %>% 
  group_by(label) %>%
  summarise(count = n()/60) %>%
  filter(count > 2) %>%
  select(label) %>% 
  unlist(use.names = FALSE)

train <- train %>%
  filter(label %in% minor_label_removed)

train_unit <- train %>% select(unit)
train <- train %>% select(-unit)

test_unit <- test %>% select(unit)
test <- test %>% select(-unit)

train <- train %>% mutate(label = factor(label))

#### 2. RF Modeling with Resampling Method ####

# simple rf model + base data
train_crtl <- trainControl(method = "cv", number = 5)

model_rf <- train(label ~ .,
                  data = train,
                  method = "rf",
                  trControl = train_crtl,
                  allowParallel = TRUE)

model_rf <- randomForest(label ~ ., data = train, ntree = 50)

pred <- predict(model_rf, newdata = test, type = "prob")


pred <- bind_cols(test_unit, as_tibble(pred))
output <- pred %>% 
  group_by(unit) %>%
  summarise_all(max)

write_csv(output, "base_rf_2.csv")


# simple rf model + distance var added data

# lightgbm model
