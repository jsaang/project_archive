train <- read_csv("v04_train_all.csv")
test  <- read_csv("v04_test_all.csv")

dtrain <- lgb.Dataset(
  data  = as.matrix(train %>% select(-id, -label)),
  label = as.matrix(train %>% select(label))
)

bst <- lightgbm(
  data = dtrain,
  params = bst_param,
  num_threads = 6,
  learning_rate = 0.02,
  nrounds = 100
)

bst_param <- list(objective = "multiclass",
                  num_class = 198,
                  metric = "multi_logloss",
                  feature_fraction  = 0.7,
                  bagging_fraction = 0.7,
                  bagging_freq = 5,
                  is_unbalance = TRUE)


