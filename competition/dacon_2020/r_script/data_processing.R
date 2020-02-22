#### 0. preparation ####

#### 0.1 load libraries ####
library(tidyverse)
library(data.table)
library(NbClust)
library(RColorBrewer)
library(viridis) 
library(caret)
library(randomForest)
library(e1071)
library(DMwR)
library(lightgbm)

#### 0.2 ggplot2 settings ####
theme_set(theme_minimal() +
            theme(axis.title.x = element_text(size = 15, hjust = 1),
                  axis.title.y = element_text(size = 15),
                  axis.text.x = element_text(size = 12),
                  axis.text.y = element_text(size = 12),
                  panel.grid.major = element_line(linetype = 2),
                  panel.grid.minor = element_line(linetype = 2),
                  plot.title = element_text(size = 18, colour = "grey25", face = "bold"), plot.subtitle = element_text(size = 16, colour = "grey44")))

col_pal <- c("#5EB296", "#4E9EBA", "#F29239", "#C2CE46", "#FF7A7F", "#4D4D4D")


#### 1. data import ####

#### 1.1 import fake data ####
fread_idx <- function(file_name){
  fread(file_name, nrows = 60) %>%
    mutate(unit = file_name)
}

# fake train data
train_datasets <- list.files(
  path = "./train", 
  pattern = "*.csv", 
  full.names = TRUE
) %>%
  # exclude real data
  `[`(. != "./train/30.csv") %>%
  purrr::map_df(~fread_idx(.))

# fake test data
test_datasets <- list.files(
  path = "./test", 
  pattern = "*.csv", 
  full.names = TRUE
) %>%
  # exclude real data
  `[`(. != "./test/1154.csv") %>%
  `[`(. != "./test/1168.csv") %>%
  purrr::map_df(~fread_idx(.))

#### 1.2 import real data ####
train_real <- read_csv("train/30.csv", n_max = 60)
test_real_1 <- read_csv("test/1154.csv", n_max = 60)
test_real_2 <- read_csv("test/1168.csv", n_max = 60)

#### 1.3 merge fake and real data ####

# add unit name to real data
train_real <- train_real %>% mutate(unit = "./train/30.csv")
test_real_1 <- test_real_1 %>% mutate(unit = "./test/1154.csv")
test_real_2 <- test_real_2 %>% mutate(unit = "./test/1168.csv")

# specify data role type
train_datasets <- train_datasets %>% mutate(type = "train")
train_real <- train_real %>% mutate(type = "train")
test_datasets <- test_datasets %>% mutate(type = "test")
test_real_1 <- test_real_1 %>% mutate(type = "test")
test_real_2 <- test_real_2 %>% mutate(type = "test")

# merge each dataset
train_data <- rbind(train_datasets, train_real) 
test_data <- rbind(test_datasets, test_real_1, test_real_2)

# clean unit names
train_data <- train_data %>%
  mutate(unit = stringr::str_sub(unit, 9, -5) %>% as.numeric) %>% 
  as_tibble

test_data <- test_data %>%
  mutate(unit = stringr::str_sub(unit, 8, -5) %>% as.numeric) %>% 
  as_tibble

all_datasets <- rbind(train_data, test_data)

# reorder variables
all_datasets <- all_datasets %>% select(type, unit, time, everything())


#### 2. data preprocessing ####

#### 2.1 remove useless variables ####
unique_count <- all_datasets %>%
  select(-1, -2, -3) %>%
  summarise_all(list(~n_distinct(.))) %>%
  slice(1) %>%
  unlist(., use.names = FALSE) %>%
  as_tibble() %>%
  mutate(
    id = names(all_datasets)[-c(1, 2, 3)]
  )

# get rid of only category var
continuous_col <- unique_count %>% 
  filter(value > 2) %>%
  select(id) %>%
  unlist(., use.names = FALSE)

all_datasets <- all_datasets %>% 
  select(type, unit, time, one_of(continuous_col))

#### 2.2 remove character and NA variables(only in real data) ####
temp <- all_datasets %>% select(-1, -2, -3)
temp <- temp %>% select_if(is.numeric)
temp <- temp[, !colSums(is.na(temp))]
idx <- all_datasets %>% select(1:3)

all_datasets <- bind_cols(idx, temp)

#### 2.3 save dataset ####
write_csv(all_datasets, "base_data.csv")