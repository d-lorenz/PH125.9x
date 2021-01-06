


# install and load packages

if (!require(tidyverse)) install.packages('tidyverse')
if (!require(caret)) install.packages('caret')
if (!require(ggthemes)) install.packages('ggthemes')
if (!require(lubridate)) install.packages('lubridate')

library(tidyverse)
library(caret)
library(ggthemes)
library(lubridate)


#### Loading ####

# Load recipes
recipes <- read_csv(file = "RAW_recipes.csv")
# Load recipes
reviews <- read_csv(file = "RAW_interactions.csv")


revRec <- inner_join(recipes, reviews, by = c("id" = "recipe_id"))


#### Wrangling ####

# filter out reviews with a rating of 0 that mean no testing of the recipes
revRec <- revRec %>% 
  filter(rating > 0)

# filter out reviews with more than 5 hours of preparation time
revRec <- revRec %>% 
  filter(minutes < 300)

# filter out reviews with more than 50 steps of preparation
revRec <- revRec %>% 
  filter(n_steps < 50)

### Fix date format
revRec <- revRec %>% 
  mutate(submission_month = month(submitted, label = TRUE),
         submission_year = year(submitted),
         review_month = month(date, label = TRUE),
         review_year = year(date))



#### Visualization ####

data.frame(table(rating = revRec$rating)) %>%
  ggplot()+
  geom_bar(aes(rating, Freq), stat = "identity",
           fill = "cornflowerblue", color = "grey12") +
  theme_minimal()+
  ylab("Number of ratings (Log10)")+
  xlab("Rating")+
  ggtitle("Distribution of Recipes' Ratings")+
  theme_minimal()

revRec %>%
  ggplot()+
  geom_histogram(aes(minutes),
                 fill = "cornflowerblue", binwidth = 10, color = "grey12") +
  geom_vline(xintercept = mean(revRec$minutes),
             color="firebrick", linetype = 2, size = 1.3)+
  theme_minimal()+
  ylab("Number of Recipes")+
  xlab("Preparation Time (minutes)")+
  ggtitle("Distribution of Recipes' preparation times")+
  theme_minimal()

revRec %>%
  ggplot()+
  geom_histogram(aes(n_steps),
                 fill = "cornflowerblue", binwidth = 1, color = "grey12") +
  theme_minimal()+
  geom_vline(xintercept = mean(revRec$n_steps),
             color="firebrick", linetype = 2, size = 1.3) +
  ylab("Number of Recipes")+
  xlab("Number of steps")+
  ggtitle("Distribution of Recipes' preparation steps")+
  theme_minimal()

revRec %>%
  ggplot()+
  geom_histogram(aes(n_ingredients),
                 fill = "cornflowerblue", binwidth = 1, color = "grey12") +
  theme_minimal()+
  geom_vline(xintercept = mean(revRec$n_ingredients),
             color="firebrick", linetype = 2, size = 1.3)+
  ylab("Number of Recipes")+
  xlab("Number of Ingredients")+
  ggtitle("Distribution of Recipes' preparation ingredients")+
  theme_minimal()


revRec %>% 
  group_by(submission_year) %>% 
  summarize(rating = mean(rating)) %>%
  ggplot(aes(submission_year, rating)) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  ylab("Average Rating") +
  xlab("Year of submission of the recipe") +
  ggtitle("Distribution of Average Recipes' rating against the recipe submission year")

revRec %>% 
  group_by(review_year) %>% 
  summarize(rating = mean(rating)) %>%
  ggplot(aes(review_year, rating)) +
  geom_point() +
  geom_smooth() +
  theme_minimal() +
  ylab("Average Rating") +
  xlab("Year of pubblication of the revies") +
  ggtitle("Distribution of Average Recipes' rating against the review pubblication year")

revRec %>% 
  group_by(minutes) %>% 
  summarize(rating = mean(rating)) %>%
  ggplot() +
  geom_point(aes(minutes, rating), alpha = 0.7, color = "grey12") +
  stat_summary_bin(aes(minutes, rating), fun  = mean, bins = 10,
                   color = 'firebrick', geom = 'line', linetype = 2, size = 1.5) +
  theme_minimal() +
  ylab("Average Rating") +
  xlab("Number of minutes") +
  ggtitle("Distribution of Average Recipes' rating against the number of minutes")

revRec %>% 
  group_by(n_ingredients) %>% 
  summarize(rating = mean(rating)) %>%
  ggplot() +
  geom_point(aes(n_ingredients, rating), alpha = 0.7, color = "grey12") +
  stat_summary_bin(aes(n_ingredients, rating), fun  = mean, bins = 10,
                   color = 'firebrick', geom = 'line', linetype = 2, size = 1.5) +
  theme_minimal() +
  ylab("Average Rating") +
  xlab("Number of Ingredients") +
  ggtitle("Distribution of Average Recipes' rating against the number of Ingredients")

revRec %>% 
  group_by(n_steps) %>% 
  summarize(rating = mean(rating)) %>%
  ggplot() +
  geom_point(aes(n_steps, rating), alpha = 0.7, color = "grey12") +
  stat_summary_bin(aes(n_steps, rating), fun  = mean, bins = 10,
                   color = 'firebrick', geom = 'line', linetype = 2, size = 1.5) +
  theme_minimal() +
  ylab("Average Rating") +
  xlab("Number of Steps") +
  ggtitle("Distribution of Average Recipes' rating against the number of Steps")



#### Modelling ####

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = revRec$rating, times = 1, p = 0.1, list = FALSE)
train <- revRec[-test_index,]
test <- revRec[test_index,]

# Check that id and user_id are in both datasets
validation <- test %>% 
  semi_join(train, by = "id") %>%
  semi_join(train, by = "user_id")
removed <- anti_join(test, validation)
train <- rbind(train, removed)
test <- validation


#### Just the average ####

# compute the average rating across all the training records
mu_hat <- mean(train$rating)

# compute the rmse between the average and the test set
naive_rmse <- RMSE(test$rating, mu_hat)
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results


#### Recipe Effect Model ####

mu_hat <- mean(train$rating) 
recipe_avgs <- train %>% 
  group_by(id) %>% 
  summarize(b_i = mean(rating - mu_hat))
qplot(b_i, data = recipe_avgs, bins = 30, color = I("black"))

predicted_ratings <- mu_hat + test %>% 
  left_join(recipe_avgs, by='id') %>%
  pull(b_i)

# testNa <- which(is.na(predicted_ratings))
model_1_rmse <- RMSE(predicted_ratings, test$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Recipe Effect Model",  
                                 RMSE = model_1_rmse))
rmse_results %>% knitr::kable()


#### User Effect Model ####

user_avgs <- train %>% 
  group_by(user_id) %>%
  summarize(b_u = mean(rating - mu_hat))

qplot(b_u, data = user_avgs, bins = 30, color = I("black"))

predicted_ratings <- test %>% 
  left_join(user_avgs, by='user_id') %>%
  mutate(pred = mu_hat + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, test$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="User Effects Model",  
                                 RMSE = model_2_rmse))
rmse_results %>% knitr::kable()


#### Recipe + User Effect Model ####

user_avgs <- train %>% 
  left_join(recipe_avgs, by='id') %>%
  group_by(user_id) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))

qplot(b_u, data = user_avgs, bins = 30, color = I("black"))

predicted_ratings <- test %>% 
  left_join(recipe_avgs, by='id') %>%
  left_join(user_avgs, by='user_id') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, test$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Recipe + User Effects Model",  
                                 RMSE = model_2_rmse))
rmse_results %>% knitr::kable()


#### Regularized User Effect Model ####

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu_hat <- mean(train$rating) 
  
  # b_r <- train %>% 
  #   group_by(id) %>%
  #   summarize(b_r = sum(rating - mu_hat)/(n()+l))
  
  b_u <- train %>% 
    # left_join(b_r, by="id") %>%
    group_by(user_id) %>%
    summarize(b_u = sum(rating - mu_hat)/(n()+l))
  
  predicted_ratings <- test %>%
    left_join(b_u, by='user_id') %>%
    mutate(pred = mu_hat + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})
lambdas[which.min(rmses)]
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized User Effects Model",  
                                 RMSE = min(rmses)))
rmse_results %>% knitr::kable()


#### User + Minutes Effects Model ####

minutes_avgs <- train %>% 
  left_join(user_avgs, by='user_id') %>%
  group_by(minutes) %>% 
  summarize(b_m = mean(rating - mu_hat - b_u))

qplot(b_m, data = minutes_avgs, bins = 30, color = I("black"))

predicted_ratings <- test %>% 
  left_join(user_avgs, by='user_id') %>%
  left_join(minutes_avgs, by='minutes') %>%
  mutate(pred = mu_hat + b_u + b_m) %>%
  pull(pred)

model_3_rmse <- RMSE(predicted_ratings, test$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="User + Minutes Effects Model",  
                                 RMSE = model_3_rmse))
rmse_results %>% knitr::kable()



#### User + Minutes + Steps Effects Model ####

step_avgs <- train %>% 
  left_join(user_avgs, by='user_id') %>%
  left_join(minutes_avgs, by='minutes') %>%
  group_by(n_steps) %>%
  summarize(b_s = mean(rating - mu_hat - b_u - b_m))

qplot(b_s, data = step_avgs, bins = 30, color = I("black"))

predicted_ratings <- test %>% 
  left_join(user_avgs, by='user_id') %>%
  left_join(minutes_avgs, by='minutes') %>%
  left_join(step_avgs, by='n_steps') %>%
  mutate(pred = mu_hat + b_u + b_m + b_s) %>%
  pull(pred)

model_4_rmse <- RMSE(predicted_ratings, test$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="User + Minutes + Steps Effects Model",  
                                 RMSE = model_4_rmse))
rmse_results %>% knitr::kable()



#### User + Minutes + Steps + Ingredients Effects Model ####

ingredients_avgs <- train %>% 
  left_join(user_avgs, by='user_id') %>%
  left_join(minutes_avgs, by='minutes') %>%
  left_join(step_avgs, by='n_steps') %>%
  group_by(n_ingredients) %>%
  summarize(b_i = mean(rating - mu_hat - b_u - b_m - b_s))

qplot(b_i, data = ingredients_avgs, bins = 30, color = I("black"))

predicted_ratings <- test %>% 
  left_join(user_avgs, by='user_id') %>%
  left_join(minutes_avgs, by='minutes') %>%
  left_join(step_avgs, by='n_steps') %>%
  left_join(ingredients_avgs, by='n_ingredients') %>%
  mutate(pred = mu_hat + b_u + b_m + b_s + b_i) %>%
  pull(pred)

model_5_rmse <- RMSE(predicted_ratings, test$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="User + Minutes + Steps + Ingredients Effects Model",  
                                 RMSE = model_5_rmse))
rmse_results %>% knitr::kable()



#### Regularized User + Minutes + Steps + Ingredients Effects Model ####

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu_hat <- mean(train$rating) 
  
  b_u <- train %>%
    group_by(user_id) %>%
    summarize(b_u = sum(rating - mu_hat)/(n()+l))
  
  b_m <- train %>%
    left_join(b_u, by='user_id') %>%
    group_by(minutes) %>%
    summarize(b_m = sum(rating - mu_hat - b_u)/(n()+l))
  
  b_s <- train %>%
    left_join(b_u, by='user_id') %>%
    left_join(b_m, by='minutes') %>%
    group_by(n_steps) %>%
    summarize(b_s = sum(rating - mu_hat - b_u - b_m)/(n()+l))
  
  b_i <- train %>%
    left_join(b_u, by='user_id') %>%
    left_join(b_m, by='minutes') %>%
    left_join(b_s, by='n_steps') %>%
    group_by(n_ingredients) %>%
    summarize(b_i = sum(rating - mu_hat - b_u - b_m - b_s)/(n()+l))
  
  predicted_ratings <- test %>%
    left_join(b_u, by='user_id') %>%
    left_join(b_m, by='minutes') %>%
    left_join(b_s, by='n_steps') %>%
    left_join(b_i, by='n_ingredients') %>%
    mutate(pred = mu_hat + b_u + b_m + b_s + b_i) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})
lambdas[which.min(rmses)]
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized User + Minutes + Steps + Ingredients Effects Model",  
                                 RMSE = min(rmses)))
rmse_results %>% knitr::kable()



#### Submission Year Effects Model ####

subY_avgs <- train %>% 
  group_by(submission_year) %>%
  summarize(b_sy = mean(rating - mu_hat))

qplot(b_sy, data = subY_avgs, bins = 30, color = I("black"))

predicted_ratings <- test %>% 
  left_join(subY_avgs, by='submission_year') %>%
  mutate(pred = mu_hat + b_sy) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, test$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Submission Year Effects Model",  
                                 RMSE = model_2_rmse))
rmse_results %>% knitr::kable()



#### Review Year Effects Model ####

revY_avgs <- train %>% 
  left_join(subY_avgs, by='submission_year') %>%
  group_by(review_year) %>% 
  summarize(b_ry = mean(rating - mu_hat - b_sy))

qplot(b_ry, data = revY_avgs, bins = 30, color = I("black"))

predicted_ratings <- test %>% 
  left_join(subY_avgs, by='submission_year') %>%
  left_join(revY_avgs, by='review_year') %>%
  mutate(pred = mu_hat + b_sy + b_ry) %>%
  pull(pred)


model_2_rmse <- RMSE(predicted_ratings, test$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Review Year Effects Model",  
                                 RMSE = model_2_rmse))
rmse_results %>% knitr::kable()



#### User + Minutes + Steps + Ingredients + Sub + Rev Effects Model ####

revY_avgs <- train %>% 
  left_join(user_avgs, by='user_id') %>%
  left_join(minutes_avgs, by='minutes') %>%
  left_join(step_avgs, by='n_steps') %>%
  left_join(ingredients_avgs, by='n_ingredients') %>%
  left_join(subY_avgs, by='submission_year') %>%
  group_by(review_year) %>% 
  summarize(b_ry = mean(rating - mu_hat - b_u - b_m - b_s - b_i - b_sy))

qplot(b_i, data = ingredients_avgs, bins = 30, color = I("black"))

predicted_ratings <- test %>% 
  left_join(user_avgs, by='user_id') %>%
  left_join(minutes_avgs, by='minutes') %>%
  left_join(step_avgs, by='n_steps') %>%
  left_join(ingredients_avgs, by='n_ingredients') %>%
  left_join(subY_avgs, by='submission_year') %>%
  left_join(revY_avgs, by='review_year') %>%
  mutate(pred = mu_hat + b_u + b_m + b_s + b_i + b_sy + b_ry) %>%
  pull(pred)

model_5_rmse <- RMSE(predicted_ratings, test$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="User + Minutes + Steps + Ingredients + Sub + Rev Effects Model",  
                                 RMSE = model_5_rmse))
rmse_results %>% knitr::kable()



#### Regularized User + Minutes + Steps + Ingredients + Years Effects Model ####

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu_hat <- mean(train$rating) 
  
  b_u <- train %>%
    group_by(user_id) %>%
    summarize(b_u = sum(rating - mu_hat)/(n()+l))
  
  b_m <- train %>%
    left_join(b_u, by='user_id') %>%
    group_by(minutes) %>%
    summarize(b_m = sum(rating - mu_hat - b_u)/(n()+l))
  
  b_s <- train %>%
    left_join(b_u, by='user_id') %>%
    left_join(b_m, by='minutes') %>%
    group_by(n_steps) %>%
    summarize(b_s = sum(rating - mu_hat - b_u - b_m)/(n()+l))
  
  b_i <- train %>%
    left_join(b_u, by='user_id') %>%
    left_join(b_m, by='minutes') %>%
    left_join(b_s, by='n_steps') %>%
    group_by(n_ingredients) %>%
    summarize(b_i = sum(rating - mu_hat - b_u - b_m - b_s)/(n()+l))
  
  b_sy <- train %>%
    left_join(b_u, by='user_id') %>%
    left_join(b_m, by='minutes') %>%
    left_join(b_s, by='n_steps') %>%
    left_join(b_i, by='n_ingredients') %>%
    group_by(submission_year) %>%
    summarize(b_sy = sum(rating - mu_hat - b_u - b_m - b_s - b_i)/(n()+l))
  
  b_ry <- train %>%
    left_join(b_u, by='user_id') %>%
    left_join(b_m, by='minutes') %>%
    left_join(b_s, by='n_steps') %>%
    left_join(b_i, by='n_ingredients') %>%
    left_join(b_sy, by='submission_year') %>%
    group_by(submission_year) %>%
    summarize(b_ry = sum(rating - mu_hat - b_u - b_m - b_s - b_i - b_sy)/(n()+l))
  
  
  predicted_ratings <- test %>%
    left_join(b_u, by='user_id') %>%
    left_join(b_m, by='minutes') %>%
    left_join(b_s, by='n_steps') %>%
    left_join(b_i, by='n_ingredients') %>%
    left_join(b_sy, by='submission_year') %>%
    left_join(revY_avgs, by='review_year') %>%
    mutate(pred = mu_hat + b_u + b_m + b_s + b_i + b_sy + b_ry) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})
lambdas[which.min(rmses)]
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized User + Minutes + Steps + Ingredients + Years Effects Model",  
                                 RMSE = min(rmses)))
rmse_results %>% knitr::kable()

