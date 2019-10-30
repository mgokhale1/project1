options(digits = 5)

#Generic function to calculcate RMSE given two sets of ratings to compare
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# We will predict all ratings in validation set to be the avg of all ratings in the training set
mu_hat <- mean(edx$rating)
mu_hat
naive_RMSE <- RMSE(validation$rating,mu_hat)
naive_RMSE


# Now we will try to model the movie effect
mu <- mean(edx$rating) 
movie_avgs <- edx %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + validation %>% left_join(movie_avgs, by='movieId') %>% .$b_i
model_1_rmse <- RMSE(predicted_ratings, valiation$rating)
model_1_rmse

#Now we will add the user effect to the model
user_avgs <- edx %>% left_join(movie_avgs, by='movieId') %>% group_by(userId) %>% summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- validation %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId') %>% mutate(pred = mu + b_i + b_u) %>% .$pred
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
model_2_rmse

# Now we will fit the model with regularized estimates. For this we will create a cross validation set out of the training (edx) set
options(digits = 5)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
temp <- edx[test_index,]
edx_val <- temp %>% semi_join(edx_train, by = "movieId") %>% semi_join(edx_train, by = "userId")
removed <- anti_join(temp, edx_val)
edx_train <- rbind(edx_train, removed)

# Now we will use cross validation to find a optimal parameter value for penalty term
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx_train$rating)
  b_i <- edx_train %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx_train %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- edx_val %>% left_join(b_i, by = "movieId") %>%left_join(b_u, by = "userId") %>% mutate(pred = mu + b_i + b_u) %>% .$pred
  return(RMSE(predicted_ratings, edx_val$rating))
})
lambda <- lambdas[which.min(rmses)]
lambda

#Use the value of lamba obtained above as a parameter for regularized model
mu <- mean(edx$rating)

movie_reg_avgs <- edx %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 
user_reg_avgs <- edx %>% left_join(movie_reg_avgs, by="movieId") %>% group_by(userId) %>% summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
predicted_ratings <- validation %>% left_join(movie_reg_avgs, by = "movieId") %>% left_join(user_reg_avgs, by = "userId") %>% mutate(pred = mu + b_i + b_u) %>%.$pred
model_3_rmse <- RMSE(predicted_ratings, validation$rating)
model_3_rmse

# Print all the results
print(paste("Method: Naive RMSE:, RMSE ->",format(naive_RMSE, digits=7)))
print(paste("Method: Movie Effect Model:, RMSE ->",format(model_1_rmse,digits=7)))
print(paste("Method: Movie + User Effect Model:, RMSE ->",format(model_2_rmse,digits=7))) 
print(paste("Method: Regularized Movie + User Effects Model:, RMSE ->",format(model_3_rmse,digits=7))) 




