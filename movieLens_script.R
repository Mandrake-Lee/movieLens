#R Script
#Author Jorge Amorós Argos
#Date   June 2021
#Script to load and develop a movie predictor program
#Prerequisites:
# * A subfolder "rda" must be avaible
# * Inside the same, 2 data files "edx.rda" and "validation.rda" must be stored
# * Those files come from the wrangling of raw data

### Libraries
library(tidyverse)
#library(lubridate)

### Functions
# Define loss function as Root Mean Square between 2 "vectors"
RMSE <- function (a, b){
  sqrt(mean((a-b)^2))
}

# Compute estimates b_i and b_u based on lambda
# Returns a dataframe with columns movieId, userId, b_i, b_u
estimate_b_x <- function(movieData, lambda){
  mu <- mean(movieData$rating)
  
  b_i <- movieData %>%
    select(-timestamp, -title, -genres) %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating-mu)/(n()+lambda))

  b_u <- movieData %>%
    select(-timestamp, -title, -genres) %>%    
    left_join(b_i, by= "movieId") %>%    
    group_by(userId) %>%
    summarise(b_u = sum(rating -b_i-mu)/(n()+lambda))

  result <- movieData %>%
    select(-timestamp, -title, -genres) %>%    
    left_join(b_i, by= "movieId") %>%  
    left_join(b_u, by="userId")
}

### Loading the dataframes, stored in ./rda

edx_file <- file.path(getwd(),"rda","edx.rda")
validation_file <- file.path(getwd(),"rda","validation.rda")

# Check if directory ./rda is created
# Note: UNDECLARED() is a workaround to stop the script
ifelse(dir.exists(file.path(getwd(),"rda")),
       print("[OK] Subdirectory /rda is available"),
       {stop("[NOK] Subdirectory /rda must be available", call. = FALSE)
        UNDECLARED()
      }
)

# Check that the data files are properly stored and available
ifelse(file.exists(edx_file) & file.exists(validation_file),
      {
      print("[OK] Data files are available. Loading...")
      load(edx_file)
      load(validation_file)},
      # Abort the script with a message
      {stop("[NOK] The data files:\n\trda/edx.rda\n\trda/validation.rda\nmust exist!\n
       Please prepare the data accordingly.", call. = FALSE)
       UNDECLARED()
      }
)

# Cleaning
rm(edx_file, validation_file)

### Wrangling the data to know all the genres
genres_tag <- str_split(as.character(edx$genres),"\\|",simplify=TRUE)
genres_tag <- unique(as.vector(t(genres_tag)))
genres_tag <- genres_tag[-3]

tmp <- edx %>%
        group_by(movieId) %>%
        summarise(genres = genres) %>%
        distinct()

tmp <- edx %>%
  select(movieId, genres) %>%
  group_by(movieId)

# After this loop, tmp wil store the breakdown of each genre into the
# basic tags.
for (tag in genres_tag){
  new_col_name <- paste0(tag)
  tmp <- tmp %>% mutate(!!sym(new_col_name) := str_detect(genres, new_col_name))
}

# The frequency each tag appears in all movies
genre_means <- tmp %>%
      ungroup() %>%
      summarise(across(genres_tag,mean))

# The number of tags combined that form a gender classification
genre_combos <- tmp %>%
      summarise(combos = sum(across(genres_tag,mean)))

# Finally, the information stored here is not useful.
# See the report for an explanation.
rm(tmp)


### Starting to fit the model
message("Starting to fit the model.")
## The most basic model, taken only as starting point
mu_hat <- mean(edx$rating)

rmse_test0 <- RMSE(validation$rating, mu_hat)
rmse_results <- data.frame(method="Only mean", RMSE = rmse_test0)

## Now consider movie and user effect with regularization
message("Entering lambda search...")
## 1st loop
# Send a scout to find optimum lambda
lambdas1 <- seq(0, 10, 1)
start_time <- Sys.time()

rmses1 <- sapply(lambdas1, function(l){
  result <- estimate_b_x(edx, l)
  mu <- mean(edx$rating)
  y_hat <- result %>%
        mutate(pred=mu+b_i+b_u) %>%
        pull(pred)  
  return(RMSE(y_hat, edx$rating))
  })

end_time <- Sys.time()

message("[info] The first tuning loop took:\t",as.period(end_time%--%start_time))

## 2nd loop: fine tuning of lambda
# Note: from 1st loop, we know that optimum must be between 0~2

lambdas2 <- seq(0, 2, 0.1)
start_time <- Sys.time()

rmses2 <- sapply(lambdas2, function(l){
  result <- estimate_b_x(edx, l)
  mu <- mean(edx$rating)
  y_hat <- result %>%
    mutate(pred=mu+b_i+b_u) %>%
    pull(pred)  
  return(RMSE(y_hat, edx$rating))
})

end_time <- Sys.time()

message("[info] The second tuning loop took:\t",as.period(end_time%--%start_time))

lambda_opt <- lambdas2[which.min(rmses2)]
rmse_opt <- min(rmses2)

### Last step for regularization, let's evaluate the accuracy on the test set

b_x <- estimate_b_x(edx, lambda_opt)

b_i_hat <- b_x %>%
          select(movieId,b_i) %>%
          group_by(movieId,b_i) %>%
          summarise(b_i_hat = b_i) %>%
          distinct()

b_u_hat <- b_x %>%
          select(userId, b_u) %>%
          group_by(userId,b_u) %>%
          summarise(b_u_hat = b_u) %>%
          distinct()  

# Compose the test dataframe
y_hat1 <- validation %>%
  select(-timestamp, -rating, -title,-genres) %>%
  left_join(b_i_hat, by= "movieId") %>%
  left_join(b_u_hat, by= "userId") %>%
  mutate(pred=mu_hat+b_i_hat+b_u_hat) %>%
  pull(pred)

rmse_test1 <- RMSE(y_hat1, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Movie & user & regularization",
                          RMSE= rmse_test1))

### Last model, considering genre bias
message("Calculating beta per genre")

# Calculate beta per movieId, userId and genre
beta_hat <- edx %>%
          select(-timestamp, -title) %>%
          left_join(b_i_hat, by= "movieId") %>%
          left_join(b_u_hat, by= "userId") %>%
          mutate(beta=rating-(mu_hat+b_i_hat+b_u_hat)) %>%
          group_by(genres) %>%
          summarise(count=n(),beta_hat=mean(beta)) %>%
          filter(count>1000) %>%
          select(-count)

# A more refined model. Let's consider some genres are filtered out before
# because they are not very rated, and therefore they are not in the validation
# set. Then we consider beta_hat=0

y_hat2 <- validation %>%
  select(-timestamp, -rating, -title) %>%
  left_join(b_i_hat, by= "movieId") %>%
  left_join(b_u_hat, by= "userId") %>%
  left_join(beta_hat, by="genres") %>%
  mutate(beta_hat = ifelse(is.na(beta_hat),0,beta_hat), pred=mu_hat+b_i_hat+b_u_hat+beta_hat) %>%
  pull(pred)

rmse_test2 <- RMSE(y_hat2, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data.frame(method="Movie & user & regularization & genre",
                                     RMSE= rmse_test2))

### Print on screen the results of each model
knitr::kable(rmse_results)

### Last not least, save relevant data in order to knit the R report
resultFile <- file.path(getwd(),"rda","moviePredictor.rda")
save(rmse_results,mu_hat, b_i_hat,b_u_hat,beta_hat,rmse_opt,lambda_opt,
    lambdas1, rmses1, lambdas2, rmses2, genre_combos, genre_means,
    genres_tag, file=resultFile)

message("Done. Relevant results saved in 'rda/resultFile.rda'")