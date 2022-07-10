library(tidyverse)
library(rsample)

so <- read.csv("data/stackoverflow.csv")

so_summary <- group_by(so, country, remote) |>
  summarise(respondents = n(), med_salary = median(salary, na.rm = TRUE))

ggplot(data = so,
       aes(x = country)) + 
  geom_bar(fill = NA, color = "brown") + theme_minimal()

count(so, remote)

so <- mutate(so, 
             remote_binary = ifelse(remote == "Not remote", 0, 1)) |>
  relocate(remote_binary, remote)

########

set.seed(2001)

### Split the data into two data frames
split <- initial_split(so, prop = 0.8)

train <- training(split)
test <- testing(split)



##### Logistic regression model using glm 

logistic_regression_model <- glm(remote_binary~country+salary+years_coded_job,
                                 data = train, 
                                 family = binomial(link = "logit"))


so <- mutate(so, pred1 = predict(logistic_regression_model, newdata = so,
                                 type = "response"))

hist(so$pred1)

ggplot(data = so,
       aes(x = pred1)) + 
  geom_histogram(fill = "gray", color = "black")


#### As an aside: we can make a decision tree model
library(rpart)
library(rpart.plot)
dt_model <- rpart(remote_binary~country+salary+years_coded_job,
                  data = train)
#dt_model2 <- rpart(remote~country+salary+years_coded_job,
                   #data = train)
rpart.plot(dt_model)
#rpart.plot(dt_model2)

### To balance the data

### To handle imbalance of data 

remote_df <- filter(so, remote == "Remote")
notremote_df <- filter(so, remote == "Not remote")

### Now take half of the remote workers for the training set. Then take the 
### same amount from the not remote group 

set.seed(2001)
sample1 <- sample(718, 359) # chooses 359 numbers between 1 and 718
remote_train <- remote_df[sample1, ]
sample2 <- sample(6273, 359)
notremote_train <- notremote_df[sample2, ]

new_train <- bind_rows(remote_train, notremote_train)

### New logistic regression model trained on balanced training set
logistic_regression_model2 <- glm(remote_binary~country+salary+years_coded_job,
                                  data = new_train, family = binomial(link = "logit"))

so <- mutate(so, pred2 = predict(logistic_regression_model2, newdata = so,
                                 type = "response"))


ggplot(data = so,
       aes(x = remote, y = pred2)) + 
  geom_boxplot()

ggplot(data = so,
       aes(x = remote, y = pred2)) + 
  geom_point(alpha = 0.05) + theme_minimal()

ggplot(data = so,
       aes(x = remote, y = pred2)) +
  geom_jitter() + 
  geom_hline(yintercept = 0.4, 
             linetype = "dashed", color = "red")
ggplot(data = so,
       aes(x = pred2)) + geom_density() + 
  facet_wrap(~remote, ncol = 1) + 
  geom_vline(xintercept = 0.4, color = "red", linetype = "dashed") + 
  geom_vline(xintercept = 0.6, color = "red", linetype = "dashed")



















