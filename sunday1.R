library(tidyverse)

cars <- read.csv("data/cars2020.csv")

ggplot(data = cars,
       mapping = aes(x = disp, y = mpg,
                     color = transmission)) + 
  geom_point() 

ggplot(data = cars,
       mapping = aes(x = disp, y = mpg,
                     color = transmission)) + 
  geom_point() + facet_wrap(~transmission)

### Using boxplot to compare mpg for different transmission types

ggplot(data = cars,
       aes(y = transmission, x = mpg)) + 
  geom_boxplot()

ggplot(data = cars,
       aes(x = fuel)) + geom_bar()

ggplot(data = cars,
       aes(x = mpg, y = fuel)) + geom_boxplot()


### Making a simple linear model

model1 <- lm(mpg~transmission, data = cars)

cars$pred1 <- predict(model1, newdata = cars)

model2 <- lm(mpg~disp, data = cars)

cars <- mutate(cars, pred2 = predict(model2, newdata = cars))
cars <- cars2020 

ggplot(data = cars, 
       aes(x = mpg, y = pred1)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  theme_minimal() + ylim(c(0,60)) + xlim(c(0,60))

ggplot(data = cars,
       aes(x = mpg, y = pred2)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  theme_minimal() + ylim(c(0,60)) + xlim(c(0,60))

model3 <- lm(mpg~disp+transmission, data = cars)

cars <- cars |> mutate(pred3 = predict(model3, newdata = cars))
# same as mutate(cars, pred3 = predict(model3, newdata = cars))

ggplot(data = cars, 
       aes(x = mpg, y = pred3)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  theme_minimal() + ylim(c(0,60)) + xlim(c(0,60))

model3 <- lm(mpg~disp+transmission, data = cars)

cars <- cars |> mutate(pred3 = predict(model3, newdata = cars))

temp_df <- cars |> 
  pivot_longer(pred1:pred3, names_to = "model prediction")

ggplot(data = temp_df,
       aes(x = mpg, y = value)) +
  geom_point() + 
  facet_wrap(~`model prediction`) + 
  geom_abline(intercept = 0, slope = 1) + 
  theme_minimal() + ylim(c(0,60)) + xlim(c(0,60))
  
#summary(model3)

library(rpart)
library(rpart.plot)
model4 <- rpart(mpg~disp+transmission, data = cars)

rpart.plot(model4)

cars <- mutate(cars, pred4 = predict(model4, newdata = cars))

temp_df <- cars |> 
  pivot_longer(pred1:pred4, names_to = "model prediction")

ggplot(data = temp_df,
       aes(x = mpg, y = value)) +
  geom_point() + 
  facet_wrap(~`model prediction`) + 
  geom_abline(intercept = 0, slope = 1) + 
  theme_minimal() + ylim(c(0,60)) + xlim(c(0,60))

model5 <- rpart(mpg~disp+transmission+fuel+atvType, data = cars)
rpart.plot(model5)

















