install.packages("ROCR")
library(car)
library(lmtest)
library(ROCR)
weather = read.csv("weather.csv", stringsAsFactors = F)
dim(weather)
head(weather)
str(weather)
weather_df <- weather[ , c(-1, -6, -8, -14)]
str(weather_df)
weather_df
weather_df$RainTomorrow[weather_df$RainTomorrow == 'Yes'] <- 1
weather_df$RainTomorrow[weather_df$RainTomorrow == 'No'] <- 0
weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow)
head(weather_df)
idx <- sample(1:nrow(weather_df ), nrow(weather_df) * 0.7)
train <- weather_df[idx, ]
test <- weather_df[-idx, ]
weather_model <- glm(RainTomorrow ~ ., data = train, family = 'binomial', na.action=na.omit)
weather_model
summary(weather_model)
pred <- predict(weather_model, newdata = test, type = "response")
pred
result_pred <- ifelse(pred >= 0.5, 1, 0)
result_pred
table(result_pred)
table(result_pred, test$RainTomorrow)
pr <- prediction(pred, test$RainTomorrow)
prf <- performance(pr, measure = "tpr", x.maeasure = "fpr")
plot(prf )
