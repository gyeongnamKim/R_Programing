#mpg 데이터 셋 로딩
library(ggplot2)
data(mpg)
mpg <- mpg[,c("displ","cyl","year","cty")]
mpg
str(mpg)
table(mpg$cty)
#학습, 검정 데이터 샘플링
library(caret)
train <- createDataPartition(y = mpg$cty, p = 0.7, list = F)
train_mpg <- mpg[train,]
test_mpg <- mpg[-train,]


#formula 생성
formula <- cty ~ .

#의사결정나무
library(party)
mpg_ctree <- ctree(formula ,data=train_mpg)
mpg_ctree_pd <- predict(mpg_ctree,test_mpg)
table(round(mpg_ctree_pd),test_mpg$cty)
plot(mpg_ctree)
#변수 중요도
#install.packages("C50")
library(C50)
cty_factor <- as.factor(train_mpg$cty)
c <- C5.0(train_mpg[-4],cty_factor)
summary(c)

#랜덤포레스트
library(randomForest)
mpg_rf <- randomForest(formula, nodesize = 3, mtry = 2, ntree = 15, data=train_mpg)
mpg_tf_pd <- predict(mpg_rf,test_mpg)
table(round(mpg_tf_pd),test_mpg$cty)

#변수 중요도
mpg_rf$importance




#weather 데이터 가져오기
weather <- read.csv("weather.csv",header = T)
weather <- weather[,-c(1,14)]
dim(weather)

#데이터 샘플링
idx <- createDataPartition(y = weather$RainTomorrow, p = 0.7, list = F)
train_weather <- weather[idx,]
test_weather <- weather[-idx,]

#의사결정트리
weather_rpart <- rpart(RainTomorrow ~ ., data = weather)
weather_pred <- predict(weather_rpart,test_weather)
weather_binary <- ifelse(weather_pred[,2] >= 0.5,"Yes Rain","No rain" )
weather_table <- table(weather_binary,test_weather$RainTomorrow)
(weather_table[1,1]+weather_table[2,2]) / nrow(test_weather)


#SVM
library(e1071)
weather$RainyDay_AshIsland <- ifelse(weather[13] == "Yes",1,0 )
train_weather <- weather[idx,]
test_weather <- weather[-idx,]
dim(test_weather)
weather$RainyDay_AshIsland
weather_svm <- svm(factor(RainyDay_AshIsland) ~ .,data=train_weather)
weather_svm

weather_pred2 <- predict(weather_svm,train_weather,drop = FALSE)
#test_weather으로 실행시 
#Error in newdata[, object$scaled, drop = FALSE] : 
#(subscript) logical subscript too long
#오류가 나옴 원인 못찾음

#predict()를 실행하면 몇개의 행이 드랍되서 결과로 반환되어서 길이를 맞춰줌줌
length(weather_pred2)
train_weather <- train_weather[1:length(weather_pred2),]

weather_svm_table <- table(weather_pred2,train_weather$RainyDay_AshIsland)
(weather_svm_table[1,1]+weather_svm_table[2,2]) / nrow(train_weather)
