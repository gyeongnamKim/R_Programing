#1. product.csv 파일의 데이터를 이용하여 다음의 단계별로 다중 회귀분석을 수행하시오
setwd("D:/Rwork/")
product <- read.csv("product.csv",header = T)
names(product)
#1단계: 학습데이터(train), 검정데이터(test)를 &:3비율로 샘플링
#변수모델링: y변수는 제품_만족도, x변수는 제품_적절성과 제품_친밀도
library(car)
model <- lm(formula = 제품_만족도 ~ 제품_적절성 + 제품_친밀도, data = product)
summary(model)

#2단계: 학습데이터 이용 회귀모델 생성
x <-sample(1:nrow(product), 0.7 * nrow(product))
train <- product[x,]
test <- product[-x,]

#3단계: 검정데이터 이용 모델 예측치 생성
pred <- predict(model, test)

#4단계: 모델 평가: cor()함수 이용
cor(pred, test$제품_만족도)

#2. ggplot2 패키지에서 제공하는 diamonds 데이터 셋을 대상으로 carat, table, depth 변수 중에서
#다이아몬드의 가격(price)에 영향을 미치는 관계를 다중회귀 부석을 이용하여 예측하시오
library(ggplot2)
data("diamonds")

#조건1: 다이아몬드 가격 결정에 가장 큰 영향을 미치는 변수는?
# carat
diamonds <- diamonds[,c("carat","table","depth","price")]
cor(diamonds$price,diamonds$carat)
cor(diamonds$price,diamonds$table)
cor(diamonds$price,diamonds$depth)

#조건2: 다중회귀 분석 결과를 정(+)과 부(-)관계로 해설
# 정:carat 부:table,depth
model_2 <- lm(formula = diamonds$price ~ diamonds$carat + diamonds$table + diamonds$depth,
              data = diamonds)
summary(model_2)
