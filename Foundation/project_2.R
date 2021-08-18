#pima-indians-diabetes
#1. Prima Indians Diabetes 데이터셋을 대상으로 당뇨의 여부(0 or 1)를 분류하는 분류기법
#3개이상을 적용하여 기법별 결과를 비교하시오.
setwd("D:/Rwork/")
diabetes_data <- read.csv("pima-indians-diabetes.csv",skip = 9,header = F)
colnames(diabetes_data) = c( 'preg', 'plas','pres','skin','test','mass', 'pedi','age','class')
diabetes_data$class <- as.factor(diabetes_data$class)
head(diabetes_data)
dim(diabetes_data)

#install.packages("adabag")
#bagging 모델 학습 라이브러리(함수)
library(adabag)

#bagging 트레이닝 샘플
train <- sample(1:768,538)
train_bagging <- diabetes_data[train,]

#bagging 실행
diabetes_bagging <- bagging(class ~ .,data = train_bagging,mfinal = 100)

#bagging 트리별 결과
diabetes_bagging$trees

#install.packages("rpart.plot")
library(rpart.plot)

#bagging 결과 트리 시각화(첫번째 트리리)
rpart.plot(diabetes_bagging$trees[[1]])


#bagging으로 분류된 결과 테이블로 확인
bagging_table <- table(diabetes_data$class[-train],predict(diabetes_bagging,diabetes_data[-train,])$class)

#bagging 정오분류표
bagging_table

#정분류율
sum(bagging_table[row(bagging_table) == col(bagging_table)])/sum(bagging_table)

#오분류율율
1-sum(bagging_table[row(bagging_table) == col(bagging_table)])/sum(bagging_table)


#boosting 트레이닝 샘플
train <- sample(1:768,538)
train_boosting <- diabetes_data[train,]

#boosting 실행
diabetes_boosting <- boosting(class ~ .,data = train_boosting,mfinal = 100,boos = T)

#boosting 트리별 결과
diabetes_boosting$trees

#boosting 결과 트리 시각화(첫번째 트리리)
rpart.plot(diabetes_boosting$trees[[1]])

#bagging으로 분류된 결과 테이블로 확인
boosting_table <- table(diabetes_data$class[-train],predict(diabetes_boosting,diabetes_data[-train,])$class)

#boosting 정오분류표
boosting_table

#정분류율
sum(boosting_table[row(boosting_table) == col(boosting_table)])/sum(boosting_table)

#오분류율율
1-sum(boosting_table[row(boosting_table) == col(boosting_table)])/sum(boosting_table)

#나이브 베이즈 분류

#트레이닝 데이터 70%로 부여
train <- sample(1:768,538)
naive_train <- diabetes_data[train,]
naive_test <- diabetes_data[-train,]
naive_train

#install.packages("klaR")
library(klaR)
#나이브 베이즈 실행
nb <- NaiveBayes(class ~ ., data = naive_train)
predict(nb,naive_test)$class
#정오분류표
temp <- table(naive_test$class, predict(nb, naive_test)$class)
temp
#정분류율
sum(tt[row(tt) == col(tt)])/sum(tt)
#오분류율
1-sum(tt[row(tt) == col(tt)])/sum(tt)

#2. Abalone Data 데이터셋을 대상으로 전복의 나이를 예측하고자 한다. 예측기법 3개이상을
#적용하여 기법별 결과를 비교하시오.
#-종속변수는 Rings를사용
abalone_data <- read.csv("abalone.csv")
colnames(abalone_data) = c("sex","length","diameter","height","whole_weight",
                           "shucked_weight","viscera_weight","shell_weight","rings")
abalone_data$sex[abalone_data$sex == "M"] <- 0
abalone_data$sex[abalone_data$sex == "F"] <- 1
abalone_data$sex[abalone_data$sex == "I"] <- 2
abalone_data$sex <- as.numeric(abalone_data$sex)
head(abalone_data)
str(abalone_data)
dim(abalone_data)

#트레이닝 데이터 샘플
train <- sample(1:4176,2926)
abalone_train_data <- abalone_data[train,]
abalone_test_data <- abalone_data[-train,]
#인공신경망
library(neuralnet)
#정규화
normalize <- function(x){
  return((x - min(x)) / (max(x) - min(x)))
}

abalone_train_nor <- caret::preProcess(abalone_train_data[,-9],method = "range")
abalone_train_nor <- predict(abalone_train_nor,abalone_train_data)
abalone_train_nor
abalone_test_nor <- caret::preProcess(abalone_test_data[,-9],method = "range")
abalone_test_nor <- predict(abalone_test_nor,abalone_test_data)
str(abalone_test_nor)
#인공신경망 실행
names(abalone_train_nor)
abalone_nnt <- neuralnet(rings ~ sex + length + diameter + height +  whole_weight + shucked_weight
                         + viscera_weight + shell_weight, data = abalone_train_nor, hidden =1,stepmax = 1e6)
summary(abalone_nnt)
abalone_nnt

# 인공신경망 시각화
plot(abalone_nnt)
.# 모델 성능평가 및 예측
abalone_nnt_result <- compute(abalone_nnt, abalone_test_nor[1:8])

temp <- abalone_nnt_result$net.result
temp
abalone_test_nor$rings <- as.numeric(abalone_test_nor$rings)
cor(temp, abalone_test_nor$rings)
#은닉층 1개 인공신경망 예측 적중률 0.6358375


#의사결정트리
library(rpart)
library(rpart.plot)
#의사결정트리 실행
abalone_rpart <- rpart(rings ~ .,data=abalone_train_data)
#의사결정트리 시각화
summary(abalone_rpart)
rpart.plot(abalone_rpart)
#의사결정트리 결과 확인
abalone_reslut <- predict(abalone_rpart, newdata = abalone_test_data)
abalone_reslut
cor(abalone_reslut, abalone_test_data$rings)
#의사결정트리 결과 예측 적중률 0.6740629


#랜덤포레스트
library(randomForest)

# 랜덤포레스트 실행
abalone_rf <- randomForest(rings ~ ., data = abalone_train_data,
                          mtry = 5, importance = T)
plot(abalone_rf)

# 변수 중요도 
importance(abalone_rf) 
varImpPlot(abalone_rf)

#랜덤 포레스트 결과
abalone_rf_result <- predict(abalone_rf, newdata = abalone_test_data)

cor(abalone_rf_result, abalone_test_data$rings)
#랜덤 포레스트 예측 적중률 0.7587502

#3. iris 데이터 셋에서 1-4 번 변수를 대상으로 유클리드 거리 매트릭스와 마할라노비스
#(Mahalanobis) 거리 매트릭스를 구하고 계층적 클러스터링을 적용하여 결과를 시각화 하시오.
data(iris)
library(cluster)
#트레이닝 데이터
train <- sample(1:150,100)
#유클리드 거리 생성 함수
iris_dist <- dist(scale(iris[train,-5]),method = "euclidean")
#군집분석
iris_hclust <- hclust(iris_dist,method = "ward.D")
summary(iris_hclust)
head(iris_hclust$merge)
#맨해튼 거리 생성 함수
iris_agnes <- agnes(iris, metric="manhattan", stand=TRUE)

par(mfrow=c(1,1))
#유클리드 덴드로그램 시각화 + 음수제거
plot(iris_hclust, hang = -1)
#그룹수 3개 그룹별 테두리 표시
rect.hclust(iris_hclust, k=3)
#맨해튼 덴드로그램 시각화 + 음수제거
par(mfrow=c(1,2))
plot(iris_agnes,hang = -1)
#그룹수 3개 그룹별 테두리 표시
rect.hclust(iris_agnes,k=3)


#4. iris데이터에서 species 컬럼 데이터를 제거한 후 k-means clustering를 실행하고 시각화하시오.
data(iris)
iris_temp <- iris[c(1:4)]
#k-means clustering
iris_kmeans <- kmeans(iris_temp,3)
#시각화
plot(iris_temp[c(1:2)], col=iris_kmeans$cluster)
points(iris_kmeans$centers[, c(1:2)], col=1:4, pch=13, cex=5)
plot(iris_temp[c(3:4)], col=iris_kmeans$cluster)
points(iris_kmeans$centers[, c(3:4)], col=1:4, pch=13, cex=5)
