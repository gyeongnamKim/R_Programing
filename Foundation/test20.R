install.packages("party")
install.packages("caret")
library(party) 
library(caret) 
# data sampling
data1 <- iris[sample(1:nrow(iris), replace=T),] 
data2 <- iris[sample(1:nrow(iris), replace=T),]
2
data3 <- iris[sample(1:nrow(iris), replace=T),]
data4 <- iris[sample(1:nrow(iris), replace=T),]
data5 <- iris[sample(1:nrow(iris), replace=T),]
# 예측모형 생성
citree1 <- ctree(Species~., data1) 
citree2 <- ctree(Species~., data2)
citree3 <- ctree(Species~., data3)
citree4 <- ctree(Species~., data4)
citree5 <- ctree(Species~., data5)
# 예측수행
predicted1 <- predict(citree1, iris)
predicted2 <- predict(citree2, iris)
predicted3 <- predict(citree3, iris)
predicted4 <- predict(citree4, iris)
predicted5 <- predict(citree5, iris)
# 예측모형 결합하여 새로운 예측모형 생성
newmodel <- data.frame(Species=iris$Species, 
                       predicted1,predicted2,predicted3,predicted4,predicted5)
head(newmodel)
newmodel
# 최종모형으로 통합
funcValue <- function(x) { 
  result <- NULL
  for(i in 1:nrow(x)) {
    xtab <- table(t(x[i,]))
    rvalue <- names(sort(xtab, decreasing = T) [1])
    result <- c(result,rvalue)
  }
  return (result)
}
newmodel
# 최종 모형의 2번째에서 6번째를 통합하여 최종 결과 생성
newmodel$result <- funcValue(newmodel[, 2:6])
3
newmodel$result
# 최종결과 비교
table(newmodel$result, newmodel$Species)
