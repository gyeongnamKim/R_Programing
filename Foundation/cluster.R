library(cluster)
x <- matrix(1:9, nrow = 3, by = T)

dist <- dist(x, method = "euclidean")
hc <- hclust(dist)
plot(hc)
interview <- read.csv("interview.csv", header = TRUE)
names(interview)
head(interview)
interview_df <- interview[c(2:7)]
idist <- dist(interview_df)
head(idist)
interview_df
hc <- hclust(idist)
hc
plot(hc, hang = -1)
rect.hclust(hc, k = 3, border ="red")
g1 <- subset(interview, no == 108 | no == 110 | no == 107 |
               no == 112 | no == 115)
g2 <- subset(interview, no == 102 | no == 101 | no == 104 |
               no == 106 | no == 113)
g3 <- subset(interview, no == 105 | no == 114 | no == 109 |
               no == 103 | no == 111)
summary(g1)
summary(g2)
summary(g3)
idist <- dist(iris[1:4])
hc <- hclust(idist)
plot(hc, hang = -1)
ghc <- cutree(hc, k = 3)
ghc
iris$ghc <- ghc
table(iris$ghc)
head(iris)
g1 <- subset(iris, ghc == 1)
summary(g1[1:4])
g2 <- subset(iris, ghc == 2)
summary(g2[1:4])
g3 <- subset(iris, ghc == 3)
summary(g3[1:4])

library(ggplot2)
data(diamonds)
t <- sample(1:nrow(diamonds), 1000)
test <- diamonds[t, ]
dim(test)
head(test)
mydia <- test[c("price", "carat", "depth", "table")]
head(mydia)
result <- hclust(dist(mydia), method = "average")
result
plot(result, hang = -1)
result2 <- kmeans(mydia, 3)
names(result2)
result2$cluster
mydia$cluster <- result2$cluster
head(mydia)
cor(mydia[ , -5], method = "pearson")
plot(mydia[ , -5])

install.packages("mclust")
library(mclust)
install.packages("corrgram")
library(corrgram)
corrgram(mydia[ , -5], upper.panel = panel.conf)
corrgram(mydia[ , -5], lower.panel = panel.conf)
plot(mydia$carat, mydia$price, col = mydia$cluster)
points(result2$centers[ , c("carat", "price")],
       col = c(3, 1, 2), pch = 8, cex = 5)

### Hierarchical Clustering
# iris 데이터셋 로딩(50개)
data(iris)
idx <- sample(1:dim(iris)[1], 50) 
idx 
irisSample <- iris[idx, ] 
head(irisSample)
# species 데이터 제거
irisSample$Species <- NULL 
head(irisSample)
# 계층적 군집법 실행
hc_result <- hclust(dist(irisSample), method="ave")
hc_result
# 군집 결과 시각화
plot(hc_result, hang=-1, labels = iris$Species[idx]) 
rect.hclust(hc_result, k=4)
