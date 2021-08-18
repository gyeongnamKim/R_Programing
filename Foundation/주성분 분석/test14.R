data("iris")
head(iris)
cor(iris[1:4])
iris2 <- iris[, 1:4] 
ir.species <- iris[,5] 
prcomp.result2 <- prcomp(iris2, center=T, scale=T)
prcomp.result2
summary(prcomp.result2)
plot(prcomp.result2, type="l")
prcomp.result2$rotation
iris2
Result3 <- as.matrix(iris2) %*% prcomp.result2$rotation
head(Result3)

head(Result3)
final2 <- cbind(ir.species, as.data.frame(Result3))
final2
final2[,1] <- as.factor(final2[,1])
colnames(final2)[1] <- "label1"
final2
fit3 <- lm(label1 ~ PC1 + PC2, data=final2) 
fit3_pred <-predict(fit3, newdata=final2)
b2 <- round(fit3_pred)
a2 <- ir.species
table(b2,a2)

install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")

