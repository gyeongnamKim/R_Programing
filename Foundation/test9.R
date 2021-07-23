ibrary(prettyR)
setwd("D:/Rwork/")
data <- read.csv("twomethod.csv",header = T)
summary(data)
result <- subset(data,!is.na(score),c(method,score))
method_a <- subset(result,result$method ==1)
method_b <- subset(result,result$method == 2)
score_a <- method_a$score
score_b <-method_b$score
var.test(score_a,score_b)
t.test(score_a, score_b, altr = "tow.sided")
t.test(score_a, score_b, altr = "greater")
t.test(score_a, score_b, altr = "less")

setwd("D:/Rwork/")
data <- read.csv("two_sample.csv",header = T)
head(data)
x <- data$gender
y <- data$survey
table(x)
table(y)
table(x,y,useNA = "ifany")
prop.test(c(138,107),c(174,126),alternative = "two.sided",conf.level = 0.95)
prop.test(c(138,107),c(174,126),alternative = "greater",conf.level = 0.95)
prop.test(c(138,107),c(174,126),alternative = "less",conf.level = 0.95)

setwd("D:/Rwork/")
stheight <- read.csv("student_height.csv",header = T)
head(stheight)
height <- stheight$height
summary(height)
describe(height)

shapiro.test(height)

hist(height)
result <- wilcox.test(height, mu = 148.5, alternative = "two.sided")
result$p.value

setwd("D:/Rwork/")
hdtv <- read.csv("hdtv.csv",header = T)
buy <- hdtv$buy
freq(buy)
table(buy)
table(hdtv$buy, useNA="ifany") 
binom.test(c(10,40),p=0.15)
binom.test(c(10,40), p=0.15, alternative="two.sided", conf.level=0.95)