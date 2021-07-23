#1. descriptive.csv 데이터 셋을 대상으로 다음 조건에 맞게 빈도분석 및 기술통계량 분석을
#수행하시오
setwd("D:/Rwork/")
getwd()
install.packages("moments")
library(moments)

data <- read.csv("descriptive.csv",header = T)
names(data)

#1) 명목척도 변수인 학교유형(type), 합격여부(pass)변수에 대해 빈도분석을 수행하고 결과를 막대
#그래프와 파이 차트로 시각화
attach(data)
round(prop.table(table(type))*100,2)
par(mfrow=  c(1,2))
barplot(table(type))
pie(table(type))

round(prop.table(table(pass))*100,2)
par(mfrow=  c(1,2))
barplot(table(pass))
pie(table(pass))
#detach(data,unload = T)

#2) 비율척도 변수인 나이 변수에 대해 요약치(평균, 표준편차)와 비대칭도(왜도와 첨도) 통계량을
#구하고, 히스토그램 을 작성하여 비대칭도 통계량 설명
summary(age)
sd(age)

skewness(age)
kurtosis(age)
par(mfrow = c(1, 1))
hist(age,freq=F)

#3) 나이 변수에 대한 밀도분포 곡선과 정규분포 곡선으로 정규분포 검정
lines(density(age),col="orange")
x <- seq(30, 69, 0.1)
curve(dnorm(x, mean(age), sd(age)), col = 'green', add = T)
detach(data,unload = T)
#2. MASS 패키지에 있는 Animals 데이터 셋을 이용하여 각 단계에 맞게 기술통계량을 구하시오

#1) MASS 패키지 설치와 메모리 로딩
install.packages("MASS")
library(MASS)
data("Animals")
#2) R 의 기본함수를 이용하여 brain 컬럼을 대상으로 다음의 제시된 기술통계량 구하기

#(1) Animals 데이터 셋 차원보기
dim(Animals)
class(Animals)

#(2) 요약통계량
summary(Animals)
plot(Animals$body)
plot(Animals$brain)
Animals <- subset(Animals,Animals$body <= 2000 & Animals$brain <= 2000)
attach(Animals)
#(3) 평균
mean(brain)
#(4) 중위수
median(brain)
#(5) 표준편차
sd(brain)
#(6) 분산
var(brain)
#(7) 최대값
max(brain)
#(8) 최소값
min(brain)
detach(Animals,unload = T)
