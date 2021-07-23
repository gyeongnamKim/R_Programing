# mlbench package 설치
install.packages("mlbench")
# 데이터 로딩
library(mlbench)
data("BostonHousing")
names(BostonHousing)
dim(BostonHousing)
summary(BostonHousing)
head(BostonHousing)
str(BostonHousing)
# Medv(주택의 중위가격)을 반응변수로 하여 다중회귀분석 실행
num_medv <- lm(formula = medv ~ 1,data = BostonHousing) # 상수만 회귀분석
lm_medv <-lm(formula = medv ~ .,data = BostonHousing) #모든 변수 회귀분석
summary(lm_medv)

# 전진 선택법
step(num_medv,direction = "forward",scope = list(lower=~1,upper = lm_medv))

# 후진 제거법
step(lm_medv,direction = "back")

# 단계 선택법
step(num_medv,direction = "both",scope = list(upper = lm_medv))

#spss파일 읽어오기 위한 haven 패키지 설치 후 로딩
library(haven)
data <- read_spss("drinking_water_example.sav")
data<-data[1:7]
data <- as.data.frame(data)
data
summary(data)
str(data)
head(data)
#주성분분석
pca_data <-prcomp(data,center = T,scale. = T)
pca_data
summary(pca_data)

#주성분분석 시각화
library(psych)
pairs.panels(data,scale = T)
plot(pca_data)

#베리맥스 회전법, 요인수 2개, 요인점수 회귀분석 방법을 적용하여 요인 분석을 실행하시오.
result <- factanal(data, factors = 2,
                   rotation = "varimax",
                   scores = "regression")

# 데이터: http://data.krx.co.kr/contents/MDC/MDI/mdiLoader/index.cmd?menuId=MDC0201010101#
data <- read.csv("dataset/kospi.csv", header = T)
str(data)
summary(data)

# KOSPI 지수(종가기준) 데이터를 기준으로(오늘날짜 : 21.07.22)
kospi <-  ts(data$'종가', start = c(2011,07,22),
             end = c(2021,07,22),
             frequency = 365) 
kospi # 확인

# 1) 추세선 확인
plot(kospi,col='green',main='과거 10년 일별 KOSPI 지수',
     xlab="11-07-22 ~ 21-07-22의 kospi", ylab="kospi 지수")

# 2) 4가지 시계열 자료의 변동요인을 분해

plot(stl(kospi,'periodic'))  # periodic : 주기
a <-decompose(kospi)
attributes(a)

# 3) 시각화 
plot(a, xlab="11-07-22 ~ 21-07-22의 kospi")

