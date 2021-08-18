#MASS 패키지에 있는 Animals 데이터 셋에 대해 R의 기본 함수를 이용하여 body컬럼을 
#대상으로 다음의 기술통계량을 구하시오
library("MASS")
data("Animals")
#(1) Animals 데이터 셋 구조 보기
mode(Animals);class(Animals)
dim(Animals);names(Animals)
str(Animals);head(Animals)

#(2) 요약통계량
summary(Animals)

#(3) 평균
apply(Animals,2,mean)

#(4) 표준편차
apply(Animals,2,sd)

#(5) Animals 데이터 셋의 빈도수 구하기
table(Animals)
library("ggplot2")
a <- ggplot(data=Animals, aes(x=body, y=brain))
a + geom_point(shape=20, size=5, colour="red")

# 2. 다음은 학생별 과목별 시험 점수이다. 아래 문제를 실행하시오.
#(1) 3명 학생의 과목점수를 이용하여 데이터프레임(DataFrame)을 생성하여 화면출력하시오
Lee <- c(90,75,85)
Kim <- c(70,95,70)
Kang <- c(80,60,90)
subject_1 <- c("국어(Kor)","영어(Eng)","수학(Mat)")
df <- data.frame(subject_1,Lee,Kim,Kang)
colnames(df) <- c("과목","이순신(Lee)","김유신(Kim)","강감찬(Kang)")
df

#(2) 영어과목에서 최고점을 구하시오.
max(df[2,2:4])

#(3) 김유신의 과목 평균점수를 구하시오.
mean(df[,3])

#(4) 수학과목의 분산을 구하시오.
ex <- as.numeric(df[3,2:4])
var(ex)

#(5) 국어과목의 표준편차를 구하시오.
sd(df[1,2:4])

#3. dply패키지와 iris 데이터 넷을 대상으로 아래의 문제를 실행하는 R코드를 작성하여 제출하시오
library("dplyr")
data("iris")
iris
#(1) iris의 꽃받침의 폭(Sepal.Width)이 3.7 이상의 값만 필터링하여 화면출력하시오
subset_iris <- subset(iris,Sepal.Width >= 3.7)
subset_iris

#(2) (1)의 결과에서 2, 4, 5번째 컬럼을 선택하시오
subset_iris2 <- subset_iris[,c(2,4,5)]
subset_iris2

#(3) (2)의 결과에서 2번 컬럼의 값에서 4번 컬럼의 값을 뺀 diff파생변수를 만들고, 앞부분 10개만 
#출력하시오
subset_iris3 <-  mutate(subset_iris2, diff = Sepal.Width - Petal.Width )
head(subset_iris3,10)

#(4) (3)의 결과에서 꽃의 종(Species)별로 그룹화하여 Sepal.Width와 Petal.Width 변수의 평균을 
#계산하시오
subset_iris3 %>% group_by(Species) %>% summarise(Sepal.Width_mean = mean(Sepal.Width),Petal.Width_mean=mean(Petal.Width))

#(5) (3)의 결과에서 위에서 4번째 꽃의 종(Species)는 무엇인가?
subset_iris3[4,"Species"]

#4. iris 데이터를 대상으로 다음 조건에 맞게 시각화 하시오
#(1) 1번 컬럼을 x축으로 하고 3번 컬럼을 y축으로 한다.
ggplot(iris,aes(x=Sepal.Length,y=Petal.Length)) + geom_point()

#(2) 5번 컬럼을 색상 지정한다.
qplot(Sepal.Length, Petal.Length, data = iris, color = factor(Species))

#(3) 차트 제목을 “Scatter plot for iris data”로 추가한다.
qplot(Sepal.Length, Petal.Length, data = iris, color = factor(Species),
      main = "Scatter plot for iris data")

#(4) 폭(720 픽셀)과 높이 (480픽셀)를 설정하여 작성한 차트를 파일명이 “iris_(본인 
#영문이니셜).jpg”인 파일에 저장하고 제출한다.
jpeg(filename = "iris_kgn.jpg",width = 720,height = 480,
     units = "px",bg="transparent")
qplot(Sepal.Length, Petal.Length, data = iris, color = factor(Species),
      main = "Scatter plot for iris data")
dev.off()
