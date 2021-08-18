# 1. 현재 작업공간을 확인하고, “C:/Temp”로 변경하시오.
#  C:/Temp가 없어서 D:/Temp 로 변경
setwd("D:/Temp")
getwd()
# 2. 다음 조건에 맞게 name, age, address 변수를 생성하고 처리하시오
#  1) 각 변수의 특성에 맞게 값을 초기화하고 결과를 확인한다
name <- "KGN"
age <- 23
address <- "Incheon Yeonsu"
#2) 다음 함수를 이용하여 각 변수의 자료형(data type)을 확인한다.
mode(name)
mode(age)
mode(address)
# 3. R 에서 제공하는 women 데이터 셋을 다음과 같이 처리하시오
#  1) women 데이터 셋은 어떤 데이터의 모음인가?
women
# Average Heights and Weights
# for American Women
head(women)
#  2) women 데이터 셋의 자료형과 자료구조는?
mode(data)
class(data)
#  3) plot()함수를 이용하여 기본 차트 그리기
plot(women)
# 4. R에서 제공하는 c()함수를 이용하여 벡터를 생성하고 데이터를 처리하시오
#  1) 1-100까지 벡터를 생성한다.
nums <- c(1:100)
nums
#  2) 생성된 벡터를 대상으로 평균을 구한다
mean(nums)

