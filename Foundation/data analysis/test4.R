library(reshape2)
data(iris)
# 1) 꽃의 종류(Species)를 기준으로 ‘넓은 형식’을 ‘긴 형식’으로 변경하기(힌트. melt()함수 이용)
m <- melt(iris,id = 'Species',na.rm = T)
m
# 2) 꽃의 종별로 나머지 4가지 변수의 합계 구하기
data <- dcast(m,Species  ~ ...,sum)
data

# 2 dplyr패키지와 iris데이터 셋을 이용하여 다음을 실행하시오
library(dplyr)
data(iris)
# 1) iris의 꽃잎의 길이( Petal.Length)컬럼을 대상으로 1.5이상의 값만 필터링하시오
iris_data <- iris %>% filter(Petal.Length >= 1.5)
iris_data
# 2) 1)번의 결과에서 1, 3, 5번 컬럼을 선택하시오
iris_data2 <- iris_data %>%  select(1,3,5)
iris_data2
# 3) 2)번의 결과에서 1-3번 컬럼의 값을 뺀 diff 파생변수를 만들고, 앞부분 6개만 출력하시오.
iris_data3 <- iris_data2 %>% mutate( diff = Sepal.Length - Petal.Length )
head(iris_data3,6)
# 4) 3)번의 결과에서 꽃의 종(Species)별로 그룹화하여 Sepal.Length와 Petal.Length변수의 평균을계산하시오
iris_data4 <-  iris_data3 %>% group_by(Species) %>% summarise(Sepal.Length_mean = mean(Sepal.Length),Petal.Length_mean = mean(Petal.Length))
iris_data4
