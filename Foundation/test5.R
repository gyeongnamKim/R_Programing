dataset <- read.csv("dataset.csv", header = T)
dataset2 <- subset(dataset, age >= 20 & age <= 69)

dataset2$resident2[dataset2$resident == 1] <- '1.서울특별시'
dataset2$resident2[dataset2$resident == 2] <- '2.인천광역시'
dataset2$resident2[dataset2$resident == 3] <- '3.대전광역시'
dataset2$resident2[dataset2$resident == 4] <- '4.대구광역시'
dataset2$resident2[dataset2$resident == 5] <- '5.시구군'
dataset2$age2[dataset2$age <= 30] <- "청년층"
dataset2$age2[dataset2$age > 30 & dataset2$age <= 55] <- "중년층"
dataset2$age2[dataset2$age > 55 ] <- "장년층"
dataset2$job2[dataset2$job == 1] <- '공무원'
dataset2$job2[dataset2$job == 2] <- '회사원'
dataset2$job2[dataset2$job == 3] <- '개인사업'

# 1. 본문에서 생성된 dataset2의 직급(postion)컬럼을 대상으로 1급  5급, 5급  1급 형식으로 역코딩하여 position2컬럼에 추가하시오

survey <- dataset2$position
csurvey <- 6 - survey
dataset2$position2 <- csurvey

# 2. 본문에서 생성된 dataset2의 resident 컬럼을 대상으로 NA값을 제거한 후 resident2변수에 저장하시오.

resident2 <- subset(dataset2,!is.na(dataset2$resident))
head(resident2)

# 3. 본문에서 생성된 dataset2의 gender컬럼을 대상으로 1  “남자”, 2 “여자”로 코딩 변경하여gender2 컬럼에 추가하고, 파이차트로 결과를 확인하시오.

dataset2$gender2 <- ifelse(dataset2$gender == 1,"남자","여자")
pie(table(dataset2$gender2),col = c("blue","red"))

# 4. 본문에서 생성된 dataset2의 age컬럼을 대상으로 30세이하  1, 30-55세  2, 55이상  3으로리코딩하여 age3컬럼에 추가한 뒤에 age, age2, age3 컬럼만 확인하시오.

dataset2$age3[dataset2$age <= 30] <- 1
dataset2$age3[30 < dataset2$age & dataset2$age < 55] <- 2
dataset2$age3[55 <= dataset2$age] <- 3

print(dataset2[c("age","age2","age3")])

# 5. 정제된 data를 대상으로 작업 디렉터리(“C/Rwork/”)에 파일 이름을 “cleandata.csv”로 하여 따옴표와 행 이름을 제거하여 저장하고, 저장된 파일의 내용을 읽어 new_data변수에 저장하고 확인하시오.
getwd()
setwd("D:/Rwork")
write.csv(dataset2, "cleandata.csv", quote = F, row.names = F)

new_data <- read.csv("cleandata.csv",header = T)
head(new_data)

# 6. dataset#3 내 “user_data.csv”, “return_data.csv”파일을 이용하여 고객별 반폼사유코드(return_code)를 대상으로 다음과 같이 파생변수를 추가하시오.
library(reshape2)

user_data <- read.csv("user_data.csv",header = T)
head(user_data)
return_data <- read.csv("return_data.csv",header = T)
head(return_data2,10)
return_data1 <- dcast(return_data, user_id ~ return_code,length)
#1) 반품사유코드에 대한 파생변수 컬럼명 설명
#제품이상(1) -> return_code1
#변심(2) -> return_code2
#원인불명(3) -> return_code3
#기타(4) -> return_code4
names(return_data1) <- c("user_id","return_code1" ,"return_code2","return_code3","return_code4" )
return_data2 <- join(user_data,return_data1,by="user_id")

# 7. iris데이터를 이용하여 5겹 2회 반복하는 교차 검정 데이터를 샘플링하시오.

data("iris")

iris_result <- cvFolds(n = nrow(iris),K = 5, R = 2,type = "random")
iris_result$subsets
R = 1:2
K = 1:5
for(i in R){
  for(j in K){
    datas_idx <- iris_result$subsets[iris_result$which == j,i]
    cat('K = ', j,'R = ', i, '검정데이터 \n')
    print(iris[datas_idx, ])
    
    cat('K = ', j,'R = ', i, '훈련데이터 \n')
    print(iris[-datas_idx, ])
    
  }
}

