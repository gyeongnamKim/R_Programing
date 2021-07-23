# 1. R에서 제공하는 CO2 데이터 셋을 대상으로 다음과 같은 단계로 파일에 저장하시오.
# 1단계: Treatment 컬럼 값이 ‘nonchilled’ 인 경우 ‘CO2_df1.csv’ 파일로 행번호를 제외하고 저장한다.
data("CO2")
head(CO2)
d <- CO2
nonchilled <- subset(CO2,CO2$Treatment == "nonchilled")
nonchilled
write.csv(nonchilled, "‘CO2_df1.csv", row.names = FALSE,quote = F)

#2단계: Treatment 컬럼 값이 ‘chilled’인 경우 ‘CO2_df2.csv’파일로 행 번호를 제외하고 저장한다.
chilled <- subset(CO2,CO2$Treatment == "chilled")
chilled
write.csv(chilled, "‘CO2_df2.csv", row.names = FALSE,quote = F)

# 2. 본문에서 작성한 titanic변수를 이용하여 다음을 실행하시오
# 1) ‘titanic.csv’파일을 titanicData변수로 가져와서 결과를 확인하고, titanicData의 관측치와 컬럼수를 확인힌다. (힌트, str()함수 이용)data("titanic")
titanic <-read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/COUNT/titanic.csv")
write.csv(titanic, "titanic.csv", row.names = FALSE,quote = F)
titanicData <- read.csv(file = "titanic.csv")
titanicData
str(titanicData)
dim(titanicData)
l <- ncol(titanic)#titanic 컬럼 개수

titanicSel <- cbind(titanic[2],titanic[4:l])
titanicSel

str(head(titanicSel,6))

