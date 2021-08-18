# 1. 다음의 벡터 EMP는 ‘입사연도이름급여’순으로 사원의 정보가 기록된 데이터이다. 벡터 EMP를# 이용하여 다음과 같은 출력 결과가 나타나도록 함수를 정의하시오.
library(stringr)
#나름 전처리
EMP <- c("2014홍길동220", "2002이순신300", "2010유관순260")

entry <- unlist(str_extract_all(EMP,"[0-9]{4}"))

name <- unlist(str_extract_all(EMP,"[가-힣]{3}"))

pay <- str_extract_all(EMP,"[0-9]{3}",simplify = T)
pay <- pay[,-1]
pay <- as.numeric(pay)

EMP <- data.frame(entry,name,pay)

emp_pay<-function(x){
  cat("전체급여 평균: ",mean(x$pay))
  cat("\n")
  cat("평균 이상 급여 수령자\n")
  temp2 <- subset(x,x$pay >= mean(x$pay))
  for(i in 1:nrow(temp2)){
    cat(temp2[i,2]," => ",temp2[i,3])
    cat("\n")
  }
}

emp_pay(EMP)

name <- c("유관순","홍길동","이순신","신사임당")
gender <- c("F","M","M","F")
price <- c(50, 65, 45, 75)
# 1) 3개의 벡터 객체를 이용하여 client 데이터프레임을 생성하시오.
df <- data.frame(name,gender,price)

# 2) price변수의 값이 65만원 이상이며 문자열 “beat”, 65만원 미만이면 문자열 “Normal” 을 변수result에 추가하시오. (힌트, ifelse()사용)
df$result <- ifelse(df$price >= 65,"Best","Normal")
# 3) result변수를 대상으로 빈도수를 구하시오.
table(df$result)

