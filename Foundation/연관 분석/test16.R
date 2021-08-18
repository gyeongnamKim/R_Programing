# tranExam.csv 파일 불러오기
setwd("D:/Rwork/")
library(arules)
library(arulesViz)
tran <- read.transactions("tranExam.csv", format = "single",
                  sep = ",", cols = c(1,2),rm.duplicates = T)

# 트랜잭션 객체 생성 및 확인
tran_df <- as(tran, "data.frame")
head(tran_df)
str(tran_df)
summary(tran)
# 아이템 별 빈도수
itemFrequency(tran)
itemFrequencyPlot(tran, support = 0.1)

# 규칙 생성 (supp = 0.3, conf = 0.1)
tran_rule <- apriori(tran, parameter = list(supp = 0.3, conf = 0.1))

# 연관 규칙 결과 확인
inspect(tran_rule)
# 신뢰도 기준 내림차순 정렬
tran_rule_conf <- sort(tran_rule,decreasing = T, by = "confidence")
inspect(tran_rule_conf)
# 향상도 기준 내림차순 정렬
tran_rule_lift <- sort(tran_rule,decreasing = T, by = "lift")
inspect(tran_rule_lift)
# 규칙 시각화
plot(tran_rule,method = "graph")


# Audult 데이터 셋 불러오기
data(Adult)
class(Adult)
# 최소 support = 0.5, 최소 confidence = 0.9를 지정하여 연관규칙을 생성한다.
adult_rule <- apriori(Adult, parameter =  list(supp = 0.5, conf = 0.9))
# 수행한 결과를 lift기준으로 정렬하여 상위 10개 규칙을 기록한다
adult_rule_lift <- sort(adult_rule,decreasing = T, by = "lift")[1:10]
inspect(adult_rule_lift)
# 연관분석 결과를 LHS와 RHS의 빈도수로 시각화한다.
plot(adult_rule,method = "grouped")
plot(adult_rule_lift,method = "grouped")
# 연관분석 결과를 연관어의 네트워크 형태로 시각화한다.
plot(adult_rule,method = "graph")

#상위 규칙 10개만 가지고 시각화
plot(adult_rule_lift,method = "graph")
#install.packages("igraph")
library(igraph)
labels <- labels(adult_rule_lift,ruleSep = " ")
head(labels)
labels <- sapply(labels, strsplit, " ",USE.NAMES = F)
labels_mat <- do.call("rbind",labels)
edgelist <- graph.edgelist(labels_mat,directed = F)
plot.igraph(edgelist,vertex.lavel=V(edgelist)$name,vertex.lavel.cex = 0.8,
            vertex.lavel.color = "black",vertex.size = 30, vertex.color = "green",vertex.frame.color = "red")

#  연관어 중심 단어를 해설한다.
print("12개의 룰을 적용하여 결과를 확인하면 capital-gain=None,capital-loss=None 이 중심단어이고
      상위 10개의 룰만 적용하면(신뢰도 내림차순 기준) native-country=United-States가 중신단어가 된다. ")
