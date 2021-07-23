#1. iris데이터 셋을 대상으로 다음 조건에 맞게 시각화 하시오
data("iris")

# 1) 1번 컬럼을 x축으로 하고 3번 컬럼을 y축으로 한다.
x <- iris$Sepal.Length; y <- iris$Petal.Length
plot(x,y)

# 2) 5번 컬럼으로 색상지정한다
plot(x,y,
     col = iris$Species)

# 3) 차트 제목을 “iris 데이터 산포도”로 추가한다
plot(x,y,
     col = iris$Species,
     main = "iris 데이터 산포도")

# 4) 다음 조건에 맞추어 작성한 차트를 파일에 저장한다.
#- 작업 디렉토리: “C:/Rwork/output”
#- 파일명: “iris.jpg”
#- 크기: 폭(720픽셀), 높이(480픽셀)
jpeg(filename = "iris.jpg",width = 300,height = 600,
     units = "px",bg="transparent")
plot(x,y,
     col = iris$Species,
     main = "iris 데이터 산포도")
dev.off()

#2, iris3 데이터 셋을 대상으로 다음 조건에 맞게 산점도를 그리시오
# 1) iris3 데이터 셋의 컬럼명을 확인한다.
data("iris3")
colnames(iris3)
dimnames(iris3)

# 2) iris2 데이터 셋의 구조를 확인한다.
str(iris3)
dim(iris3)

#3) 꽃의 종별로 산점도 그래프를 그린다.
par(mfrow=c(1,1))

library(scatterplot3d)
#꽃 종류별 분류
colnames(iris3) <- c("Sepal_L","Sepal_W","Petal_L","Petal_W")
iris_setosa <- iris3[,,"Setosa"]
iris_versicolor <- iris3[,,"Versicolor"]
iris_virginica <- iris3[,,"Virginica"]

#3차원 산점도
d3 <- scatterplot3d(iris3[,"Petal_L",], 
                    iris3[,"Sepal_L",],
                    iris3[,"Sepal_W",], 
                    type = 'n',
                    main = "name",
                    xlab = "X",
                    ylab = "Y",
                    zlab = "Z")
d3$points3d(iris_setosa[,3],
            iris_setosa[,1],
            iris_setosa[,2], 
            bg = 'orange', pch = 21)
d3$points3d(iris_versicolor[,3],
            iris_versicolor[,1],
            iris_versicolor[,2], 
            bg = 'blue', pch = 21)
d3$points3d(iris_virginica[,3],
            iris_virginica[,1],
            iris_virginica[,2], 
            bg = 'green', pch = 21)

