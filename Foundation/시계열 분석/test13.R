data(AirPassengers)
par(mfrow = c(1, 2))
ts.plot(AirPassengers)
diff <- diff(AirPassengers)
plot(diff )

par(mfrow = c(1, 2))
plot(AirPassengers)
log <- diff(log(AirPassengers))
plot(log)

data("WWWusage")
str(WWWusage)
WWWusage
X11()
ts.plot(WWWusage, type = "l", col = "red")
data(EuStockMarkets)
head(EuStockMarkets)
EuStock <- data.frame(EuStockMarkets)
head(EuStock)
X11()
EuStock$DAX[1:1000]
plot(EuStock$DAX[1:1000], type = "l", col = "red")
plot.ts(cbind(EuStock$DAX[1:1000], EuStock$SMI[1:1000]),
        main = "주가지수 추세선")
data <- c(45, 56, 45, 43, 69, 75, 58, 59, 66, 64, 62, 65, 
          55, 49, 67, 55, 71, 78, 71, 65, 69, 43, 70, 75, 
          56, 56, 65, 55, 82, 85, 75, 77, 77, 69, 79, 89)
length(data)
tsdata <- ts(data, start = c(2016, 1), frequency = 12)
tsdata
x11()
ts.plot(tsdata)
plot(stl(tsdata, "periodic"))
m <- decompose(tsdata)
attributes(m)
plot(m)
par(mfrow = c(1, 1))
plot(tsdata - m$seasonal)
plot(tsdata - m$trend)
plot(tsdata - m$seasonal - m$trend)
input <- c(3180, 3000, 3200, 3100, 3300, 3200, 
           3400, 3550, 3200, 3400, 3300, 3700)
length(input)
tsdata <- ts(input, start = c(2015, 2), frequency = 12)
