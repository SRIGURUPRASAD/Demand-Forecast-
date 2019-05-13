library(ggplot2)
library(dplyr)
library(fUnitRoots)
library(lmtest)
library(FitAR)
NinjaSales <-read.csv(file.choose(),header = TRUE)
head(NinjaSales)
typeof(NinjaSales)
NinjaSales <- NinjaSales[,3:14]
head(NinjaSales)
summary(NinjaSales)


plot(NinjaSales$returns)
plot(NinjaSales$mkt_pr)
table(NinjaSales$skuid)
barplot(table(NinjaSales$skuid))

ggplot(NinjaSales, aes(date, sales)) + geom_line() + xlab("Dates")  + ylab("sales") 
ggplot() +
  geom_line(data = NinjaSales, aes(x = date, y = sales)) + ylab('sales trend')


plot1 <- ggplot() + geom_line(aes(y = NinjaSales$sales, x = NinjaSales$date),
                              data = NinjaSales)
plot1
plot2 <- ggplot() + geom_line(aes(y = NinjaSales$lag_w1_sales, x = NinjaSales$date),
                              
                              data = NinjaSales)
plot2
plot3 <- ggplot() + geom_line(aes(y = NinjaSales$lag_w2_sales, x = NinjaSales$date),
                              
                              data = NinjaSales)
plot3
NinjaSales$sales = ma(NinjaSales$sales, order=7) 
ggplot()+
  geom_line(data = NinjaSales, aes(x = date, y = NinjaSales$sales,   colour = "Weekly Moving Average"))  +
  ylab('Bicycle Count')

head(NinjaSales)
a <- NinjaSales[,1:2]
b <- NinjaSales[,10:12]
Sales <- cbind(a,b)
head(Sales)


Sales %>%
  group_by(skuid)%>%
  select(skuid,date,sales)
summarize(n())


Sku69 <- Sales %>%
  filter(skuid = "69") %>%
  select(skuid,sales)
Sku69

Sku31 <- Sales %>%
  filter(skuid == "31") %>%
  select(sales)
Sku31

Sku35 <- Sales %>%
  filter(skuid == "35") %>%
  select(sales)
Sku35

Sku43 <- Sales %>%
  filter(skuid == "43") %>%
  select(sales)
Sku43

Sku86 <- Sales %>%
  filter(skuid == "86") %>%
  select(sales)
Sku86

--------------------------------------------------------------------------------------------
  
  head(Sku69)
TSD69 = ts(Sku69, start = c(2016,1), frequency = 365)
acf(TSD69,lag.max=34) 
urkpssTest(TSD69, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
TSS1 = diff(TSData, differences=1)
plot(TSS)

acf(TSD69,lag.max=34)
acf(TSS1, lag.max=34)
pacf(TSS1, lag.max=34)

ARIMAmodel1 <- arima(TSD69, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 30),method="ML")

coeftest(ARIMAmodel1) 
confint(ARIMAmodel1)

acf(ARIMAmodel1$residuals)

boxresult-LjungBoxTest (ARIMAmodel1$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(ARIMAmodel1$residuals)
qqline(ARIMAmodel1$residuals)
auto.arima(TSD69, trace=TRUE) 
Prediction69<- predict(ARIMAmodel1,n.ahead = 2)
Prediction69

------------------------------------------------------------------------------------------------------------------
  
  head(Sku31)
TSD31 = ts(Sku31, start = c(2016,1), frequency = 365)
acf(TSD31,lag.max=34) 
urkpssTest(TSD31, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
TSS2 = diff(TSD31, differences=1)
plot(TSS2)

acf(TSD31,lag.max=34)
acf(TSS2, lag.max=34)
pacf(TSS2, lag.max=34)

ARIMAmodel2 <- arima(TSD31, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 30),method="ML")
coeftest(ARIMAmodel2) 
confint(ARIMAmodel2)

acf(ARIMAmodel2$residuals)
boxresult-LjungBoxTest (ARIMAmodel2$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(ARIMAmodel2$residuals)
qqline(ARIMAmodel2$residuals)
auto.arima(TSD31, trace=TRUE) 
Prediction31<- predict(ARIMAmodel2,n.ahead = 2)
Prediction31
----------------------------------------------------------------------------------------------------------
  
  head(Sku35)
TSD35 = ts(Sku35, start = c(2016,1), frequency = 365)
acf(TSD35,lag.max=34) 
urkpssTest(TSD35, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
TSS3 = diff(TSD35, differences=1)
plot(TSS3)

acf(TSD35,lag.max=34)
acf(TSS3, lag.max=34)
pacf(TSS3, lag.max=34)

ARIMAmodel5 <- arima(TSD35, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 30),method="ML")
coeftest(ARIMAmodel5) 
confint(ARIMAmodel5)

acf(ARIMAmodel5$residuals)
boxresult-LjungBoxTest (ARIMAmodel5$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(ARIMAmodel5$residuals)
qqline(ARIMAmodel5$residuals)
auto.arima(TSD35, trace=TRUE) 
Prediction35<- predict(ARIMAmodel5,n.ahead = 2)
Prediction35
-----------------------------------------------------------------------------------------------------------
  
  head(Sku43)
TSD43 = ts(Sku43, start = c(2016,1), frequency = 365)
acf(TSD43,lag.max=34) 
urkpssTest(TSD43, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
TSS4 = diff(TSD43, differences=1)
plot(TSS4)

acf(TSD43,lag.max=34)
acf(TSS4, lag.max=34)
pacf(TSS4, lag.max=34)

ARIMAmodel6 <- arima(TSD43, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 30),method="ML")
coeftest(ARIMAmodel6) 
confint(ARIMAmodel6)

acf(ARIMAmodel6$residuals)
boxresult-LjungBoxTest (ARIMAmodel6$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(ARIMAmodel6$residuals)
qqline(ARIMAmodel6$residuals)
auto.arima(TSD43, trace=TRUE) 
Prediction43<- predict(ARIMAmodel6,n.ahead = 2)
Prediction43

--------------------------------------------------------------------------------------------------------------
  
  head(Sku86)
TSD86 = ts(Sku86, start = c(2016,1), frequency = 365)
acf(TSD86,lag.max=34) 
urkpssTest(TSD86, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
TSS5 = diff(TSD86, differences=1)
plot(TSS5)

acf(TSD86,lag.max=34)
acf(TSS5, lag.max=34)
pacf(TSS5, lag.max=34)

ARIMAmodel3 <- arima(TSD86, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 30),method="ML")
coeftest(ARIMAmodel3) 
confint(ARIMAmodel3)

acf(ARIMAmodel3$residuals)
boxresult-LjungBoxTest (ARIMAmodel3$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(ARIMAmodel3$residuals)
qqline(ARIMAmodel3$residuals)
auto.arima(TSD86, trace=TRUE) 
Prediction86<- predict(ARIMAmodel3,n.ahead = 2)
Prediction86









