# CSC425
Time series analysis forecasting

######## Problem 1#########
#1. LOAD LIBRARIES
library(tseries)
library(fBasics)
library(forecast)
library(lmtest) # to compute t-test on model parameters

#2. IMPORT DATA
#  Load data with no variable names into the data frame "data"
setwd("~/Desktop/CSC425/homework1")
data=read.table("crudeoil_w0416.csv",header=T,sep=',')

#3. CREATE TIME SERIEs using ts() function.
# ts(datavector, start=c(year, index), freq=nperiods)
cru = data[,2]
cts = ts(cru, start=c(2004,1), freq=52)


#a.Create a time plot for the spot price
plot(cts, type="l", xlab="Time", ylab="crudeoil")

#b.Compute the percentage change rate of the spot prices
growth = (cts-lag(cts,k=-1))/lag(cts,k=-1)
head(growth)

#c. Create histogram and normal quantile plot
# par(mfcol=c(n,m)) creates n by m array for multiple plots
par(mfcol=c(1,1)) 
hist(growth, xlab="crudeoil price growth rate", prob=TRUE, main="Histogram")
# add approximating normal density curve
xfit<-seq(min(growth),max(growth),length=30)
yfit<-dnorm(xfit,mean=mean(growth),sd=sd(growth))
lines(xfit, yfit, col="blue", lwd=2) 

qqnorm(growth)
qqline(growth, col = 2) 

#d. Test Symmetry (skewness test)
#COMPUTE SUMMARY STATISTICS
basicStats(growth)

skew_test = skewness(growth)/sqrt(6/length(growth))
skew_test
#P-value
2* (1-pnorm(abs(skew_test)))

#e. Test Kurtosis
# Fat-tail test
k_stat = kurtosis(growth)/sqrt(24/length(growth))
print("Kurtosis test statistic")
k_stat
print("P-value = ")
2*(1-pnorm(abs(k_stat)))

#f.Perform Jarque-Bera normality test.
normalTest(growth,method=c("jb")) 

#g. Create a time plot for the time series of rate
plot(growth, type="l", xlab="Time", ylab="crudeoil price growth rate")

#h.Compute and plot the first 15 lags
Acf(growth,plot=T, lag=15)

#i. Difference between lag-1 and lag-2
acf_value=acf(coredata(growth), plot=F, lag=2)
acf_value


####### Problem 2######
#1. LOAD LIBRARIES
library(tseries)
library(fBasics)
library(forecast)
library(lmtest) # to compute t-test on model parameters

#2. IMPORT DATA
#  Load data with no variable names into the data frame "data"
setwd("~/Desktop/CSC425/homework1")
data=read.table("groceries.csv",header=T,sep=',')

#3. CREATE TIME SERIEs using ts() function.
# ts(datavector, start=c(year, index), freq=nperiods)
grc = data[,2]
gts = ts(grc, start=c(2008,1), freq=52)

#4. Compute the percentage change rate of the sales
growth = (gts-lag(gts,k=-1))/lag(gts,k=-1)
# COMPUTE SUMMARY STATISTICS
basicStats(growth)

#a. Create histogram and normal quantile plot
# par(mfcol=c(n,m)) creates n by m array for multiple plots
par(mfcol=c(1,1)) 
hist(growth, xlab="sales growth rate", prob=TRUE, main="Histogram")
# add approximating normal density curve
xfit<-seq(min(growth),max(growth),length=30)
yfit<-dnorm(xfit,mean=mean(growth),sd=sd(growth))
lines(xfit, yfit, col="blue", lwd=2) 

qqnorm(growth)
qqline(growth, col = 2) 

#b.Perform Jarque-Bera normality test.
normalTest(growth,method=c("jb"))

#c.Create a time plot for the sales
plot(gts, type="l", xlab="Time", ylab="Weekly Sales")

#d.Compute and plot the first 15 lags
Acf(growth,plot=T, lag=15)

#e.Ljung Box test
# to Lag 3
Box.test(growth,lag=3,type='Ljung')
# to Lag 6
Box.test(growth,lag=6, type='Ljung')

