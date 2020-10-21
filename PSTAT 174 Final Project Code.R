install.packages("qpcR")
library(timeSeries)


# Read in the data

library(readr)

file <- read_csv("file.csv")
View(file)

# file <- read_csv("~/UCSB/Spring 2020/PSTAT 174/Final Project/file.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load the dataset with start date and interval frequency

yearRate <- read.table("file.csv", header=TRUE, sep=",")
yearRate

rate = ts(yearRate[,2], start=c(1982,1), end=c(2013,12), frequency = 4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plot the time series data

ts.plot(rate, ylab ="Percentage of Hotel Rooms Occupied", main = "Hotel Room Occupancy")

library(MASS)
t = 1:length(rate)
fit = lm(rate~t)
bcTransform = boxcox(rate~t,plotit = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# BOX-COX Transformations, which we will not use

lambda = bcTransform$x[which(bcTransform$y == max(bcTransform$y))]
lambda = 1.5
rate_BoxCoX = (1/lambda)*(rate^lambda-1)

rate_BoxCoX_LOG = log(rate)

par(mfrow=c(1,2))
ts.plot(rate_BoxCoX)
ts.plot(rate_BoxCoX_LOG)
ts.plot(rate, ylab ="Percentage of Hotel Rooms Occupied", main = "Hotel Room Occupancy")

require(graphics)
plot(stl(rate_BoxCoX,s.window="per"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Difference at lag = 1 to make the mean constant over time, which

rate_BC_D1 = diff(rate, lag = 1)
ts.plot(rate_BC_D1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# BOX COX of differenced timeseries

plot(rate_BC_D1, main="Differenced Time Series after BOX-COX")
op = par(mfrow=c(1,2))
acf(rate_BC_D1, lag.max = 20,main="ACF of CPI_BC_D1")
pacf(rate_BC_D1, lag.max = 20,main="PACF of CPI_BC_D1")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Finding AICc values for the chosen model

library(qpcR)
print("AICC FOR ARIMA(4,1,0)")
print(AICc(arima(rate_BoxCoX, order=c(4,1,0), method="ML")))
print("AICC FOR ARIMA(7,1,0)")
print(AICc(arima(rate_BoxCoX, order=c(7,1,0), method="ML")))
print("AICC FOR ARIMA(5,1,7)")
print(AICc(arima(rate_BoxCoX, order=c(5,1,7), method="ML")))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# We want to fix rate of BOX COX

fit <- arima(rate_BoxCoX, order=c(5,1,7), method="ML")
rsdls = residuals(fit)
fit
print("Below is the AICc Score")
AICc(fit)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plot the AR and MA roots on the unit circle
# Code is from https://robjhyndman.com/hyndsight/arma-roots/

arroots <- function(object)
{
  if(!("Arima" %in% class(object)) &
     !("ar" %in% class(object)))
    stop("object must be of class Arima or ar")
  if("Arima" %in% class(object))
    parvec <- object$model$phi
  else
    parvec <- object$ar
  if(length(parvec) > 0)
  {
    last.nonzero <- max(which(abs(parvec) > 1e-08))
    if (last.nonzero > 0)
      return(structure(list(
        roots=polyroot(c(1,-parvec[1:last.nonzero])),
        type="AR"),
        class='armaroots'))
  }
  return(structure(list(roots=numeric(0), type="AR"),
                   class='armaroots'))
}

# Compute MA roots
maroots <- function(object)
{
  if(!("Arima" %in% class(object)))
    stop("object must be of class Arima")
  parvec <- object$model$theta
  if(length(parvec) > 0)
  {
    last.nonzero <- max(which(abs(parvec) > 1e-08))
    if (last.nonzero > 0)
      return(structure(list(
        roots=polyroot(c(1,parvec[1:last.nonzero])),
        type="MA"),
        class='armaroots'))
  }
  return(structure(list(roots=numeric(0), type="MA"),
                   class='armaroots'))
}

plot.armaroots <- function(x, xlab="Real", ylab="Imaginary",
                           main=paste("Inverse roots of", x$type,
                                      "characteristic polynomial"),
                           ...)
{
  oldpar <- par(pty='s')
  on.exit(par(oldpar))
  plot(c(-1,1), c(-1,1), xlab=xlab, ylab=ylab,
       type="n", bty="n", xaxt="n", yaxt="n", main=main, ...)
  axis(1, at=c(-1,0,1), line=0.5, tck=-0.025)
  axis(2, at=c(-1,0,1), label=c("-i","0","i"),
       line=0.5, tck=-0.025)
  circx <- seq(-1,1,l=501)
  circy <- sqrt(1-circx^2)
  lines(c(circx,circx), c(circy,-circy), col='gray')
  lines(c(-2,2), c(0,0), col='gray')
  lines(c(0,0), c(-2,2), col='gray')
  if(length(x$roots) > 0)
  {
    inside <- abs(x$roots) > 1
    points(1/x$roots[inside], pch=19, col='black')
    if(sum(!inside) > 0)
      points(1/x$roots[!inside], pch=19, col='red')
  }
}

library(forecast)
plot(arroots(arima(rate_BoxCoX,c(5,1,7))))
plot(maroots(arima(rate_BoxCoX,c(5,1,7))))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Iterating over all the possible choices of the model parameters
parameters <- 12
w <- expand.grid(rep(list(0:1),parameters))
for(i in 1:(2^parameters)){
  for(j in 1:parameters){
    if(w[i,j]==1){
      w[i,j] = NA
    }
  }
}
calcAICc <- c(rep(Inf,2^parameters))
for(i in 1:2^12){
  x <- i
  w[x,]
  ar1 <- w[x,1]
  ar2 <- w[x,2]
  ar3 <- w[x,3]
  ar4 <- w[x,4]
  ar5 <- w[x,5]
  ma1 <- w[x,6]
  ma2 <- w[x,7]
  ma3 <- w[x,8]
  ma4 <- w[x,9]
  ma5 <- w[x,10]
  ma6 <- w[x,11]
  ma7 <- w[x,12]
  try(
    {
      calcAICc[i]<- AICc(arima(rate_BC_D1, order=c(5,1,7),
                               fixed = c(ar1,ar2,ar3,ar4,ar5, ma1,ma2,ma3,ma4,ma5,ma6,ma7),
                               method="ML"))
    },
    silent = TRUE
  )
}
print(calcAICc)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plot of residual
plot(rsdls, main="Fitted Residuals")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plot of ACF, PACF, HISTOGRAM and QQPLOT

op <- par(mfrow=c(2,2))
acf(rsdls,main = "Autocorrelation")
pacf(rsdls,main = "Partial Autocorrelation")
hist(rsdls,main = "Histogram")
qqnorm(rsdls,main = "QQPlot")
qqline(rsdls,col="blue")
par(op)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SHAPIRO WILK NORMALITY TEST
shapiro.test(rsdls)

# Ljung-Box test
Box.test(rsdls, type = "Ljung-Box", lag = 16, fitdf = 8)

# Box-Pierce test
Box.test(rsdls, type = "Box-Pierce", lag = 16, fitdf = 8)

# McLeod-Li test
Box.test(rsdls^2, type = "Box-Pierce", lag = 16, fitdf = 0)

# SPECTRAL ANALYSIS
# P
# FISHER's TEST
library("GeneCycle")
fisher.g.test(rsdls)

# KOLMOGOROV-SIMRNOV TEST
cpgram(rsdls,main="Kolmogorov-Simrnov Test")

# Forecasting ahead
forecast_series <-predict(fit, n.ahead = 24)
values = (((forecast_series$pred)*lambda+1)^(1/lambda))
errors = (((forecast_series$se)*lambda+1)^(1/lambda))

#errors = forecast_series$se
print(values)
print(errors)
rate_2022 = ts(file[,2], start=c(1982,1), end=c(2022,12),frequency = 4)
ts.plot(rate_2022)
lines(values,lty=1,col="red")
lines(values+1.96*errors,lty=2)
lines(values-1.96*errors,lty=2)

library(timeSeries)
library(timeDate)
library(qpcR)
library(sarima)

'
aiccs = matrix(NA, nr=9, nc=3)
colnames(aiccs) = c("p","q","AICc")
i=0
d=0
for (p in 0:2) {
  for (q in 0:2) {
    aiccs[i+1, 1] = p
    aiccs[i+1, 2] = q
    aiccs[i+1, 3] = sarima(rate_BC_D2,p,d,q,P=1,D=1,Q=1,S=4,details=FALSE)$AICc
    i=i+1
  }
}

# Models with the first three smallest AICc
aiccs[order(aiccs[,3])[1:3],]
'
sarima(ArModel,0,1,0,P=2,D=1,Q=1,S=4, details=F)

install.packages("mvtnorm")

install.packages("AICcmodavg")

```{r, include=FALSE}
fit.1 = arima(rate_BC_D2, c(0,1,0), seasonal=list(order=c(1,1,1), period=4))
fit.1$coef
AICc(fit.1)

fit.2 = arima(rate_BC_D2, c(0,1,0), seasonal=list(order=c(2,1,1), period=4))
fit.2$coef
AICc(fit.2)

fit.3 = arima(rate_BC_D2, c(0,1,0), seasonal=list(order=c(6,1,1), period=4))
fit.3$coef
AICc(fit.3)

fit.4 = arima(rate_BC_D2, c(0,1,1), seasonal=list(order=c(1,1,1), period=4))
fit.4$coef
AICc(fit.4)

fit.5 = arima(rate_BC_D2, c(0,1,1), seasonal=list(order=c(2,1,1), period=4))
fit.5$coef
AICc(fit.5)

fit.6 = arima(rate_BC_D2, c(0,1,1), seasonal=list(order=c(6,1,1), period=4))
fit.6$coef
AICc(fit.6)

fit.7 = arima(rate_BC_D2, c(1,1,0), seasonal=list(order=c(1,1,1), period=4))
fit.7$coef
AICc(fit.7)

fit.8 = arima(rate_BC_D2, c(1,1,0), seasonal=list(order=c(2,1,1), period=4))
fit.8$coef
AICc(fit.8)

fit.9 = arima(rate_BC_D2, c(1,1,0), seasonal=list(order=c(6,1,1), period=4))
fit.9$coef
AICc(fit.9)

fit.10 = arima(rate_BC_D2, c(1,1,1), seasonal=list(order=c(1,1,1), period=4))
fit.10$coef
AICc(fit.10)

fit.11 = arima(rate_BC_D2, c(1,1,1), seasonal=list(order=c(2,1,1), period=4))
fit.11$coef
AICc(fit.11)

fit.12 = arima(rate_BC_D2, c(1,1,1), seasonal=list(order=c(6,1,1), period=4))
fit.12$coef
AICc(fit.12)
```

Code is from https://robjhyndman.com/hyndsight/arma-roots/
  ```{r}
library(forecast)

arroots <- function(object)
{
  if(!("Arima" %in% class(object)) &
     !("ar" %in% class(object)))
    stop("object must be of class Arima or ar")
  if("Arima" %in% class(object))
    parvec <- object$model$phi
  else
    parvec <- object$ar
  if(length(parvec) > 0)
  {
    last.nonzero <- max(which(abs(parvec) > 1e-08))
    if (last.nonzero > 0)
      return(structure(list(
        roots=polyroot(c(1,-parvec[1:last.nonzero])),
        type="AR"),
        class='armaroots'))
  }
  return(structure(list(roots=numeric(0), type="AR"),
                   class='armaroots'))
}

# Compute MA roots
maroots <- function(object)
{
  if(!("Arima" %in% class(object)))
    stop("object must be of class Arima")
  parvec <- object$model$theta
  if(length(parvec) > 0)
  {
    last.nonzero <- max(which(abs(parvec) > 1e-08))
    if (last.nonzero > 0)
      return(structure(list(
        roots=polyroot(c(1,parvec[1:last.nonzero])),
        type="MA"),
        class='armaroots'))
  }
  return(structure(list(roots=numeric(0), type="MA"),
                   class='armaroots'))
}

plot.armaroots <- function(x, xlab="Real", ylab="Imaginary",
                           main=paste("Inverse roots of", x$type,
                                      "characteristic polynomial"),
                           ...)
{
  oldpar <- par(pty='s')
  on.exit(par(oldpar))
  plot(c(-1,1), c(-1,1), xlab=xlab, ylab=ylab,
       type="n", bty="n", xaxt="n", yaxt="n", main=main, ...)
  axis(1, at=c(-1,0,1), line=0.5, tck=-0.025)
  axis(2, at=c(-1,0,1), label=c("-i","0","i"),
       line=0.5, tck=-0.025)
  circx <- seq(-1,1,l=501)
  circy <- sqrt(1-circx^2)
  lines(c(circx,circx), c(circy,-circy), col='gray')
  lines(c(-2,2), c(0,0), col='gray')
  lines(c(0,0), c(-2,2), col='gray')
  if(length(x$roots) > 0)
  {
    inside <- abs(x$roots) > 1
    points(1/x$roots[inside], pch=19, col='black')
    if(sum(!inside) > 0)
      points(1/x$roots[!inside], pch=19, col='red')
  }
}

plot(arroots(arima(rate,c(5,1,7))))
plot(maroots(arima(rate,c(5,1,7))))
```
5) PLOT THE ACF/PACF OF THE LOG-TRANSFORMED DATA
```{r}
var(rate)
var(rate_BoxCoX_LOG)
op = par(mfrow = c(1,2))
acf(rate_BoxCoX_LOG,lag.max = 60,main = "")
pacf(rate_BoxCoX_LOG,lag.max = 60,main = "")
title("Box-Cox Transformed Time Series", line = -1, outer=TRUE)
```
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

11) FIT CANDIDATE MODELS

We'll fit: SARIMA(p=0,d=1,q=0) x (P=1,D=1,Q=1) s=4 fit.1
           SARIMA(p=0,d=1,q=0) x (P=2,D=1,Q=1) s=4 fit.2
           SARIMA(p=0,d=1,q=0) x (P=6,D=1,Q=1) s=4 fit.3
           
           SARIMA(p=0,d=1,q=1) x (P=1,D=1,Q=1) s=4 fit.4
           SARIMA(p=0,d=1,q=1) x (P=2,D=1,Q=1) s=4 fit.5
           SARIMA(p=0,d=1,q=1) x (P=6,D=1,Q=1) s=4 fit.6
           
           SARIMA(p=1,d=1,q=0) x (P=1,D=1,Q=1) s=4 fit.7
           SARIMA(p=1,d=1,q=0) x (P=2,D=1,Q=1) s=4 fit.8
           SARIMA(p=1,d=1,q=0) x (P=6,D=1,Q=1) s=4 fit.9
           
           SARIMA(p=1,d=1,q=1) x (P=1,D=1,Q=1) s=4 fit.10
           SARIMA(p=1,d=1,q=1) x (P=2,D=1,Q=1) s=4 fit.11
           SARIMA(p=1,d=1,q=1) x (P=6,D=1,Q=1) s=4 fit.12

```{r, message=FALSE}
library(qpcR)

fit.1 = arima(rate, c(0,1,0), seasonal=list(order=c(1,1,1), period=4, method="ML"))
#fit.1
AICc(fit.1)

fit.2 = arima(rate, c(0,1,0), seasonal=list(order=c(2,1,1), period=4))
#fit.2
AICc(fit.2)

fit.3 = arima(rate, c(0,1,0), seasonal=list(order=c(5,1,1), period=4))
#fit.3
AICc(fit.3)

fit.4 = arima(rate, c(0,1,1), seasonal=list(order=c(1,1,1), period=4))
#fit.4
AICc(fit.4)

fit.5 = arima(rate, c(0,1,1), seasonal=list(order=c(2,1,1), period=4))
#fit.5
AICc(fit.5)

fit.6 = arima(rate, c(0,1,1), seasonal=list(order=c(5,1,1), period=4))
#fit.6
AICc(fit.6)

fit.7 = arima(rate, c(1,1,0), seasonal=list(order=c(1,1,1), period=4))
#fit.7
AICc(fit.7)

fit.8 = arima(rate, c(1,1,0), seasonal=list(order=c(2,1,1), period=4))
#fit.8
AICc(fit.8)

fit.9 = arima(rate, c(1,1,0), seasonal=list(order=c(5,1,1), period=4))
#fit.9
AICc(fit.9)

fit.10 = arima(rate, c(1,1,1), seasonal=list(order=c(1,1,1), period=4))
#fit.10
AICc(fit.10)

fit.11 = arima(rate, c(1,1,1), seasonal=list(order=c(2,1,1), period=4))
#fit.11
AICc(fit.11)

fit.12 = arima(rate, c(1,1,1), seasonal=list(order=c(5,1,1), period=4))
#fit.12
AICc(fit.12)
```
Because SARIMA(p=1,d=1,q=1) x (P=2,D=1,Q=1) s=4 has the lowest AICc value,
we will choose this to be our final model.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

```{r}
plot.roots <- function(ar.roots=NULL, ma.roots=NULL, size=2, angles=FALSE, special=NULL, sqecial=NULL,my.pch=1,first.col="blue",second.col="red",main=NULL)
{xylims <- c(-size,size)
      omegas <- seq(0,2*pi,pi/500)
      temp <- exp(complex(real=rep(0,length(omegas)),imag=omegas))
      plot(Re(temp),Im(temp),typ="l",xlab="x",ylab="y",xlim=xylims,ylim=xylims,main=main)
      abline(v=0,lty="dotted")
      abline(h=0,lty="dotted")
      if(!is.null(ar.roots))
        {
          points(Re(1/ar.roots),Im(1/ar.roots),col=first.col,pch=my.pch)
          points(Re(ar.roots),Im(ar.roots),col=second.col,pch=my.pch)
        }
      if(!is.null(ma.roots))
        {
          points(Re(1/ma.roots),Im(1/ma.roots),pch="*",cex=1.5,col=first.col)
          points(Re(ma.roots),Im(ma.roots),pch="*",cex=1.5,col=second.col)
        }
      if(angles)
        {
          if(!is.null(ar.roots))
            {
              abline(a=0,b=Im(ar.roots[1])/Re(ar.roots[1]),lty="dotted")
              abline(a=0,b=Im(ar.roots[2])/Re(ar.roots[2]),lty="dotted")
            }
          if(!is.null(ma.roots))
            {
              sapply(1:length(ma.roots), function(j) abline(a=0,b=Im(ma.roots[j])/Re(ma.roots[j]),lty="dotted"))
            }
        }
      if(!is.null(special))
        {
          lines(Re(special),Im(special),lwd=2)
        }
      if(!is.null(sqecial))
        {
          lines(Re(sqecial),Im(sqecial),lwd=2)
        }
}

plot.roots(NULL,polyroot(c(1, 0.6711421)),main="Roots of AR part")

plot.roots(NULL,polyroot(c(1, 0.3107242)), main="Roots of SAR part")

plot.roots(NULL,polyroot(c(1, -0.9528503)), main="Roots of SMA part")
```
m = c(ar, ma)
#w = arima.sim(m, 120)
w = ts(rsdls)
plot(w)
Box.test(w, type="Ljung-Box")
```