\name{covidSmooth}
\alias{covidSmooth}
\title{
Kalman smoothing of the covid model
}
\description{
This function does Kalman smoothing for the simple model  that is used to predict new COVID-19 cases. 
}
\usage{
covidSmooth(par, y)
}
\arguments{
  \item{par}{
Logarithms of the variance parameters of drift, seasonal component, and error term
}
  \item{y}{
Univariate numeric time series of new COVID-19 cases
}
}
\details{
See loglikCovid.
}
\value{
  \item{Xif}{Matrix of filtered values, where the state vectors are given as rows}
  \item{Xis}{Matrix of smoothed values, where the state vectors are given as rows}
  \item{Pmat}{Array of state uncertainty matrices, evaluated at time \emph{t-1}. The first  array index is for time.}
  \item{Pfmat}{Array of state uncertainty matrices, evaluated at time \emph{t}. The first  array index is for time.}
\item{Psmat}{Array of state uncertainty matrices, evaluated at time \emph{n}, where \emph{n} is the number of observations. The first array index is for time.}
}

\author{
Arto Luoma
}

\seealso{
\code{\link{loglikCovid}}
}
\examples{
#Preparing a time series
library(zoo)
data(dataCovidFin)
timeindex <- dataCovidFin[dataCovidFin$Alue=="Kaikki Alueet","Aika"]
series <- dataCovidFin[dataCovidFin$Alue=="Kaikki Alueet","val"]
series <- window(zoo(series,order.by=timeindex),start="2020-03-01",
                 end="2021-03-01") 
#Fitting a state space model and smoothing the components
p0 <- c(-9,-7,-3.3)
fit <- nlm(loglikCovid,p=p0,y=series)
out <- covidSmooth(fit$estimate,y=series)

#Plotting the filtered and smoothed components
smoothed <- zoo(out$Xis[,1:3],order.by=time(series))
filtered <- zoo(out$Xif[,1:3],order.by=time(series))
colnames(smoothed) <- colnames(filtered) <- c("Level","Drift","Seasonal")
plot(filtered,xlab="Time",main="Filtered components of the time series")
plot(smoothed,xlab="Time",main="Smoothed components of the time series")

#Plotting the original time series, and the filtered and smoothed local level 
#series after transforming them to original scale
plot(series,xlab="Time",ylab="Time series")
lines(exp(filtered[,1])-2,col=3)
lines(exp(smoothed[,1])-2,col=2)
legend("topleft",c("original","filtered","smoothed"),col=c(1,3,2),lty=1)
}
