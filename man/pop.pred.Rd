\name{pop.pred}
\alias{pop.pred}
\title{
Population forecasting
}
\description{
Population forecasting using mortality forecast and simple time series forecast for
age 0 population
}
\usage{
pop.pred(mort, mort.fcast)
}
\arguments{
  \item{mort}{
  mortality data of class 'demogdata'
}
  \item{mort.fcast}{
  mortality forecast of class 'fmforecast'
}
}
\details{
ARIMA(0,2,2)-model is used to forecast age 0 populaton. 
}
\value{
population forecast of class 'demogdata'
}
\author{
Arto Luoma <arto.luoma@wippies.com>
}
\examples{
data(fin)
data(fin.fcast)
fin.pcast <- pop.pred(fin,fin.fcast)
plot(fin,plot.type="functions",series="total",transform=FALSE,datatype="pop",
  ages=c(0:100), years=c(1990+0:5*10), xlab="Age")
lines(fin.pcast,plot.type="functions",series="total",transform=FALSE, 
  datatype="pop",ages=c(0:100), years=c(1990+0:5*10), lty=2)
}
