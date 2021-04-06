\name{loglikCovid}
\alias{loglikCovid}
\title{
Computing the log-likelihood of the covid model 
}
\description{
This function computes -2 times the log-likelihood of the simple model that is used to predict new COVID-19 cases and to estimate the effective reproduction number. 
}
\usage{
loglikCovid(y, par, it = TRUE)
}
\arguments{
  \item{y}{
Univariate numeric time series of new COVID-19 cases
}
  \item{par}{
Logarithms of the variance parameters of drift, seasonal component, and error term
}
  \item{it}{
A logical value indicating if only the log-likelihood is returned. 
}
}
\details{
Some multiplicative and additive constants are omitted when the negative log-likelihood is computed. Before computing the log-likelihood, the transformation \emph{y=log(x+a)}, where \emph{a=2}, is applied to the time series. The model is a simple local linear model with local level, drift and seasonal component. The variance parameters of the level and seasonal component are estimated while the variance of the level component is computed as  \emph{max(exp(xi[1]) - a, 0.1)/exp(xi[1])^2}, where \emph{xi[1]} is the current estimate of the level. This is based on the assumption that the number of new cases is approximately Poisson distributed, so that the variance equals the level. The \emph{max} operation is taken in order to prevent the exression from being negative. In order to facilitate estimation, a penalty term is added which corresponds to a prior of  \emph{N(-9,1)} for the logarithm of the drift variance.       
}
\value{
\item{loglik}{-2 times the penalized log likelihood apart from some additive constants}
\item{ll}{Vector of the increments of the log-likelihood corresponding to individual observations} 
\item{Xi}{Matrix of one-step predictions of the state vector. The vectors at different time points are given as rows.}
\item{Xif}{Matrix of filtered values, where the state vectors are given as rows}
\item{Pfmat}{Array of state uncertainty matrices, evaluated at time \emph{t}. The first  array index is for time.}
\item{Q}{Covariance matrix of the error vector of the state equation}
}
\references{
Hamilton (1994) \emph{Time Series Analysis}, Princeton University Press, (see Chapter 13 The Kalman Filter).
}
\author{
Arto Luoma <arto.luoma@wippies.com>
}

\seealso{
\code{\link{covidSmooth}}
}
\examples{
#See examples for covidSmooth.
}

