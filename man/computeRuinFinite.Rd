\name{computeRuinFinite}
\alias{computeRuinFinite}
\title{
Ruin probability computation with finite time horizon
}
\description{
This function uses classical ruin theory to compute either ruin probability, safety loading or initial capital, given two of them. The time horizon is finite. Gamma distribution is used to model claim sizes.
}
\usage{
computeRuinFinite(T0, U0 = NULL, theta = NULL, eps = NULL, lambda, alpha, beta)
}
\arguments{
  \item{T0}{
time horizon (in years)
}
  \item{U0}{
initial capital
}
  \item{theta}{
safety loading
}
  \item{eps}{
ruin probability
}
  \item{lambda}{
claim intensity (mean number of claims per year)
}
  \item{alpha}{
shape parameter of gamma distribution
}
  \item{beta}{
rate parameter of gamma distribution
}
}
\value{
The value is a list with the following components:
\item{LundbergExp}{Lundberg's exponent R}
\item{initialCapital}{initial capital}
\item{safetyLoading}{safety loading}
\item{ruinProb}{ruin probability}
}
\author{
Arto Luoma <arto.luoma@wippies.com>
}
\seealso{
\code{\link{computeRuin}}, \code{\link{solveLund}}
}
\examples{
computeRuinFinite(T0=100,U0=1000,theta=0.01,lambda=100,alpha=1,beta=0.1)
computeRuinFinite(T0=1,eps=0.005,theta=0.001,lambda=100,alpha=1,beta=0.1)
computeRuinFinite(T0=500,U0=5347,eps=0.005,lambda=100,alpha=1,beta=0.1)
}

