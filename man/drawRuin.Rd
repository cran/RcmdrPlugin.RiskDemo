\name{drawRuin}
\alias{drawRuin}
\title{Plotting simulations of a surplus process
}
\description{
This function plots simulation paths of a surpluss process. The claims are assumed to arrive according to a Poisson process and the claim sizes are assumed to be gamma distributed.
}
\usage{
drawRuin(nsim = 10, Tup = 10, U0 = 1000, theta = 0.01, lambda = 100, 
  alpha = 1, beta = 0.1)
}
\arguments{
  \item{nsim}{
number of simulations
}
  \item{Tup}{
maximum value in the time axis
}
  \item{U0}{
initial capital
}
  \item{theta}{
risk loading
}
  \item{lambda}{
intensity of claim process (mean number of claims per year)
}
  \item{alpha}{
shape parameter of gamma distribution
}
  \item{beta}{
rate parameter of gamma distribution
}
}

\value{
No value; only a figure is plotted.
}
\references{
Kaas, Goovaerts, Dhaene, Denuit (2008) \emph{Modern actuarial risk theory using R, 2nd ed.}, Springer.
}
\author{
Arto Luoma <arto.luoma@wippies.com>
}
\seealso{
\code{\link{computeRuinFinite}},
}
\examples{
computeRuinFinite(T0=10,U0=1000,eps=0.05,lambda=100,alpha=1,beta=0.1)
drawRuin(nsim=10,Tup=10,U0=1000,theta=0.0125,lambda=100,alpha=1,beta=0.1)
}
