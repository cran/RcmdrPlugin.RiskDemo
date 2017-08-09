\name{computeRuin}
\alias{computeRuin}
\title{
Ruin probability computation with infinite time horizon
}
\description{
This function uses classical ruin theory to compute either ruin probability, safety loading or initial capital, given two of them. The time horizon is infinite. Gamma distribution is used to model claim sizes.
}
\usage{
computeRuin(U0 = NULL, theta = NULL, eps = NULL, alpha, beta)
}
\arguments{
  \item{U0}{
initial capital
}
  \item{theta}{safety loading
}
  \item{eps}{
ruin probability
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
\references{
Gray and Pitts (2012) \emph{Risk Modelling in General Insurance: From Principles to Practice}, Cambridge University Press.
}
\author{
Arto Luoma <arto.luoma@wippies.com>
}
\seealso{
\code{\link{computeRuinFinite}}, \code{\link{solveLund}}
}
\examples{
computeRuin(U0=1000,theta=0.01,alpha=1,beta=0.1)
computeRuin(eps=0.005,theta=0.01,alpha=1,beta=0.1)
computeRuin(U0=5399.24,eps=0.005,alpha=1,beta=0.1)
}

