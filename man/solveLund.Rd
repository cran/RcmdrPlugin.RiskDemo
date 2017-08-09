\name{solveLund}
\alias{solveLund}
\title{
Solving Lund's exponent
}
\description{
This function solves Lund's exponent or adjustment coefficient. The claim sizes are assumed to be gamma distributed.
}
\usage{
solveLund(alpha, beta, theta)
}
\arguments{
  \item{alpha}{
shape parameter of gamma distribution
}
  \item{beta}{
rate parameter of gamma distribution
}
  \item{theta}{
 safety loading
}
}
\value{
Lundberg's exponent (or adjustment coefficient)
}
\references{
Gray and Pitts (2012) \emph{Risk Modelling in General Insurance: From Principles to Practice}, Cambridge University Press.
}
\author{
Arto Luoma <arto.luoma@wippies.com>
}
\seealso{
\code{\link{computeRuin}},  \code{\link{computeRuinFinite}}
}
\examples{
solveLund(1,1,0.1)
}

