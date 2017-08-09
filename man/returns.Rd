\name{returns}
\alias{returns}
\title{
Computing expected returns and their covariance matrix
}
\description{
Computing expected returns and their covariance matrix when the returns are lognormal.
}
\usage{
returns(volvec, indexvol, beta)
}
\arguments{
  \item{volvec}{
 vector of volatilities
}
  \item{indexvol}{
 volatility of the portfolio index
}
  \item{beta}{
 vector of betas
}
}
\details{
The arguments are given in decimals. The single index model is used to compute the covariance matrix of a multivariate normal distribution. The mean vector is assumed to be zero. The properties of the log-normal distribution are then  used to compute the mean vector and covariance matrix of the corresponding multivariate log-normal distribution. 
}
\value{
\item{mean}{vector of expected returns}
\item{cov}{covariance matrix of returns}
}
\references{
Bodie, Kane, and Marcus (2014) \emph{Investments, 10th Global Edition}, McGraw-Hill Education, (see  Section 8.2 The Single-Index Model).
}
\author{
Arto Luoma <arto.luoma@wippies.com>
}
\examples{
returns(volvec=c(0.1,0.2,0.3),indexvol=0.2, beta=c(0.5,-0.1,1.1)) 
}
