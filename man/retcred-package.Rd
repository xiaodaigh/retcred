\name{retcred-package}
\alias{retcred-package}
\alias{retcred}
\docType{package}
\title{
Retail Credit Risk
}
\description{
Various retail credit risk and Basel Accorrd (e.g. BaselIII) related functions including functions for building scorecards and computing Probability of Default (PD), Exposure at Default (EAD), Loss Given Default (LGD), and Risk-Weighted Assets (RWA)
}
\details{
\tabular{ll}{
Package: \tab retcred\cr
Type: \tab Package\cr
Version: \tab 0.1.0\cr
Date: \tab 2014-11-11\cr
License: \tab MIT \cr
}

The package provides functions for computing the capital requiremnt, k, and RWA required under the Basel II for retail exposures

}
\author{
AnalytixWare Ltd Pty

Maintainer: ZJ <dzj@analytixware.com>

}
\references{

}
\keyword{ retail credit risk }
\seealso{

}
\examples{
# this will compute the rwa for the asset calss Qualifying retail revolving exposures
rwa(pd = 0.04, lgd = 0.2, asset.class = "QRRE")
}
