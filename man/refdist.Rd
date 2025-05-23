% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/refdist.R
\name{refdist}
\alias{refdist}
\alias{print.refdist}
\alias{summary.refdist}
\title{Generate reference distribution for POTH for a given network structure}
\usage{
refdist(x, d, pooled, nsim = 25, verbose = TRUE)

\method{print}{refdist}(x, ...)

\method{summary}{refdist}(object, ...)
}
\arguments{
\item{x}{A \code{\link[netmeta]{netmeta}} object.}

\item{d}{A vector of the desired relative effects, must be in the same
order as \code{x$trts}.}

\item{pooled}{A character string indicating whether the treatment hierarchy
is based on a common or random effects model. Either \code{"common"} or
\code{"random"}, can be abbreviated.}

\item{nsim}{Number of samples from reference distribution.}

\item{verbose}{A logical indicating whether progress information should
be printed.}

\item{\dots}{Additional arguments passed on to print or summary function.}

\item{object}{A \code{refdist} object.}
}
\value{
A vector of POTH values.
}
\description{
Generate reference distribution for POTH for a given network structure
}
\details{
By default, argument \code{pooled} is equal to "random" if only the random
effects model was considered in the network meta-analysis \code{x}.
Otherwise, argument \code{pooled} is equal to "common".

If argument \code{d} is missing, the respective relative effects are taken
to be all 0.
}
\examples{
\donttest{
library("netmeta")
data(Senn2013)
net1 <- netmeta(TE, seTE, treat1.long, treat2.long, studlab,
  data = Senn2013,
  sm = "MD")

# POTH (based on common effects model)
poth(net1)

# Sample POTH values from reference distribution (common effects model)
set.seed(1909)
poths <- refdist(net1)
summary(poths)

# POTH (based on random effects model)
poth(net1, pooled = "random")

# Sample POTH values from reference distribution (common effect model)
poths.r <- refdist(net1, pooled = "random")
summary(poths.r)
}

}
\seealso{
\code{\link[netmeta]{netmeta}}
}
