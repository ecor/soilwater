\name{unitResponse}
\alias{unitResponse}
\title{The water table recharge: the response unit}
\usage{
unitResponse(t, d = 1, D = 1, H = d, m = 100)
}
\arguments{
  \item{t}{time coordinate}

  \item{d}{depth of unsaturated zone along the slope-normal
  direction}

  \item{H}{soil depth}

  \item{D}{soil water diffusivity}

  \item{m}{maximum limit of summary truncation. Default is
  100.}
}
\description{
The water table recharge: the response unit
}
\note{
This function is experimental and under development by
Emanuele Cordano.
}
\examples{
library(soilwater)


t <- seq(0,2,by=0.001)
d <- c(1,0.75,0.5,0.25)
val1 <- unitResponse(t, d = d[1], D = 1, H = 1, m = 500)

val2 <- unitResponse(t, d = d[2], D = 1, H = 1, m = 500)

val3 <- unitResponse(t, d = d[3], D = 1, H = 1, m = 500)

val4 <- unitResponse(t, d = d[4], D = 1, H = 1, m = 500)
}

