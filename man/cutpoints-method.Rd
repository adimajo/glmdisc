% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generic_cutpoints.R, R/method_cutpoints.R
\docType{methods}
\name{cutpoints}
\alias{cutpoints}
\alias{cutpoints,glmdisc-method}
\title{Obtaining the cutpoints and / or regroupments of a discretization.}
\usage{
cutpoints(object)

\S4method{cutpoints}{glmdisc}(object)
}
\arguments{
\item{object}{generic glmdisc object}

\item{glmdisc}{The trained glmdisc S4 object.}
}
\description{
This defines the generic method "cutpoints" which will provide the cutpoints of a discretization scheme of S4 class \code{\link{glmdisc}}.

This defines the method to provide the cutpoints of a trained glmdisc.
}
\examples{
# Simulation of a discretized logit model
set.seed(1)
x <- matrix(runif(300), nrow = 100, ncol = 3)
cuts <- seq(0, 1, length.out = 4)
xd <- apply(x, 2, function(col) as.numeric(cut(col, cuts)))
theta <- t(matrix(c(0, 0, 0, 2, 2, 2, -2, -2, -2), ncol = 3, nrow = 3))
log_odd <- rowSums(t(sapply(seq_along(xd[, 1]), function(row_id) {
  sapply(
    seq_along(xd[row_id, ]),
    function(element) theta[xd[row_id, element], element]
  )
})))
y <- rbinom(100, 1, 1 / (1 + exp(-log_odd)))

sem_disc <- glmdisc(x, y,
  iter = 50, m_start = 4, test = FALSE,
  validation = FALSE, criterion = "aic"
)
cutpoints(sem_disc)
}
\author{
Adrien Ehrhardt.
}
