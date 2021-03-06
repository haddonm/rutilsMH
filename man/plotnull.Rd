% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rutils.R
\name{plotnull}
\alias{plotnull}
\title{plotnull generates an empty plot when one is needed}
\usage{
plotnull(msg = "")
}
\arguments{
\item{msg}{a message to be printed in the middle of the empty plot.}
}
\value{
nothing but it does generate a plot
}
\description{
plotnull there are often circumstances, for example, when
    plotting up results from each year and each SAU, where there will be
    combinations of year and SAU that have no data, but to avoid a problem
    with the plotting it is necessary to generate an empty plot.
}
\examples{
plotnull("An empty plot")
}
