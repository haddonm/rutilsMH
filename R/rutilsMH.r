
#' rutilsMH: a set of functions to assist with code development
#'
#' The rutilsMH package provides an array of utility functions:
#' these include documentation functions and summary functions
#'
#' @section Documentation functions:
#' \describe{
#'   \item{codeBlock}{generates a comment section ready to copy into code}
#'   \item{classDF}{Tabulates the class of each column in a dataframe}
#'   \item{getname}{returns the name of a variable as character}
#'   \item{listFunctions}{lists all functions in a given R file}
#'   \item{properties}{characterizes properties of data.frame columns}
#' }
#' @section Summary functions:
#' \describe{
#'   \item{classDF}{identifies the class of each column in a data.frame}
#' }
#' @section Plotting function:
#' \describe{
#'   \item{newplot}{bare-bones plotprep, opens a new device + default par}
#'   \item{parsyn}{prints par command syntax to the console to be copied}
#'   \item{plotprep}{sets up a plotting device external to Rstudio}
#'   \item{setplot}{writes a base graphics template to the console}
#' }
#' @section Utility functions:
#' \describe{
#'   \item{countgtOne}{counts values >1 in a vector}
#'   \item{countgtzero}{counts values >0 in a vector}
#'   \item{countNAs}{count the numbr of NA values in a vector}
#'   \item{countones}{count the number of values = 1 in a vector}
#'   \item{countzeros}{count the number of values = 0 in a vector}
#'   \item{lininterpol}{linearly replaces NA values in a vector}
#'   \item{greplow}{a case ignoring 'grep'}
#'   \item{printV}{prints a vector as a column with index numbers}
#'   \item{quants}{used in apply to estimate quantiles across a vector}
#'   \item{which.closest}{finds closest value in a vector to a given number}
#' }
#' @docType package
#' @name rutilsMH
NULL

#' @importFrom grDevices dev.cur dev.new dev.off png palette
#' @importFrom graphics par grid plot axis mtext polygon title hist lines text
#' @importFrom graphics points
#' @importFrom utils tail head str write.table write.csv
#' @importFrom stats quantile loess sd 
#' @importFrom knitr kable
NULL
