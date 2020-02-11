

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
#' @importFrom graphics par grid plot axis mtext polygon title
#' @importFrom utils tail head str
#' @importFrom stats quantile loess sd 
NULL

#' @title classDF - tabluate the class of each column in a da
#'
#' @description classDF - tabluate the class of each column in a dataframe.
#'
#' @param dataframe - the input dataframe for examination
#' @return generates paired column names with their classes
#' @export classDF
#' @examples
#' \dontrun{
#'  data(ChickWeight)
#'  classDF(ChickWeight)
#' }
classDF <- function(dataframe) {
  nvar <- dim(dataframe)[2]
  for (i in 1:nvar) cat(colnames(dataframe)[i],class(dataframe[,i]),"\n")
} # end of class_DF

#' @title countones used in apply to count the number of ones in a vector
#'
#' @description countones used in apply to count number of ones in a vector
#' @param invect vector of values
#' @return A single value of zero or the number of ones
#' @export countones
#' @examples
#' \dontrun{
#'   set.seed(12346)
#'   x <- trunc(runif(10)*10)
#'   x
#'   countones(x)  # should be 2
#' }
countones <- function(invect) {
  pick <- which(invect == 1)
  return(length(pick))
}

#' @title countzeros used in apply to count the number of zeros in a vector
#'
#' @description countzeros used in apply to count zeros in a vector
#' @param invect vector of values
#' @return A single value of zero or the number of zeros
#' @export countzeros
#' @examples
#' \dontrun{
#'   set.seed(12346)
#'   x <- trunc(runif(10)*10)
#'   x
#'   countzeros(x)  # should be 1
#' }
countzeros <- function(invect) {
  pick <- which(invect == 0.0)
  return(length(pick))
}

#' @title countgtzero used in apply to count the number >0 in a vector
#'
#' @description countgtzero used in apply to count number >0 in a vector
#' @param invect vector of values
#' @return A single value of number of values > 0
#' @export countgtzero
#' @examples
#' \dontrun{
#'   set.seed(12346)
#'   x <- trunc(runif(10)*10)
#'   x
#'   countgtzero(x)  # should be 9
#' }
countgtzero <- function(invect) {
  pick <- which(invect > 0)
  return(length(pick))
}

#' @title countNAs used in apply to count the number of NAs in a vector
#'
#' @description countNAs used in apply to count number of NAs in a vector
#' @param invect vector of values
#' @return A single value of zero or the number of NAs
#' @export countNAs
#' @examples
#' \dontrun{
#'   set.seed(12346)
#'   x <- trunc(runif(10)*10)
#'   x[c(3,7)] <- NA
#'   countNAs(x)  # should be 2
#' }
countNAs <- function(invect) {
  pick <- which(is.na(invect))
  return(length(pick))
}

#' @title countgtOne used in apply to count the number > 1 in a vector
#'
#' @description countgtOne used in apply to count the number > 1 in a vector
#' @param invect vector of values
#' @return A single value of zero or the number of NAs
#' @export countgtOne
#' @examples
#' \dontrun{
#'   set.seed(12346)
#'   x <- trunc(runif(10)*10)
#'   x
#'   countgtone(x)  # should be 7
#' }
countgtOne <- function(invect) {
  pick1 <- which(invect > 1.0)
  return(length(pick1)/length(invect))
}

#' @title facttonum converts a vector of numeric factors into numbers
#'
#' @description facttonum converts a vector of numeric factors into numbers.
#'     If the factors are not numeric then the outcome will be a series of 
#'     NA. It is up to you to apply this function only to numeric factors. 
#'     A warning will be thrown if the resulting output vector contains NAs
#'
#' @param invect vector of numeric factors to be converted back to numbers
#'
#' @return an output vector of numbers instead of the input factors
#' @export
#'
#' @examples
#' \dontrun{
#'  DepCat <- as.factor(rep(seq(100,600,100),2)); DepCat
#'  5 * DepCat[3]
#'  as.numeric(levels(DepCat))  # #only converts levels not the replicates
#'  DepCat <- facttonum(DepCat)
#'  5 * DepCat[3]
#'  x <- factor(letters)
#'  facttonum(x)
#' }
facttonum <- function(invect){
  if (class(invect) == "factor") {
    outvect <- suppressWarnings(as.numeric(levels(invect))[invect])
  }
  if (class(invect) == "numeric") outvect <- invect
  if (any(is.na(outvect)))
    warning("NAs produced, input vector may have non-numbers present \n")
  return(outvect)
} # end of facttonum

#' @title freqMean calculates the mean and stdev of count data
#'
#' @description freqMean calculates the mean and stdev of count data
#'     it requires both the values and their associated counts and
#'     return a vector of two numbers.
#'
#' @param values the values for which there are counts
#' @param infreqs the counts for each of the values empty cells can be
#'     either 0 or NA
#'
#' @return a vector containing the mean and st.dev.
#' @export
#'
#' @examples
#' \dontrun{
#' vals <- c(1,2,3,4,5)
#' counts <- c(3,NA,7,4,2)
#' freqMean(vals,counts)  # should give 3.125 and 1.258306
#' }
freqMean <- function(values,infreqs) {
  N <- length(values)
  if (N != length(infreqs)) {
    cat("vectors have different lengths \n")
    ans <- c(NA,NA)
    names(ans) <- c("mean","stdev")
  } else {
    nobs <- sum(infreqs,na.rm=T)
    sumX <- sum(values * infreqs,na.rm=T)
    av <- sumX/nobs
    if (nobs > 1) {
      sumX2 <- sum(values * values * infreqs,na.rm=T)
      stdev <- sqrt((sumX2 - (sumX * sumX)/nobs)/(nobs-1))
    } else { stdev <- NA
    }
    ans <- c(av,stdev)
    names(ans) <- c("mean","stdev")
  }
  return(ans)
} # end of freqMean

#' @title geomean log-normal bias corrected geometric mean of a vector
#'
#' @description Calculates log-normal bias corrected geometric mean of a 
#'     vector. NAs and zeros are removed from consideration.
#' @param invect is a vector of numbers in linear space.
#' @return The bias-corrected geometric mean of the vector
#' @export geomean
#' @examples
#' \dontrun{
#'  x <- c(1,2,3,4,5,6,7,8,9)
#'  geomean(x)
#' }
geomean <- function(invect) {
  pick <- which((invect <= 0.0))
  if (length(pick) == 0) {
    avCE <- mean(log(invect),na.rm=TRUE)
    stdev <- sd(log(invect),na.rm=TRUE)
  } else {
    avCE <- mean(log(invect[-pick]),na.rm=TRUE)
    stdev <- sd(log(invect[-pick]),na.rm=TRUE)
  }
  gmean <- exp(avCE + (stdev^2)/2)
  return(gmean)
}  # end of geomean

#' @title getmin generates the lower bound for a plot
#'
#' @description getmin generates lower bound for a plot where it is unknown
#'     whether the minimum is less than zero of not. If less than 0 then
#'     multiplying by the default mult of 1.05 works well but if the outcome
#'     if > 0 then the multiplier needs to be adjusted appropriately so 
#'     the minimum is slightly lower than the minimum of the data
#'
#' @param x the vector of data to be tested for its minimum
#' @param mult the multiplier for both ends, defaults to 1.05 (=0.95 if >0)
#'
#' @return a suitable lower bound for a plot if required
#' @export
#'
#' @examples
#' \dontrun{
#' vect <- rnorm(10,mean=0,sd=2)
#' sort(vect)
#' getmin(vect,mult=1.0)
#' }
getmin <- function(x,mult=1.05) {
  ymin <- min(x,na.rm=TRUE)
  if (ymin < 0) {
    ymin <- ymin * mult
  } else {
    ymin <- ymin * (2 - mult)
  }
  return(ymin)
} # end of getmin

#' @title getmax generates the upper bound for a plot
#'
#' @description getmax generates upper bound for a plot where it is unknown
#'     whether the maximum is greater than zero of not. If > 0 then
#'     multiplying by the default mult of 1.05 works well but if the outcome
#'     if < 0 then the multiplier needs to be adjusted appropriately so the 
#'     maximum is slightly higher than the maximum of the data
#'
#' @param x the vector of data to be tested for its maximum
#' @param mult the multiplier for both ends, defaults to 1.05 (=0.95 if < 0)
#'
#' @return a suitable upper bound for a plot if required
#' @export
#'
#' @examples
#' \dontrun{
#'  vect <- rnorm(10,mean=0,sd=2)
#'  sort(vect,decreasing=TRUE)
#'  getmax(vect,mult=1.0)
#'  vect <- rnorm(10,mean = -5,sd = 1.5)
#'  sort(vect,decreasing=TRUE)
#'  getmax(vect,mult=1.0)
#' }
getmax <- function(x,mult=1.05) {
  ymax <- max(x,na.rm=TRUE)
  if (ymax > 0) {
    ymax <- ymax * mult
  } else {
    ymax <- ymax * (2 - mult)
  }
  return(ymax)
} # end of getmax

#' @title getname returns the name of a variable as character
#'
#' @description getname runs 'deparse(substitute(x))' to get the
#'     name of the input variable. Saves remembering the syntax
#'
#' @param x any variable whose name is wanted as a character string
#'
#' @return a character string with the name of input variable
#' @export
#'
#' @examples
#' \dontrun{
#' a_variable <- c(1,2,3,4,5,6,7,8)
#' getname(a_variable)
#' }
getname <- function(x) {
  return((deparse(substitute(x))))
}

#' @title getnamespace returns the namespace for a given function
#'
#' @description getnamespace searches the loaded NameSpaces and returns the
#'     name of the NameSpace or package for the input function. This is used
#'     in by 'network'. If the namespace is not loaded this will not be able
#'     to be found.
#'
#' @param fun the name of the function of interest. It must be of class
#'     character, which can be obtained using 'getname'
#'
#' @return the name of the loaded NameSpace or package within which a 
#'     function can be found.
#' @export
#'
#' @examples
#' \dontrun{
#'    getnamespace(getname(lm))
#'    getnamespace(getname(anova))
#' }
getnamespace <- function(fun) {
  if (nchar(fun) == 0) return(NA)
  nss <- loadedNamespaces()
  envs <- c(lapply(nss,.getNamespace))
  return(nss[vapply(envs, function(env) exists(fun, env, inherits = FALSE),logical(1))])
} # end of get_namespace

#' @title gettime calculates time in seconds passed each day
#' 
#' @description gettime is a function designed to facilitate the measurement
#'     of time between intervals within R software that are expected to
#'     take a maximum of hours. It calculates the time as seconds elapsed 
#'     from the start of each day. As long as the timing of events does not
#'     pass from one day to the next accurate results will be generated.
#'
#' @return the time in seconds from the start of a day
#' @export
#'
#' @examples
#' \dontrun{
#'   begin <- gettime()
#'   for (i in 1:1e6) sqrt(i)
#'   finish <- gettime()
#'   print(finish - begin)
#' }
gettime <- function() {
  tim <- unlist(as.POSIXlt(Sys.time()))
  hr <- as.numeric(tim["hour"])*3600
  min <- as.numeric(tim["min"])*60
  sec <- as.numeric(tim["sec"])
  return(hr+min+sec)
} # end of gettime

#' @title greplow - uses tolower in the search for the pattern
#'
#' @description greplow a grep implementation that ignores the case of 
#'     either the search pattern or the object to be search. Both are 
#'     converted to lower case before using grep.
#' @param pattern - the text to search for in x
#' @param x - the vector or object within which to search for 'pattern' once
#'    both have been converted to lowercase.
#'
#' @return the index location within x of 'pattern', if it is present, 
#'     an empty integer if not
#' @export greplow
#'
#' @examples
#' \dontrun{
#' txt <- c("Long","Lat","LongE","LatE","Depth","Zone","Effort","Method")
#' greplow("zone",txt)
#' greplow("Zone",txt)
#' greplow("long",txt)
#' }
greplow <- function(pattern,x) {
  return(grep(tolower(pattern),tolower(x)))
}


#' @title halftable halves the height of a tall narrow data.frame
#'
#' @description halftable would be used when printing a table using kable
#'     from knitr where one of the columns was Year. The objective would be 
#'     to split the table in half taking the bottom half and attaching it on
#'     the right hand side of the top half. The year column would act as the
#'     index.
#'
#' @param inmat the data.frame to be subdivided
#' @param yearcol the column name of the year field
#' @param subdiv the number of times the data.frame should be subdivided;
#'     the default is 3 but the numbers can only be 2 or 3.
#'
#' @return a data.frame half the height and double the width of the original
#' @export
#'
#' @examples
#' \dontrun{
#' x <- as.data.frame(matrix(runif(80),nrow=20,ncol=4))
#' x[,1] <- 1986:2005
#' x[,4] <- paste0("text",1:20)
#' halftable(x,yearcol="V1",subdiv=2)
#' halftable(x[,c(1,2,4)],yearcol="V1")
#' x1 <- rbind(x,x[1,])
#' x1[21,"V1"] <- 2006
#' halftable(x1,yearcol="V1",subdiv=3)
#' }
halftable <- function(inmat,yearcol="Year",subdiv=3) {
  if (!(subdiv %in% c(2,3))) stop("\n subdiv must be 2 or 3 \n")
  numrow <- dim(inmat)[1]
  numcol <- dim(inmat)[2]
  extra <- rep(NA,numcol)
  if ((numrow %% subdiv) == 0) {
    newnr <- numrow/subdiv
    incomplete <- FALSE
  } else {
    newnr <- trunc(numrow/subdiv) + 1
    incomplete <- TRUE
  }
  # years <- inmat[,yearcol]
  first <- inmat[1:newnr,]
  if (subdiv == 2) {
    second <- inmat[-c(1:newnr),]
    diff <- (nrow(first) - nrow(second))
    if (diff > 0) {
      numcol <- ncol(inmat)
      third <- rbind(second,extra)
    } else {
      third <- second
    }
  } else {
    second <- inmat[c(newnr+1):c(2*newnr),]
    first <- cbind(first,second)
    third <- inmat[-c(1:(2*newnr)),]
    diff <- nrow(first) - nrow(third)
    if (diff > 0) third <- rbind(third,extra)
    if (diff > 1) third <- rbind(third,extra)
  }
  outmat <- cbind(first,third)
  rownames(outmat) <- 1:newnr
  return(outmat)
} # end of halftable


info <- function(invar) {
  cat("Class: ",class(invar),"\n")
  str(invar)
  cat("\n")
  categories <-  c("matrix","array","data.frame")
  if (class(invar) %in% categories) {
    cat("Dimension: ",dim(invar),"\n")
    print(head(invar,2))
  } else {
    cat("Length: ",length(invar),"\n")
  }
} # end of info

#' @title inthist a replacement for the hist function for use with integers
#'
#' @description inthist - a replacement for the hist function for use with
#'     integers because the ordinary function fails to count them correctly 
#'     at the end. The function is designed for integers and if it is given 
#'     real numbers it will issue a warning and then round all values before 
#'     plotting.
#' @param x the vector of integers to be counted and plotted OR a matrix of
#'     values in column 1 and counts in column 2
#' @param col the colour of the fill; defaults to black = 1, set this to 0
#'     for an empty bar, but then give a value for border
#' @param border the colour of the outline of each bar defaults to col
#' @param width denotes the width of each bar; defaults to 1, should be >0
#'     and <= 1
#' @param xlabel the label for the x axis; defaults to ""
#' @param ylabel the label for the y axis; defaults to ""
#' @param main the title for the individual plot; defaults to ""
#' @param lwd the line width of the border; defaults to 1
#' @param xmin sets the lower bound for x-axis; used to match plots
#' @param xmax sets the upper bound for x axis; used with multiple plots
#' @param ymax enables external control of the maximum y value; mainly of
#'     use when plotting multiple plots together.
#' @param plotout plot the histogram or not? Defaults to TRUE
#' @param prop plot the proportions rather than the counts
#' @param inc sets the xaxis increment; used to customize the axis;
#'     defaults to 1.
#' @param xaxis set to FALSE to define the xaxis outside of inthist;
#'     defaults to TRUE
#' @param ... available to pass extra plot arguments, such as 
#'     panel.first=grid(), or whatever to the internal plot call
#' @return a matrix of values and counts is returned invisibly
#' @export inthist
#' @examples
#' \dontrun{
#'   x <- trunc(runif(1000)*10) + 1
#'   plotprep(width=6,height=4)
#'   inthist(x,col="grey",border=3,width=0.75,xlabel="Random Uniform",
#'           ylabel="Frequency")
#' }
inthist <- function(x,col=1,border=NULL,width=1,xlabel="",ylabel="",
                    main="",lwd=1,xmin=NA,xmax=NA,ymax=NA,plotout=TRUE,
                    prop=FALSE,inc=1,xaxis=TRUE,...) {
  if (class(x) == "matrix") {
    counts <- x[,2]
    values <- x[,1]
  } else {
    counts <- table(x)
    if (length(counts) == 0) stop("No data provided \n\n")
    values <- as.numeric(names(counts))
  }
  if (sum(!(abs(values - round(values)) < .Machine$double.eps^0.5)) > 0) {
    warning("Using 'inthist' with non-integers; Values now rounded \n")
    values <- round(values,0)
  }
  if ((width <= 0) | (width > 1)) {
    warning("width values should be >0 and <= 1")
    width <- 1
  }
  counts <- as.numeric(counts)
  nct <- length(counts)
  propor <- counts/sum(counts,na.rm=TRUE)
  if (is.na(xmin)) xmin <- min(values,na.rm=TRUE)
  if (is.na(xmax)) xmax <- max(values,na.rm=TRUE)
  if (prop) {
    outplot <- propor
  } else {
    outplot <- counts
  }
  if (is.na(ymax)) {
    if (nchar(main) > 0) {
      ymax <- max(outplot,na.rm=TRUE) * 1.15
    } else {
      ymax <- max(outplot,na.rm=TRUE) * 1.05
    }
  }
  if (plotout) {
    plot(values,outplot,type="n",
         xlim=c((xmin-(width*0.75)),(xmax+(width*0.75))),
         xaxs="r",ylim=c(0,ymax),yaxs="i",xlab="",ylab="",xaxt="n",...)
    if (xaxis) axis(side=1,at=seq(xmin,xmax,inc),labels=seq(xmin,xmax,inc))
    if (length(counts) > 0) {
      for (i in 1:nct) {  # i <- 1
        x1 <- values[i] - (width/2)
        x2 <- values[i] + (width/2)
        x <- c(x1,x1,x2,x2,x1)
        y <- c(0,outplot[i],outplot[i],0,0)
        if (is.null(border)) border <- col
        polygon(x,y,col=col,border=border,lwd=lwd)
      }
      title(ylab=list(ylabel, cex=1.0, font=7),
            xlab=list(xlabel, cex=1.0, font=7))
      if (nchar(main) > 0) mtext(main,side=3,line=-1.0,outer=FALSE,cex=0.9)
    }
  } # end of if-plotout
  if (length(counts) > 0) {
    answer <- cbind(values,counts,propor);
    rownames(answer) <- values
    colnames(answer) <- c("values","counts","proportion")
  } else { answer <- NA  }
  class(answer) <- "inthist"
  return(invisible(answer))
}  # end of inthist

#' @title listExamples lists all the examples in a package R file
#'
#' @description listExamples lists all the examples in a package R file. It
#'     comments out the first line number and any dontrun statements along 
#'     with their following curly bracket.
#'
#' @param infile - a character variable containing the path and filename
#' @return Creates an R file in the working directory and prints its name to
#'     the console
#'
#' @export
#' @examples
#' \dontrun{
#' txt <- vector("character",4)
#' txt[1] <- "#' @examples "
#' txt[2] <- "#' /dontrun{"
#' txt[3] <- "#' print("This is an example of using listExamples")"
#' txt[4] <- "#' }"
#' infile <- textConnection(txt)
#' listExamples(infile)
#' }
listExamples <- function(infile) {  
  outfile <- paste0("examples_",tail(unlist(strsplit(infile,"/")),1))
  cat("All the example code from  \n",file=outfile,append=FALSE)
  cat(infile,"\n\n",file=outfile,append=TRUE)
  content <- readLines(con=infile)
  egLines <- grep("@examples",content)
  funlines <- grep("<- function",content)
  nline <- length(egLines)
  for (i in 1:nline) {
    # find extent of example  i = 1
    count <- 1
    cat("#Linenumber: ",egLines[i] + count,"\n",file=outfile,append=TRUE)
    repeat {  # i=1; count=1
      tmpline <- content[egLines[i] + count]
      if (substr(tmpline,1,1) == "#") {
        lenc <- nchar(tmpline)
        if ((length(grep("dontrun",tmpline)) > 0) |
            (length(grep("#' }",tmpline)) > 0)) {
          cat("# ",tmpline,file=outfile,append=TRUE)
        }
        cat(substr(tmpline,3,lenc),"\n",file=outfile,append=TRUE)
        count <- count + 1
      } else {
        cat("# ",tmpline,"\n",file=outfile,append=TRUE)
        cat("\n\n\n\n",file=outfile,append=TRUE)
        break()
      }
    }
  }
  print(outfile)
} # end of list_Examples

#' @title lininterpol - linearly interpolate values in a vector with NAs
#'
#' @description lininterpol - linearly interpolate values in a vector with 
#'     NAs. A common problem when plotting up time series is where there are
#'     missing values or NAs the plotted line will have gaps, one can always
#'     plot points on top of a line to identify where there are missing 
#'     values but an alternative would be to interpolate the missing values 
#'     linearly and plot that line as a dashed line. This function generates
#'     those linear interpolations. The input vector cannot have missing 
#'     values at the beginning or the end. If there are no missing values 
#'     the original vector is returned
#'
#' @param invect - the vector of values including missing values
#'
#' @return invect but with NAs replaced with linearly interpolated values.
#' @export
#'
#' @examples
#' \dontrun{
#'  Expt <- c(20102,18465,16826,15333,14355,NA,13843.7,NA,NA,NA,15180)
#'  lininterpol(Expt)
#' }
lininterpol <- function(invect) { 
  npt <- length(invect)
  answer <- invect
  pickNA <- which(is.na(invect))
  nna <- length(pickNA)
  if (nna == 0) return(invect)  # no NAs
  if ((pickNA[1] == 1) | (pickNA[nna] == nna))
    #   picknNA <- which(invect > 0)
    stop("input vector in lin-interpol cannot start or end with an NA")
  # identify groups of NAs
  group <- c(pickNA[1])
  count <- 1
  ans <- vector("list",nna) # possible each NA is an individual
  for (i in 2:nna) {
    if ((pickNA[i] - pickNA[(i-1)]) > 1) {
      ans[[count]] <- group
      group <- pickNA[i]
      count <- count + 1
    } else {
      group <- c(group,pickNA[i])
    }
  }
  ans[[count]] <- group
  for (i in 1:count) {  # i <- 2
    pickNA <- ans[[i]]
    begin <- (pickNA[1] - 1)
    finish <- (tail(pickNA,1) + 1)
    first <- invect[begin]
    second <- invect[finish]
    answer[begin:finish] <- seq(first,second,length=(length(pickNA) + 2))
  }
  return(answer)
}  # end of lin_interpol

#' @title listFunctions: Lists all the functions in the R infile
#'
#' @description listFunctions: Lists all the functions in the R infile
#' @param infile - a character variable containing the filename for parsing
#' @param console logical determining whether to print directly to the 
#'     screen the default = TRUE
#' @return Generates text listing all functions but also outputs invisibly
#'     a list of the functions and syntax, and separately the function names
#'
#' @export listFunctions
#' @examples
#' \dontrun{
#' txt <- vector("character",5)
#' txt[1] <- "But an ordinary line of text should be ignored - 1"
#' txt[2] <- "The term function on its own should be ignored"
#' txt[3] <- "Also any line with phrase: '<- function', should be detected"
#' txt[4] <- "But an ordinary line of text should be ignored - 2"
#' txt[5] <- "But an ordinary line of text should be ignored - 3"
#' infile <- textConnection(txt)
#' listFunctions(infile)
#' }
listFunctions <- function(infile,console=TRUE) { # infile=filename
  content <- readLines(con=infile)
  funLines <- grep("function",content)
  nLine <- length(funLines)
  delF <- NULL
  for (i in 1:nLine) {
    tmpLine <- gsub(" ","",content[funLines[i]])
    if ((length(grep("function\\(",tmpLine)) == 0) |
        (substr(tmpLine,1,2) == "#'") |
        (length(grep("<-function",tmpLine)) == 0) |
        (length(grep("} #",tmpLine)) > 0)) delF <- c(delF,i)
  }
  ndelF <- length(delF)
  if (ndelF > 0) {
    funLines <- funLines[-delF]
  }
  if (ndelF == nLine) {
    txt <- paste0(infile,"  contained no recognizable functions")
    warning(cat(txt,"\n\n"))
    return(txt)
  } else {
    outlines <- sort(c(funLines))
    out <- content[outlines]
    funnames <- out
    n <- length(out)
    for (i in 1:n) {  # i=1
      out[i] <- unlist(strsplit(out[i],"#"))[1]
      funnames[i] <- removeEmpty(unlist(strsplit(out[i],"<-"))[1])
      if (console) {
        if (nchar(out[i]) == 2) {
          cat("\n")
        } else {
          cat(outlines[i],out[i],"\n")
        }
      }
    }
  }
  return(invisible(list(functions=out,funnames=funnames,funlines=funLines)))
} # end of list_Functions

#' @title magnitude returns the magnitude of numbers
#'
#' @description magnitude is useful when using an
#'     optimizer such as optim, which uses a parscale parameter.
#'     magnitude can determine the respective parscale value for each
#'     parameter value.
#'
#' @param x the vector of numbers (parameters) whose magnitudes are
#'     needed
#'
#' @return a vector of magnitudes
#' @export
#'
#' @examples
#' \dontrun{
#'   x <- c(0,0.03,0.3,3,30,300,3000)
#'   magnitude(x)
#' }
magnitude <- function(x) {
  return(10^(floor(log10(abs(x)))))
}

#' @title makeUnit generates a unit matrix whose diagonal can be changed
#' 
#' @description makeUnit generates a unit matrix but includes the facility
#'     to alter the diagonal value away from 1.0 if desired.
#'
#' @param N the order of the matrix
#' @param diagvalue defaults to 1.0, but otherwise can be a different 
#'     constant or a vector of dimension N
#'
#' @return a square matrix defaulting to a unit matrix
#' @export
#'
#' @examples
#' \dontrun{
#'   makeUnit(4)
#'   surv <- exp(-0.2)
#'   makeUnit(4,surv)
#' }
makeUnit <- function(N,diagvalue=1.0) {
  N <-trunc(N)
  UnitM <- matrix(0,nrow=N,ncol=N,dimnames=list(1:N,1:N))
  diag(UnitM) <- diagvalue
  return(UnitM)
}  # end of makeUnit

#' @title newplot simple floating window setup a plot
#'
#' @description newplot is a bare-bones setup routine to generate a plot in
#'     RStudio using a floating window. If you want to alter the default par
#'     settings then you can use either setplot() to get suitable syntax or,
#'     more simply, use parsyn() which only gives a template for the par 
#'     syntax
#' @param width defaults to 6 inches = 15.24cm - width of plot
#' @param height defaults to 3.6 inches = 9.144cm - height of plot
#' @param newdev reuse a previously defined graphics device or make a new 
#'     one, defaults to TRUE
#' @return Checks for and sets up a graphics device and sets the default 
#'     plotting par values. This changes the current plotting options!
#' @export
#' @examples
#' \dontrun{
#'  x <- rnorm(1000,mean=0,sd=1.0)
#'  plotprep()
#'  hist(x,breaks=30,main="",col=2)
#' }
newplot <- function(width=6,height=3.6,newdev=TRUE) {
  if  ((names(dev.cur()) != "null device") & (newdev)) 
    suppressWarnings(dev.off())
  if (names(dev.cur()) %in% c("null device","RStudioGD"))
    dev.new(width=width,height=height,noRStudioGD = TRUE)
  par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0))
  par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
} # end of new_plot

#' @title outfit tidy print of output from optim, nlminb, or nlm
#'
#' @description outfit takes in the output list from either optim,
#'     nlminb, or nlm and prints it more tidily to the console, In the
#'     case of nlm it also prints the conclusion regarding the
#'     solution
#'
#' @param inopt the list object output by nlm, nlminb, or optim
#' @param backtransform a logical default = FALSE. If TRUE it assumes
#'     that the parameters have been log-transformed for stability
#'     and need back-transforming
#'
#' @return nothing but it does print the list to the console tidily
#' @export
#'
#' @examples
#' \dontrun{
#'  x <- 1:10  # generate power function data from c(2,2) + random
#'  y <- c(2.07,8.2,19.28,40.4,37.8,64.68,100.2,129.11,151.77,218.94)
#'  alldat <- cbind(x=x,y=y)
#'  pow <- function(par,x) return(par[1] * x ^ par[2])
#'  ssq <- function(par,indat) {
#'     return(sum((indat[,"y"] - pow(par,indat[,"x"]))^2))
#'  }
#'  par=c(2,2)
#'  best <- nlm(f=ssq,p=par,typsize=magnitude(par),indat=alldat)
#'  outfit(best)  # a=1.3134 and b=2.2029 -veLL=571.5804
#' }
outfit <- function(inopt,backtransform=FALSE){
  nlmcode <- c("relative gradient close to zero, probably solution.",
               "repeated iterates in tolerance, probably solution.",
               paste0("nothing lower than estimate.",
                      "Either ~local min or steptol too small."),
               "iteration limit exceeded.",
               paste0("stepmax exceeded ,5 times. Unbounded or ",
                      "asymptotic below or stepmax too small."))
  if (length(grep("value",names(inopt))) > 0) { # optim
    cat("optim solution:  \n")
    cat("par         : ",inopt$par,"\n")
    cat("minimum     : ",inopt$value,"\n")
    cat("iterations  : ",inopt$counts," iterations, gradient\n")
    cat("code        : ",inopt$convergence,"\n")
    cat("message     : ",inopt$message,"\n")
  }
  if (length(grep("minimum",names(inopt))) > 0) {  # nlm - preferred
    cat("nlm solution:  \n")
    cat("minimum     : ",inopt$minimum,"\n")
    cat("iterations  : ",inopt$iterations,"\n")
    cat("code        : ",inopt$code,"  ",nlmcode[inopt$code],"\n")
    if (backtransform) {
      ans <- cbind(par=inopt$estimate,gradient=inopt$gradient,
                   transpar=round(exp(inopt$estimate),6))
    } else {
      ans <- cbind(par=inopt$estimate,gradient=inopt$gradient)
    }
    rownames(ans) <- 1:length(inopt$estimate)
    print(ans)
  }
  if (length(grep("objective",names(inopt))) > 0) {
    cat("nlminb solution:  \n")   # nlminb seems to be deprecated
    cat("par        : ",inopt$par,"\n")
    cat("minimum    : ",inopt$objective,"\n")
    cat("iterations : ",inopt$iterations,"\n")
    cat("code       : ",inopt$evaluations," iterations, gradient\n")
    cat("message    : ",inopt$message,"\n")
  }
  if (length(grep("hessian",names(inopt))) > 0) {
    cat("hessian     : \n")
    print(inopt$hessian)
  }
} # end of outfit

#' @title parset alters the current base graphics par settings
#'
#' @description parset alters the current base graphics par settings
#'     to suit a single standard plot. It is merely here to simplify
#'     and speed the coding for exploratory base graphics. The font
#'     and its size default to 0.85 and font 7 (Times bold). The
#'     default values can be seen by typing parset with no brackets in
#'     the console. If a different
#'     set of par values are needed then the function parsyn() can be
#'     used to act as a prompt for the correct syntax. The output to
#'     the console can be copied to your script and modified to suit.
#'
#' @param plots vector of number of rows and columns, defaults to c(1,1)
#' @param cex the size of the font used, defaults to 0.85
#' @param font the font used, defaults to 7 which is Times Bold, 6 is
#'     Times, 1 is Sans and 2 is Sans Bold.
#' @param outmargin default=c(0,0,0,0) and defines the outer margin used by
#'     mtext
#' @param margin default=c(0.45,0.45,0.05,0.05), which avoids whitespace 
#'     but leaves plenty of room for titles
#'
#' @return nothing but it changes teh base graphics par settings
#' @export
#'
#' @examples
#' \dontrun{
#' parset()
#' parsyn()
#' }
parset <- function(plots=c(1,1),cex=0.75,font=7,outmargin=c(0,0,0,0),
                   margin=c(0.45,0.45,0.05,0.05)) {
  par(mfrow=plots,mai=margin,oma=outmargin)
  par(cex=cex, mgp=c(1.35,0.35,0), font.axis=font,font=font,
      font.lab=font)
} # end of parset

#' @title parsyn types standard syntax for the par command to the console
#'
#' @description parsyn types the standard syntax for the par command to the
#'     console so it can be copied and pasted into your own code.
#'
#' @return it writes two lines of R code to the console
#' @export
#'
#' @examples
#' \dontrun{
#' parsyn()
#' }
parsyn <- function() {
  cat("par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0)) \n")
  cat("par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)  \n")
}

#' @title pkgfuns names all functions within a package
#'
#' @description pgkfuns when given the name of a loaded library gives the 
#'     names of all functions within that library sorted in alphebetical 
#'     order.
#'
#' @param packname the name of the package as character
#'
#' @return a character vector containing the names of all functions in the 
#'     named package
#' @export
#'
#' @examples
#' \dontrun{
#'   pkgfuns("graphics")
#'   pkgfuns("rutilsMH")
#' }
pkgfuns <- function(packname) { # packname=pkgname
  funcs <- names(.getNamespace(packname))
  pick <- grep("__",funcs)
  funcs <- funcs[-pick]
  pick <- which(funcs == ".packageName")
  funcs <- funcs[-pick]
  return(sort(funcs))
} # end of pgkfuns

#' @title plot1 a simple way to plot an xy line plot
#'
#' @description plot1 provides a quick way to plot out a single xy
#'     line plot. It can be used with plotprep to generate a plot
#'     outside of Rstudio or by itself to generate one within Rstudio.
#'     It uses a standard par setup and permits custom labels, font,
#'     and font size (cex). It checks the spread of y and if a ymax is
#'     not given in the parameters finds the ymax and checks to see if
#'     y goes negative in which case it uses getmin, so the
#'     y-axis is set to 0 - ymax or ymin - ymax
#'
#' @param x The single vector of x data
#' @param y the single vector of y data. If more are required they can
#'     be added spearately after calling plot1.
#' @param xlabel the label fot the x-axis, defaults to empty
#' @param ylabel the label fot the y-axis, defaults to empty
#' @param type the type of plot "l" is for line, the default, "p" is
#'     points. If you want both plot a line and add points afterwards.
#' @param usefont which font to use, defaults to 7 which is Times bold
#' @param cex the size of the fonts used. defaults to 0.85
#' @param limity defaults to c(0,0), which does nothing. If a value is given
#'     then this value is used rather than estimating from the input y
#' @param defpar if TRUE then plot1 will declare a par statement. If false 
#'     it will expect one outside the function. In this way plot1 can be
#'     used when plotting multiple graphs, perhaps as mfrow=c(2,2)
#' @param ... room for other graphics commands like col, pch, and lwd
#'
#' @return nothing but it does plot a graph and changes the par setting
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rnorm(20,mean=5,sd=1)
#' plot1(x,x,xlabel="x-values",ylabel="yvalues")
#' }
plot1 <- function(x,y,xlabel="",ylabel="",type="l",usefont=7,cex=0.85,
         limity=c(0,0),defpar=TRUE,...){
  if (defpar) {
    par(mfrow = c(1,1), mai = c(0.45,0.45,0.1,0.05),oma = c(0,0,0,0))
    par(cex = cex, mgp = c(1.35, 0.35, 0), font.axis = usefont,
        font = usefont, font.lab = usefont)
  }
  if (limity[2] > 0) limy <- limity  else limy <- c(0,getmax(y))
  if (min(y,na.rm=TRUE) < 0.0) limy[1] <- getmin(y)
  plot(x,y,type=type,ylim=limy,yaxs="i",
       ylab=ylabel,xlab=xlabel,cex=cex,panel.first=grid(),...)
} # end of plot1

#' @title plotprep sets up a window and the par values for a single plot
#'
#' @description plotprep sets up a window and the par values for a single 
#'     plot. It checks to see if a graphics device is open and opens a new 
#'     one if not. This is simply a utility function to save typing the 
#'     standard syntax. Some of the defaults can be changed. Typing the name
#'     without () will provide a template for modification. If 'windows' is 
#'     called repeatedly this will generate a new active graphics device 
#'     each time leaving the older ones inactive but present. For quick 
#'     exploratory plots this behaviour is not wanted, hence the check if 
#'     an active device exists already or not.
#'
#' @param width defaults to 6 inches = 15.24cm - width of plot
#' @param height defaults to 3 inches = 7.62cm - height of plot
#' @param usefont default is 7 (bold Times) 1 sans serif, 2 sans serif bold
#' @param cex default is 0.85, the font size font used for text in the plots
#' @param newdev reuse a previously defined graphics device or make new one;
#'     defaults to TRUE
#' @param filename defaults to "" = do not save to a filename. If a
#'     filename is input the last three characters will be checked and if
#'     they are not png then .png will be added.
#' @param resol resolution of the png file, if defined, default=300
#' 
#' @return Checks for and sets up a graphics device and sets the default 
#'     plotting par values. This changes the current plotting options!
#' @export
#' @examples
#' \dontrun{
#'  x <- rnorm(1000,mean=0,sd=1.0)
#'  plotprep()
#'  hist(x,breaks=30,main="",col=2)
#' }
plotprep <- function(width=6,height=3.6,usefont=7,cex=0.85,
                     newdev=TRUE,filename="",resol=300) {
  if  ((names(dev.cur()) != "null device") &
       (newdev)) suppressWarnings(dev.off())
  lenfile <- nchar(filename)
  if (lenfile > 3) {
    end <- substr(filename,(lenfile-3),lenfile)
    if (end != ".png") filename <- paste0(filename,".png")
    png(filename=filename,width=width,height=height,units="in",res=resol)
  } else {
    if (names(dev.cur()) %in% c("null device","RStudioGD"))
      dev.new(width=width,height=height,noRStudioGD = TRUE)
  }
  par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0.0,0.0,0.0))
  par(cex=cex, mgp=c(1.35,0.35,0), font.axis=usefont,font=usefont,
      font.lab=usefont)
  if (lenfile > 0) 
    cat("\n Remember to place 'graphics.off()' after the plot \n")
} # end of plotprep

#' @title plotxyy plots two vectors of numbers against single x-axis
#' 
#' @description plotxyy plots two plots on the single graph so that
#'     they share the x-axis. The first series is plotted on the left
#'     vertical axis and the second on the right-hand axis.
#'
#' @param x the x values
#' @param y1 the left-hand axis values
#' @param y2 the right-hand axis values
#' @param xlab the x label, default=""
#' @param ylab1 the left-hand y label, default=""
#' @param ylab2 the right-hand y label, default=""
#' @param cex the size of font on the axes, default=0.85
#' @param fnt the font used on axes, default=7 (bold times)
#' @param colour a vector of two values for the colour of each line,
#'     default=c(1,2)  black and red
#'
#' @return nothing but it plots a graph
#' @export
#'
#' @examples
#' \dontrun{
#' x <- 1:20
#' yval1 <- rnorm(20,mean=5,sd=1)
#' yval2 <- rnorm(20,mean=10,sd=1)
#' plotxyy(x,yval1,yval2)
#' }
plotxyy <- function(x,y1,y2,xlab="",ylab1="",ylab2="",cex=0.85,fnt=7,
                    colour=c(1,2)) {
  par(mfrow=c(1,1),mai=c(0.5,0.45,0.15,0.05),oma=c(0.0,0.75,0.0,3.0)) 
  par(cex=cex, mgp=c(1.35,0.35,0), font.axis=fnt,font=fnt,font.lab=fnt) 
  maxy <- getmax(y1)
  plot(x,y1,type="l",lwd=2,col=colour[1],ylim=c(0,maxy),yaxs="i",
       ylab="",xlab="")
  mtext(ylab1, side=2, line=1.5)
  mtext(xlab, side=1, line=1.25)
  par(new=TRUE)
  maxy2 <- getmax(y2)
  plot(x,y2,type="l",lwd=2,col=colour[2],ylim=c(0,maxy2),axes=FALSE,
       xlab="",ylab="",yaxs="i")
  mtext(ylab2, side=4, line=1.5)
  axis(4)
  grid(ny=0)
} # end of plotxyy

#' @title printV returns a vector cbinded to 1:length(invect)
#'
#' @description printV takes an input vector and generates another vector of
#'     numbers 1:length(invect) which it cbinds to itself. This is primarily
#'     useful when trying to print out a vector which can be clumsy to read 
#'     when print across the screen. applying printV leads to a single 
#'     vector being printed down the screen
#'
#' @param invect the input vector to be more easily visualized, this can be
#'     numbers, characters, or logical. If logical the TRUE and FALSE are
#'     converted to 1's and 0's
#' @param label the column labels for vector, default is index and value
#'
#' @return a dataframe containing the vector 1:length(invect), and invect.
#' @export
#'
#' @examples
#' \dontrun{
#' vec <- rnorm(10,mean=20,sd=2)
#' printV(vec)
#' vec <- letters
#' printV(vec)
#' vec <- c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE)
#' printV(vec,label=c("index","logicstate"))
#' }
printV <- function(invect,label=c("index","value")) {
  n <- length(invect)
  outvect <- as.data.frame(cbind(1:n,invect))
  colnames(outvect) <- label
  return(outvect)
} # end of print_V

#' @title properties - used to check a data.frame before standardization
#'
#' @description properties - used to check a data.frame before
#'     standardization. The maximum and minimum are constrained to four
#'     decimal places. It allows for columns of NAs and for Posix 
#'     columns.
#' @param indat the data.frame containing the data fields to be used
#'     in the subsequent standardization. It tabulates the number of
#'     NAs and the number of unique values for each variable and finds
#'     the minimum and maximum of the numeric variables
#' @param dimout determines whether or noth the dimensions of the data.frame
#'     are printed to the screen or not; defaults to FALSE
#' @return a data.frame with the rows being each variable from the input
#'     input data.frame and the columns being the number of NAs, the
#'     number of unique values, and minimum and maximum (where possible).
#' @export properties
#' @examples
#' \dontrun{
#'  data(abdat)
#'  properties(abdat$fish)
#' }
properties <- function(indat,dimout=FALSE) {  # indat=ablong; dimout=FALSE
  dominmax <- function(x) {
    if (length(which(x > 0)) == 0) return(c(NA,NA))
    mini <- min(x,na.rm=TRUE)
    maxi <- max(x,na.rm=TRUE)
    return(c(mini,maxi))
  }
  if(dimout) print(dim(indat))
  isna <- sapply(indat,function(x) sum(is.na(x)))
  uniques <- sapply(indat,function(x) length(unique(x)))
  columns <- length(indat)
  clas <- character(columns)
  for (i in 1:columns) {
    clas[i] <- class(indat[,i])[1]
  }
  numbers <- c("integer","numeric")
  pick <- which(clas %in% numbers)
  minimum <- numeric(length(uniques))
  maximum <- minimum
  for (i in 1:length(pick)) {
    minmax <- dominmax(indat[,pick[i]])
    minimum[pick[i]] <- minmax[1]
    maximum[pick[i]] <- minmax[2]
  }
  index <- 1:length(isna)
  props <- as.data.frame(cbind(index,isna,uniques,clas,round(minimum,4),
                               round(maximum,4),t(indat[1,])))
  colnames(props) <- c("Index","isNA","Unique","Class","Min",
                       "Max","Example")
  return(props)
} # end of properties


#' @title quants used in apply to estimate quantiles across a vector
#'
#' @description quants used in 'apply' to estimate quantiles across a vector
#' @param invect vector of values
#' @param probs the quantiles wanted in the outputs; default = 
#'     c(0.025,0.05,0.5,0.95,0.975)
#' @return a vector of the c(0.025,0.05,0.5,0.95,0.975) quantiles or
#'     whatever is input to probs
#' @export quants
#' @examples
#' \dontrun{
#'  x <- runif(1000)
#'  quants(x)
#'  quants(x,probs=c(0.075,0.5,0.925))
#' }
quants <- function(invect,probs = c(0.025,0.05,0.5,0.95,0.975)) {
  ans <- quantile(invect,probs =probs,na.rm=T)
  return(ans)
}

#' @title removeEmpty removes empty strings from a vector of strings
#'
#' @description removeEmpty removes empty strings from a vector of strings.
#'     Such spaces often created by spurious commas at the end of lines. It
#'     also removes strings made up only of spaces and removes spaces from
#'     inside of inidivdual chunks of text.
#'
#' @param invect vector of input strings, possibly containing empty strings
#'
#' @return a possibly NULL vector of strings
#' @export
#'
#' @examples
#' \dontrun{
#' x <- c("1","","2","","   ","3"," ","4","","a string","end")
#' x
#' length(x)
#' length(removeEmpty(x))
#' removeEmpty(x)
#' }
removeEmpty <- function(invect) {
  tmp <- gsub(" ","",invect)
  tmp <- tmp[nchar(tmp) > 0]
  return(tmp)
}

#' @title setpalette is a shortcut for altering the palette to R4
#' 
#' @description setpalette is a shortcut for changing the 
#'     default color palette to the new R version 4.0.0 version
#'     before it comes out. The new palette was described in a
#'     blog post at developer.r-project.org and provides less 
#'     garish and a more visible set of default colours that can
#'     be called using the numbers 1 - 8. An important point is 
#'     that this alters the default colours for all sessions
#'     until a restart of R. Using something similar you can 
#'     define your own preferred palettes should you wish to.     
#'     
#' @param x either "default", "R3", or "R4", with R4 as the 
#'     default value. Use "default" or "R3" to revert back to the
#'     standard R version 3. values.
#'
#' @return nothing but it does alter the base palette
#' @export
#'
#' @examples
#' \dontrun{
#'    setpalette("R3")
#'    plot(1:8,rep(0.25,8),type="p",pch=16,cex=5,col=c(1:8))
#'    setpalette("R4")
#'    points(1:8,rep(0.3,8),pch=16,cex=5,col=c(1:8)) #toprow
#' }
setpalette <- function(x="R4") { # x="R4"
  choice <- c("default","R3","R4")
  if (x %in% choice) {
    if ((x == "R3") | (x == "default")) {
      palette("default")
    }
    if (x == "R4") {
      palette(c("#000000", "#DF536B", "#61D04F", "#2297E6",
                "#28E2E5", "#CD0BBC", "#EEC21F", "#9E9E9E"))
    }
  } else {
    cat("Currently options are default, R3, or R4 \n")
  }
} # end of setpalette

#' @title setplot provides an example plot with defaults for a standard plot
#'
#' @description Provides an example plot with defaults for a standard plot
#'   includes details of how to gnerate tiff, pdf, and png versions,
#'   mtext and legends. Currently no parameters, but the function
#'   is open to development for customization of the example plot.
#' @return prints lines of R that will define a standard plot and can be 
#'     copied into an R script.
#' @export setplot
#' @examples
#' \dontrun{
#' setplot()
#' }
setplot <- function() {
  cat('#if (names(dev.cur()) %in% c("null device","RStudioGD")) \n')
  cat('#    dev.new(width=width,height=height,noRStudioGD = TRUE) \n')
  cat('#graphfile <- "name.tiff" OR "name.pdf" OR name.png  \n')
  cat('#if (file.exists(graphfile)) file.remove(graphfile)  \n')
  cat('#tiff(file=graphfile,width=150,height=150,units="mm",res=300,
      compression=c("lzw")) OR  \n')
  cat('#pdf(file=graphfile,onefile=T,width=8,height=6,family="Times") OR \n')
  cat('#png(filename=graphfile,width=150,height=100,units="mm",res=300,
      family="Times") \n')
  cat('\n')
  cat('par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0)) \n')
  cat('par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7) \n')
  cat('ymax <- max(y,na.rm=T) * 1.05 \n')
  cat('plot(x,y,type="l",xlab="",ylab="",col=1,ylim=c(0,ymax),yaxs="i",
      lwd=2) \n')
  cat('title(ylab=list("ylabel", cex=1.0, font=7),  \n')
  cat('      xlab=list("xlabel", cex=1.0, font=7)) \n')
  cat('\n')
  cat('legend(0,0.45,c("True Mean","Precise","Imprecise"),col=c(4,1,2),
      lwd=3,bty="n",cex=1.0) \n')
  cat('mtext("label",side=2,outer=T,line=0.0,font=7,cex=1.0) \n')
  cat('\n')
  cat('#dev.off() \n')
  cat('#graphics.off() \n')
} # end of set_plot



#' @title splitDate - Generates a vector of date and time components
#'
#' @description splitDate - Generates a vector of date and time components,
#'     perhaps for inclusion in filenames or other labels; helpful for
#'     keeping different run outputs seperate and identifiable.
#' @param dat - a system time from Sys.time() to be broken in components;
#'     defaults to NA, whereupon the current time is used.
#' @return a vector od characters relating to 'Year', 'Month', 'Day','Time',
#'     and a DateTime, which is a combination of all of these suitable for
#'     inclusion in a filename.
#' @export splitDate
#' @examples
#' \dontrun{
#' tmp <- splitDate()
#' print(tmp)
#' print(names(tmp))
#' print(as.numeric(tmp[1:3]))
#' print(tmp["DateTime"])
#' }
splitDate <- function(dat=NA) {
  if(is.na(dat)) dat <- as.POSIXlt(Sys.time())
  out <- unlist(dat)
  tim <- paste(trunc(as.numeric(out[3])),trunc(as.numeric(out[2])),
               "_",trunc(as.numeric(out[1]),1),sep="")
  day <- as.character(trunc(as.numeric(out[4])))
  month <- as.character(trunc(as.numeric(out[5])) + 1)
  if (nchar(month) == 1) month <- paste(0,month,sep="")
  year <- as.character(trunc(as.numeric(out[6])) - 100)
  combined <- paste(year,month,day,"_",tim,sep="")
  ans <- c(year,month,day,tim,combined)
  names(ans) <- c("Year","Month","Day","Time","DateTime")
  return(ans)
} # end of split_Date

#' @title which.closest find the number closest to a given value
#'
#' @description which.closest finds either the number in a vector which is
#'     closest to the input value or its index value
#'
#' @param x the value to lookup
#' @param invect the vector in which to lookup the value x
#' @param index should the closest value be returned or its index; 
#'     default=TRUE
#'
#' @return by default it returns the index in the vector of the value 
#'     closest to the input value
#' @export
#'
#' @examples
#' \dontrun{
#' vals <- rnorm(100,mean=5,sd=2)
#' pick <- which.closest(5.0,vals,index=TRUE)
#' pick
#' vals[pick]
#' which.closest(5.0,vals,index=FALSE)
#' }
which.closest <- function(x,invect,index=T) {
  pick <- which.min(abs(invect-x))
  if (index) {
    return(pick)
  } else {
    return(invect[pick])
  }
} # end of which_.closest

#' @title '\%ni\%' identifies which element in x is NOT in y
#'
#' @param x a vector of elements which can be numeric or character
#' @param y a vector of elements which can be numeric or character
#'
#' @export
#' 
#' @examples
#' \dontrun{
#'   x <- 1:10
#'   y <- 6:18
#'   x %ni% y
#'   pick <- (x %ni% y)
#'   x[pick]
#' }
`%ni%` <- function(x,y) {
  !(x %in% y)
}
