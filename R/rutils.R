


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

#' @title describefunctions lists all R functions in a set of files
#' 
#' @description describefunctions lists all the R functions in a set of R files
#'     along with their syntax, the linenumber in each file, the filename, the
#'     function name, and the functions within the set of R files that each 
#'     function calls. In addition, there is now a crossreference column,
#'     which identifies which functions call each function. If just the indir
#'     is provided then all R files in that directory will be examined. .Rmd
#'     files will not be considered but any other file type starting with .R
#'     may cause trouble until I find a fix!
#'
#' @param indir the directory in which to find the R files
#' @param files a vector of filenames, as character, within which to search for 
#'     functions, default="", which means all R files in indir will be used
#' @param outfile the full path and name of the CSV file to which the results 
#'     should be saved. default="", which means the output will only be 
#'     returned invisibly. If outfile has a fullpath csv filename then it will
#'     also be written to that file as well as retunred invisibly
#'
#' @return It can produce a csv file but also returns the results invisibly 
#' @export
#'
#' @examples
#' print ("wait on suitable data") # indir=ddir; files=""; outfile=""
describefunctions <- function(indir,files="",outfile="") {
  if (nchar(files[1]) == 0) {
    dirfiles <- dir(indir)
    pickfiles <- grep(".R",dirfiles,ignore.case=TRUE)
    files <- dirfiles[pickfiles]
    pickRmd <- grep(".Rmd",dirfiles,ignore.case=TRUE)
    if (length(pickRmd) > 0) files <- files[-pickRmd]
  }
  nfiles <- length(files)
  numfuns <- matrix(0,nrow=nfiles,ncol=1,dimnames=list(files,c("nfuns")))
  allfiles <- NULL
  for (i in 1:nfiles) { # i = 1
    outfuns <- listfuns(paste0(indir,files[i]))
    if (nrow(outfuns) > 1) {
      numfuns[i,1] <- nrow(outfuns)
    } else {
      if (nchar(outfuns[1,"function"]) > 0) numfuns[i,11] <- 1
    }
    allfiles <- rbind(allfiles,outfuns)
  }
  allfilesort <- allfiles[order(allfiles[,"function"]),]
  allrefs <- matrix(0,nrow=0,ncol=1)
  for (i in 1:nfiles) {# i = 2
    if (numfuns[i] > 0) {
       allrefs <- rbind(allrefs,findfuns(indir,files[i],
                                         allfilesort[,"function"]))
    } else {
      allrefs <- rbind(allrefs,", , ")
    }
  }
  allfiles[,"references"] <- allrefs
  x <- allfiles[order(allfiles[,"function"]),]
  x[,"crossreference"] <- ""
  nfun <- nrow(x)
  for (i in 1:nfun) {
    pickf <- grep(x[i,"function"],x[,"references"])
    if (length(pickf) > 0) {
      if (length(pickf) == 1) x[i,"crossreference"] <- x[pickf,"function"]
    } else {
      x[i,"crossreference"] <- paste0(x[pickf,"function"],collapse=", ")
    }
  }
  if (nchar(outfile) > 5) write.csv(x,file = outfile)
  return(invisible(x))
} # end of describefunctions

#' @title diagrams provides the syntax of functions for making diagrams
#' 
#' @description diagrams provides the syntax of functions for making diagrams
#'
#' @return nothing but it write syntax for diagram functions to the console
#' @export
#'
#' @examples
#' diagrams()
diagrams <- function() {
  cat('circle(origx = 50, origy = 50, radius = 10, col = 1, lwd = 1) \n')
  cat('makecanvas(xstart = 0, xfinish = 100, ystart = 0, yfinish = 100) \n')
  cat('makerect(left, xinc, top, yinc, linecol = "grey", lwd = 1) \n')
  cat('makevx(init, inc) \n')
  cat('makevy(init, inc) \n')
  cat('plotoblong(x0, x1, y0, y1, border = 1, col = 0, lwd = 1)  \n')
} # end of diagrams

#' @title digitsbyrow a helper function for knitr, to specify formats by row
#'
#' @description digitsbyrow is a solution obtained from StackOverFlow, sugegsted
#'     by Tim Bainbridge in 11/12/19. knitr formats table columns as a whole,
#'     which can be a problem if one wants to mix integers with real numbers in
#'     the same columns. This first transposes the data.frame/matrix being
#'     printed, fixes the formats, and then transposes it back. In knitr one
#'     then needs to use the align argument to fix the alignment. In may version
#'     I have conserved both rownames and colnames for both data.frames and
#'     matrices (the original only did so for data.frames but I often print
#'     matrices). digitsbyrow converts all entries to character so knitr becomes
#'     necessary for printing.
#'
#' @param df the data.frame or matrix to be printed by knitr
#' @param digits a vector of the digits wanted for each row of the df or matrix
#'
#' @return a formatted data.frame or matrix depending on input
#' @export
#'
#' @examples
#' x <- matrix(c(rnorm(5,mean=5,sd=1),seq(1,10,1)),nrow=3,ncol=5,byrow=TRUE,
#'             dimnames=list(1:3,1:5))
#' digitsbyrow(x, c(3,0,0))
#' # needs knitr to use kable
#' # kable(digitsbyrow(x, c(3,0,0)),align='r',row.names=TRUE)
digitsbyrow <- function(df, digits) {
  tmp0 <- data.frame(t(df))
  tmp1 <- mapply(
    function(df0, digits0) {
      formatC(df0, format="f", digits=digits0)
    },
    df0=tmp0, digits0=digits
  )
  tmp1 <- data.frame(t(tmp1))
  rownames(tmp1) <- rownames(df)
  colnames(tmp1) <- colnames(df)
  if (class(df)[1] == "matrix") tmp1 <- as.matrix(tmp1)
  return(tmp1)
} # end of digitsbyrow


#' @title extractRcode pulls out the r-code blocks from Rmd files
#' 
#' @description extractRcode pulls out the r-code blocks from Rmd files and 
#'     saves them into a separate R file. 
#'
#' @param indir the directory in which the rmd file is to be found and into
#'     which the output file will be placed.
#' @param rmdfile the name of the Rmd file whose R code is to be extracted
#' @param filename the name of the R file into which the r-code is to go. 
#'
#' @return generates an R file in the working directory, otherwise returns nothing
#' @export
#'
#' @examples
#' print("wait on a real example")
extractRcode <- function(indir,rmdfile,filename="out.R") { # indir=indir; rmdfile=inrmd; filename="out.R"
  infile <- paste0(indir,"/",rmdfile)
  fileout <- paste0(indir,"/",filename)
  cat("# R-code from the file ",rmdfile,"\n\n",file=fileout,append=FALSE)
  txt <- readLines(infile)
  pick <- grep("```",txt)
  steps <- length(pick)
  for (i in seq(1,steps,2)) {
    begin <- pick[i]
    cat("#",txt[begin],"\n",file=fileout,append=TRUE)
    finish <- pick[i+1]
    for (j in (begin+1):(finish-1)) {
      cat(txt[j],"\n",file=fileout,append=TRUE)
    }
    cat("#",txt[finish],"\n\n\n\n",file=fileout,append=TRUE)
  }
} # end of extractRcode

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

#' @title findfuns finds references to other functions within other functions
#' 
#' @description findfuns is used when developing a complex project containing 
#'     many R files, each containing many R functions. Given a file that 
#'     contains a set of functions (infile) and a data.frame of all functions  
#'     from the project (allfuns), which is obtained using listfuns, then 
#'     findfuns searches each function for references to any of the projects
#'     functions. This allows them to be cross referenced
#'
#' @param indir the directory in which the file identified in 'infile' is
#'     located
#' @param infile the filename of the R file within which to search for the 
#'     functions listed in the allfuns data.frame derived from the listfuns
#'     function
#' @param allfuns a data.frame of functions and their properties listed in 
#'     the order of the sorted function names in the 'function' column 
#'
#' @return the same data.frame except that the references column will have been
#'     populated
#'
#' @examples
#' print("wait on suitable data-set")
findfuns <- function(indir,infile,allfuns) { 
  # indir=indir;infile=files[1]; allfuns=allfilesort[,c(,"function")]
  infile <- file.path(indir,infile)
  numfun <- length(allfuns)
  content <- readLines(con=infile)
  rfun <- tail(unlist(strsplit(infile,"/")),1)
  rfile <- substr(rfun,1,nchar(rfun)-2)
  funLines <- grep("function",content)
  titles <- grep("@title",content)
  testhash <- substr(content[funLines],1,4)
  omit <- grep("#",testhash)
  if (length(omit) > 0) {
    funLines <- funLines[-omit]
    testhash <- testhash[-omit]
  }
  omit2 <- grep("  ",testhash) # remove functions internal to other functions
  if (length(omit2) > 0) funLines <- funLines[-omit2]
  nfun <- length(funLines)
  outf <- as.data.frame(matrix("",nrow=nfun,ncol=1))
  bounds <- matrix(0,nrow=nfun,ncol=2,
                   dimnames=list(paste0(rfile,1:nfun),c("start","end")))
  bounds[,1] <- funLines + 1
  if (nfun > 1) {
    bounds[,2] <- c((titles[2:nfun] - 2),length(content))
  } else {
    bounds[,2] <- length(content)
  }
  for (i in 1:nfun) { # i=1
    funname <- removeEmpty(unlist(strsplit(content[funLines[i]],"<-"))[1])
    funcont <- content[bounds[i,1]:bounds[i,2]]
    testhash <- substr(funcont,1,5)
    omit <- grep("#",testhash)
    if (length(omit) > 0) funcont <- funcont[-omit]
    whichfun <- ", "
    for (j in 1:numfun) {  #  j = 65
      if (allfuns[j] != funname)
        if (length(grep(allfuns[j],funcont)) > 0)
          whichfun <- paste0(whichfun,allfuns[j],", ")
    }
    outf[i,] <- whichfun
  }
  return(outf)
} # end of findfuns

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
    if (length(infreqs[infreqs > 0.01]) > 1) {
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

#' @title getparplots selects the par(plots()) rows and columns for plots
#'
#' @description getparplots selects the rows and columns used by
#'     par(plots(rows,columns)). This is required when generating plots under
#'     conditions where it is not know how many plots will be required when
#'     writing the plot.function. Currently the maximum is limited to 25 plots
#'
#' @param nplots the number of plots to generate in one plot. This has a maximum
#'     value of 25. If getparplots is used with nplots > 25 it will stop after
#'     throwing an error message.
#'
#' @return a vector of 2, the rows and columns for the par(plots)
#' @export
#'
#' @examples
#' getparplots(12)
#' getparplots(13)
getparplots <- function(nplots) {
  dat <- c(1,1,2,1,2,2,2,2,3,2,3,2,4,2,4,2,3,3,5,2,4,3,4,3,5,3,5,3,5,3,
           4,4,6,3,6,3,7,3,7,3,7,3,6,4,6,4,6,4,5,5)
  nbits <- length(dat)/2
  if (nplots > nbits)
    stop(cat("getparplots currently only allows for ",nbits," plots \n"))
  ans <- matrix(dat,nrow=nbits,ncol=2,byrow=TRUE,
                dimnames=list(1:nbits,c("rows","cols")))
  return(ans[nplots,])
} # end of getparplots

#' @title getseed generates a random number seed
#' 
#' @description getseed generates a seed for use within set.seed. 
#'     It produces up to a 6 digit integer from the Sys.time. This
#'     Initially, at the start of a session there is no seed; a new one 
#'     is created from the current time and the process ID when one is 
#'     first required. Here, in getseed, we do not use the process ID so 
#'     the process is not identical but this at least allows the 
#'     set.seed value to be stored should the need to repeat a set of 
#'     simulations arise. The process generates up to a six digit number
#'     it then randomly reorders those digits and that becomes the seed.
#'     That way, if you were to call getseed in quick succession the
#'     seeds generated should differ even when they are generated close
#'     together in time.
#'
#' @return  an integer up to 7 digits long 
#' @export
#'
#' @examples
#' useseed <- getseed()
#' set.seed(useseed)
#' rnorm(5)
#' set.seed(12345)
#' rnorm(5)
#' set.seed(useseed)
#' rnorm(5)
getseed <- function() {
  pickseed <- as.character(as.integer(Sys.time()))
  nc <- nchar(pickseed)
  if (nc > 7) pickseed <- substr(pickseed,(nc-6),nc)
  nc <- nchar(pickseed)  
  pseed <- unlist(strsplit(pickseed,split=character(0)))
  pseed <- sample(pseed,nc)
  newseed <- paste(pseed,collapse="")
  newseed <- as.numeric(newseed)
  return(newseed)
} # end of getseed

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

#' @title inthist a replacement for the hist and boxplot functions
#'
#' @description inthist it is common to want to generate a list of counts as
#'     integers from an input vector and then plot then as columns of those
#'     counts. Alternatively, it is common to have a two-column matrix of 
#'     values and counts or totals where one wants to plot columns of those
#'     counts or totals against those values. inhist allows one to enter either 
#'     a vector of integers to be counted and plotted OR a matrix of values in 
#'     column 1 and counts or totals in column 2. The option of rounding 
#'     non-integers is available.

#' @param x a vector of integers to be counted and plotted OR a matrix of
#'     values in column 1 and counts or totals in column 2
#' @param col the colour of the fill; defaults to black = 1, set this to 0
#'     for an empty bar, but then give a value for border
#' @param border the colour of the outline of each bar defaults to col
#' @param width denotes the width of each bar; defaults to 0.9, should be >0
#'     and <= 1
#' @param xlabel the label for the x axis; defaults to ""
#' @param ylabel the label for the y axis; defaults to ""
#' @param main the title for the individual plot; defaults to ""
#' @param lwd the line width of the border; defaults to 1
#' @param xmin sets the lower bound for x-axis; used to match plots, defaults to 
#'     NA whereupon the minimum of values is used
#' @param xmax sets the upper bound for x axis; used with multiple plots, 
#'     defaults to NA whereupon the maximum of values is used
#' @param ymax enables external control of the maximum y value; mainly of
#'     use when plotting multiple plots together.
#' @param plotout plot the histogram or not? Defaults to TRUE
#' @param prop plot the proportions rather than the counts, default=FALSE
#' @param inc sets the xaxis increment; used to customize the axis;
#'     defaults to 1.
#' @param xaxis set to FALSE to define the xaxis outside of inthist;
#'     defaults to TRUE
#' @param roundoff if values are not integers should they be rounded off to
#'     become integers? default=TRUE. Obviously only useful when inputting a
#'     matrix.
#' @param ... available to pass extra plot arguments, such as 
#'     panel.first=grid(), or whatever to the internal plot call
#'     
#' @return a matrix of values and counts with the proportions of counts and 
#'     values is returned invisibly
#' @export
#' 
#' @examples
#'   x <- trunc(runif(1000)*10) + 1
#'   inthist(x,col="grey",border=3,width=0.75,xlabel="Random Uniform",
#'           ylabel="Frequency")
#'   x <- as.matrix(cbind(c(1,2,3,4,5,6,7,8),trunc(runif(8,1,20))))
#'   inthist(x,col="grey",border=3,width=0.75,xlabel="integers",
#'           ylabel="Frequency")
inthist <- function(x,col=1,border=NULL,width=0.9,xlabel="",ylabel="",
                    main="",lwd=1,xmin=NA,xmax=NA,ymax=NA,plotout=TRUE,
                    prop=FALSE,inc=1,xaxis=TRUE,roundoff=TRUE,...) {
  #  x=ebtipy;col=2;border=3;width=0.9;xlabel="";ylabel="";main="";lwd=1;xmin=NA
  #  xmax=NA;ymax=NA;plotout=TRUE;prop=FALSE;inc=1;xaxis=TRUE;roundoff=TRUE
  if (class(x)[1] == "matrix") {
    counts <- x[,2]
    values <- x[,1]
  } else {
    counts <- table(x)
    if (length(counts) == 0) stop("No data provided \n\n")
    values <- as.numeric(names(counts))
  }
  if ((sum(!(abs(values - round(values)) < .Machine$double.eps^0.5)) > 0) &
      (roundoff)) {
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
    colnames(answer) <- c("values","counts","propcounts")
  } else { answer <- NA  }
  class(answer) <- c("matrix","inthist")
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

#' @title listfuns produces a listing of all functions in an input R file
#'
#' @description listfuns reads in a given R file and then identifies each
#'     function header within it and pulls out the function name, its syntax,
#'     the line-number in the file, and associates that with the filename.
#'
#' @param infile the R file to be examined
#'
#' @return a data.frame of syntax, function name, line number, and file name
#'
#' @examples
#' print("wait for an example")
listfuns <- function(infile) { # infile=paste0(ddir,filen[1]); 
  content <- readLines(con=infile)
  if (length(grep("/",infile) > 0)) {
    rfun <- tail(unlist(strsplit(infile,"/")),1)
    rfile <- substr(rfun,1,nchar(rfun)-2)
  } else {
    rfile <- infile
  }
  funLines <- grep("function",content)
  testhash <- substr(content[funLines],1,4)
  omit <- grep("#",testhash)
  if (length(omit) > 0) {
    funLines <- funLines[-omit]
    testhash <- testhash[-omit]
  }
  omit2 <- grep("  ",testhash) # remove functions internal to other functions
  if (length(omit2) > 0) funLines <- funLines[-omit2]
  nLine <- length(funLines)
  delF <- NULL
  if (nLine > 0) {
    for (i in 1:nLine) {
      tmpLine <- gsub(" ","",content[funLines[i]])
      if ((length(grep("function\\(",tmpLine)) == 0) |
          (substr(tmpLine,1,2) == "#'") |
          (length(grep("<-function",tmpLine)) == 0) |
          (length(grep("} #",tmpLine)) > 0)) delF <- c(delF,i)
    }
  }  
  ndelF <- length(delF)
  if (ndelF > 0) {
    funLines <- funLines[-delF]
  }
  if (ndelF == nLine) {
    txt <- paste0(infile,"  contained no recognizable functions")
    warning(cat(txt,"\n"))
    out <- "NA"
    funnames <- ""
    funLines <- 1
    n <- 1
  } else {
    outlines <- sort(c(funLines))
    out <- content[outlines]
    funnames <- out
    n <- length(out)
    for (i in 1:n) {  # i=1
      out[i] <- gsub(" ","",(unlist(strsplit(out[i],"\\{")))[1])
      funnames[i] <- removeEmpty(unlist(strsplit(out[i],"<-"))[1])
      out[i] <- gsub("<-function","",out[i])
    }
  }
  columns <- c("syntax","linenumber","file","function","references")
  rows <- paste0(rfile,1:n)
  outfuns <- as.data.frame(matrix(NA,nrow=n,ncol=length(columns),
                                  dimnames=list(rows,columns)))
  outfuns[,"syntax"] <- out
  outfuns[,"function"] <- funnames
  outfuns[,"linenumber"] <- funLines
  outfuns[,"file"] <- rfile
  return(outfuns)
} # end of listfuns


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

#' @title makecanvas sets up a plotting area ready for the flowchart
#'
#' @description makecanvas sets up a plotting areas ready for a flowchart
#'     made up of shapes, circles, polygons, rectangles, text, and arrows
#'
#' @param xstart x-origin value defaults = 0
#' @param xfinish maximum of x axis defaults = 100
#' @param ystart y-origin value default = 0
#' @param yfinish y-axis maximum default = 100
#'
#' @return nothing but plots an empty graph ready for polygons and text
#' @export
#'
#' @examples
#' \dontrun{
#'   makecanvas(ystart=50,yfinish=93.5)
#'   polygon(makevx(2,27),makevy(90,6),col=0,lwd=1,border=1)
#' }
makecanvas <- function(xstart=0,xfinish=100,ystart=0,yfinish=100) {
  par(mfrow=c(1,1),mai=c(0.1,0.1,0.1,0.1),oma=c(0.0,0,0.0,0.0))
  par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
  plot(seq(xstart,xfinish,length=101),seq(ystart,yfinish,length=101),
       type="n",xaxt="n",yaxt="n",xlab="",ylab="", bty="n")
} # end of makecanvas

#' @title makevx make an x values vector
#'
#' @description makevx takes the left x value of a rectangle and the
#'     increment rightwards that defines a vector describing the four
#'     vertices of the rectangle topleft, topright, bottomright,
#'     bottomleft, topleft. when matched with makevy generates the
#'     descriptor for a complete rectangle.
#'
#' @param init x-value for the left-hand edge of a rectangle
#' @param inc the x-increment added to init to define the right-hand edge
#'
#' @return a vector of y-values
#' @export
#'
#' @examples
#' \dontrun{
#'  plot(0:100,seq(58,93.5,length=101),type="n",xaxt="n",yaxt="n",
#'  xlab="",ylab="", bty="n")
#'  polygon(makevx(2,27),makevy(90,6),col=0,lwd=1,border=1)
#' }
makevx <- function(init,inc) {
  return(c(init,init+inc,init+inc,init,init))
}


#' @title makevy make a y values vector
#'
#' @description makevy takes the top y value of a rectangle and the
#'     vertical increment downwards and defines a vector describing the four
#'     vertices of the rectangle topleft, topright, bottomright,
#'     bottomleft. topleft, when matched with makevx generates the
#'     descriptor for a complete rectangle.
#'
#' @param init y-value for the top edge of a rectangle
#' @param inc the y-increment subtracted from init to define the lower edge
#'
#' @return a vector of y-values
#' @export
#'
#' @examples
#' \dontrun{
#'  canvas(ystart=50,yfinish=93.5)
#'  polygon(makevx(2,27),makevy(90,6),col=0,lwd=1,border=1)
#' }
makevy <- function(init,inc) {
  return(c(init,init,init-inc,init-inc,init))
}

#' @title circle draws a circle with a given origin and radius
#' 
#' @description circle provides the means of drawing a circle of a given
#'     radius and origin within a diagram ready for the addition of text.
#'
#' @param origx the final x origin
#' @param origy the final y origin
#' @param radius the radius of the circle
#' @param col the col of the circle
#' @param lwd the line width of the circle
#'
#' @return the matrix of x and y values invisibly  
#' @export
#'
#' @examples
#'   makecanvas()
#'   circle(origx=35,origy=70,radius=30,lwd=2,col=1)
#'   circle(origx=65,origy=60,radius=30,lwd=2,col=2)
#'   circle(origx=45,origy=40,radius=30,lwd=2,col=4)
circle <- function(origx=50,origy=50,radius=10,col=1,lwd=1) {
  ans <- pol2cart(angle=seq(0,360,0.1),dist=radius,xorig=origx,yorig=origy)
  lines(ans[,"x"],ans[,"y"],lwd=lwd,col=col)
  return(invisible(ans))
} # end of circle

#' @title cart2pol converts cartesian coordinates into the polar angle
#' 
#' @description cart2pol as a step in converting cartesian coordinates into
#'     polar coordinates this calculates the angle, in degrees, from x y
#'     values
#'
#' @param x either a vector of two values of a matrix of pairs of values
#'
#' @return a single angle of vector of angles
#'
#' @examples
#' \dontrun{
#'   cart2pol(c(3,3))  # should be 45
#'   dat <- matrix(c(3,4,5,7),nrow=2,ncol=2,byrow=TRUE)
#'   print(dat)
#'   cart2pol(dat)     # should be 36.8699 twice.
#' }
cart2pol <- function(x){
  if (is.vector(x)) angle <- 180 * (atan2(x[1],x[2])) / pi
  if (is.matrix(x)) angle <- 180 * (atan2(x[,1],x[,2])) / pi
  return(angle=angle)
} # end of cart2pol

#' @title makerect draws a rectangle once a plot is available
#'
#' @description makerect draws a rectangle after canvas has been called
#'
#' @param left defines lefthand edge of rectangle
#' @param xinc left + xinc defines right-hand edge or rectangle
#' @param top defines top edge of rectangle
#' @param yinc top - yincdefines bottom edge of rectangle
#' @param linecol colour of line. default="grey"
#' @param lwd the width of the line, default=1
#' @param col the fill colour of the polygon drawn. default=NULL so not filled
#'
#' @return a vector denoting the center (x,y) of the rectangle
#' @export
#'
#' @examples
#' \dontrun{
#'    canvas(ystart=50,yfinish=93.5)
#'    makerect(left=2,xinc=27,top=90,yinc=6)
#' }
makerect <- function(left,xinc,top,yinc,linecol="grey",lwd=1,col=NULL) {
  polygon(makevx(left,xinc),makevy(top,yinc),col=col,
          lwd=lwd,border=linecol)
  centerx <- (left * 2 + xinc)/2
  centery <- (top * 2 - yinc)/2
  return(invisible(c(centerx,centery)))
}

#' @title pickbound selects an optimum number of rows and cols for a plot
#' 
#' @description pickbound enables the automatic selection of a pre-determined
#'     optimum combination of plot rows and columns to suit a number of plots
#'     up to 30. So, given a number of plots from 1 to 30 this returns a numeric 
#'     dimer containing the number of rows and columns needed for par statement
#'
#' @param n the number of plots to be included in a combined plot
#'
#' @return a vector of two with the number of rows and columns for a plot
#' @export
#'
#' @examples
#' pickbound(5)
#' pickbound(8)
pickbound <- function(n) {
  bounds <- matrix(c(1,1,1,2,2,1,3,2,2,4,2,2,5,3,2,6,3,2,7,4,2,8,4,2,9,3,3,10,3,4,
                     11,3,4,12,3,4,13,5,3,14,5,3,15,5,3,16,4,4,17,5,4,18,5,4,19,5,4,
                     20,5,4,21,5,5,22,5,5,23,5,5,24,5,5,25,5,5,26,5,6,27,5,6,28,5,6,
                     29,5,6,30,5,6),nrow=30,ncol=3,byrow=TRUE)
  out <- c(bounds[n,2],bounds[n,3])
  return(out)
} # end of pickbound

#' @title plotoblong generates an oblong from x0,x1,y0,y1
#' 
#' @description plotoblong generates an oblong from x0,x1,y0,y1
#'
#' @param x0 x-axis left
#' @param x1 x-axis right
#' @param y0 yaxis bottom
#' @param y1 yaxis top
#' @param border colour of the border, default=black=1
#' @param col colour of fill, default = 0 =  empty
#' @param lwd width of the line,default=1
#'
#' @return nothing but it plots a polygon
#' @export
#'
#' @examples
#' \dontrun{
#'   canvas()
#'   plotoblong(1,50,1,50,lwd=3,linecol=4)
#' }
plotoblong <- function(x0,x1,y0,y1,border=1,col=0,lwd=1) {
  x <- c(x0,x0,x1,x1,x0); y <- c(y0,y1,y1,y0,y0)
  polygon(x,y,lwd=lwd,border=border,col=col)
}

#' @title pol2cart polar to cartesian coordinates
#' 
#' @description pol2cart translate polar coordinates of angles (as degrees)
#'     and a distance = radius, into cartesian coordinates of x and y. The
#'     option of using arbitrary origin coordinates is included
#'
#' @param angle the angle in degrees, either a single number of a vector
#' @param dist the length of the line or radius, a single number
#' @param xorig the final xorigin
#' @param yorig the final yorigin
#'
#' @return a matrix of 1 or more rows depending on length of angle
#' @export
#'
#' @examples
#' \dontrun{
#'   ans <- pol2cart(angle=seq(0,360,15),dist=20,xorig=30,yorig=30)
#'   print(ans)
#' }
pol2cart <- function(angle,dist,xorig=0,yorig=0){
  #  angle=45:50; dist=10; xorig=0; yorig=0
  numang <- length(angle)
  coord <- matrix(0,nrow=numang,ncol=2,dimnames=list(1:numang,c("x","y")))
  angler <- angle*pi/180
  for (i in 1:numang) {
    coord[i,] <- c(xorig + dist * sin(angler[i]),
                   yorig + dist * cos(angler[i]))  
  }
  return(coord) #output the new x and y coordinates
} # end of pol2cart

#' @title pythag calculates Pythagorus' theorum on a vector of two values
#' 
#' @description pythag Pythagorus' theorum states that the length of the
#'     hypotheneuse between two lines at right angels to each other (that
#'     is in cartesian coordinates) is the sqrt of the sum of their squares.
#'
#' @param x a vector of two numbers or a matrix of pairs of numbers
#'
#' @return a single number or a vector depending on input
#' @export
#'
#' @examples
#' \dontrun{
#'  pythag(c(3,4))  # should be 5
#'  dat <- matrix(c(3,4,5,7),nrow=2,ncol=2,byrow=TRUE)
#'  print(dat)
#'  pythag(dat)     # should be 5 and 10
#' }
pythag <- function(x) {  # x = ans
  if (is.vector(x)) ans <- sqrt((x[1]^2 + x[2]^2))
  if (is.matrix(x)) ans <- sqrt((x[,1]^2 + x[,2]^2))
  return(ans) 
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
#'     solution. It might be more effective to implement an S3 method.
#'
#' @param inopt the list object output by nlm, nlminb, or optim
#' @param backtran a logical default = TRUE If TRUE it assumes
#'     that the parameters have been log-transformed for stability
#'     and need back-transforming
#' @param digits the number of digits to round the backtransformed 
#'     parameters. defaults to 5.
#' @param title character string used to label the output if desired,
#'     default = empty character string
#' @param parnames default="" which means the estimated parameters
#'     will merely be numbered. If a vector of names is given 
#'     then this will be used instead, at least, for nlm and optim.
#'
#' @return nothing but it does print the list to the console tidily
#' @export
#'
#' @examples
#'  x <- 1:10  # generate power function data from c(2,2) + random
#'  y <- c(2.07,8.2,19.28,40.4,37.8,64.68,100.2,129.11,151.77,218.94)
#'  alldat <- cbind(x=x,y=y)
#'  pow <- function(pars,x) return(pars[1] * x ^ pars[2])
#'  ssq <- function(pars,indat) {
#'     return(sum((indat[,"y"] - pow(pars,indat[,"x"]))^2))
#'  }  # fit a power curve using normal random errors
#'  pars <- c(2,2)
#'  best <- nlm(f=ssq,p=pars,typsize=magnitude(pars),indat=alldat)
#'  outfit(best,backtran=FALSE) #a=1.3134, b=2.2029 ssq=571.5804
outfit <- function(inopt,backtran=TRUE,digits=5,title="",
                   parnames=""){
  #  inopt=bestvB; backtran = FALSE; digits=5; title=""; parnames=""
  nlmcode <- c("gradient close to 0, probably solution",
               ">1 iterates in tolerance, probably solution",
               "Either ~local min or steptol too small",
               "iteration limit exceeded",
               "stepmax exceeded ,5 times")
  if (length(grep("value",names(inopt))) > 0) { # optim
    cat("optim solution: ", title,"\n")
    cat("minimum     : ",inopt$value,"\n")
    cat("iterations  : ",inopt$counts," iterations, gradient\n")
    cat("code        : ",inopt$convergence,"\n")
    if (backtran) {
      ans <- cbind(par=inopt$par,transpar=round(exp(inopt$par),digits))
    } else {
      ans <- t(inopt$par)
    }
    if ((length(parnames) > 1) & (length(parnames) == length(inopt$par))) {
      rownames(ans) <- parnames
    } else {
      rownames(ans) <- 1:length(inopt$par)
    }
    print(ans)
    cat("message     : ",inopt$message,"\n")
  } # end of optim
  if (length(grep("minimum",names(inopt))) > 0) {  # nlm - preferred
    cat("nlm solution: ", title,"\n")
    cat("minimum     : ",inopt$minimum,"\n")
    cat("iterations  : ",inopt$iterations,"\n")
    cat("code        : ",inopt$code,nlmcode[inopt$code],"\n")
    if (backtran) {
      ans <- cbind(par=inopt$estimate,gradient=inopt$gradient,
                   transpar=round(exp(inopt$estimate),digits))
    } else {
      ans <- cbind(par=inopt$estimate,gradient=inopt$gradient)
    }
    if ((length(parnames) > 1) & 
        (length(parnames) == length(inopt$estimate))) {
      rownames(ans) <- parnames
    } else {
      rownames(ans) <- 1:length(inopt$estimate)
    }
    print(ans)
  } # end of nlm
  if (length(grep("objective",names(inopt))) > 0) {
    cat("nlminb solution: ", title,"\n")   # nlminb seems to be deprecated
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
#' @param byrow should plots be made by row (mfrow; byrow=TRUE, the default),
#'     of by column (mfcol; byrow=FALSE)
#' @param ... the generic ellipsis allowing for the includion of other graphics
#'     arguments such as xaxs="n", etc.
#'
#' @return nothing but it changes the base graphics par settings
#' @export
#'
#' @examples
#' \dontrun{
#' parset()
#' parsyn()
#' }
parset <- function(plots=c(1,1),cex=0.75,font=7,outmargin=c(0,0,0,0),
                   margin=c(0.45,0.45,0.05,0.05),byrow=TRUE,...) {
  if (byrow) {
    par(mfrow=plots,mai=margin,oma=outmargin)
  } else {
    par(mfcol=plots,mai=margin,oma=outmargin)
  }
  par(cex=cex, mgp=c(1.35,0.35,0), font.axis=font,font=font,
      font.lab=font,...)
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
#' @param xlab the label fot the x-axis, defaults to empty
#' @param ylab the label fot the y-axis, defaults to empty
#' @param type the type of plot "l" is for line, the default, "p" is
#'     points. If you want both plot a line and add points afterwards.
#' @param usefont which font to use, defaults to 7 which is Times bold
#' @param cex the size of the fonts used. defaults to 0.85
#' @param maxy defaults to 0, if a value is given then that value is used rather 
#'     than estimating from the input y using getmax
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
plot1 <- function(x,y,xlab="",ylab="",type="l",usefont=7,cex=0.75,
         maxy=0,defpar=TRUE,...){
  if (defpar) {
    par(mfrow = c(1,1), mai = c(0.45,0.45,0.1,0.05),oma = c(0,0,0,0))
    par(cex = cex, mgp = c(1.35, 0.35, 0), font.axis = usefont,
        font = usefont, font.lab = usefont)
  }
  if (maxy > 0) ymax <- maxy  else ymax <- getmax(y)
  if (min(y,na.rm=TRUE) < 0.0) ymin[1] <- getmin(y) else ymin <- 0.0
  plot(x,y,type=type,ylim=c(ymin,ymax),yaxs="i",
       ylab=ylab,xlab=xlab,cex=cex,panel.first=grid(),...)
} # end of plot1

#' @title plotnull generates an empty plot when one is needed
#'
#' @description plotnull there are often circumstances, for example, when
#'     plotting up results from each year and each SAU, where there will be
#'     combinations of year and SAU that have no data, but to avoid a problem
#'     with the plotting it is necessary to generate an empty plot.
#'
#' @param msg a message to be printed in the middle of the empty plot.
#'
#' @return nothing but it does generate a plot
#' @export
#'
#' @examples
#' plotnull("An empty plot")
plotnull <- function(msg="") {
  plot(1:10,1:10,type="n",xaxt="n",yaxt="n",xlab="",ylab="")
  if (nchar(msg) > 0)
    text(x=5,y=5,msg,cex=1.0,font=7)
} # end of plotnull

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
#' @param verbose set this to FALSE to turn off the reminder to include 
#'     a graphics.off() command after the plot. Default=TRUE
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
                     newdev=TRUE,filename="",resol=300,verbose=TRUE) {
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
  oldpar <- par(no.readonly=TRUE)
  par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0.0,0.0,0.0))
  par(cex=cex, mgp=c(1.35,0.35,0), font.axis=usefont,font=usefont,
      font.lab=usefont)
  if ((lenfile > 0) & (verbose))
    cat("\n Remember to place 'dev.off()' after plot \n")
  return(invisible(oldpar))
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
#' @param defpar should the internal 'par' statement be used = defpar=TRUE, or
#'     the default=FALSE, which means the plot 'par' will be defined outside the
#'     plot.
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
                    colour=c(1,2),defpar=FALSE) {
  if (defpar) {
    par(mfrow=c(1,1),mai=c(0.5,0.45,0.15,0.05),oma=c(0.0,0.75,0.0,3.0)) 
    par(cex=cex, mgp=c(1.35,0.35,0), font.axis=fnt,font=fnt,font.lab=fnt) 
  }
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
properties <- function(indat,dimout=FALSE) {  # indat=sps1; dimout=FALSE
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
  pick <- which((clas == "character") & (isna == 0))
  if (length(pick) > 0) {
    for (i in 1:length(pick)) {
      pickna <- which(indat[,pick[i]] == "NA")
      if (length(pickna) > 0) isna[pick[i]] <- length(pickna)
    }
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

#' @title rmdcss generates some initial css style code for HTML Rmd files
#' 
#' @description rmdcss generates some initial css style code for HTML Rmd files
#'     as well as a mathjax script that will generate equation numbers for any
#'     display equations in the document. This prints the css style code and
#'     the mathjax script to the console from where it should be pasted into the
#'     Rmd file immediately following the YAML header. It now contains font 
#'     sizes for the h1 heaqder and the .inline and .display math classes
#'
#' @return nothing but it prints css style code and a mathjax script to the 
#'     console
#' @export
#'
#' @examples
#' rmdcss()
rmdcss <- function() {
  cat('<style type="text/css"> \n',
      '  body, td { \n',
      '  font-size: 16px; \n',
      '  font-family: "Times New Roman", Times, serif; \n',
      '} \n')
  cat('code.r{ \n',
      '  font-size: 15px; \n',
      '} \n')
  cat('pre {  \n',
      'font-size: 8px  \n',
      '}  \n')
  cat('h1 {  \n',
      '  font-size: 32px  \n',
      '}  \n')
  cat('.inline{font-size: 15px; } \n',
      '.display{font-size: 18px;} \n',
      '<','/style>  \n')
  cat('\n\n')
  cat('<script type="text/x-mathjax-config">  \n',
      '  MathJax.Hub.Config({  \n',
      '    TeX: {   \n',
      '      equationNumbers: {   \n',
      '        autoNumber: "all",  \n',
      '        formatNumber: function (n) {return ',3.,'+n}  \n',
      '      }  \n', 
      '    }  \n',
      '  });  \n',
      '<','/script>  \n')
} # end of rmdcss

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
#' @export
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

#' @title str1 a simple replacement for str(x,max.level=1)
#' 
#' @description str1 an abbreviated replacement for str(x,max.level=1), which I 
#'     put together because to often I make a typo when typing out the full
#'     str syntax. Hence I find str1 helpful
#'
#' @param x the object whose structure is to be listed
#'
#' @return str(x,max.level=1)
#' @export
#'
#' @examples
#' x <- matrix(rnorm(25,mean=5,sd=1),nrow=5,ncol=5)
#' str1(x)
str1 <- function(x){
  return(str(x,max.level=1))
}

#' @title str2 a simple replacement for str(x,max.level=2)
#' 
#' @description str2 an abbreviated replacement for str(x,max.level=2), which I 
#'     put together because to often I make a typo when typing out the full
#'     str syntax. For when str1 is not detailed enough.
#'
#' @param x the object whose structure is to be listed
#'
#' @return str(x,max.level=2)
#' @export
#'
#' @examples
#' x <- matrix(rnorm(25,mean=5,sd=1),nrow=5,ncol=5)
#' str2(x)
str2 <- function(x){
  return(str(x,max.level=2))
}

#' @title tidynames can replace awkward data.frame names with better ones
#'
#' @description tidynames can replace awkward or overly long data.frame
#'     column names with better ones than are easier to use. It also
#'     permits one to maintain the same set of column names within an
#'     analysis even when the source data.frame includes alterations.
#'
#' @param columns the vector of names that shoudl include the ones to be
#'     altered
#' @param replace the names to be changed, as a vector of character
#'     strings
#' @param repwith the replacement names as a vector of character strings
#'
#' @return a vector of new columns names
#' @export
#'
#' @examples
#'  print("wait")
tidynames <- function(columns,replace,repwith) {
  nreplace <- length(replace)
  if (nreplace != length(repwith))
    stop("Different number of names in replace and repwith \n")
  for (i in 1:nreplace) {
    pick <- grep(replace[i],columns)
    #cat(i,pick,"\n")
    if (pick[1] > 0) {
      columns[pick[1]] <- repwith[i]
    } else {
      warning(paste0(replace[i]," not in the dataset"))
    }
  }
  return(columns)
} # end of tidynames

#' @title toXL copies a data.frame or matrix to the clipboard
#'
#' @description toXL copies a data.frame or matrix to the clipboard
#'    so one can then switch to Excel and just type <ctrl> + V to paste the
#'    data in place
#'
#' @param x a vector or matrix
#' @param output - a boolean determining whether to print the object to the
#'    screen as well as the clipboard; defaults to FALSE
#' @return Places the object 'x' into the clipboard ready for pasting
#' @export toXL
#' @examples
#' datamatrix <- matrix(data=rnorm(100),nrow=10,ncol=10)
#' colnames(datamatrix) <- paste0("A",1:10)
#' rownames(datamatrix) <- paste0("B",1:10)
#' toXL(datamatrix,output=TRUE)
toXL <- function(x,output=FALSE) {
  write.table(x,"clipboard",sep="\t")
  if(output) print(x)
}

#' @title uphist a histogram with an upper limit on the x-axis
#' 
#' @description uphist is merely a wrapper around the base hist
#'     function, which adds the ability to limit the upper value on
#'     the x-axis. With fisheries data it is surprisingly common to 
#'     have data that has a very few extreme values that can obscure
#'     a standard plot of the data. The data are only truncated 
#'     within the uphist function so any other analyses will be on all 
#'     available data. If a maximum value is selected which 
#'     accidently eliminates all available data the script stops with
#'     an appropriate warning. If a value is selected which fails to 
#'     eliminate any data then all data are used.
#'
#' @param x the vector of values to be plotted as a histogram
#' @param maxval the maximum value to be retained in the plotted data
#' @param ... all the other arguments used by the base hist function
#'
#' @return nothing, but it does plot a histogram
#' @export
#'
#' @examples
#' \dontrun{
#'   x <- rlnorm(5000, meanlog=2, sdlog=1)
#'   hist(x,breaks=30,main="",xlab="log-normal values")
#'   uphist(x,breaks=30,main="",xlab="log-normal values",maxval=100)
#'   uphist(x,breaks=30,main="",xlab="log-normal values",maxval=1000)
#' }
uphist <- function(x,maxval=NA,...) {
  if (is.numeric(maxval)) {
    pick <- which(x > maxval)
    if (length(pick) > 0) x <- x[-pick]
  }
  if (length(x) > 0){
    hist(x,...)
  } else {
    stop("maxval in uphist too small and no data remaining. \n")
  }
} # end of uphist

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

#' @title wtedmean calculates the weighted mean of a set of values and weights
#'
#' @description wtedmean solves the problem of calculating a weighted mean
#'     value from a set of values with different weights. Within the aMSE this
#'     is common when trying to summarize across populations within an SAU or
#'     summarize SAU within a zone by finding a mean value weighted by the
#'     respective catch from each related population or SAU.
#'
#' @param x the values whose weighted mean is wanted
#' @param wts the weights to use, often a set of catches
#'
#' @return a single real number
#' @export
#'
#' @examples
#' saucpue <- c(91.0,85.5,88.4,95.2)
#' saucatch <- c(42.0,102.3,75.0,112.0)
#' wtedmean(saucpue,saucatch)
#' saucatch/sum(saucatch)  # the relative weights
wtedmean <- function(x,wts) {
  pwts <- wts/sum(wts,na.rm=TRUE)
  ans <- sum((x * pwts),na.rm=TRUE)
  return(ans)
} # end of wtedmean

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
