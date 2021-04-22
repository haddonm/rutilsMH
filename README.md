# rutilsMH

To install from github it is most simple from inside RStudio using:

if (!require(devtools)){install.packages("devtools")} 
devtools::install_github("https://github.com/haddonm/rutilsMH")

Though I still have to write a vignette

Contains a set of functions I use all the time when using R

When conducting analyses using R there are functions I use all the time for plotting, within apply functions, and others. To simplify their use I have put  together this small package, which I can call whenever I use one of my other packages.

* 0.1.13 2021-04-22 Added pickbound to automate the selection of par(plots=c(?,?)).

* 0.1.13 2021-04-21 Added str1 and str2 to speed the typing of str(x,max.level=1/2). This is obviously a minor addition but, for me, avoids making typos when explore the contents of relatively complex objects.

* 0.1.11 2021-04-13 now contains describefunctions which lists the R functions within each file of a given list of R files

* 0.1.10 2021-03-28 small modification to parset so that other grpahics parameters can be defined inside it rather than calling a separate par() statement.

* 0.1.9 2021-03-24 added getparplots (select r,c for the par(plots(r,c)) function) and changed extractcode to extractRcode to clarify that it extracts the rcode chunks from Rmd files.

* 0.1.8 2020-12-24 just the latest version as placed on github.

* 0.1.7 2020-11-12 added rmdcss, which simplifies setting up an HTML Rmd file

* 0.1.6 2020-10-06 added makecanvas, makevx, makevy, circle, cart2pol, pol2cart, makerect, pythag, and plotoblong all from diagrams. These just make it slightly easier to plot diagrams within R.

* 0.1.5 2020-09-13 added plotnull and tidynames 

* 0.1.4 revised plot1, plotxyy, and inthist. inthist now returns the proportional distribution of the counts and the proportional distribution of the values

* 0.1.3 contains no errors, no warnings, and no notes.

* 0.1.0 contains no errors, no warnings, and no notes.
