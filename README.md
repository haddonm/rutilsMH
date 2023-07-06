# rutilsMH is NOW DEPRECATED

Use:  codeutils and hplot instead, the latter separates the plotting routines from the rest, as sometimes that is all one needs.


To install from github it is most simple from inside RStudio using:

Contains a set of functions I use all the time when using R

When conducting analyses using R there are functions I use all the time for plotting, within apply functions, and others. To simplify their use I have put  together this small package, which I can call whenever I use one of my other packages.

* 0.1.21 2021-12-12 Reodered the functions internally into more coherent groups in case I sub-divide the package into smaller, more specific packages.

* 0.1.20 2021-11-11 Includes getDBdir (get DropBox directory for my own use), and improved printV

* 0.1.19 2021-06-20 Included identifyfuns and extractpathway to aid in package documentation.

* 0.1.18 2021-06-15 Included kablenow (row formatted tables) and linept (linepoint).

* 0.1.17 2021-06-14 Included revsum to allow for reverse cumulative sums as a contrast to cumsum

* 0.1.16 2021_05-28 Not sure what happened to earlier updates, but here I have added digitsbyrow, a helper function for knitr.

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
