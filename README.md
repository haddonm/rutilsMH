# rutilsMH

To install from github it is most simple from inside RStudio using:

if (!require(devtools)){install.packages("devtools")} devtools::install_github("https://github.com/haddonm/datalowSA",build_vignettes=TRUE)

Though I still have to write a full vignette

Contains a set of functions I use all the time when using R

When conducting analyses using R there are functions I use all the time for plotting, within apply functions, and others. To simplify their use I have put  together this small package, which I can call whenever I use one of my other packages.
Version 0.1.0 contains no errors, no warnings, and no notes.

* 0.1.4 revised plot1, plotxyy, and inthist. inthist now returns the proportional distribution of the counts and the proportional distribution of the values
