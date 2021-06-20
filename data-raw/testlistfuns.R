
library(rutilsMH)
ddir <- "c:/Users/User/DropBox/A_code/rutilsMH/R/"
filen <- "rutils.R" #c("aMSE.R","definezone.R")

x <- describefunctions(ddir,filen)


# Should give:

# x$"function"
# [1] "`%ni%`"            "cart2pol"          "circle"            "classDF"           "countgtOne"        
#     "countgtzero"       "countNAs"          "countones"         "countzeros"        "describefunctions"
# [11] "diagrams"          "digitsbyrow"       "extractRcode"      "facttonum"         "findfuns"          
#      "freqMean"          "geomean"           "getmax"            "getmin"            "getname"          
# [21] "getnamespace"      "getparplots"       "getseed"           "gettime"           "greplow"          
#      "halftable"         "info"              "inthist"           "kablerow"          "linept"           
# [31] "lininterpol"       "listExamples"      "listfuns"          "magnitude"         "makecanvas"        
#      "makerect"          "makeUnit"          "makevx"            "makevy"            "newplot"          
# [41] "outfit"            "parset"            "parsyn"            "pickbound"         "pkgfuns"           
#      "plot1"             "plotnull"          "plotoblong"        "plotprep"          "plotxyy"          
# [51] "pol2cart"          "printV"            "properties"        "pythag"            "quants"            
#      "removeEmpty"       "revsum"            "rmdcss"            "setpalette"        "setplot"          
# [61] "splitDate"         "str1"              "str2"              "tidynames"         "toXL"              
#      "uphist"            "which.closest"     "wtedmean"  


