# install / load Require R package manager

if(!require("Require")){install.packages("Require", repos=repository)}
library(Require)
# https://require.predictiveecology.org/


# take checkpoint of installed packages

Require::Require("checkpoint", repos=repository)

# checkpoint::checkpoint(checkpoint_date)

# load standard libraries for data processing

 
library(tictoc) # mz: added this

core_pkgs  <- c('tidyverse', 'haven', 'stringdist', 'data.table','prodest','tictoc',
				"tidytext", "textstem","textclean", "text2vec", "proxy", "lubridate", "Rfast",
				"padr", "quantmod", "fixest")
pkgs  <- c("tidyverse","dplyr","tidyr", "janitor", "scales", "magrittr", # data wrangling / analysis
			"data.table", "parallel", # performance tools
	# plotting packages
			"ggplot2", "ggforce","RColorBrewer", "paletteer","geofacet",
	# arc gis / mapping support
			"sf", "leaflet", "tidygeocoder", "tmap","tmaptools","mapsapi", "tidycensus", 
	# text manipulation / wrangling
			"purrr","lubridate","glue","tidytext", "stringr",
	# statistical analysis
			"tidymodels", "car", "mgcv", "nlme", "lme4", "survival", "caret",
			"fst", "haven", "foreign", # input / output support
			"devtools","githubinstall", 
			"here", # here() says where projects working directory is
			"httpgd", "reactable", # terminal external graphics device
			"testthat")

#require(tidyverse)
repository  <- "https://mirror.csclub.uwaterloo.ca/CRAN/"
Require::Require(pkgs, repos=repository)
Require::Require(core_pkgs, repos=repository)

# packages with external configuration needed
ext_pkgs  <- c("Rcpp", "xlsx", "reticulate")
if(!Require("Rcpp", repos=repository)){install.packages("Rcpp", repos=repository)}

if(!Require("rJava", repos=repository)){install.packages("rJava", repos=repository)}
# if warning message on install Java or check Java Home
# 	if mac apple silicon:
#		brew install java
#		sudo ln -sfn /opt/homebrew/opt/openjdk/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk.jdk

if(!Require("xlsx", repos=repository)){install.packages("xlsx", repos=repository)}
if(!Require("reticulate", repos=repository)){install.packages("reticulate", repos=repository)}
Require::Require("devtools", repos=repository)
if(!Require::Require("tm", repos=repository)){install.packages("tm", repos=repository)}

# rmarkdown / knitr
	# requires pandoc
Require::Require("rmarkdown", repos=repository)
Require::Require("knitr", repos=repository)
Require::Require("reactable", repos=repository)



# Old package calls
# require(tidyverse,lib.loc = local_library_path)
# require(haven,lib.loc = local_library_path)
# require(stringdist,lib.loc = local_library_path)
# require(data.table,lib.loc = local_library_path)
# require(prodest,lib.loc = local_library_path)
# require(tictoc,lib.loc = local_library_path)
# require(tm,lib.loc = local_library_path) # if issues installing, see https://stackoverflow.com/questions/39885408/dependency-slam-is-not-available-when-installing-tm-package
# require(tidytext,lib.loc = local_library_path)
# require(textstem,lib.loc = local_library_path)
# require(text2vec,lib.loc = local_library_path)
# require(proxy,lib.loc = local_library_path)
# require(lubridate,lib.loc = local_library_path)
# require(Rfast,lib.loc = local_library_path)
# require(padr,lib.loc = local_library_path)
# require(quantmod,lib.loc = local_library_path)

# store snapshot of installed packages

Require::pkgSnapshot(RpackageSnapshot_file, standAlone = TRUE)


