if(!require(dplyr)){
	install.packages("dplyr")
	library(dplyr)
}

if(!require(knitr)){
  install.packages("knitr")
  library(knitr)
}

if(!require(ggplot2)){
	install.packages("ggplot2")
	library(ggplot2)
}

if(!require(devtools)){
	install.packages("devtools")
	library(devtools)
  devtools::install_github("rstudio/httpuv")
}