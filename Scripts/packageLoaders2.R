if(!require(dplyr)){
	install.packages("dplyr")
	library(dplyr)
}

if(!require(knitr)){
  install.packages("knitr")
  library(knitr)
}

if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}

if(!require(ggplot2)){
	install.packages("ggplot2")
	library(ggplot2)
}

if(!require(tibble)){
  install.packages("tibble")
  library(tibble)
}

if(!require(fmsb)){
  install.packages("fmsb")
  library(fmsb)
}

if(!require(devtools)){
	install.packages("devtools")
	library(devtools)
  devtools::install_github("rstudio/httpuv")
}