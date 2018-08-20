if(!require(learnr)){
  install.packages("learnr")
  library(learnr)
}

if(!require(tidyverse)){
	install.packages("tidyverse")
	library(tidyverse)
}

if(!require(shiny)){
	install.packages("shiny")
	library(shiny)
}

if(!require(devtools)){
	install.packages("devtools")
	library(devtools)
  devtools::install_github("rstudio/httpuv")
}