#rm(list=ls()) #clean all stuff NATWIP!!!!
#setwd("C:/GIT2/envi-3d-interpolation/src/app")

#Web
library(shiny)
library(DT)
library(shinydashboard)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(shinyhelper)
library(modules)
library(rintrojs)
library(modules)
#library(shinyglide)

#saptial
library(plotly)
library(sf)
library(sp)
library(gstat)

#data
library(Metrics)
library(lattice)
library(purrr)
library(dplyr)
library(magrittr)
library(xlsx)
library(tibble)
library(tidyr)
library(phreeqc)

#options(shiny.error = browser)#type e to see failed code
#options(shiny.error = NULL)#type e to see failed code

phrLoadDatabaseString(minteq.v4.dat)

phreeqc = use("modules/phreeqc.R")

cat(file = stderr(), "Done running global\n")
