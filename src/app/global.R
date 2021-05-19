#Loading packages
library(shiny)
library(DT)
library(shinydashboard)
library(shinyBS)
library(modules)
library(plotly)
library(purrr)
library(dplyr)
library(magrittr)
library(tibble)
library(tidyr)
library(phreeqc)
library(shinyWidgets)

#options(shiny.error = browser)#type e to see failed code
#options(shiny.error = NULL)#type e to see failed code


#Loading module to run chemical modell
phrLoadDatabaseString(minteq.v4.dat)

phreeqc = use("modules/phreeqc.R")

cat(file = stderr(), "Done running global\n")
