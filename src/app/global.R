##### Loading packages #####

library(shiny)
library(DT)
library(shinydashboard)
library(shinyBS)
library(modules)
library(plotly)
library(purrr)
library(dplyr)
library(magrittr)
library(tidyverse)
library(tibble)
library(tidyr)
library(phreeqc)
library(shinyWidgets)
library(naniar)
library(stats)
library(ggplot2)
library(shinycssloaders)

cat(file = stderr(), "Done loading packages\n")

##### Debugging #####

options(shiny.error = browser)
#call "e" in browser to see failed code line

library(reactlog)
options(shiny.reactlog=T)
#press ctrl + F3 to launch log after you start app
#options(shiny.trace = TRUE)
#options(shiny.fullstacktrace = TRUE)


##### Loading module to run chemical model #####

cat(file = stderr(), "Done loading database\n")

phreeqc = use("modules/phreeqc.R")

cat(file = stderr(), "Done running global\n")
