library(shiny)
library(profvis)

profvis({
  runApp("app")
})
