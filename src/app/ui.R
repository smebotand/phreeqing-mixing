

ui = dashboardPage(
  title = "Phreeq(ing)Mixing",


  ##### HEADER #####

  dashboardHeader(
    title = span(img(src = "MiljoData.png", height = 40)),
    titleWidth = 300,
    tags$li(img(src = "ngiLogo.jpg", height = "50vh"),#),
      class = "dropdown")
  ),


  ##### SIDEBAR #####

  dashboardSidebar(disable=T),


  ##### BODY #####

  dashboardBody(

    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "app.css")
    ),
    tags$style(HTML(".dataTables_paginate .paginate_button .paginate_button.disabled .paginate_button.active .paginate_button.current {
            color: white;
            background-color: #727477;
            border-radius: 4px;
        }")),
    tags$head(
      tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png")
    ),


    ##### BODY: TITLE #####

    div(h3(em(strong("Phreeq(ing)Mixing: Chemical Speciation Modeling!"))),align = "center"),
    hr(style = "border-top: 1px solid #000000;"),


    ##### BODY: INPUTS #####

    conditionalPanel(condition = "!input.hideInputs",
                     tagList(div(h3(strong("Define Your Solution")),align = "center"),
                             splitLayout(cellWidths = c("50%", "50%"),

                                         #left side of INPUTS
                                         div(h5(em(strong("Parameter Values for Solutions"))),
                                             dataTableOutput("tableInputSolutions", width = "80%"),
                                                 align = "center"),

                                         #right side of INPUTS
                                         div(h5(em(strong("Additional Parameters"))),
                                             br(),
                                              selectInput("chrgSol1","Charge Balance Parameter  Solution 1",
                                                         c("Cl","Na")),
                                             br(),
                                             selectInput("chrgSol2","Charge Balance Parameter Solution 2",
                                                         c("Na","Cl")),
                                             br(),br(),br(),br(),br(),br(),
                                             div(actionButton("runModel", "Run Model",icon = icon("thumbs-up")),
                                                 br(),
                                                 actionButton("resetSolutions", "Reset to Default Values",icon = icon("undo")),
                                                 style = "max-width: 300px;"),
                                             align = "center",
                                             style = "padding-left: 15px;")))),
    br(),
    div(materialSwitch("hideInputs", strong("Hide Inputs"), value = FALSE, status = "success"),
        style = "padding-left: 100px;"),
    hr(style = "border-top: 1px solid #000000;"),


    ##### BODY: RESULTS #####

    div(h3(strong("Plot Your Results")),align = "center"),
    fixedRow(column(3,uiOutput("selectElements")),
             column(3,uiOutput("selectPhases")),
             column(3,uiOutput("selectSpecies"))),

    fixedRow(column(3,radioButtons("selecteDataType", "Select Data Type",choices = c("Phases","Species"))),
             column(3,radioButtons("selecteXaxis", "Select X-Axis in Plot",choices = c("Mixing Ratio","pH")))),
    br(),
    div(plotlyOutput("resultsPlot"),align = "center"),
    br(),
    div(materialSwitch("showRawDataTable", strong("Show Rawdata Table"), value = FALSE, status = "success"),
        style = "padding-left: 100px;"),
    conditionalPanel(condition = "input.showRawDataTable",
                     dataTableOutput("resultsTable")),
    br(),
    hr(style = "border-top: 1px solid #000000;"),


    ##### BODY: FOOTER #####

    div(column(12,em("NGI is not liable for any damages arising in contract, tort or otherwise from the use of or inability to use this site or any material contained in it, or from any action or decision taken as a result of using the site. This site neither stores nor collects personal information."),
               align = "center")),
    br(),br()
  )
)
