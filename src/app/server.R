
server = function(input, output, session) {


  ###### Defining Input table #####

  inputValues = reactiveValues()

  inputSolutions = reactive({

    t(dplyr::tibble(
      temp = c(20, 20),
      pH = c(4.74, 7.27),
      units = c("mg/L","mg/L"),
      Na = c(4, 7.5),
      K = c(0.4, 6.5),
      Mg = c(0.9, 4.63),
      Ca = c(1.3, 28.6),
      Al = c(0.68, 0.02),
      Fe = c(1.23, 0.09),
      Mn = c(0.05, NA),
      Cl = c(4.75, 12.35),
      S = c(0.6534, 6.16),
      F = c(0.043, 0),
      `N(5)` = c(0.2259, 0.92615),
      P = c(0.0028, 0.083),
      `Alkalinity (mmol/L)` = c(NA, 1.563),
      `O(0)` = c(8.68, 8.68))) %>%
      magrittr::set_colnames(c("Solution 1","Solution 2"))
  })


  output$tableInputSolutions = renderDataTable({

    input$resetSolutions #ensures reactivity

    datatable(data = isolate(inputSolutions()),
              editable = list(target = 'cell', disable = list(columns = c(0))),
              options = list(
                columnDefs = list(list(className = 'dt-center', targets = 1:2)),#hiding id column
                dom = "t",
                lengthMenu = list(c( -1))
              ))
  })


  #create proxy of table as a shadow table back-end to access user edits
  proxySol1 = dataTableProxy("tableInputSolutions")


  #updating tables if changes are made to the initial solutions
  observeEvent(inputSolutions(),{
    inputValues$sol1 = inputSolutions()[,"Solution 1"]
    inputValues$sol2 = inputSolutions()[,"Solution 2"]
  }, label = "OEinputSolutions")


  #updating tables back-end if changes are made through gui
  observeEvent(input$tableInputSolutions_cell_edit,{

    editsSol1 = inputValues$sol1
    editsSol2 = inputValues$sol2
    editedValue = input$tableInputSolutions_cell_edit

    if(editedValue$col == 1){
      editsSol1[editedValue$row] = editedValue$value
      inputValues$sol1 = editsSol1

    } else if (editedValue$col == 2){
      editsSol2[editedValue$row] = editedValue$value
      inputValues$sol2 = editsSol2
    }

  }, label = "OEtableInputSolutions_cell_edit")


  # Hiding Inputs When Model has been run!
  observeEvent(input$runModel, {

    updateSwitchInput(
      session = session,
      inputId = "hideInputs",
      value = TRUE)
  },
  ignoreInit = TRUE,
  label = "OErunModel")


  ##### running modell #####

  resultValues = reactiveValues(statusModel = "preparing")

  output$statusModel = renderUI({
    input$runModel
    if(resultValues$statusModel == "preparing") img(src = "preparing.PNG")
    else if(resultValues$statusModel == "finish") img(src = "finish.PNG")
  })

  observeEvent(input$runModel,{

    resultModel = phreeqc$runModel(sol1 = inputValues$sol1,
                                   chrg.sol1 = input$chrgSol1,
                                   sol2 = inputValues$sol2,
                                   chrg.sol2 = input$chrgSol2)

    resultValues$elements = resultModel$elements
    resultValues$species = resultModel$species
    resultValues$phases = resultModel$phases
    resultValues$statusModel = "finish"
  },
  label = "OEresultsModel")

  ##### Rendering selection options based on results #####

  output$selectElements = renderUI({

    if(is.null(resultValues$elements)) return()

    selectInput("selElements","Select Elements to Plot",
                choices = resultValues$elements,
                multiple = F)

  })


  availableSpecies = reactive({

    if(is.null(resultValues$species)|is.null(input$selElements)) return()

    availableSpecies = resultValues$species %>%
      filter(across(input$selElements)) %>%
      select(Species) %>%
      unique.data.frame() %>%
      as_vector()

    names(availableSpecies) = availableSpecies

    return(availableSpecies)
  }, label = "RavailableSpecies")


  availablePhases = reactive({

    if(is.null(resultValues$phases)|is.null(input$selElements)) return()

    availablePhases = resultValues$phases %>%
      filter(get(input$selElements)) %>%
      select(Phase) %>%
      unique.data.frame() %>%
      as_vector()

    names(availablePhases) = availablePhases

    return(availablePhases)
  }, label = "RavailablePhases")


  output$selectSpecies = renderUI({

    if(is.null(availableSpecies())) return()

    selectInput("selSpecies","Select Species to Plot",
                choices = availableSpecies(),
                selected = availableSpecies()[1],
                multiple = T)
  })


  output$selectPhases = renderUI({

    if(is.null(availablePhases())) return()

    selectInput("selPhases","Select Phases to Plot",
                choices = availablePhases(),
                selected = availablePhases()[1],
                multiple = T)
  })


  ##### Creating Outputs for Results #####

  selectedData = reactive({

    if(input$selecteDataType == "Species"){
      if(is.null(input$selSpecies)) return()

      selectedData = resultValues$species %>%
        filter(grepl("mix", sim_name),
               Species %in% input$selSpecies)

    } else if (input$selecteDataType == "Phases"){
      if(is.null(input$selPhases)) return()

      selectedData = resultValues$phases %>%
        filter(grepl("mix", sim_name),
               Phase %in% input$selPhases)
    }
    return(selectedData)
  }, label = "RselectedData")


  output$resultsPlot = renderPlotly({

    if(is.null(selectedData())) return()

    #defining plotting parameters based on user inputs
    if(input$selecteXaxis == "Mixing Ratio") {
      xaxis = "sol2_frac"
      xaxisTitle = "Solution 2 (%)"
      xaxisMultiple = 100
    } else {
      xaxis = "pH"
      xaxisTitle = "pH"
      xaxisMultiple = 1
    }

    if(input$selecteDataType == "Phases") {
      yaxis = "SI"
      color = "Phase" }
    else if(input$selecteDataType == "Species"){
      yaxis = "Molality"
      color = "Species"
    }

    #plotting
    plot_ly() %>%
      add_trace(x = unlist(selectedData()[,xaxis])*xaxisMultiple,
                y = unlist(selectedData()[,yaxis]),
                color = unlist(selectedData()[,color]),
                type = "scatter",
                mode = "markers+lines") %>%
      layout(xaxis = list(title = xaxisTitle),
             yaxis = list(title = yaxis))
  })


  output$resultsTable = renderDataTable({

    if(is.null(selectedData())) return()

    datatable(selectedData(),
              extensions = "Buttons",
              options = list(
                scrollX = TRUE,
                dom = "lfrtiBp",
                lengthMenu = list(c(5,10,50,-1), c("5","10","50","All")),
                buttons = list(
                  c('copy', 'csv', 'excel', 'pdf', 'print')
                )))
  })

}

