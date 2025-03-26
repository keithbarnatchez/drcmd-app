library(shiny)
library(shinyjs)

function(input, output, session) {

  # Initially disable the variable selection inputs and the "Select All" button,
  # since data isn't loaded at this point
  disable("outcome")
  disable("treatment")
  disable("covariates")
  disable("select_all")
  disable("complete_case_probability") # this needs to be disabled too*

  # Load data specified by user
  data <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
  })

  # Note: we may need a step here to account for any weird data quirks.
  # Since there won't be any output on the right by this point, this could be a good time
  # to leave warning messages. We could also just explicitly require certain
  # types of data

  # Once the data is loaded, allow users to specify Y, A and X
  observe({
    req(data())
    all_vars <- names(data())

    updateSelectInput(session, "outcome", choices = all_vars)
    updateSelectInput(session, "treatment", choices = all_vars)
    updateSelectizeInput(session, "covariates", choices = all_vars)
    updateSelectInput(session, "complete_case_probability", choices = all_vars) # this needed to be updated as well when a dataset is added. 

    enable("outcome")
    enable("treatment")
    enable("covariates")
    enable("select_all")
    enable("complete_case_probability")
  })

  # Don't want anything getting double selected. If something is selected as
  # outcome, make it so it can't be selected as treatment
  observe({
    req(data())
    all_vars <- names(data())
    if (!is.null(input$outcome) && input$outcome != "") {
      updateSelectInput(session, "treatment",
                        choices = setdiff(all_vars, input$outcome),
                        selected = if(input$treatment %in% setdiff(all_vars, input$outcome)) input$treatment else NULL)
    } else {
      updateSelectInput(session, "treatment", choices = all_vars)
    }
  })

  # Similarly, make it so selected Y + A can't be selected to go into X
  observe({
    req(data())
    all_vars <- names(data())
    exclude_vars <- c()
    exclude_ccp_vars <- c() # need to make a new one that includes Y, A, AND X**
    if (!is.null(input$outcome) && input$outcome != "") {
      exclude_vars <- c(exclude_vars, input$outcome)
      exclude_ccp_vars <- c(exclude_vars, input$outcome) 
    }
    if (!is.null(input$treatment) && input$treatment != "") {
      exclude_vars <- c(exclude_vars, input$treatment)
      exclude_ccp_vars <- c(exclude_vars, input$treatment)
    }
    if (!is.null(input$covariates) && length(input$covariates) > 0) {
      exclude_ccp_vars <- c(exclude_vars, input$covariates) # only need to updatee ccp vars here
    }
    available_covars <- setdiff(all_vars, exclude_vars)
    available_ccp_vars <- setdiff(all_vars, exclude_ccp_vars)

    updateSelectizeInput(session, "covariates",
                         choices = available_covars,
                         selected = intersect(input$covariates, available_covars))
    
    updateSelectInput(session, "complete_case_probability",
                      choices = available_ccp_vars, # same logic here with new variable
                      selected = if (input$`complete_case_probability` %in% available_ccp_vars) input$`complete_case_probability` else NULL)
  }) # essentially 

  # Button for letting user select all available covariates
  # Will select all variables excluding what's selected for Y and A, since a variable
  # can't be a covariate (in X) and an outcome/treatment simultaneously
  observeEvent(input$select_all, {
    req(data())
    all_vars <- names(data())
    exclude_vars <- c()
    if (!is.null(input$outcome) && input$outcome != "") {
      exclude_vars <- c(exclude_vars, input$outcome)
    }
    if (!is.null(input$treatment) && input$treatment != "") {
      exclude_vars <- c(exclude_vars, input$treatment)
    }
    if (!is.null(input$complete_case_probability) && input$complete_case_probability != ""){
      exclude_vars <- c(exclude_vars, input$complete_case_probability)
    }
    available_covars <- setdiff(all_vars, exclude_vars)
    updateSelectizeInput(session, "covariates", selected = available_covars)
  })
  

  # Preview of the data
  # Don't know if we'll actually keep this feature but it's here for now
  output$datatable <- renderTable({
    req(data())
    head(data())
  })
  #-----------------------------------------------------------------------------
  # Analysis + output

  # run drcmd (will store results as output object)
  # we'll get to this eventually, leaving blank for now to focus on inputs
  drcmd_obj <- eventReactive(input$run_drcmd, {
    req(data(), input$outcome, input$treatment)
    vals <- 1
    vals
  })

  # Output drcmd_results
  output$drcmd_output <- renderPrint({
    req(drcmd_obj())
    print(drcmd_obj()) # will want to do something that makes better-looking output
  })
}
