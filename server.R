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
  disable("run_regression") # button only unlocks when a valid dataset is uploaded
  disable("proxy_variables") # same

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
    updateSelectInput(session, "complete_case_probability", choices = all_vars)
    updateSelectInput(session, "proxy_variables", choices = all_vars)# this needed to be updated as well when a dataset is added. 

    enable("outcome")
    enable("treatment")
    enable("covariates")
    enable("select_all")
    enable("complete_case_probability")
    enable("proxy_variables")
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
    exclude_ccp_vars <- c()# need to make a new one that includes Y, A, AND X**
    exclude_proxy_vars <- c() # same as above, also disclude ccp column
    if (!is.null(input$outcome) && input$outcome != "") {
      exclude_vars <- c(exclude_vars, input$outcome)
      exclude_ccp_vars <- c(exclude_vars, input$outcome) 
      exclude_proxy_vars <- c(exclude_vars, input$outcome) 
    }
    if (!is.null(input$treatment) && input$treatment != "") {
      exclude_vars <- c(exclude_vars, input$treatment)
      exclude_ccp_vars <- c(exclude_vars, input$treatment)
      exclude_proxy_vars <- c(exclude_vars, input$treatment) 
    }
    if (!is.null(input$covariates) && length(input$covariates) > 0) {
      exclude_ccp_vars <- c(exclude_vars, input$covariates) # only need to updatee ccp vars her
      exclude_proxy_vars <- c(exclude_vars, input$covariates)
    }
    if (!is.null(input$complete_case_proability) && length(input$complete_case_probability) > 0) {
      exclude_ccp_vars <- c(exclude_vars, input$complete_case_probability) # only need to update proxy vars from here on
      exclude_proxy_vars <- c(exclude_vars, input$complete_case_probability)
    }
    available_covars <- setdiff(all_vars, exclude_vars)
    available_ccp_vars <- setdiff(all_vars, exclude_ccp_vars)
    available_proxy_vars <- setdiff(all_vars, exclude_proxy_vars)

    updateSelectizeInput(session, "covariates",
                         choices = available_covars,
                         selected = intersect(input$covariates, available_covars))
    
    updateSelectInput(session, "complete_case_probability",
                      choices = available_ccp_vars, # same logic here with new variable
                      selected = "")
                      
    updateSelectInput(session, "proxy_variables",
                      choices = available_proxy_vars,
                      selected = ""
    )
  }) # essentially only allow non-conflicting columns else null

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
  
  # Logic for handling correct data
  
  observe({
    req(data(), input$treatment, input$outcome)
    treatment_values <- data()[[input$treatment]]
    outcome_values <- data()[[input$outcome]]
    
    covariate_values <- NULL
    if (!is.null(input$covariates) && length(input$covariates) > 0) {
      covariate_values <- data()[, input$covariates, drop = FALSE]
    } 
    
    ccp_values <- NULL
    if (!is.null(input$complete_case_probability) && length(input$complete_case_probability) > 0) {
      ccp_values <- data()[, input$complete_case_probability, drop = FALSE]
    } 
    
    proxy_values <- NULL
    if (!is.null(input$proxy_variables) && length(input$proxy_variables > 0)) {
      proxy_values <- data()[, input$proxy_variables, drop = FALSE]
    } 
    
    errors <- list() # make it a list of errors so it can dynamically change 
    
    if (!all(treatment_values %in% c(0,1))) {
      errors <- append(errors, "Your treatment column must contain only 1's and 0's")
      }
    if(!is.numeric(outcome_values)) {
      errors <- append(errors, "Your outcome column must be entirely numeric")
    }
    if(!is.null(input$covariates) && !is.numeric(covariate_values)) {
      errors <- append(errors, "Your covariates column must be entirely numeric")
    }
    if(!is.null(input$complete_case_probability) && !is.numeric(ccp_values)) {
      errors <- append(errors, "your complete case probabilties must be entirely numeric probabilities ∈ [0,1] ")
    }
    if(!is.null(input$proxy_variables) && !is.numeric(ccp_values)) {
      errors <- append(errors, "your proxy variables must be entirely numeric")
    }
    if (length(errors) > 0) { # only disable the button if there are any errors
      disable("run_regression")
      output$validation_message <- renderUI({
        div(style = "color: red;", paste("ERRORS[",length(errors),"]: ", errors, collapse = ", ")) # errors are neatly listed on the right
      })
    } else {
      enable("run_regression")
      output$validation_message <- renderUI({NULL}) # not needed
    }
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
