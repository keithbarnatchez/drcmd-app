library(shiny)
library(shinyjs)
library(SuperLearner)
library(drcmd)
library(rJava)
library(ranger)

function(input, output, session) {
  
  # ccp_proxy_default_choices <- c("None selected" = NA)
  ccp_default_choices <- c("None selected" = "")
  proxy_default_choices <- c("None selected" = "")
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
    
    updateSelectInput(session, "outcome", choices = all_vars, selected = "")
    updateSelectInput(session, "treatment", choices = all_vars, selected = "")
    updateSelectizeInput(session, "covariates", choices = all_vars, selected = NULL)
    updateSelectInput(session, "complete_case_probability", 
                      choices = c(ccp_default_choices, all_vars), 
                      selected = "")
    updateSelectInput(session, "proxy_variables", 
                      choices = c(proxy_default_choices, all_vars), 
                      selected = "")
    
    enable("outcome")
    enable("treatment")
    enable("covariates")
    enable("select_all")
    enable("complete_case_probability")
    enable("proxy_variables")
  })
  
  # Don't want anything getting double selected. If something is selected as
  # outcome, make it so it can't be selected as treatment
  
  
  # Similarly, make it so selected Y + A can't be selected to go into X
  observe({
    req(data())
    all_vars <- names(data())
    exclude_vars <- c()
    exclude_ccp_vars <- c()# need to make a new one that includes Y, A, AND X**
    exclude_proxy_vars <- c() # same as above, also disclude ccp column
    
    
    if (!is.null(input$outcome) && input$outcome != "") {
      exclude_vars <- c(exclude_vars, input$outcome)
      exclude_ccp_vars <- c(input$outcome) 
      exclude_proxy_vars <- c(input$outcome) 
    }
    if (!is.null(input$treatment) && input$treatment != "") {
      exclude_vars <- c(exclude_vars, input$treatment)
      exclude_ccp_vars <- c(exclude_ccp_vars, input$treatment)
      exclude_proxy_vars <- c(exclude_proxy_vars, input$treatment) 
    }
    
    
    available_covars <- setdiff(all_vars, exclude_vars)
    available_ccp_vars <- c(ccp_default_choices, setdiff(all_vars, exclude_ccp_vars))
    available_proxy_vars <- c("None selected" = "",
                              setdiff(all_vars, exclude_proxy_vars))
    
    
    updateSelectizeInput(session, "covariates",
                         choices = available_covars,
                         selected = intersect(input$covariates, available_covars))
    
    updateSelectInput(session, "complete_case_probability",
                      choices  = available_ccp_vars,
                      selected = NA)
    
    updateSelectInput(session, "proxy_variables",
                      choices  = available_proxy_vars,
                      selected = "")
    
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
      covariate_values <- data()
    }
    
    ccp_values <- NULL
    if (!is.null(input$complete_case_probability) && input$complete_case_probability!= "") {
      ccp_values <- data()
    }
    
    
    proxy_values <- NULL
    if (!is.null(input$proxy_variables) && input$proxy_variables!= "") {
      proxy_values <- data()
    }
    
    
    errors <- list()
    
    treatment_values[treatment_values == "" | treatment_values == "NA"] <- NA
    outcome_values[outcome_values == "" | outcome_values == "NA"] <- NA
    if (!is.null(covariate_values)) covariate_values[covariate_values == "" | covariate_values == "NA"] <- NA
    if (!is.null(ccp_values)) ccp_values[ccp_values == "" | ccp_values == "NA"] <- NA
    if (!is.null(proxy_values)) proxy_values[proxy_values == "" | proxy_values == "NA"] <- NA
    
    
    if (!all(treatment_values %in% c(0,1,NA))) {
      errors <- append(errors, "Your treatment column must contain only 1's, 0's, blanks/NA's")
    }
    
    if(!all(is.numeric(outcome_values))) {
      errors <- append(errors, "Your outcome column must be entirely numeric")
    }
    
    if (!is.null(covariate_values)) {
      numeric_check <- sapply(covariate_values, is.numeric)
      if (!all(numeric_check)) {
        errors <- append(errors, "Your covariates column(s) must be entirely numeric.")
      }
    }
    
    if (input$complete_case_probability != "") {
      ccp_col <- input$complete_case_probability
      ccp_values <- data()[[ccp_col]]  # get specific column
      
      if (!is.numeric(ccp_values) || 
          any(ccp_values <= 0 | ccp_values >= 1, na.rm = TRUE)) {
        errors <- append(errors, "Complete case probabilities must be numeric ∈ (0,1)")
      }
    }
    
    if (!is.null(input$proxy_variables) && input$proxy_variables!= "") {
      if (!all(sapply(proxy_values, is.numeric))) {
        errors <- append(errors, "Your proxy variables must be entirely numeric.")
      }
    }
    
    selected_columns <- list(
      treatment_values,
      outcome_values,
      ccp_values,
      proxy_values
    )
    
    all_missing <- all(sapply(selected_columns, function(col) {
      if (is.null(col)) {
        return(TRUE)
      }
      if (is.data.frame(col)) {
        return(all(is.na(unlist(col))))
      } else {
        return(all(is.na(col)))
      }
    }))
    
    if (all_missing) {
      errors <- append(errors, "You need to have at least one data column selected without ANY missing values.")
    }
    
    # Condition to delay the warning until covariates are selected
    if (!is.null(input$covariates)) {
      any_fully_missing <- any(sapply(selected_columns, function(col) {
        if (is.null(col)) {
          return(FALSE)
        }
        if (is.data.frame(col)) {
          return(any(sapply(col, function(x) all(is.na(x)))))
        } else {
          return(all(is.na(col)))
        }
      }))
      
      if (any_fully_missing) {
        errors <- append(errors, "One or more selected columns is completely filled with missing values.")
      }
    }
    
    
    if (length(errors) > 0) {
      disable("run_regression")
      output$validation_message <- renderUI({
        div(style = "color: red;", paste("WARNINGS[",length(errors),"]: ", errors, collapse = ", "))
      })
    } else {
      enable("run_regression")
      output$validation_message <- renderUI({NULL})
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
  drcmd_obj <- eventReactive(input$run_regression, {
    req(data(), input$outcome, input$treatment)
    df <- data()
    
    Y_var <- df[[input$outcome]]
    A_var <- df[[input$treatment]]
    X_var <- df[, input$covariates, drop = FALSE]
    
    # Handle R (use user-provided column if specified)
    R_var <- if (input$complete_case_probability != "") {
      df[[input$complete_case_probability]]
    } else {
      NULL
    }
    
    # Learners handling with NULL instead of NA
    fixLearnerParameters <- function(learner_param) {
      if (is.null(learner_param)) return(NULL)
      if (length(learner_param) == 0 || all(learner_param == "") || all(is.na(learner_param))) return(NULL)
      return(learner_param)
    }
    
    check_learners <- function(x) {
      if (is.null(x)) return(NULL)
      x_clean <- x[x != "" & x != "NA" & !is.na(x)]
      if (length(x_clean) == 0) return(NULL)
      x_clean
    }
    
    fixCcpProxy <- function(ccpProxy) {

    }
    
    result <- drcmd::drcmd(
      Y = Y_var,
      A = A_var,
      X = X_var,
      W = if (input$proxy_variables != "") df[, input$proxy_variables, drop = FALSE] else NULL,
      R = R_var, # Use user-provided R here
      default_learners = fixLearnerParameters(input$default_learners),
      m_learners = check_learners(input$m_learners),
      g_learners = check_learners(input$g_learners),
      r_learners = check_learners(input$r_learners),
      po_learners = check_learners(input$po_learners),
      eem_ind = input$empirical_efficiency == "True",
      tml = input$targetted_maximum == "True", # Ensure logical
      k = input$`Cross-fitting`,
      cutoff = input$`truncate-propensity`,
      nboot = 0
    )
    
    result
  })
  
    
    output$results_table <- renderDT({
      req(drcmd_obj())
      
      estimates <- drcmd_obj()$results$estimates
      ses <- drcmd_obj()$results$ses
      
      data.frame(
        Estimand = c("ATE", "E[Y(1)]", "E[Y(0)]", "Risk Ratio", "Odds Ratio"),
        Estimate = c(estimates$psi_hat_ate, estimates$psi_1_hat, estimates$psi_0_hat, 
                     estimates$psi_hat_rr, estimates$psi_hat_or),
        SE = c(ses$psi_hat_ate, ses$psi_1_hat, ses$psi_0_hat, 
               ses$psi_hat_rr, ses$psi_hat_or),
        CI = c(
          sprintf("[%.3f, %.3f]", estimates$psi_hat_ate - 1.96*ses$psi_hat_ate, estimates$psi_hat_ate + 1.96*ses$psi_hat_ate),
          sprintf("[%.3f, %.3f]", estimates$psi_1_hat - 1.96*ses$psi_1_hat, estimates$psi_1_hat + 1.96*ses$psi_1_hat),
          sprintf("[%.3f, %.3f]", estimates$psi_0_hat - 1.96*ses$psi_0_hat, estimates$psi_0_hat + 1.96*ses$psi_0_hat),
          if (!is.na(estimates$psi_hat_rr)) sprintf("[%.3f, %.3f]", estimates$psi_hat_rr - 1.96*ses$psi_hat_rr, estimates$psi_hat_rr + 1.96*ses$psi_hat_rr) else NA,
          if (!is.na(estimates$psi_hat_or)) sprintf("[%.3f, %.3f]", estimates$psi_hat_or - 1.96*ses$psi_hat_or, estimates$psi_hat_or + 1.96*ses$psi_hat_or) else NA
        )
      )
    }, options = list(dom = 't')) # only show table, no search bar
    
    output$u_variables <- renderPrint({
      req(drcmd_obj())
      paste(drcmd_obj()$U, collapse = ", ")
    })
    
    output$z_variables <- renderPrint({
      req(drcmd_obj())
      paste(drcmd_obj()$Z, collapse = ", ")
    })
    
    output$details_output <- renderPrint({
      req(drcmd_obj())
      list(
        k_folds = drcmd_obj()$params$k,
        nboot = drcmd_obj()$params$nboot,
        m_learners = drcmd_obj()$params$m_learners,
        g_learners = drcmd_obj()$params$g_learners,
        r_learners = drcmd_obj()$params$r_learners,
        po_learners = drcmd_obj()$params$po_learners,
        estimation_method = if (drcmd_obj()$params$tml) "Targeted Maximum Likelihood" else if (result()$params$eem_ind) "Augmented one-step with EEM" else "Augmented one-step"
      )
    })
  
  
  
  # output drcmd_results
  output$drcmd_output <- renderPrint({
    req(drcmd_obj())
    summary(drcmd_obj())  
  })
  
}


