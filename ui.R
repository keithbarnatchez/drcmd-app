library(shiny)
library(shinythemes)
library(shinyjs) # need this for allowing buttons to interact with each other
library(shinyBS) # allows for hovering descriptions
library(tippy)


# SL libraries available to the user
# Note: we'll want to create a "crosswalk" that translates these into plain-english
# options users will be able to interpret. e.g. SL.nnet -> Neural networks (nnet package)
learners = SuperLearner::listWrappers(what='SL')
learners = learners[startsWith(learners, 'SL.')]

learners_with_none <- c("None" = "", learners)

fluidPage(
  
  tags$head(
    tags$style(HTML("
    .tippy-content {
      text-align: justify !important;
      font-size: 12px !important;
      max-width: 500px !important;
      padding: 2px !important;
    }
    #truncate-propensity[type='number']::-webkit-outer-spin-button,
        #truncate-propensity[type='number']::-webkit-inner-spin-button {
            -webkit-appearance: none; /*here I remove the webkit appearance pseudo elements/*
            margin: 0;
        }
        #truncate-propensity {
            -moz-appearance: textfield;
        }
  "))
  ),
  
  theme = shinytheme("flatly"),
  useShinyjs(),  # Initialize shinyjs
  titlePanel("drcmd: Doubly-robust causal inference with missing data"),
  sidebarLayout(
    
    # Panel on the lefthand side for input
    sidebarPanel(
      
      # Upload data
      fileInput("datafile", "Upload Dataset (CSV)", accept = ".csv"),
      
      #-------------- Variable selection -------------
      selectInput("outcome", "Select Outcome Variable", choices = NULL), # Y
      # actionButton("infoButton1", "hover", title = "This is a tooltip"),
      selectInput("treatment", "Select Treatment Variable", choices = NULL), # A
      selectizeInput(
        "covariates", "Select Covariate(s)",
        label = tags$span("Select Covariates", id = "covariates_label"),
        choices = NULL,
        multiple = TRUE
      ), # X
      tippy_this("covariates_label",
                 tooltip = "Covariates are additional variables that help control for confounding effects in the regression analysis.",
                 placement = "right",
                 animation = "shift-away",
                 theme = "light-border"),
      
      # Eventually we'll want a proxy variable (W) window, but we'll hold off for now
      # selectizeInput("proxies", "Select Proxy Variables", choices = NULL, multiple = TRUE), # W
      
      # Button to auto-select all available covariates.
      actionButton("select_all", "Select All Covariates"),
      br(), br(),
      
      #-------------- Parameter value selection -------------
      
      # Menu to set params (always visible)
      selectInput('default_learners', 'Default Learners',
                  label = tags$span("Default Learners", id = "default_learners_label"),
                  choices=learners,
                  multiple=TRUE),
      
      # Collapsible section for advanced params. Will want this to match everything
      # in drcmd, and have them set to the default values in drcmd
      # Idea is some params won't be of interest to users, so no need to show them by default
      # check drcmd documentation to make sure all params ae covered, we can add hovering
      # descriptions eventually as well
      bsCollapse(id = "advancedOptions", open = NULL,  # nothing open by default
                 bsCollapsePanel(
                   "Advanced Options",  # title of the collapsible section
                   # Add your menu options here with default values
                   tags$label(id = "cross_fitting_label", "Number of folds"), # Need to make a tag for just the title. 
                   numericInput("Cross-fitting", NULL, value = 1), #"By default, DRCMD uses a single fold"
                   tippy_this("cross_fitting_label", "Cross-fitting estimates parameters by splitting data into multiple folds (k-folds). This helps reduce bias when using complex models like random forests. By default, drcmd uses a single fold. Set k to enable cross-fitting. See the technical details section for more info.", placement = "right", animation= "shift away", theme="light-border"), # Copied your look for the pop-up here and for the rest of the popups
                   
                   tags$label(id = "empirical_efficiency_label", "Empirical Efficiency"),
                   selectInput("empirical_efficiency", NULL, choices = c("True", "False")),#I was unsure if I should make these actual booleans or not
                   tippy_this("empirical_efficiency_label", "Empirical Efficiency Maximization (EEM) helps improve the estimation of difficult nuisance functions (like φa(Z)) by minimizing the **asymptotic variance** of the causal estimand instead of just focusing on minimizing the mean squared error. This approach, which can be applied when estimating φa through complex nuisance learners, is an optional method. Set `eem_ind = TRUE` to enable it.", placement = "right", animation= "shift away", theme="light-border"),
                   
                   tags$label(id = "targetted_maximum_label", "Targetted maximum likelihood estimation"),
                   selectInput("targetted_maximum", NULL , choices = c("True", "False")), #I was also unsure here if I should make these actual booleans or not
                   tippy_this("targetted_maximum_label", "Targeted Maximum Likelihood Estimation (TML) is an alternative approach to estimate counterfactual means and treatment effects. Unlike the default one-step debiased estimators, TML refines the final estimators by targeting the likelihood function to improve accuracy. Set `tml = TRUE` to use this asymptotically equivalent method.", placement = "right", animation= "shift away", theme="light-border"), 
                   
                   tags$label(id = "truncate_propensity_label", "Trimming of propensity scores"),
                   numericInput("truncate-propensity", NULL, value = 0.025, min = 0, max = 0.5),
                   tippy_this("truncate_propensity_label", "Trimming of propensity scores helps prevent unstable estimators by limiting extreme values, which are used in inverse probability weights. By default, drcmd trims scores at 0.025 and 0.975. You can adjust this range by setting the `cutoff` argument to specify custom limits (e.g., setting `cutoff = 0` avoids trimming) The number you select is symmetrical. Value ∈ [0,0.5]", placement = "right", animation= "shift away", theme="light-border"), 
                   
                   selectInput('complete_case_probability', '',
                               label = tags$span("Select complete case probability column", id = "complete_case_label"),
                               choices=NULL,
                               multiple=FALSE),
                   
                   selectInput('proxy_variables', '', # added this for proxy
                               label = tags$span("Select proxy variable column", id = "proxy_variables_label"),
                               choices=NULL,
                               multiple=FALSE),
                   
                   selectInput('treatment_learners', '',
                               label = tags$span("Select treatment learner", id = "treatment_learners_label"),
                               choices=learners_with_none,
                               multiple=FALSE,
                               ),
                   
                   selectInput('outcome_learners', '',
                               label = tags$span("Select outcome learner", id = "outcome_learners_label"),
                               choices=learners_with_none,
                               multiple=FALSE,
                               ),
                   
                   selectInput('pseudo_outcome_learners', '',
                               label = tags$span("Select pseudo-outcome learner", id = "psuedo-learners_label"),
                               choices=learners_with_none,
                               multiple=FALSE,
                               ),
                   
                   selectInput('complete_case_probability_learners', '',
                               label = tags$span("Select complete-case probability learner", id = "complete_case_learners_label"),
                               choices=learners_with_none,
                               multiple=FALSE,
                               ),
                   
                   
                   style = "primary"
                 )
      ),
      
      #-------------- Hovering labels for menu items -------------
      
      # Hovering labels for menu items
      tippy_this("covariates_label",
                 tooltip = "Covariates are additional variables that help control for confounding effects in the regression analysis.",
                 placement = "right",
                 animation = "shift-away",
                 theme = "light-border"),
      
      tippy_this("complete_case_label", 
                 tooltip = "In some study designs (e.g., two-phase sampling), the probability of an observation being a complete case is known and can be controlled by the researcher. In these cases, users can provide these probabilities using the `Rprobs` argument, instead of relying on drcmd’s default estimation.",
                 placement = "right",
                 animation = "shift-away",
                 theme = "light-border"),
      
      tippy_this("default_learners_label", # here is where it starts for the learner inputs
                 tooltip = "drcmd fits multiple regression models en route to forming causal effect estimates. Users have flexibility to choose how to fit these models -- drcmd uses the SuperLearner prediction library, which supports numerous machine learning algorithms and simpler methods like linear regression.<br/><br/>If multiple are selected, SuperLearner will form predictions as a weighted average of all selected methods. ",
                 placement = "right",
                 animation = "shift-away",
                 theme = "light-border"),
      
      tippy_this("treatment_learners_label",
                 tooltip = "In the case you would like to use a specific learner for the treatment model, rather than using a default learner for all of the variables, you may select it here.",
                 placement = "right",
                 animation = "shift-away",
                 theme = "light-border"),
      
      tippy_this("outcome_learners_label",
                 tooltip = "In the case you would like to use a specific learner for the outcome model, rather than using a default learner for all of the variables, you may select it here.",
                 placement = "right",
                 animation = "shift-away",
                 theme = "light-border"),
      
      tippy_this("psuedo-learners_label",
                 tooltip = "In the case you would like to use a specific learner for the pseudo outcome model, rather than using a default learner for all of the variables, you may select it here.",
                 placement = "right",
                 animation = "shift-away",
                 theme = "light-border"),
      
      tippy_this("complete_case_learners_label",
                 tooltip = "In the case you would like to use a specific learner for the complete case probability model, rather than using a default learner for all of the variables, you may select it here.",
                 placement = "right",
                 animation = "shift-away",
                 theme = "light-border"),
      
      tippy_this("proxy_variables_label",
                 tooltip = "TESTING, TEST", # I dont really know how to describe these well in context
                 placement = "right",
                 animation = "shift-away",
                 theme = "light-border"),
      
      
      
      # Button to run the regression.
      actionButton("run_regression", "Run drcmd")
      
    ),
    
    # Main panel on the right for output
    mainPanel(
      # h4("Dataset Preview"),
      tableOutput("datatable"),
      hr(),
      h4("Results"),
      verbatimTextOutput("drcmd_output"),
      hr(),
      h4("Diagnostic Plots"),
      hr(),
      h4("Read more"),
      uiOutput("validation_message")
    )
  )
)
