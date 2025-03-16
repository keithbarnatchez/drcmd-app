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

fluidPage(

  tags$head(
    tags$style(HTML("
    .tippy-content {
      text-align: justify !important;
      font-size: 12px !important;
      max-width: 500px !important;
      padding: 2px !important;
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
      selectInput("treatment", "Select Treatment Variable", choices = NULL), # A
      selectizeInput(
        "covariates", "Select Covariates",
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
                   selectInput("option1", "Select Option 1",
                               choices = c("A", "B", "C"),
                               selected = "A"),
                   numericInput("option2", "Numeric Option", value = 10),
                   textInput("option3", "Text Option", value = "default"),
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

      tippy_this("default_learners_label",
                 tooltip = "drcmd fits multiple regression models en route to forming causal effect estimates. Users have flexibility to choose how to fit these models -- drcmd uses the SuperLearner prediction library, which supports numerous machine learning algorithms and simpler methods like linear regression.<br/><br/>If multiple are selected, SuperLearner will form predictions as a weighted average of all selected methods. ",
                 placement = "right",
                 animation = "shift-away",
                 theme = "light-border"),



      # Button to run the regression.
      actionButton("run_regression", "Run drcmd")
    ),

    # Main panel on the right for output
    mainPanel(
      h4("Dataset Preview"),
      tableOutput("datatable"),
      hr(),
      h4("Results"),
      verbatimTextOutput("drcmd_output")
    )
  )
)
