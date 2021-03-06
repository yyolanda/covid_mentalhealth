
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)

sidebar <- dashboardSidebar(
  width = 250,

  sidebarMenu(
    id='tabs',
    hr(),
    menuItem("Overview", tabName = "Dashboard", icon = icon("dashboard")),
    hr(),
    selectInput("wave", "Select a Wave",
                c("Wave 4"=4,
                  "Wave 5"=5,
                  "Wave 6"=6,
                  "Wave 7"=7,
                  "Wave 8"=8,
                  "Wave 10"=10),selected = 4),
    menuItem("Analysis", tabName = "Team", icon = icon("bar-chart-o"),
             startExpanded = TRUE,
             menuSubItem("Missing Values", tabName = "MissingVal"),
             menuSubItem("Correlations", tabName = "Corr"),
             menuSubItem("ENet (depressed)", tabName = "ENet_d"),
             menuSubItem("ENet (anxious)", tabName = "ENet_a")),
    hr(),
    menuItem("Data Dictionary", tabName = "Dict", icon = icon("book-open")),
    hr(),
    menuItem("Logs", tabName = "Logs", icon = icon("th"))
    
    
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Dashboard",
            fluidRow(
              box(title = tagList(shiny::icon("facebook"), "Overview"),
                  width=12,solidHeader = T,collapsible = T,
                  h4("In this project, we utilized the Facebook Survey data from the Delphi's
                     COVIDcast API. The data contains many variables including some mental health 
                     indicators such as 'depressed over the past 5 or 7 days' and 'anxious over the past 5 or 7 days'.
                     ElasticNet-regularized regression models were used to identify variables 
                     that were associated with these mental health indicators during the COVID19 pandemic."),
                  h4("We split the data based on the waves of COVID19 and the analysis was performed on
                  each wave separately. The table below shows the timeline of the waves. 
                  Please note that there was no data available on wave 9; 
                  wave 1-3 were not used in our analysis because no mental health data were
                     collected during wave 1-3."),
                  h3('Please select a wave using the selector on the sidebar to get started.'),
                  h4("In summary,"),
                  h5("- We performed missing value filtering and imputation."),
                  h5("- We checked the correlations between the predictors."),
                  h5("- We performed ElasticNet-regularized regression on the data of each wave to 
                     identify variables that were associated with 'depressed' or 'anxious' over the
                     past 5 or 7 days and selected the best-performing model using cross-validation."),
                  h5("- We checked the features that were selected by the final ElasticNet model.")
                  ),
              box(title = tagList(shiny::icon("calendar-alt"), "Timeline of the waves"),
                  width=12,solidHeader = T,collapsible = T,
                  div(style = 'overflow-x: auto',
                      dataTableOutput('waveDat'), align = "center")
                  )
              )),
    tabItem(tabName = "MissingVal",
            fluidRow(
              box(title = 'Missing values',
                  solidHeader = TRUE,
                  div(style = 'overflow-x: auto',
                      h5('We filtered out observations and variables with >80% missing values or 
                     have missing values in the response variables. The plot below randomly selected 
                         1000 observations and checked if there were any missing values.
                         Since there were a few missing values after filtering, we also performed
                         missing value imputation based on the median values.'),
                      plotOutput('vismiss',height=800,width=800), align = "center"),
                  width=12, collapsible = TRUE)
            )
    ),
    
    tabItem(tabName = "Corr",
            fluidRow(
              box(title = 'Correlations between variables',
                  solidHeader = TRUE,
                  div(style = 'overflow-x: auto',
                      h5('After missing value imputation, we checked if the predictors were
                         correlated.'),
                      plotOutput('corrplot',height=800,width=800), align = "center"),
                  width=12, collapsible = TRUE)
            )
    ),
    
    tabItem(tabName = "ENet_d",
            fluidRow(
              box(title = 'Overview',
                  solidHeader = TRUE,
                  h5('Here we use ElasticNet to identify variables that were associated with 
                     "depressed over the past 5 or 7 days". Please allow 10-20 seconds for
                     the process to complete.'),
              width=12, collapsible = TRUE),
            
              box(title = 'Model performance using different combinations of tuning parameters',
                  solidHeader = TRUE,
                  div(style = 'overflow-x: auto',
                      h5('We performed a 25-fold cross validation to
                         identify the best-performing tuning parameters.'),
                      plotOutput('enetPlot_d',height=400,width=800), align = "center"),
                   width=12, collapsible = TRUE),
              box(title = "Best tuning parameters",
                  solidHeader = T,
                  div(style = 'overflow-x: auto',
                      h5('These are the best-performing tuning paramerters used to refit the model.'),
                      dataTableOutput('tuneDat_d'), align = "center"),
                  width=12, collapsible = TRUE),
              box(title = 'Selected features',
                  solidHeader = TRUE,
                  div(style = 'overflow-x: auto',
                      h5('These are the features selected by the final model 
                         ranked by the manitude of the beta coefficients.
                         The training and testing error were indicated in the plot title.'),
                      plotOutput('featurePlot_d',height=400,width=800), align = "center"),
                  width=12, collapsible = TRUE)
            )
    ),
    
    tabItem(tabName = "ENet_a",
            fluidRow(
              box(title = 'Overview',
                  solidHeader = TRUE,
                  h5('Here we use ElasticNet to identify variables that were associated with 
                     "anxious over the past 5 or 7 days". Please allow 10-20 seconds for
                     the process to complete.'),
                  width=12, collapsible = TRUE),
              box(title = 'Model performance using different combinations of tuning parameters',
                  solidHeader = TRUE,
                  div(style = 'overflow-x: auto',
                      h5('We performed a 25-fold cross validation to
                         identify the best-performing tuning parameters.'),
                      plotOutput('enetPlot_a',height=400,width=800), align = "center"),
                  width=12, collapsible = TRUE),
              box(title = "Best tuning parameters",
                  solidHeader = T,
                  div(style = 'overflow-x: auto',
                      h5('These are the best-performing tuning paramerters used to refit the model.'),
                      dataTableOutput('tuneDat_a'), align = "center"),
                  width=12, collapsible = TRUE),
              box(title = 'Selected features',
                  solidHeader = TRUE,
                  div(style = 'overflow-x: auto',
                      h5('These are the features selected by the final model 
                         ranked by the manitude of the beta coefficients. 
                         The training and testing error were indicated in the plot title.'),
                      plotOutput('featurePlot_a',height=400,width=800), align = "center"),
                  width=12, collapsible = TRUE)
            )
    ),
    
    tabItem(tabName = "Dict",
            fluidRow(
              box(title = tagList(shiny::icon("gear"), "Overview"),
                  width=12,solidHeader = T,collapsible = T,
                  h4("One of the challenges we had was the inconsistency 
                     of the variables in each wave because of the 
                     refinement of the Facebook survey 
                     questions. Some variables in one wave are not available
                     in another wave. Below are the variables available in
                     the wave that you have selected."),
              ),
              box(title = tagList(shiny::icon("gear"), "Data dictionary"),
                  width=12,solidHeader = T,collapsible = T,
                  div(style = 'overflow-x: auto',
                      dataTableOutput('dict'), align = "center")
              )
            )),
    
    tabItem(tabName = "Logs",
            h4("2021-Apr-28: version 0.1."),
    )
  )
)

# Put them together into a dashboardPage
dashboardPagePlus(
  header=dashboardHeader(title = "COVID19 Mental Health",
                         titleWidth = 250),
  sidebar=sidebar,
  body=body,
  skin = "purple"
)
