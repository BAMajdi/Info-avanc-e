library(shiny)
library(shinydashboard)
library(shinythemes)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(corrplot)
library(dashboardthemes)
library(caret)
library(ROCR)
library(pROC)
library(rpart)
library(rpart.plot)
library(randomForest)
library(nnet)
library(NeuralNetTools)
library(e1071)
library(rsconnect)



shinyUI(dashboardPage(
    
    dashboardHeader(title="Heart Failure Prediction",titleWidth = 400
    ),
    
    
    dashboardSidebar(sidebarMenu(
        id= "tabs" ,
        
        
        menuItem("Import dataset",tabName = "Import",icon=icon("database")
        ),
        
        
        
        menuItem("Descrptive Analysis",tabName = "an" ,icon=icon("area-chart "),
                 menuSubItem("Analyse univariee",tabName = "Analyseunivariee",icon=icon("sliders")),
                 menuSubItem("Analyse bivariee",tabName = "Analysebivariee",icon=icon("line-chart")
                 )
                 
        ),
        menuItem("Machine learning",tabName = "Machinelearning" ,icon=icon("area-chart ")
        )
        
        
        
        
        
    )),
    
    dashboardBody(
        shinyDashboardThemes(theme = "blue_gradient"),
        style = "max-height: 90vh; overflow-y: auto;", 
        tabItems(  tabItem(tabName = "Import",fillPage(),
                           tabsetPanel(tabPanel("Data",tabName="sens",
                                                
                                                fluidRow(
                                                    box(style="color:white",
                                                        width = 3,
                                                        title = "Download manager",
                                                        helpText(tags$b("Please uplaod .xlsx")),
                                                        tags$hr(),
                                                        fileInput('csv_file1', 'Choose file to upload',
                                                                  accept = c(
                                                                      'text/csv',
                                                                      'text/comma-separated-values',
                                                                      'text/tab-separated-values',
                                                                      'text/plain',
                                                                      '.xlsx',
                                                                      '.csv'
                                                                  )
                                                        )
                                                        
                                                        
                                                        
                                                        
                                                        
                                                    ),
                                                    box(width = 9,
                                                        title = "Data",
                                                        DT::dataTableOutput("filetable")
                                                    ))
                           )
                           
                           
                           
                           
                           )),
                   
                   tabItem("Analyseunivariee",
                           tabsetPanel( tabPanel("Variables Qualitatives",
                                                 sidebarPanel(
                                                     uiOutput("quali")
                                                     
                                                 ),
                                                 mainPanel(
                                                     plotOutput("plot1")
                                                     
                                                     
                                                 )
                           ),
                           tabPanel("Variables Quantitatives", 
                                    sidebarPanel(
                                        uiOutput("quanti")
                                    ),
                                    mainPanel(
                                        plotlyOutput("plot2"),
                                        h4("Resume Statistique"),
                                        textOutput("statdes")
                                    )
                           ))),
                   
                   tabItem("Analysebivariee",
                           tabsetPanel( tabPanel("DEATH_EVENT~ var.quantitative",
                                                 sidebarPanel(
                                                     uiOutput("quanti1")
                                                 ),
                                                 mainPanel(
                                                     plotlyOutput("boxplot1"),
                                                     h5("t.test"),
                                                     tableOutput("test1")
                                                 )
                                                 
                           ),
                           tabPanel("DEATH_EVENT ~ var.qualitative",
                                    sidebarPanel(
                                        uiOutput("quali1")
                                    ),
                                    mainPanel(
                                        plotOutput("plot3"),
                                        h5("fisher.test"),
                                        tableOutput("test2"),
                                        h6("khi2.test",col="red"),
                                        tableOutput("test3")
                                        
                                    )
                           ),
                           
                           tabPanel("Matrice de correlation",
                                    mainPanel(
                                        box(title = "Matrice de correlation quanti",width = 700,
                                            height = 750,status = "primary", solidHeader = TRUE,
                                            plotOutput("corquanti",width = 700,height = 700)),
                                        box(title = "Matrice de correlation quali ",width = 700,
                                            height = 750,status = "primary", solidHeader = TRUE,
                                            plotOutput("corquali",width = 700,height = 700)))
                           ))
                           
                   ),
                   tabItem("Machinelearning",
                           tabsetPanel(tabPanel("Logistic REGRESSION",
                                                mainPanel(
                                                    plotOutput("boxplot5"),
                                                    h5("indice de perfermance"),
                                                    textOutput("auc1"),
                                                    verbatimTextOutput("mat1")
                                                    
                                                )),
                                       tabPanel("Decision Tree",
                                                mainPanel(
                                                    plotOutput("plot6"),
                                                    plotOutput("plot7"),
                                                    h6("indice de perfermance"),
                                                    textOutput("auc2")
                                                    ,
                                                    verbatimTextOutput("mat2")
                                                )),
                                       tabPanel("Random Forest",
                                                mainPanel(
                                                    plotOutput("plot8"),
                                                    h5("indice de perfermance"),
                                                    textOutput("auc3"),
                                                    verbatimTextOutput("mat3")
                                                )),
                                       tabPanel("Neural Network",
                                                mainPanel(
                                                    plotOutput("plot15"),
                                                    h5("indice de perfermance"),
                                                    textOutput("auc4"),
                                                    verbatimTextOutput("mat4")
                                                )),
                                       tabPanel("Comparaison",
                                                mainPanel(
                                                    plotOutput("plot9"),
                                                    tableOutput("table")
                                                    
                                                ))
                           )
                   ))
        
        
    ))
)  
