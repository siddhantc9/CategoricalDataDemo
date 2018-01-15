# Load required libraries (please install them with install.packages()  if missing )
library(shiny)
library(shinydashboard)
library(shinyjs)
library(data.table)
library(DT)
library(rpart)
library(rattle)
library(randomForest)
library(sampling)
library(e1071)
library(caTools)

# Include helper files
source('functions.R')

# Add URL prefix for loading additional resource, such as images
addResourcePath('resources',"www")

# Custom ShinyJS hack code to let the boxes collapse automatically
jscode <- "shinyjs.collapse = function(boxid) { $('#' + boxid).closest('.box').find('[data-widget=collapse]').click(); }";




ui <- dashboardPage(
  dashboardHeader(title = "Menu"),
  dashboardSidebar(
    
    sidebarMenu(id = "tab",
                tags$div(tags$p(" ")),
                
                # List of menu items in our app
                menuItem("Welcome", tabName = "welcome", icon = icon("home")),
                menuItem("Upload Dataset", tabName = "Upload", icon = icon("database")),
                menuItem("Data", tabName = "Data", icon = icon("database")),
                menuItem("Case Overview", tabName = "CaseOverview", icon = icon("bar-chart")),
                menuItem("EDA Summary", tabName = "ExploreSummary", icon = icon("list-ol")),
                menuItem("EDA Density", tabName = "ExploreDensity", icon = icon("bar-chart")),
                menuItem("EDA Correlation", tabName = "ExploreCorrelation", icon = icon("area-chart")),
                menuItem("Rebalancing", tabName = "Rebalance", icon = icon("pie-chart")),
                menuItem("Feature Select", tabName = "Feature", icon = icon("list-ol")),
                menuItem("Decision Tree", tabName = "RPartModel", icon = icon("leaf")),
                menuItem("RandomForest", tabName = "RFmodel", icon = icon("tree")),
                menuItem("SVM", tabName = "SVMmodel", icon = icon("expand"))
                
    )
    
  ),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jscode),
    tabItems(
      
      # Start of welcome tab
      tabItem(tabName = "welcome",
              fluidRow(
                box(title = "Welcome to the Categorical Data Processing demo", status = "primary", solidHeader = TRUE,width = 12,
                    tags$p("The demo can be used for analysis of categorical data set. This provides validation so user can choose files with extension .csv or .txt or .xlsx In this demo we have provided different functionalities like:"), 
                    tags$p(tags$p("   1.	The user can select Categorical Variable  for analysis.")),
                       tags$p("   2.	It provides Rebalancing and summary about the dataset ."),
                            tags$p("   3.	Also it helps to visualize graphs by plotting Histogram, Correlogram, Barplot  for better understanding of  the dataset , so from these  we  come to know about the correlation between different variables and density between levels of categorical value."),
                           tags$p("   4.	Different algorithms can also be applied such as Decision Tree, Random Forest And  Support Vector Machine."),
                    
                    
                    tags$p("Example-"),
                    tags$p("Consider the Breast Cancer diagnosis use case."),
                    tags$div(tags$p(" ")),
                    tags$div(align="center",
                             img(src="resources/breast_cancer_picture.jpg", align="center")
                    ),tags$div(tags$p(" ")),
                    
                     tags$p("In the sample dataset Diagnosis is categorical variable. Using this variable we have performed different functionality on dataset like Rebalancing and implemented algorithms such  as Decision Tree, Random Forest And  Support Vector Machine it is helpful for prediction and better accuracy.")
                  
                  
                    
                )
              ),
              fluidRow(
                box(title = "About the dataset", status = "primary", solidHeader = TRUE,width = 12,
                    tags$p("The demo is based on Categorical dataset. In this application there is functionality for user to upload   dataset ."),
                    tags$p("We have made this application generic so it will work for any Categorical dataset for classification.")
                    
                    
                    )
              )
      ),
      ## End of Welcome tab
      
      #start of upload tab
      tabItem(tabName = "Upload",
              fluidRow(
                box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 12,
                    tags$p("In This Section  Upload any data set and submit. After submitting the dataset will be visible in Data tab.")
                )),
              fluidRow(
                box(title = "Upload the Dataset", status = "primary", solidHeader = TRUE,width = 12,
                    #tags$div(class="header", checked=NA,
                                    box(  
                                      # Input: Select a file ----
                             fileInput("file1", "Choose CSV File",
                                       multiple = TRUE,
                                       accept = c("text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv",".data")),
                             
                             # Horizontal line ----
                             tags$hr(),
                             
                             # Input: Checkbox if file has header ----
                             checkboxInput("header", "Header", TRUE),
                             
                             # Input: Select separator ----
                             radioButtons("sep", "Separator",
                                          choices = c(Comma = ",",
                                                      Semicolon = ";",
                                                      Tab = "\t"),
                                          selected = ","),
                             
                             # Input: Select quotes ----
                             radioButtons("quote", "Quote",
                                          choices = c(None = "",
                                                      "Double Quote" = '"',
                                                      "Single Quote" = "'"),
                                          selected = '"'),
                             
                             # Horizontal line ----
                             tags$hr(),
                             
                             # Input: Select number of rows to display ----
                             radioButtons("disp", "Display",
                                          choices = c(Head = "head",
                                                      All = "all"),
                                          selected = "head"),
                             
                             actionButton('btnUpload', label="Submit", icon = icon("ok", lib = "glyphicon"))
                             
                    )
                    ,hr(htmlOutput("validation"))
                    )
                )
      ),
      
      
      
      # Begin of the Data tab.
      tabItem(tabName = "Data",
              # Output: Data file ----
              fluidRow(
                box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 12,
                    tags$p("Here you will be able to see those columns whose number of levels are minimum. You can change an Integer variable to Categorical as well as per requirement."),
                    tags$p("Select the categorical variable under analysis.")
                )),
              fluidRow(
                htmlOutput("dat"),
                box(title = "Select Column", status = "primary", solidHeader = TRUE, width = 12,
                    uiOutput("DataSelectInput"),
                    actionButton('btnCatData', label="Submit", icon = icon("ok", lib = "glyphicon"))
              )),
              fluidRow(
               box(title = "Source Data", status = "primary", solidHeader = TRUE, width = 12,
                    dataTableOutput('contents')
                )
              )
      ),
      # End of the data tab
      
      # Begin of the case overview tab
      tabItem(tabName = "CaseOverview",
              fluidRow(
                box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 12,
                    tags$p("In the graph below the number of levels of categorical variable are shown for this analysis case")
                )),
              fluidRow(
                htmlOutput("cas"),
                box(title = "Case Statistics", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("CaseStatsPlot") 
                )
              )
              
      ),
      # End of the case overview tab
      
      # Begin of the summary statistics tab.
      tabItem(tabName = "ExploreSummary",
              fluidRow(
                box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 12,
                    tags$p("Below we can see summary of whole data.")
                    )
                ),
              fluidRow(
                htmlOutput("exp"),
                box(title = "Summary Statistics", status = "primary", solidHeader = TRUE, width = 12,
                    dataTableOutput('dtSummary')
                )
              )
              
              
              ),
      # End of the summary statistics tab
      
      # Begin of the density compare tab
      tabItem(tabName = "ExploreDensity",
              fluidRow(
                box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 12,
                    tags$p("Below we explore the spread of the selected measurement. The first graph is a standard histogram of all the values. In the second graph, the
                           density between levels of categorical value are compared.")
                    )
                ),
              fluidRow(
                htmlOutput("den"),
                box(title = "Selection 2nd variable", status = "primary", solidHeader = TRUE,width = 12,
                    uiOutput("densitySelectInput2")
                )
              ),
              fluidRow(
                box(title = "Plot", status = "primary", solidHeader = TRUE,width = 12,
                    plotOutput("DensityPlot",height="750px") 
                )
              )
              
              
              ),
      # End of the density compare tab
      
      # Begin of the correlation plot tab
      tabItem(tabName = "ExploreCorrelation",
              fluidRow(
                box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 12,
                    tags$p("One of the key activities of Exploratory Data Analysis (EDA) is to investigate the correlation between variables. In the correlogram
                           below we compared the correlation all all variables in our dataset.")
                    )
                ),
              fluidRow(
                htmlOutput("cor"),
                box(title = "Number of variables", status = "primary", solidHeader = TRUE, width = 12,
                    sliderInput("intNoCorrVars","Number of variables:",min = 2,max = 600,value = 30,step = 1)
                )
              ),
              fluidRow(
                box(title = "Correlogram", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("CorrelogramPlot",height="500px") 
                )
              )
              
              
              ),
      # End of the correlation plot tab
      
      # Begin tab for SMOTE example
      tabItem(tabName = "Rebalance",
              fluidRow(
                box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 12,
                    tags$p("In the example below we will rebalance our dataset, so it has enough FAILed cases. This is done with the help of SMOTE (a Synthetic Minority Over-sampling Technique)")
                )
              ),
              fluidRow(
                htmlOutput("reb"),
                box(title = "Running SMOTE", status = "primary", solidHeader = TRUE,width = 12,
                    uiOutput("rebalanceSelectInput"),
                    actionButton('btnRunSMOTE', label="Rebalance", icon = icon("ok", lib = "glyphicon"))
                )
              ),
              fluidRow(
                box(id = "Box_SMOTE_CaseStats1",title = "Class Balance", status = "primary", solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = TRUE,
                    plotOutput("Plot_SMOTE_Old_CaseStats",height = '200px'),
                    plotOutput("Plot_SMOTE_New_CaseStats",height = '200px')
                )
              ),
              fluidRow(
                box(id = "Box_SMOTE_Dataset", title = "New Dataset", status = "primary", solidHeader = TRUE,collapsible = TRUE, collapsed = TRUE,width = 12,
                    dataTableOutput('dtSMOTEresult')
                )
              )
              
      ),
      # End of the SMOTE example tab
      
      # Begin decision tree tab
      tabItem(tabName = "RPartModel",
              fluidRow(
                box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 12,
                    tags$p("For building a very basic prediction model, we can use decision trees. The algorithm we used for this is RPart (Recursive partitioning for classification,
                           regression and survival trees.")
                    )
                ),
              fluidRow(
                htmlOutput("rpa"),
                box(title = "Running RPart", status = "primary", solidHeader = TRUE,width = 12,
                    checkboxInput("chkRPartWithSMOTE",label="First run SMOTE to balance classes",value = FALSE),
                    actionButton('btnRunRPart', label="Run RPart", icon = icon("ok", lib = "glyphicon"))
                )
              ),
              fluidRow(
                box(id = "Box_RPart_Results_plot",title = "Tree Model", status = "primary", solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = TRUE,
                    plotOutput('Plot_rpart', width='100%', height=800)
                )),
              fluidRow(
                box(id = "Box_RPart_Results_summary",title = "Results", status = "primary", solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = TRUE,
                    verbatimTextOutput("Out_rpart")
                )
                
              )
              
              ),
      # End of the decision tree tab
      
      # Begin of Random Forest modelling tab
      tabItem(tabName = "RFmodel",
              fluidRow(
                box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 12,
                    tags$p("In order to move towards a more advanced model, we use RandomForest. This is classification and regression based on a forest of trees using random inputs.
                           This example uses the Breiman and Cutler's Random Forests algorithm implementation.")
                    )
                ),
              fluidRow(
                htmlOutput("rfm"),
                box(title = "Running RandomForest", status = "primary", solidHeader = TRUE,width = 12,
                    sliderInput("intNoOfRF","Number of tree models:",min = 2,max = 3500,value = 50,step = 1,width = '300px'),
                    checkboxInput("chkRFWithSMOTE",label="First run SMOTE to balance classes",value = FALSE),
                    checkboxInput("chkRFWithImpute",label="Use RF Impute for more advanced missing value imputation",value = FALSE),
                    checkboxInput("chkRFWithTest",label="Split in Training/Test set",value = FALSE),
                    actionButton('btnRunRF', label="Run RandomForest", icon = icon("ok", lib = "glyphicon"))
                )
              ),
              fluidRow(
                box(id = "Box_RF_Results_plot",title = "RandomForest results", status = "primary", solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = TRUE,
                    plotOutput('plot_rf_varimp1', width='98%', height=600),
                    plotOutput('plot_rf_varimp2', width='98%', height=600),
                    plotOutput('plot_rf_error', width='98%', height=600)
                )),
              fluidRow(
                box(id = "Box_RF_Results_conf",title = "Confusion Matrix", status = "primary", solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = TRUE,
                    verbatimTextOutput("Out_rf")
                ))
              
              ),
      # End of the Random Forest modelling tab
      
      
      # Begin of the feature selection example tab
     tabItem(tabName = "Feature",
              fluidRow(
              box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 12,
                   tags$p("In the example below we will select the most relevant features (i.e. columns) from our dataset. This is done with a combination of XGBoost and RandomForest. The best
                          100 features are returned")
                    )
               ),
             fluidRow(
             htmlOutput("fea"),
               box(title = "Running Feature Selection", status = "primary", solidHeader = TRUE,width = 12,
                 sliderInput("intNoCorrVars1","Number of variables:",min = 2,max = 100,value = 30,step = 1),
                 actionButton('btnRunFeature', label="Run", icon = icon("ok", lib = "glyphicon"))
                )
              ),
             fluidRow(
                box(id = "Box_Feature_Table",title = "Most promising features", status = "primary", solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                    dataTableOutput("FeatureTable")
              )
             )
              
              
          ),
      # End of the feature selection tab
      
      
      # Begin of the SVM modelling tab
      tabItem(tabName = "SVMmodel",
              fluidRow(
                box(title = "What is this?", status = "primary", solidHeader = TRUE,width = 12,
                    tags$p("In order to move towards a more advanced model, we use SVM (Support Vector Machines).\n SMOTE requires numeric data only.")
                )
              ),
              fluidRow(
                htmlOutput("svm"),
                box(title = "Running SVM", status = "primary", solidHeader = TRUE,width = 12,
                    sliderInput("intNoCorrVars2","Number of variables:",min = 2,max = 100,value = 30,step = 1),
                    checkboxInput("chkSVMWithSMOTE",label="First run SMOTE to balance classes",value = FALSE),
                    checkboxInput("chkSVMWithTest",label="Split in Training/Test set",value = FALSE),
                    actionButton('btnRunSVM', label="Run SVM", icon = icon("ok", lib = "glyphicon"))
                )
              ),
              fluidRow(
                box(id = "Box_SVM_Results_conf",title = "SVM Results", status = "primary", solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                    verbatimTextOutput("Out_svm")
                ))
              
      )
      # End of the SVM modelling tab
      
      )
    # End of the dashboard body
    
    )
  )





  
