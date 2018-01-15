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
library(dplyr)
library(tools)
# Include helper files
source('functions.R')

# Add URL prefix for loading additional resource, such as images
addResourcePath('resources',"www")

# Custom ShinyJS hack code to let the boxes collapse automatically
jscode <- "shinyjs.collapse = function(boxid) { $('#' + boxid).closest('.box').find('[data-widget=collapse]').click(); }";

# Define variables for having the datasat globally
assign("data", NULL, envir = .GlobalEnv);
assign("yVar", NULL, envir = .GlobalEnv);


server <- function(input, output,session) {
  

 
  observe({

    
    #Using this tab user can select any dataset for analysis.validation provided here to check its format. so dataset is in only .csv,.txt . Otherwise it gives error message it is in wrong format
    
    suppressWarnings(if(input$tab == "Upload")
    {
      
      observeEvent(input$btnUpload, {
       
        ##########################################
        output$validation <- renderPrint({
          data <<- NULL;
   
        File <- input$file1        
        #catches null exception
        if (is.null(File))
          return(NULL)
        
        validate(
          need(file_ext(File$name) %in% c(
            'text/csv',
            'text/comma-separated-values',
            'text/tab-separated-values',
            'text/plain',
            'csv',
            'tsv'
          ),"Wrong File Format!!! \n Please upload a .csv ,.txt,.xlxs file"))
        data <<- loadData1(input$file1$datapath,header = input$header,sep = input$sep, quote = input$quote);
        })
        ##########################################
       
      })
      
    }
    )
    
    
    
    ## Here we can see whole data that was uploaded.if There is no any data set uploaded then it gives alert to selecting data.
    ##Also here we provided option to select categorical coloumn.backgroundly check this variable have class factor or not.Here it ask for user to confirmation for you are continue with this varible or not.
    if (input$tab == 'Data')
    {
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      if(is.null(data))
      {
        output$dat <- renderText({
          validate(
            need(data != "",  shinyjs::alert("Please select a data set")) 
          )})
      }
      if(!is.null(data))
      {
      output$contents <- renderDataTable({
      datatable(data, options = list(scrollX = TRUE,pageLength = 10))
      })
     
   ######## change require here ########
      output$DataSelectInput <- renderUI({
        dataNames <- levels_coloumn()
        selectInput("datacat", "Choose Option:",dataNames) 
       
      })
      
     
      observeEvent(input$btnCatData, {
       # yVar <<- input$datacat;
      #  data[,yVar] <<- as.factor(data[,yVar]);
     # )}
    #  values <- reactiveValues()
    # values$name <- input$datacat;
    #  observeEvent(input$BUTyes, {
      #  toggleModal(session, "modalnew", toggle = "close")
        yVar <<- input$datacat;
        #data[,yVar] <- as.factor( data[,yVar])
        
      
        if(class(data[,yVar]) == 'integer' || class(data[,yVar]) == 'numeric' )
        {
          
          shinyjs::alert("WARNING: Submitting this will convert numerical data to categorical.")
        }
        
       
        #if(nlevels( as.factor(data[,yVar])) > 4 )
       # {
         # shinyjs::alert("ERROR: Number of levels exceeded for this coloumn. Please select valid coloumn.")
          
          
          
       # }
      
        #data[,yVar] <- as.factor(data[,yVar])
      })
      
    #  observeEvent(input$BUTno, {
        #toggleModal(session, "modalnew", toggle = "close")
       # updateTextInput(session, "newName", value=values$name)
     # })
      
      #session$sendCustomMessage(type = 'resetInputValue', message =  "btnCatData")
      
      }

    }
    
    
    # Logic behind the summary statistcs tab
    
    
    
    if (input$tab == 'ExploreSummary')
    {
      if(is.null(data))
      {
        output$exp <- renderText({
          validate(
            need(data != "", shinyjs::alert("Please select a data set")) 
          )})
      }
      if(!is.null(data))
      {
        
        if(is.null(yVar))
        {
          output$exp <- renderText({
            validate(
              need(yVar != "", shinyjs::alert("Please select a categorical coloumn")) 
            )})
        }
          
          
          if(!is.null(yVar))
          {
      
      output$dtSummary = renderDataTable({
        datatable(suppressWarnings(suppressWarnings(createSummaryTable())), options = list(scrollX = TRUE,pageLength = 20))
        
      })
          }
      }
    }
    
    # Logic behind the density compare tab
    if (input$tab == 'ExploreDensity')
    {
    
      if(is.null(data))
      {
        output$den <- renderText({
          validate(
            need(data != "", shinyjs::alert("Please select a data set")) 
          )})
      }
      
      if(!is.null(data))
      {
        if(is.null(yVar))
        {
         
          output$den <- renderText({
            validate(
              need(yVar != "", shinyjs::alert("Please select a categorical coloumn")) 
            )})
        }
        
        output$densitySelectInput2 <- renderUI({
          dataNames <- all_coloumn1()
          selectInput("column", "Choose Option:",dataNames ) 
          
        })
        if(!is.null(yVar))
        {
          
          

      output$DensityPlot <- renderPlot({ suppressWarnings(plotDensity(yVar,(input$column))) });
      
      }
      }
    }
    
    
    # Logic behind the correlation tab 
    ## for plotting correlation between variables we pass number of variable to plotCorrGram.
    if (input$tab == 'ExploreCorrelation')
    {
      if(is.null(data))
      {
        output$cor <- renderText({
          validate(
            need(data != "", shinyjs::alert("Please select a data set")) 
          )})
      }
      if(!is.null(data))
      {
      output$CorrelogramPlot <- renderPlot({ 
        suppressWarnings(plotCorrGram(input$intNoCorrVars))
        
      });
      }
    }
    
    # Logic behind the case stats overview tab
    if (input$tab == 'CaseOverview')
    {
     
      if(is.null(data))
      {
        output$cas <- renderText({
          validate(
            need(data != "", shinyjs::alert("Please select a data set")) 
          )})
      }
      if(!is.null(data))
      {
        
        
        if(is.null(yVar))
        {
          output$cas <- renderText({
            validate(
              need(yVar != "", shinyjs::alert("Please select a categorical coloumn")) 
            )})
        }
        
        
        if(!is.null(yVar))
        {
        output$CaseStatsPlot <- renderPlot({ suppressWarnings(plotCaseStats(yVar)) }); 
        }
        
        
         }
       
   
    }
  
    
    
    ## logic behind rebalance tab.
    if(input$tab == 'Rebalance')  
    {
      if(is.null(data))
      {
        output$reb <- renderText({
          validate(
            need(data != "", shinyjs::alert("Please select a data set")) 
          )})
      }
    }
  })
  
 
  
  
  ### after click on this button graph shows status of observation for level of categorical coloumn after smote and befor smote.
  ###here we  are trying to balance the observation of levels of categorical varible .so there is no shortage of values from dataset for prediction using multiple algorithm . we use this functionality for random forest,decision tree and SVM.This is used for accuracy purpose.
  observeEvent(input$btnRunSMOTE, {
    if(!is.null(data))
    {
      
      
      
      if(is.null(yVar))
      {
        output$reb <- renderText({
          validate(
            need(yVar != "", shinyjs::alert("Please select a categorical coloumn")) 
          )})
      }
      
      if(!is.null(yVar))
      {
    data[,yVar] <- as.factor(data[,yVar])
    data_old <- data
    v1 <- c()
    for(i in 1: length(data_old))
    {
      
      if(class(data_old[,i]) == 'integer' || class(data_old[,i]) == 'numeric')
      { 
        
        v1 <-  c(v1,names(data_old[i]))
        
      }
    }
    
    v1 <- c(v1,yVar)
    
    
    data_old <- subset(data_old,select=v1);
    table(data[,yVar]) #Statistics before 
    names(data_old)[names(data_old) == yVar] <- "yVar"
    data_balanced <- SMOTE(form = yVar ~ ., data = data_old, perc.over = 500, perc.under = 120)
    names(data_balanced)[names(data_balanced) == "yVar"] <- yVar
    names(data_old)[names(data_old) == "yVar"] <- yVar
    output$Plot_SMOTE_Old_CaseStats <- renderPlot({ 
      out <- table(data[,yVar])
      linch <-  max(strwidth(out, "inch")+0.7, na.rm = TRUE)
      par(mai=c(1.02,linch,0.82,0.42))
      x <- barplot(out,horiz = TRUE,cex.names=0.9,las=1,xlab=paste("# of cases"),xlim=c(0,max(out,na.rm=TRUE)+50),col="cornflowerblue",main = 'Before SMOTE')
      text(out+pmin((5+out*0.7),20),x,labels=round(out), col="black",cex=0.75)
      
    })
    
    output$Plot_SMOTE_New_CaseStats <- renderPlot({ 
      
      out <- table(data_balanced[,yVar])
      linch <-  max(strwidth(out, "inch")+0.7, na.rm = TRUE)
      par(mai=c(1.02,linch,0.82,0.42))
      x <- barplot(out,horiz = TRUE,cex.names=0.9,las=1,xlab=paste("# of cases"),xlim=c(0,max(out,na.rm=TRUE)+50),col="cornflowerblue",main = 'After SMOTE')
      text(out+pmin((5+out*0.7),20),x,labels=round(out), col="black",cex=0.75)
      
    })
    
    output$dtSMOTEresult = renderDataTable({
      datatable(data_balanced, options = list(scrollX = TRUE,pageLength = 20))
    })
    
    # Call our custom box collapse hack
    js$collapse("Box_SMOTE_CaseStats1")
    js$collapse("Box_SMOTE_Dataset")
    
    
    
    
    }
    }
  })
  

  
  
  
  #### This is used for decision tree algorithm.here we used Rpart function.
  observeEvent(input$btnRunRPart, {
    if(is.null(data))
    {
      output$rpa <- renderText({
        validate(
          need(data != "", shinyjs::alert("Please select a data set")) 
        )})
    }
    # Code below implements the decision tree functionality
   if(!is.null(data))
   {
     
     if(is.null(yVar))
     {
       output$rpa <- renderText({
         validate(
           need(yVar != "", shinyjs::alert("Please select a categorical coloumn")) 
         )})
     }
     
     if(!is.null(yVar))
     {
     
     
     
     isolate({
       data[,yVar] <- as.factor(data[,yVar])
      
      if (input$chkRPartWithSMOTE == TRUE )
      {
     
        
      
        data_old <- data
        v1 <- c()
        for(i in 1: length(data_old))
        {
          
          if(class(data_old[,i]) == 'integer' || class(data_old[,i]) == 'numeric')
          { 
            
            v1 <-  c(v1,names(data_old[i]))
            
          }
        }
        
        v1 <- c(v1,yVar)
        
        
        data_old <- subset(data_old,select=v1);
        table(data[,yVar]) #Statistics before 
        names(data_old)[names(data_old) == yVar] <- "yVar"
        data_balanced <- SMOTE(form = yVar ~., data = data_old, perc.over = 500,perc.under = 120)
        names(data_balanced)[names(data_balanced) == "yVar"] <- yVar
        names(data_old)[names(data_old) == "yVar"] <- yVar
      } else {
        data_balanced <-data.table(copy(data))
      }
      
      
      # grow tree 
      names(data_balanced)[names(data_balanced) == yVar] <- "yVar"
      fit <- rpart(yVar ~ .,
                   method="class", data=data_balanced, control=rpart.control(minsplit=2))
      names(fit)[names(fit) == "yVar"] <- yVar
      names(data_balanced)[names(data_balanced) == "yVar"] <- yVar
      
      output$Out_rpart = renderPrint({
        summary(fit)
      })
      
      
      output$Plot_rpart = renderPlot({
        fancyRpartPlot(fit,main="Decision Tree",digits=10)
      })
      
    })
   }
    js$collapse("Box_RPart_Results_plot");
    js$collapse("Box_RPart_Results_summary");
   }
    
  })
  
  
  
  
  ### here is the logic for random forest algorithm here we use functionality like smote,splitting data and filling missing value.
  observeEvent(input$btnRunRF, {
    # Below code implements the RandomForest modelling functionality
    #data[,yVar] <- as.factor(data[,yVar])
    if(is.null(data))
    {
      output$rfm <- renderText({
        validate(
          need(data != "", shinyjs::alert("Please select a data set")) 
        )})
    }
    if(!is.null(data))
    {
   
      
        data[,yVar] <- as.factor(data[,yVar])
    isolate({
      
      
      if (input$chkRFWithTest == TRUE)
      {
        # Do a 75/25 split
        sample = sample.split(data[,yVar], SplitRatio = .75)
        data_train = subset(copy(data), sample == TRUE)
        data_test = subset(copy(data), sample == FALSE)
      } else {
        # Just make both sets the same
        data_train <- copy(data);
        data_test <- copy(data);
      }
      
      
      if (input$chkRFWithSMOTE == TRUE )
      {
        data_old <- data_train
       
        #######

        v1 <- c()
        for(i in 1: length(data_old))
        {
          
          if(class(data_old[,i]) == 'integer' || class(data_old[,i]) == 'numeric')
          { 
            
            v1 <-  c(v1,names(data_old[i]))
            
          }
        }
        
        v1 <- c(v1,yVar)
       

        data_old <- subset(data_old,select=v1);
        
       # print(yVar)
        table(data[,yVar]) #Statistics before 
        #print(table(data[,yVar]))
        
        names(data_old)[names(data_old) == yVar] <- "yVar"
        data_balanced <- SMOTE(form = yVar ~., data = data_old, perc.over = 500,perc.under = 120)
        names(data_balanced)[names(data_balanced) == "yVar"] <- yVar
        names(data_old)[names(data_old) == "yVar"] <- yVar
      } else {
        
        
        ####################
        data_balanced <- data_train #speeding up by turning into data.table
        #data_old <- data_train
        
        #######
        
        v1 <- c()
        for(i in 1: length(data_balanced))
        {
          
          if(class(data_balanced[,i]) == 'integer' || class(data_balanced[,i]) == 'numeric')
          { 
            
            v1 <-  c(v1,names(data_balanced[i]))
            
          }
        }
        
        v1 <- c(v1,yVar)
        
        
        data_balanced <- subset(data_balanced,select=v1);
        #####################
        
        
        
      }
      
      columns.to.keep<-names(which(colMeans(is.na(data_balanced)) < 0.5)) # this removes those columns with more than 50% NULLs
      data_balanced<-subset(data_balanced,select = columns.to.keep) #the columns will stay which has less than 50% NAs
      

      if (input$chkRFWithImpute == TRUE )
      {
        # Since RandomForest cannot handle missing values, we will try to impute them
        names(data_balanced)[names(data_balanced) == yVar] <- "yVar"
        test1.nona <- data_balanced[ , colSums(is.na(data_balanced)) != 0]
        
        if(ncol(test1.nona) > 0)
        {
        data.imputed <- rfImpute(yVar ~ ., data=data_balanced, iter=2, ntree=30)
        }else{
          data.imputed <- data_balanced;
        }
        names(data_balanced)[names(data_balanced) == "yVar"] <- yVar
        names(data.imputed)[names(data.imputed) == "yVar"] <- yVar
        
        
      } else {
        
        # The very cheap version of setting the missing values to something very high
        data.imputed <- data_balanced;
        data.imputed[is.na(data.imputed)]<--9999 #just some random number that never happened in the data
      }
      
      #### Random Forest
      names(data.imputed)[names(data.imputed) == yVar] <- "yVar"
      
      fit <- randomForest(yVar ~ .,
                          data=data.imputed, 
                          importance=TRUE, 
                          ntree=input$intNoOfRF)
      names(data.imputed)[names(data.imputed) == "yVar"] <- yVar
      names(fit)[names(fit) == "yVar"] <- yVar
      
      output$plot_rf_varimp1 = renderPlot({
        varImpPlot(fit,type=1,main = "Variable Importance")
      })
      
      output$plot_rf_varimp2 = renderPlot({
        varImpPlot(fit,type=2,main = "Variable Importance")
      })        
      
      output$plot_rf_error = renderPlot({
        plot(fit,main = "Error")
      })
      
      
      test_data <- data_test
      test_result <- test_data[,yVar] # Make sure we have the results seaprate
      
      pred_data <- predict(object = fit, newdata = test_data)
      
      xtab <- table(pred_data, test_result)
      
      output$Out_rf = renderPrint({
        confusionMatrix(xtab)
      })
      
      
    })
    
  }

    js$collapse("Box_RF_Results_plot");
    js$collapse("Box_RF_Results_conf");
    
    
  })
  

  
  observeEvent(input$btnRunFeature, {
    # Code below calls the function to find the TOP100 most promising features
   
    output$FeatureTable = renderDataTable({
      if(is.null(data))
      {
        output$fea <- renderText({
          validate(
            need(data != "", shinyjs::alert("Please select a data set")) 
          )})
      }

        if(!is.null(data))
        {
          if(is.null(yVar))
          {
            output$fea <- renderText({
              validate(
                need(yVar != "", shinyjs::alert("Please select a categorical coloumn")) 
              )})
          }
          
          if(!is.null(yVar))
          {
            data[,yVar] <- as.factor(data[,yVar])
          
      #isolate({
        datatable(suppressWarnings(findTop100Features(copy(data),input$intNoCorrVars1))) 
     # })
        }
          
        }
    })
    
    
  })
  
  
  
  
  observeEvent(input$btnRunSVM, {
    # This code implements the SVM modelling functionality. it is advanced kind of algorithm .This is supervised learning model
    if(is.null(data))
    {
      output$svm <- renderText({
        validate(
          need(data != "", shinyjs::alert("Please select a data set")) 
        )})
    }
    if(!is.null(data))
    {
      if(is.null(yVar))
      {
        output$svm <- renderText({
          validate(
            need(yVar != "", shinyjs::alert("Please select a categorical coloumn")) 
          )})
      }
      
      if(!is.null(yVar))
      {
      
      data[,yVar] <- as.factor(data[,yVar])
    
      isolate({
        v1 <- c()
        data_old <- data
        for(i in 1: length(data_old))
        {
          
          if(class(data_old[,i]) == 'integer' || class(data_old[,i]) == 'numeric')
          { 
            
            v1 <-  c(v1,names(data_old[i]))
            #print(v1)
            
          }
        }
        
        v1 <- c(v1,yVar)
        data_old <- subset(data_old,select=v1);
        
        ######################
        if (input$chkSVMWithTest == TRUE)
        {
          # Do a 75/25 split.75 percent of whole data goes into training set and remaining it into testing set.Testing set used for prediction purpose.
          
          
         
      
         sample = sample.split( data_old[,yVar], SplitRatio = .75)
          data_train = subset(copy(data_old), sample == TRUE)
          data_test = subset(copy(data_old), sample == FALSE)
        } else {
          # Just make both sets the same
          data_train <- copy(data_old);
          data_test <- copy(data_old);
        }
        
       # if (input$chkSVMWithTOPFeatures == TRUE)
        #{
          top100_cols <- findTop100Features(data_train,input$intNoCorrVars2);
          
          top100_cols <- as.vector(top100_cols[,1])
        #}
        
        if (input$chkSVMWithSMOTE == TRUE )
        {
          
         
          
          
          
          #print(yVar)
          table(data[,yVar]) #Statistics before 
          #print(table(data[,yVar]))
          
          names(data_old)[names(data_old) == yVar] <- "yVar"
          data_balanced <- SMOTE(form = yVar ~., data = data_old, perc.over = 500,perc.under = 120)
          
          names(data_balanced)[names(data_balanced) == "yVar"] <- yVar
          
          names(data_old)[names(data_old) == "yVar"] <- yVar
        } else {
          data_balanced <- data_train 
          #data_balanced <- subset(data_balanced,select=-TIMESTAMP);
        }
        
       # if (input$chkSVMWithTOPFeatures == TRUE)
        #{
          data_balanced <- subset(data_balanced,select=c(top100_cols,yVar));
        #}
        
        columns.to.keep<-names(which(colMeans(is.na(data_balanced)) < 0.5)) # this removes those columns with more than 50% NULLs
        data_balanced<-subset(data_balanced,select = columns.to.keep) #the columns will stay which has less than 50% NAs
        
        
        names(data_balanced)[names(data_balanced) == yVar] <- "yVar"
        test1.nona <- data_balanced[ , colSums(is.na(data_balanced)) != 0]
        
        if(ncol(test1.nona) > 0)
        {
          data_balanced <- rfImpute(yVar ~ ., data=data_balanced, iter=2, ntree=30)
        }else{
          data_balanced <- data_balanced;
        } 
        
        names(data_balanced)[names(data_balanced) == "yVar"] <- yVar
        
        nzv <- nearZeroVar(data_balanced)
        if(length(nzv) > 0)
        {
          data_balanced <- data_balanced[,-nzv]
        }
        
        data_balanced[is.na(data_balanced)]<--9999
        names(data_balanced)[names(data_balanced) == yVar] <- "yVar"
        xData <- subset(data_balanced,select=-yVar);
        names(data_balanced)[names(data_balanced) == "yVar"] <- yVar
        #names(xData)[names(xData) == "yVar"] <- yVar
        yData <- data_balanced[,yVar];
      
        fit.svm<-svm(x = xData,y= yData)
        
        
        # Get our test set from the original split
        test_data <- data_test
        test_result <- test_data[,yVar]; # Make sure we have the results seaprate - as some functions like it separate
        
        # Remove the target and timestamp
        
        names(test_data)[names(test_data) == yVar] <- "yVar"
        test_data <- subset(test_data,select=-yVar);
        names(test_data)[names(test_data) == "yVar"] <- yVar
        #test_data <- subset(test_data,select=-FAIL)
        
        # NA's are not desired, as it hinders prediction.
        test_data[is.na(test_data)]<--9999 #just some random number that never happened in the data
        
       # if (input$chkSVMWithTOPFeatures == TRUE)
        #{
          test_data<-subset(test_data,select = c(top100_cols));
       # }
        
        test_data<-subset(test_data,select = setdiff(columns.to.keep,yVar)) #the columns will stay which has less than 50% NAs
        if(length(nzv) > 0)
        {
          test_data <- test_data[,-nzv]
        }
        
        # Do the actual prediction with our previously trained model
        pred_data <- predict(object = fit.svm, newdata = test_data)
        
        xtab <- table(pred_data, test_result)
        data <- data
        # Write results back to the app
        output$Out_svm = renderPrint({
          isolate({
            cat("Started with options:\r\n")
            cat("* SMOTE : ", input$chkSVMWithSMOTE,"\r\n")
            #cat("* TOP100 Features : ",input$chkSVMWithTOPFeatures,"\r\n")
            cat("* TEST/TRAINING SET : ",input$chkSVMWithTest,"\r\n")
            cat("---------------------------------------------------------\r\n")
            confusionMatrix(xtab)
          })
        })
        ####################
        
        
        
 
        })
    }
      }
    })
}
