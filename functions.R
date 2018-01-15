############################################################
# Supporting functions file for the Innovatos 2017 demo app


############################################################



###### Load all Packages which we are going to use.
library(shinyBS)
library(sm)
library(pastecs)

library(DMwR)
library(corrplot)
library(caret)
library(xgboost)

options(scipen=100)
options(digits=2)
options(shiny.maxRequestSize=30*1024^2)










#####  Here Load data from dataset which is selected by user,Using Upload dataset tab. This function returns data.

loadData1 <- function(file1,header,sep,quote)
{
  data <- read.csv(file1,header=header,sep = sep,quote = quote)

  return(data)
}




#### Here Taking all coloumns of dataset into vector.this is used for user to selecting categorical coloumn
####from the list of coloumns of dataset which is returned by this function

all_column <- function()
{
  allcolumn <- names(data)
}


levels_coloumn <- function()
{  
  level_c <- c()
  
  for(i in 1: length(data))
{
  
    if(nlevels(as.factor(data[,i])) < 10 )
    {
      level_c <- c(level_c,names(data[i]))
    }

  }
  level_c
}

#### Here extracting coloumns from all coloumns of dataset which has integer and numeric class.

all_coloumn1 <- function()
{
  
  v2 <- c()
  for(i in 1: length(data))
  {
    
    if( class(data[,i]) == 'integer' ||class(data[,i]) == 'numeric' )
    { 
      
      v2 <-  c(v2,names(data[i]))

    }
    
  }
  v2
  
}


####.Here we plotting the graph , The first graph is a standard histogram of all the values using the Hist function . In the second graph, the
#### we compared density between levels of categorical coloumn using  sm.density.compare.


plotDensity <- function(colname,colname1)
{
  
  v2 <- c()
  for(i in 1: length(data))
  {
    
    if( class(data[,i]) == 'integer' ||class(data[,i]) == 'numeric' )
    { 
      
      v2 <-  c(v2,names(data[i]))
      
    }
    
  }
  v2 <- c(v2,yVar)
  
  data1 <- subset(copy(data),select=c(v2));
  data_value <- subset(data1,select=c(colname,colname1));
  names(data_value) <- c(colname,colname1);
  data_value <- na.omit(data_value);
  
  par(mfrow=c(2,1));
  hist(data_value[,colname1],xlab=paste("Value:",colname),main = 'Histogram',xlim=c(min(data_value[,colname1]),max(data_value[,colname1])));
  sm.density.compare(as.numeric(data_value[,colname1]),group = data_value[,colname], xlab=paste("Value:",colname),xlim=c(min(data_value[,colname1]),max(data_value[,colname1])));
  title("Density compare");
  legend("topright",levels(as.factor(data_value[,colname])), fill=c(2:(2+length(levels(as.factor(data_value[,colname]))))),cex=1);
  
  
}



#### In the graph below the number of levels of categorical coloumn are shown for this analysis case.so we will know that how many levels of categorical coloumn and their ratio.
plotCaseStats <- function(col_name)
{

  data[,col_name] <- as.factor(data[,col_name])
  out <- table(data[,col_name])
  
  linch <-  max(strwidth(out, "inch")+0.7, na.rm = TRUE)
  par(mai=c(1.02,linch,0.82,0.42))
  x <- barplot(out,horiz = TRUE,cex.names=0.9,las=1,xlab=paste("# number of observations"),xlim=c(0,max(out,na.rm=TRUE)+50),col="cornflowerblue")
  text(out+pmin((5+out*0.7),20),x,labels=round(out), col="black",cex=0.75)
  
}



### here this function gives us summary of whole data.In summary we can see min,max,range median,mean,var,standard deviation,coef. variation
createSummaryTable <- suppressWarnings(function()
{
  v1 <- c()
  for(i in 1: length(data))
  {
    
    if(class(data[,i]) == 'integer' || class(data[,i]) == 'numeric')
    { 
      
      v1 <-  c(v1,names(data[i]))
      
    }
  }
  
  
  
  data_old <- subset(data,select=v1);
  data_value <- copy(data_old[,1:ncol(data_old)]);
  
  return(t(stat.desc(data_value)))

}
)




### Here this function is used for calculating correlation between variables.
###In the correlogram  we compared the correlation of all variables in our dataset.Here for this function we pass number of variables so according to that it plotted correlogram for this number of variable.
plotCorrGram <- function(no_cols)
{
  
  # Exclude timestamp and FAIL, as they are not  numerical
  v1 <- c()
  for(i in 1: length(data))
  {
    
    if(class(data[,i]) == 'integer' || class(data[,i]) == 'numeric' )
    { 
      
      v1 <-  c(v1,names(data[i]))
      
    }
    
  }
  data_value <- subset(copy(data),select=v1);
  
  
  columns.to.keep<-names(which(colMeans(is.na(data_value)) < 0.5)) # this removes those columns with more than 50% NULLs
  data_value<-subset(data_value,select = columns.to.keep) #the columns will stay which has less than 50% NAs
  
  
if (no_cols > ncol(data_value))
 {
   no_cols <- ncol(data_value);
 }
  
  data_value <-  data_value[,1:no_cols];
  
  M <- cor(data_value,use = 'pairwise.complete.obs')
  col3 <- colorRampPalette(c("red", "white", "blue")) 
  corrplot(M, method="color",col = col3(20),tl.col="black",na.label=" ",tl.cex=0.5)
  
}




### Here for this function we need to pass data and number of varibles we want for further implementation.
###This function returns important features from dataset and it require entopy package for calculating importance of each variable .On the basis of those variables we can implement SVM ,random forest and decision tree.
findTop100Features <- function(input_data,featuresno)
{
  
  library(entropy)
  H.x <- entropy(table(input_data[,yVar]))
  v1 <- c()
  for(i in 1: length(input_data))
  {
    
    if(class(input_data[,i]) == 'integer' || class(input_data[,i]) == 'numeric')
    { 
      
      v1 <-  c(v1,names(input_data[i]))
      
    }
    
  }
  data_value <- subset(input_data,select=v1);
  mi <- apply(data_value, 2, function(col) { H.x + entropy(table(col)) - entropy(table(input_data[,yVar], col))})
  sort_vect <- sort(mi, decreasing=TRUE)
  
  sort_vect <- as.data.frame(sort_vect)
  
  d3 <- data.frame(row.names(sort_vect),sort_vect$sort_vect)
  names(d3)<-c("Variable","importance")
  d2 <- data.frame(names(sort_vect), t(sort_vect))
  importance.v <- data.frame(Variable=d3$Variable,Importance=d3$importance); 
  #v <- (importance.v[1:length(v1),1]) 
  l <- length(v1)
  if(featuresno > l)
  {
  return(importance.v[1:l,])
  }else{
    return(importance.v[1:featuresno,])
  }
}

