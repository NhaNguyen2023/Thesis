# source("classification/sampling.r")
# library("DMwR") #smote sampling
# options(error=recover)
# 
# independentFeatures <- c("con_mental", "con_physical", "con_temporal", "con_performance", "con_frustration",  "con_effort")
# targetFeature = "mental_workload_recode1" 
#  
# myvars <- c(targetFeature, independentFeatures)
# nasaTrain <- nasaTrain[myvars]
# nasaTrain[myvars] <- sapply(mydata[myvars], as.numeric)
# nasaTrain[targetFeature] <- sapply(mydata[targetFeature], as.integer)
# 
# form <- as.formula(paste(targetFeature, "~ ."))
# trainset <- SMOTE(form, data= nasaTrain)
# nasaSmoteTrain <- SamplingOnTrainingSet(nasaTrain, targetFeature, independentFeatures, "smote")
# table(nasaSmoteTrain)

# library(ROSE)
# rose_train <- ROSE(mental_workload_recode ~ ., data=nasaTrain, N=155, seed=2713)$data  #require response variable has 2 levels                      
# table(rose_train$mental_workload_recode)


#grid of plots 2x2
par(mfrow=c(1,1))

trainedModelList <- list()

nasa <- read_csv("~/Documents/03_Dissertation/NHA_dataset_NASAquestionaire.csv")
wp <- read_csv("~/Documents/03_Dissertation/NHA_dataset_WPquestionaire.csv")
efs <- read_csv("~/Documents/03_Dissertation/NHA_dataset_LUCAquestionaire.csv")
nasa$scale <- "nasa"
wp$scale <- "wp"
efs$scale <- "efs"

for (featureSet in 6:10){
  mydata <- NULL
  mydata <- read.csv("~/Documents/03_Dissertation/NHA_dataset_NASAquestionaire.csv", stringsAsFactors=FALSE)	
  myvars <- NULL
  independentFeatures <- NULL
  independentFeaturesStr <- NULL
  targetFeature = "mental_workload"
  
  if (featureSet == 1) {
    print(featureSet)
    independentFeatures <- c("NASA")
    independentFeaturesStr <- paste("NASA")
  }
  else if (featureSet == 2) {
    print(featureSet)
    independentFeatures <- c("WP")
    independentFeaturesStr <- paste("WP")
  }
  else if (featureSet == 3) {
    print(featureSet)
    independentFeatures <- c("EFS")
    independentFeaturesStr <- paste("EFS")
  }
  else if (featureSet == 4) {
    print(featureSet)
    independentFeatures <- c("NASA", "WP","EFS")
    independentFeaturesStr <- paste("NASA", "WP", "EFS", sep = "+")
  }
}

myvars <- c(targetFeature, independentFeatures)
mydata <- mydata[myvars]
mydata[myvars] <- sapply(mydata[myvars], as.numeric)
mydata[targetFeature] <- sapply(mydata[targetFeature], as.integer)

#===========printing summary statistics as box plots of multiple variables grouped by another variable
if (FALSE){
  MinMeanSEMMax <- function(x) {
    v <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
    names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
    v
  }
  
  taskNum <- 0
  filteredByInterface <- mydata[ which(mydata$InterfaceVersion == taskNum), ]
  
  x<- filteredByInterface$TaskNumber
  nasa<-filteredByInterface$NASA
  wp<-filteredByInterface$WP
  efs<-filteredByInterface$EFS
  df<- (as.data.frame(cbind(x,nasa,wp,efs)))
  dfmelt<-melt(df, measure.vars = 2:4)    
  
  g0 <- ggplot(dfmelt, aes(factor(x), y=value, fill=factor(variable))) +
    stat_summary(fun.data=MinMeanSEMMax, geom="boxplot", position=position_dodge()
    ) + labs(x="task") 
  
  
  taskNum <- 1
  filteredByInterface <- mydata[ which(mydata$InterfaceVersion == taskNum), ]
  
  x<- filteredByInterface$TaskNumber
  nasa<-filteredByInterface$NASA
  wp<-filteredByInterface$WP
  efs<-filteredByInterface$EFS
  df<- (as.data.frame(cbind(x,nasa,wp,efs)))
  dfmelt<-melt(df, measure.vars = 2:4)    
  
  g1 <- ggplot(dfmelt, aes(factor(x), y=value, fill=factor(variable))) +
    stat_summary(fun.data=MinMeanSEMMax, geom="boxplot", position=position_dodge()
    ) + labs(x="task") 
  
  grid.arrange(g0, g1, ncol=2)
}

#==================== prepare training and test sets
if (TRUE){
  trainingSetPercentage =70
  testSetPercentage = 100 - trainingSetPercentage
  
  lev <- c(8) #selection of sample methods
  for (sampleMethod in unique(lev)){
    
    x <- NULL
    trainset <- NULL
    testset  <- NULL
    sampleMethodLabel <- NULL
    
    if (sampleMethod==1) {
      #==================================== prepare dataset (training) with STRATIFIED SAMPLING (on full dataset)
      x <- stratifiedSampling(mydata, targetFeature, trainingSetPercentage)
      trainset = x[[1]]
      testset = x[[2]]
    }
    else if (sampleMethod==6) {
      #==================================== prepare dataset (training) with SMOTE SAMPLING (on full dataset)
      x <- smoteSamplingOnFullDataset(mydata, targetFeature, trainingSetPercentage, independentFeatures)
      trainset = x[[1]]
      testset = x[[2]]
    }
    else if (sampleMethod==7) {
      #==================================== prepare dataset (training) with SMOTE SAMPLING (on training stratified dataset)
      x <- stratifiedSampling(mydata, targetFeature, trainingSetPercentage)
      testset = x[[2]]
      x <- smoteSamplingOnTrainingSet(x[[1]], targetFeature, independentFeatures)
      trainset = x[[1]]
    }
    else if (sampleMethod==9) {
      #==================================== prepare dataset (training) withSTRATIFIED (with caret createFolds)
      mydata[,c(targetFeature)]<- factor(mydata[,c(targetFeature)])
      names(mydata)[names(mydata) == "Class"] <- targetFeature
      
      #by default the createFolds function applies stratification
      numOfFolds <- 10
      cvIndex <- createFolds(factor(mydata$FinalPerformanceClass), k= numOfFolds, returnTrain=TRUE)
      str(cvIndex)		
      
      trainset <- mydata		
    }
  }
}
    
#=====================  Look at a summary of the training and test sets
if (FALSE){
  sapply(trainset,summary)   
  NROW(trainset)
  NROW(testset)
  #=======trainset
  #trainset[,c(targetFeature)] <- as.factor(trainset[,c(targetFeature)])
  table(trainset[,c(targetFeature)] )
  #=======testset
  table(testset[,c(targetFeature)] )
  print(hist(as.numeric(trainset[,c(targetFeature)])))
  print(hist(as.numeric(testset[,c(targetFeature)])))
}

#=======================================================================  prepare training scheme
#Train/Test split: if you have a lot of data and determine you need a lot of data to build accurate models
#Cross Validation: 5 folds or 10 folds provide a commonly used tradeoff of speed of compute time and generalize error estimate.
#Repeated Cross Validation: 5- or 10-fold cross validation and 3 or more repeats to give a more robust estimate, only if you have a small dataset and can afford the time.
if (TRUE){
  methodPar <- "repeatedcv" #, "repeatedcv" #or "cv" or "LOOCV", or "oob" 
  #note: "oob" (out of bag is just for rf, treebag, cforest, bagEarth, bagEarthGCV, bagFDA, bagFDAGCV, parRF)
  numberPar <- 10 #folds
  repeatsPar <- 3
  withProb <- FALSE #for the ROC test metric
  verboseStatus = TRUE
  
  control <- trainControl(
    index = cvIndex,
    method=methodPar 
    #number= numberPar,
    #repeats= repeatsPar, 
    #classProbs = withProb,
    #returnResamp='none'
  )
  
  randomSeed = 34567
  
  #test metrics
  #Classification:
  #Accuracy: x correct divided by y total instances. Easy to understand and widely used.
  #Kappa: easily understood as accuracy that takes the base distribution of classes into account.
  #Regression:
  #RMSE: root mean squared error. Again, easy to understand and widely used.
  #Rsquared: the goodness of fit or coefficient of determination.
  
  testMetric <- "Accuracy" #"Accuracy" #"ROC" #"Kappa"
  tuneLength <- 15
  #==================================================================  MODEL BUILDING
  #=============decision regression tree models with caret package (it uses the information gain for splits)
  set.seed(randomSeed)
  modelTrained <- train(FinalPerformanceClass ~ ., 
                        data= trainset,
                        method = "rpart", 
                        metric= testMetric, 
                        trControl = control, 
                        preProcess = c("center","scale"), 
                        tuneLength = tuneLength,  
                        parms=list(split='information')
  )
  print(plot(modelTrained))
  key <- NULL
  #key <- paste(modelTrained$method, sampleMethod, independentFeaturesStr, sep = "_", collapse = NULL)
  key <- paste(modelTrained$method, "Info", "(", independentFeaturesStr, ")", sep = "", collapse = NULL)
  trainedModelList[[key]] <- modelTrained
  
  #===================== comparisons of tested models through accuracy testing
  if (FALSE){
    
    for (m in trainedModelList){
      #print(m)
      v <- validation(m, testset, targetFeature)
      ConfusionMatrix <- v[2]
      overall <- v$overall
      accuracy <- round(as.numeric(overall['Accuracy']),3)
      kappa <- round(as.numeric(overall['Kappa']),3)
      modelName <- m$method
      
      trainsetLength <- NROW(trainset)
      row <- paste(" ", featureSet, "&", trainsetLength, "\t&", format(round(accuracy, 2), nsmall = 2), "&", format(round(kappa, 2), nsmall = 2),  "&", sampleMethod,   "&", modelName , "&",  independentFeaturesStr, "\n")
      cat(row)
    } 
}