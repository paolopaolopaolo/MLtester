MLtreat<-function(DF,seed=0000, treatments=NULL, class.column=NULL,ignore.columns=NULL, train.test.ratio=c(0.70,0.30),tune=FALSE){
  #Global Variables
  formula<-NULL
  DF.train<-NULL
  DF.test<-NULL
  
  #Download required classifier packages
  EnsurePackage<-function(x){
    x<-as.character(x)
    if(!require(x,character.only=TRUE)){
      install.packages(pkgs=x, repos="http://cran.r-project.org")
      require(x,character.only=TRUE)
    }
    
  }
  
  PrepClassifiers<-function(){
    EnsurePackage("rpart")
    EnsurePackage("party")
    EnsurePackage("randomForest")
    EnsurePackage("party")
    EnsurePackage("e1071")
    EnsurePackage("kernlab")
    EnsurePackage("nnet")
    EnsurePackage("neuralnet")
    EnsurePackage("RSNNS")
    EnsurePackage("ROCR")
    EnsurePackage("stringr")
  }
  
  #Load Packages
  PrepClassifiers()
  print("Classifiers loaded! Run the function again!")
  
  #check parameters against global variables
  if (is.null(treatments)){
    print("Specify the treatment(s) you wish to evaluate!")
  }
  else if(length(treatments)>1){
    print("System does not yet support multiple treatments at once.")
    print("Run each treatment separately!")
    
  }
  
  
  #Divide DF to testing and training sets
  set.seed(seed)
  ind<-sample(2,nrow(DF),replace=TRUE,prob=train.test.ratio)
  DF.train<-DF[ind==1,]
  DF.test<-DF[ind==2,]
  
  #Set Formula... this section is kinda redundant
  if (is.numeric(class.column)||is.integer(class.column)){
    classes<-as.character(colnames(DF)[class.column])
    DFhold<-DF[,-class.column]
    if (!is.null(ignore.columns)) {
      if(is.numeric(ignore.columns)||is.integer(ignore.columns)) DFhold<-DFhold[,-ignore.columns]
      else if(is.character(ignore.columns)) DFhold<-DFhold[,colnames(DFhold)!=ignore.columns]
      else print("ignore.columns must be a numeric,integer,or character vector")
    }
    vars<-NULL
    for (columnIndex in 1:ncol(DFhold)) vars<-paste(vars,colnames(DFhold)[columnIndex],sep="+")
    formula<-paste(classes,vars)
    str_sub(formula,start=nchar(classes)+1,end=nchar(classes)+2)<-"~"
    formula<-as.formula(formula)
  }
  else if (is.character(class.column)){
    classes<-class.column
    DFhold<-DF[,colnames(DF)!=classes]
    if (!is.null(ignore.columns))  {
      if(is.numeric(ignore.columns)||is.integer(ignore.columns)) DFhold<-DFhold[,-ignore.columns]
      else if(is.character(ignore.columns)) DFhold<-DFhold[,colnames(DFhold)!=ignore.columns]
      else print("ignore.columns must be a numeric,integer,or character vector")
    }
    vars<-NULL
    for (columnIndex in 1:ncol(DFhold)) vars<-paste(vars,colnames(DFhold)[columnIndex],sep="+")
    formula<-paste(classes,vars)
    str_sub(formula,start=nchar(classes)+1,end=nchar(classes)+2)<-"~"
    formula<-as.formula(formula)
  }  
  else{
    print("class.column must be character,integer or numeric vector of 1!")  
  }
  
  #Start tuning procedures, then run optimal treatment
  if(tune==TRUE){
    if(treatments=="randomForest"||treatments=="cforest"){
      DF.tuneRF<-tuneRF(DF.train[,colnames(DF.train)!=class.column],DF.train[,colnames(DF.train)==class.column],doBest=TRUE)
      DF.pred<-predict(DF.tuneRF,newdata=DF.test)
      if (is.numeric(class.column)||is.integer(class.column)) table1<-table(DF.pred, DF.test[,class.column])
      if(is.character(class.column)) table1<-table(DF.pred, DF.test[,colnames(DF.test)==class.column])
      print(table1)
      print (paste("The accuracy of this treatment is",classAgreement(table1)$diag))
      return (DF.tuneRF)
    }
  }
  
  #Run treatment of choice, sans tuning
  else{
    if(treatments=="ctree"){
      DF.ctree<-ctree(formula,data=DF.train)
      DF.pred<-predict(DF.ctree,newdata=DF.test)
      if (is.numeric(class.column)||is.integer(class.column)) table1<-table(DF.pred, DF.test[,class.column])
      if(is.character(class.column)) table1<-table(DF.pred, DF.test[,colnames(DF.test)==class.column])
      print(table1)
      print (paste("The accuracy of this treatment is",classAgreement(table1)$diag))
      return (DF.ctree)
    }
  
    else if(treatments=="cforest"){
      DF.cforest<-cforest(formula,data=DF.train)
      DF.pred<-predict(DF.cforest,newdata=DF.test)
      if (is.numeric(class.column)||is.integer(class.column)) table1<-table(DF.pred, DF.test[,class.column])
      if(is.character(class.column)) table1<-table(DF.pred, DF.test[,colnames(DF.test)==class.column])
      print(table1)
      print(paste("The accuracy of this treatment is",classAgreement(table1)$diag))
      return(DF.cforest)
    }
    else if(treatments=="randomForest"){
      DF.randomForest<-randomForest(formula,data=DF.train)
      DF.pred<-predict(DF.randomForest,newdata=DF.test)
      if (is.numeric(class.column)||is.integer(class.column)) table1<-table(DF.pred, DF.test[,class.column])
      if(is.character(class.column)) table1<-table(DF.pred, DF.test[,colnames(DF.test)==class.column])
      print(table1)
      print(paste("The accuracy of this treatment is",classAgreement(table1)$diag))
      return(DF.randomForest)
    }
  
    else if(treatments=="svm"){
      DF.svm<-svm(formula,data=DF.train)
      DF.pred<-predict(DF.svm,newdata=DF.test)
      if (is.numeric(class.column)||is.integer(class.column)) table1<-table(DF.pred, DF.test[,class.column])
      if(is.character(class.column)) table1<-table(DF.pred, DF.test[,colnames(DF.test)==class.column])
      print(table1)
      print(paste("The accuracy of this treatment is",classAgreement(table1)$diag))
      return(DF.svm)
    }
    
  
    else{
      print("That treatment is unavailable!")
      
    }
  
  }
}