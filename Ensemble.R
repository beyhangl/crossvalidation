             ###################################This  ensemble is  optimizing  loss function##########################
			                        # on random  sampling 
RMPSE1<- function(preds, dtrain) {
  labels <- dtrain
  elab<-exp(as.numeric(labels))-1
  epreds<-exp(as.numeric(preds))-1
  err <- sqrt(mean((epreds/elab-1)^2))
  return(err)
}
RMPSE<- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  elab<-exp(as.numeric(labels))-1
  epreds<-exp(as.numeric(preds))-1
  err <- sqrt(mean((epreds/elab-1)^2))
  return(list(metric = "RMPSE", value = err))
}
 		  #for 3  different learners  minimizing  rmpse  loss function on random  sample evaluation    #
				  
train=read.csv("d:/ensemble1/trainBayes.csv",header=T,sep=",")
test=read.csv("d:/ensemble1/testBayes.csv",header=T,sep=",")
train=train[,-1]
test=test[,-1]
train$order=as.numeric(train$order)
test$order=as.numeric(test$order)
intNumeric <- function(train) modifyList(train, lapply(train[, sapply(train, is.integer)],   
                                                   as.numeric))
train=intNumeric(train)
test=intNumeric(test)
features=c(names(train)[-c(8:10)])
library(randomForest)

meanerror=c()
optimerror=c()
library(xgboost)
library(randomForest)
rounds=100
for( i in  c(1:rounds))
   {
   #get sample random  dropout which will  be used as  evaluation  for optimization
   h=sample(nrow(train),10000)
   eval=train[h,]
   #evaluation df
   evaluation<-xgb.DMatrix(data=data.matrix(train[h,features]),label=log(train$Sales+1)[h])
   #learning df
   learning<-xgb.DMatrix(data=data.matrix(train[-h,features]),label=log(train$Sales+1)[-h])
   watchlist<-list(val=evaluation,train=learning)
   ################################################# fit model 1#######################################################
   param <- list(  objective           = "reg:linear", 
						booster = "gbtree",
						#eta                 = 0.01, # 0.06, #0.01,
						max_depth           = 7, #changed from default of 8
						subsample           = 0.7, # 0.7
						colsample_bytree    = 0.7
						
					 
						#alpha = 0.0001, 
						#ambda = 1
		)
	clf <- xgb.train(   params              = param, 
							data                = learning, 
							nrounds             = 1200, #300, #280, #125, #250, # changed from 300
							verbose             = 1,
						   early.stop.round    = 30,
							watchlist           = watchlist,
							maximize            = FALSE,
							feval=RMPSE
		)
    err1=clf$bestScore		
	p1=predict(clf,data.matrix(eval[,features]))
	predtest1=predict(clf,data.matrix(test[features]))
	
	#######################################################fit model  2 ####################################################
       param <- list(  objective           = "reg:linear", 
						booster = "gbtree",
						#eta                 = 0.01, # 0.06, #0.01,
						max_depth           = 12, #changed from default of 8
						subsample           = 0.7, # 0.7
						colsample_bytree    = 0.7
						
					 
						#alpha = 0.0001, 
						#ambda = 1
		)
	clf <- xgb.train(   params              = param, 
							data                = learning, 
							nrounds             = 1200, #300, #280, #125, #250, # changed from 300
							verbose             = 1,
						   early.stop.round    = 50,
							watchlist           = watchlist,
							maximize            = FALSE,
							feval=RMPSE
		)  
	err2=clf$bestScore
	p2=predict(clf,data.matrix(eval[,features]))
	predtest2=predict(clf,data.matrix(test[features]))
	############################################## do optimization and prediction ##########################
	meanerror=c(meanerror,(err1+err2)/2)
	loss=function(w)
      {
      pred=w[1]*p1+w[2]*p2
      return(RMPSE1(pred,log(eval$Sales+1)))
	  }
	 o=optim(c(0.5,0.5),loss)
	 w1=o$par[1]
	 w2=o$par[2]
	 optimerror=c(optimerror,RMPSE1((w1*p1+w2*p2),log(eval$Sales+1)))
	 prediction=w1*(exp(predtest1)-1)+w2*(exp(predtest2)-1)
     assign(paste0("pred",i),prediction)
   
   }
   
##################################################retrieve results ##############################################   
m=matrix(,nrow=length(pred1),ncol=rounds)
for( i in c(1:rounds))
{
  m[,i]=eval(parse(text=paste0("pred",i)))
}
submission <- data.frame(Id=test$Id, Sales=apply(m,1,mean))
write.csv(submission, "d:/stores/1model.csv",row.names=FALSE)

