library(h2o)
library(Metrics)
h2o.init(nthreads=-1,max_mem_size='55G')  
train<-h2o.uploadFile("/root/Scripts/stackedTrain1.csv",destination_frame = "train.hex")
predictors<-c("Agencia_ID","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID",
              "mean_client"     ,  "mean_prod"      ,   "mean_ruta"     ,   "mean_canal"    ,    "mean_agencia"  )
#sample train
nrows=dim(train)[1]
sample=round(20/100*nrows)
iterations=3
cv_error=c()
for( i in 1:iterations){
  idx=sample(1:nrows,sample)
  idx=sort(idx)
  tr=train[-idx,]
  eval=train[idx,]
  ############fit model
  g<-h2o.gbm(training_frame = tr,x=predictors,y="Demanda_uni_equil",model_id="gbm1",ntrees = 50,learn_rate = 0.3)
  pred=h2o.predict(g,eval)
  predicted=as.numeric(as.data.frame(pred[,1])$predict)
  predicted[predicted<0]=0
  ground_truth= as.numeric(as.data.frame(eval)$Demanda_uni_equil)
  cv_error=c(cv_error,rmsle(ground_truth,predicted))
  
}
print(cv_error)

