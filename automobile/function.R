# initial 環境
inicathaymodel<-function(){
  if (!require("h2o")) install.packages("h2o")
  library('h2o')
  if (!require("h2oEnsemble")) install.packages("h2oEnsemble")
  library('h2oEnsemble')
  if (!require("readr")) install.packages("readr")
  library('readr')
}


# 車體險模型資料處理 批次
batchGroup.cardataprocess <- function(dataset){
  print(paste(Sys.time(),"開始車體險模型資料處理"))
  colnames(dataset)<-toupper(names(dataset))
  DM_result=read.csv("/home/9400200/public/automobile/model/automobile_model_datatype.csv")
  dataset<-autoTypeConv(dataset,DM_result$COLNAME,DM_result$TYPE)
  # 剔除缺少重要變數資料
  missdata=Reduce(union,list(which(is.na(dataset$INS_MARRIAGE)),
                             which(is.na(dataset$APC_RELATION_INS)),
                             which(is.na(dataset$APC_SEX))))
  if(length(missdata)>0){
    print(paste(Sys.time(),"部份資料因缺少INS_MARRIAGE、APC_RELATION_INS、APC_SEX等重要欄位，將進行剔除"))
    write.table(dataset[missdata,'CONTRACT_NO'], file = "/home/9400200/public/automobile/MISSIMPORTANCE.TXT",row.names = F,sep = ",")
    print(paste(Sys.time(),"缺少資料的契約編號已輸出 MISSIMPORTANCE.TXT 檔"))
    dataset=dataset[-missdata,]
    print(paste(Sys.time(),"移除缺少重要變數資料，占",round(length(missdata)/nrow(dataset),3),"%"))
  }
  # 部分資料補值
  print(paste(Sys.time(),"資料遺失值處理"))
  dataset$IS_ADD_OUTFIT[is.na(dataset$IS_ADD_OUTFIT)]<-0
  return(dataset)
}


# 批次 車體險核保模型
batchGroup.carmodel<-function(dataset){
  # 跑batch腳本
  print(paste(Sys.time(),"開始模型運算"))
  print(paste(Sys.time(),"進行資料分群處理"))
  dataset[,'NO']=1:nrow(dataset) # 標序列
  d1=subset(dataset,dataset$IS_CONTINUE_POLICY=='0' & dataset$VEHICLE_KIND_NO %in% c('03','07'))
  d2=subset(dataset,dataset$IS_CONTINUE_POLICY=='1' & dataset$VEHICLE_KIND_NO %in% c('03','07'))
  d3=subset(dataset,dataset$IS_CONTINUE_POLICY=='0' & !(dataset$VEHICLE_KIND_NO %in% c('03','07')))
  d4=subset(dataset,dataset$IS_CONTINUE_POLICY=='1' & !(dataset$VEHICLE_KIND_NO %in% c('03','07')))
  print(paste(Sys.time(),'第一群有',dim(d1)[1],'筆資料'))
  print(paste(Sys.time(),'第二群有',dim(d2)[1],'筆資料'))
  print(paste(Sys.time(),'第三群有',dim(d3)[1],'筆資料'))
  print(paste(Sys.time(),'第四群有',dim(d4)[1],'筆資料'))
  print(paste(Sys.time(),'開始載入模型檔'))
  # 載入模型檔
  model_group1 <- h2o.load_ensemble(path = "/home/9400200/public/automobile/model/h2o-ensemble-model-group1")
  model_group2 <- h2o.load_ensemble(path = "/home/9400200/public/automobile/model/h2o-ensemble-model-group2")
  model_group3 <- h2o.load_ensemble(path = "/home/9400200/public/automobile/model/h2o-ensemble-model-group3")
  model_group4 <- h2o.load_ensemble(path = "/home/9400200/public/automobile/model/h2o-ensemble-model-group4")
  print(paste(Sys.time(),'篩選各群所需欄位'))
  d1=d1[,which(colnames(d1) %in% c(model_group1$x,'NO'))]
  d2=d2[,which(colnames(d2) %in% c(model_group2$x,'NO'))]
  d3=d3[,which(colnames(d3) %in% c(model_group3$x,'NO'))]
  d4=d4[,which(colnames(d4) %in% c(model_group4$x,'NO'))]
  # 進行預測
  result=NULL 
  data_no=NULL
  for(i in 1:4){
    print(paste(Sys.time(),'進行第',i,'群預測'))
    tmp=get(paste0('d',i))
    # 保留續列資訊
    data_no=c(data_no,tmp[,ncol(tmp)])
    # 排除序列欄位
    test=tmp[,-ncol(tmp)]
    if(nrow(test)!=0){
      test=as.h2o(test)
      perf_t <- predict(get(paste0('model_group',i)), newdata = test)
      result=rbind(result,as.data.frame(perf_t$pred))    
    }
  }
  dim(result)
  result=cbind(result,data_no)
  # 根據序列排回 
  result=result[order(result$data_no),] 
  print(paste(Sys.time(),'模型預測完成'))
  return(data.frame(PRED=result$predict,RISK_PROB=result$p1))
}



# 單筆 車體險核保模型
one.carmodel<-function(dataset,script){
  # 跑one腳本
  print(paste(Sys.time(),"開始模型運算"))
  switch(script,
         'group1'={
           print(paste(Sys.time(),"資料為第 1 群"))
           model_one <- h2o.load_ensemble(path = "/home/9400200/public/automobile/model/h2o-ensemble-model-group1")
         },
         'group2'={
           print(paste(Sys.time(),"資料為第 2 群"))
           model_one <- h2o.load_ensemble(path = "/home/9400200/public/automobile/model/h2o-ensemble-model-group2")
         },
         'group3'={
           print(paste(Sys.time(),"資料為第 3 群"))
           model_one <- h2o.load_ensemble(path = "/home/9400200/public/automobile/model/h2o-ensemble-model-group3")
         },
         'group4'={
           print(paste(Sys.time(),"資料為第 4 群"))
           model_one <- h2o.load_ensemble(path = "/home/9400200/public/automobile/model/h2o-ensemble-model-group4")
         },
{
  print(paste(Sys.time(),"發生未預期錯誤"))
  break
}
  )
dataset=dataset[,which(colnames(dataset) %in% c(model_one$x))]
test=as.h2o(dataset)
print(paste(Sys.time(),"開始模型預測"))
perf_t <- predict(model_one, newdata = test)
result <- as.data.frame(perf_t$pred)
return(data.frame(PRED=result$predict,RISK_PROB=result$p1))
}








