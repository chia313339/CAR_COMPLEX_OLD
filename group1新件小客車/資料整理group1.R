rm(list = ls())
.rs.restartR()
library(readr)
library(cathayR)
inicathayR()
setwd("/home/cxl_etl/datastage")
dataset=read_csv('CAR_ABT_GROUP1_20180615.csv')
dim(dataset) #group1:213603*320 #group4:86477*320 #group3:33729*320

# POLICY_RESULT_CODE (自核/承保)
dataset$POLICY_RESULT_CODE=as.factor(dataset$POLICY_RESULT_CODE)

# 移除欄位:契約編號,引擎號碼
remove<- names(dataset)  %in% c('CONTRACT_NO','ENGINE_NO')
dataset<-dataset[!remove]
dim(dataset) 

# 修改欄位型態
setwd("~/")
DM_result=read.csv("data_mining_20180615.csv",fileEncoding = "BIG-5")
colnames(dataset)<-toupper(names(dataset))
dataset<-autoTypeConv(dataset,DM_result$COLNAME,DM_result$TYPE)

# 刪除全為空值的欄位
col_space=apply(dataset,2,function(x) all(x==''))
dataset=dataset[,-which(col_space=='TRUE')]
dim(dataset) # 213603*315 86477*315 33729*315

# 把欄位值都一樣的變數刪除
tmp=sapply(dataset,function(x) length(unique(x)))
length(which(tmp==1))
apply(dataset[,which(tmp==1)],2,unique)
dataset=dataset[,-which(tmp==1)]
dim(dataset) # 213603*272 86477*283 33729*240

# missing value補值 (刪完後有個變數需處理)
tmp=sapply(dataset,function(x) length(which(is.na(x))))
which(tmp>0)
round(tmp[which(tmp>0)]/nrow(dataset),5)

# 刪除變數APC_AGE,APC_MARRIAGE(約有一半都是遺失值),INS_ZIP_CODE(98%遺失值) 
length(which(dataset$INS_ZIP_CODE==''))/nrow(dataset)
dataset=dataset[ ,-which(colnames(dataset) %in% c('APC_MARRIAGE','APC_AGE','INS_ZIP_CODE'))]
#刪除類別變數超過35類者
dataset=dataset[,-which(colnames(dataset) %in% c('INTRDUCE_DIV_NO','COUNSEL_DIV_NO','AGENT_DIV_NO'))]
dim(dataset) # 213603*266 86477*277 33729*277

# 遺失值少且重要的變數->有遺失值則整筆刪除
u=Reduce(union,list(which(is.na(dataset$INS_MARRIAGE)),
                    which(is.na(dataset$APC_RELATION_INS)),
                    which(dataset$APC_SEX=='')))
length(u)/nrow(dataset)
dataset=dataset[-u,] 
dim(dataset) # 213027*266 86151*277 33653*234

# 其他處理
length(which(is.na(dataset[,'IS_ADD_OUTFIT'])))/nrow(dataset)
dataset[which(is.na(dataset[,'IS_ADD_OUTFIT'])),'IS_ADD_OUTFIT']=0 #加裝配備註記遺失值補0

# INSURE:保單總理賠金額為空值的LOSS_RATE都是零,INSURE空值補0
unique(dataset[is.na(dataset$INSURE),c('LOSS_RATE','INSURE')])
dataset[which(is.na(dataset[,'INSURE'])),'INSURE']=0 #INSURE空值補0

#刪除4.5年理賠資料
colnames(dataset)[grep('FOUR',colnames(dataset))]
colnames(dataset)[grep('FIVE',colnames(dataset))]
dataset=dataset[,-grep('FOUR',colnames(dataset))]
dataset=dataset[,-grep('FIVE',colnames(dataset))]
dim(dataset) # 213027*202 86151*213 33653*178

dataset=dataset[,-which(colnames(dataset)=='INSURE')]
dim(dataset) # 213027*201 86151*212 33653*177

# copy=dataset
