rm(list = ls())
gc()
# .rs.restartR()
library(readr)
library(cathayR)
inicathayR()
library('doParallel')
library(caret)
library(xgboost)
cl<-makeCluster(3) #建議不要超過4，自行根據硬體設備設定
registerDoParallel(cl)

setwd("~/R/ML/CAR")

# 資料讀取 
# dataset1 <- read_csv("dataset/CAR_ABT_GROUP1.csv") #group 1 新件小客車
# dataset2 <- read_csv("dataset/CAR_ABT_GROUP2.csv") #group2續保小客車
# dataset3 <- read_csv("dataset/CAR_ABT_GROUP3.csv") #group3新件非小客車
# dataset4 <- read_csv("dataset/CAR_ABT_GROUP4.csv") #group4續保非小客車
# dataset = rbind(dataset1,dataset2,dataset3,dataset4) #868170 * 320
# write.table(dataset,'CAR_ABT.csv',sep=',', row.names=F)
dataset <- read_csv("CAR_ABT.csv")

dataset = as.data.frame(dataset)
dim(dataset)  #868170*320
str(dataset)

# 找出全為空的欄位
col_space = apply(dataset,2,function(x) all(is.na(x)))
droplist1 = names(col_space[which(col_space>0)]) # "OCCU_CATCODE_INS"  "OCCU_CATCODE_APC"  "INS_SCHOOL_DEGREE"

# 全為單一值
col_uni = apply(dataset,2,function(x) all(length(unique(x))==1))
droplist2 = names(col_uni[which(col_uni>0)]) # 27個

# 缺失值
col_missing = sapply(dataset,function(x) round(sum(is.na(x))/nrow(dataset),4))
col_missing[col_missing>0] 
# 針對低缺失比欄位補值
dataset['INS_MARRIAGE'][is.na(dataset['INS_MARRIAGE'])] = names(table(dataset['INS_MARRIAGE']))[which.max(table(dataset['INS_MARRIAGE']))] 
dataset['APC_RELATION_INS'][is.na(dataset['APC_RELATION_INS'])] = names(table(dataset['APC_RELATION_INS']))[which.max(table(dataset['APC_RELATION_INS']))] 
dataset['IS_ADD_OUTFIT'][is.na(dataset['IS_ADD_OUTFIT'])] = names(table(dataset['IS_ADD_OUTFIT']))[which.max(table(dataset['IS_ADD_OUTFIT']))] 
dataset['APC_SEX'][is.na(dataset['APC_SEX'])] = names(table(dataset['APC_SEX']))[which.max(table(dataset['APC_SEX']))] 
# 重新確認缺失值
col_missing = sapply(dataset,function(x) round(sum(is.na(x))/nrow(dataset),4))
col_missing[col_missing>0] 
droplist3 = names(col_missing[col_missing>0])

# 無意義欄位
droplist4 = c('CONTRACT_NO','ENGINE_NO','INSURE','CATEGORY_CODE','POLICY_RESULT_CODE','COUNSEL_DIV_NO','AGENT_DIV_NO')

# 移除不要的欄位
remove<- names(dataset)  %in% c(droplist1,droplist2,droplist3,droplist4)
dataset<-dataset[!remove]
dim(dataset) #283
sum(is.na(dataset))


# 型態轉換
numeric_col = c('INSRNCE_AMOUNT',	'LOSS_RATE',	'DISCOUNT_PREMIUM_AGN',	'CONCLUDE_NTD_AMOUNT_AGN',	'POLICY_COUNT_AGN',	'LOSS_RATE_AGN',	'DEPRECIA_RATE')
integer_col1 = c('INSURE',	'APC_AGE',	'INS_AGE',	'THREE_INSUR_AMOUNT_C',	'FOUR_INSUR_AMOUNT_C',	'TWO_INSUR_AMOUNT_C',	'FIVE_INSUR_AMOUNT_C',	'ONE_INSUR_AMOUNT_C',	'DISCOUNT_PREMIUM',	'THREE_INSUR_AMOUNT',	'THREE_INSUR_AMOUNT_BODY',	'TWO_INSUR_AMOUNT',	'TWO_INSUR_AMOUNT_BODY',	'FOUR_INSUR_AMOUNT',	'FOUR_INSUR_AMOUNT_BODY',	'FIVE_INSUR_AMOUNT',	'FIVE_INSUR_AMOUNT_BODY',	'THREE_INSUR_AMOUNT_BODY_C',	'TWO_INSUR_AMOUNT_BODY_C',	'FOUR_INSUR_AMOUNT_BODY_C',	'FIVE_INSUR_AMOUNT_BODY_C',	'DRIVER_CONCLUDE_AMOUNT_3Y',	'DRIVER_CONCLUDE_AMOUNT_2Y',	'ONE_INSUR_AMOUNT',	'ONE_INSUR_AMOUNT_BODY',	'ONE_INSUR_AMOUNT_BODY_C',	'DRIVER_CONCLUDE_AMOUNT_1Y',	'INS_ISSUE_YEAR',	'CAR_AGE',	'FIVE_INSUR_CNT_C',	'THREE_INSUR_CNT_C',	'FOUR_INSUR_CNT_C',	'BUMP_CNT_C',	'TWO_INSUR_CNT_C',	'BODY_CLAIM_CODE',	'BUMP_CNT_BODY_C',	'ONE_INSUR_CNT_C',	'FIVE_INSUR_CNT_BODY_C',	'FOUR_INSUR_CNT_BODY_C',	'THREE_INSUR_CNT_BODY_C',	'DRIVER_TIMES_3Y',	'FOUR_INSUR_CNT',	'FIVE_INSUR_CNT',	'FOUR_INSUR_CNT_BODY',	'FIVE_INSUR_CNT_BODY',	'TWO_INSUR_CNT_BODY_C',	'THREE_INSUR_CNT',	'THREE_INSUR_CNT_BODY',	'DRIVER_TIMES_2Y',	'BUMP_CNT',	'BUMP_CNT_BODY',	'TWO_INSUR_CNT',	'TWO_INSUR_CNT_BODY',	'ONE_INSUR_CNT_BODY_C',	'DRIVER_TIMES_1Y',	'ONE_INSUR_CNT',	'ONE_INSUR_CNT_BODY',	'TWO_PARK_CNT_C',	'THREE_PARK_CNT_C',	'TWO_PARK_CNT',	'THREE_PARK_CNT',	'TWO_PARK_CNT_BODY',	'THREE_PARK_CNT_BODY',	'TWO_PARK_CNT_BODY_C',	'THREE_PARK_CNT_BODY_C',	'ONE_PARK_CNT',	'ONE_PARK_CNT_BODY',	'ONE_PARK_CNT_C',	'ONE_PARK_CNT_BODY_C',	'IS_BUMP_C',	'ONE_INSUR_AMOUNT_BURG',	'ONE_INSUR_CNT_BURG',	'TWO_INSUR_AMOUNT_BURG',	'TWO_INSUR_CNT_BURG',	'THREE_INSUR_AMOUNT_BURG',	'THREE_INSUR_CNT_BURG',	'FOUR_INSUR_AMOUNT_BURG',	'FOUR_INSUR_CNT_BURG',	'FIVE_INSUR_AMOUNT_BURG',	'FIVE_INSUR_CNT_BURG',	'ONE_PARK_CNT_BURG',	'TWO_PARK_CNT_BURG',	'THREE_PARK_CNT_BURG',	'BUMP_CNT_BURG',	'ONE_INSUR_AMOUNT_THIRD',	'ONE_INSUR_CNT_THIRD',	'TWO_INSUR_AMOUNT_THIRD',	'TWO_INSUR_CNT_THIRD',	'THREE_INSUR_AMOUNT_THIRD',	'THREE_INSUR_CNT_THIRD',	'FOUR_INSUR_AMOUNT_THIRD',	'FOUR_INSUR_CNT_THIRD',	'FIVE_INSUR_AMOUNT_THIRD',	'FIVE_INSUR_CNT_THIRD',	'ONE_PARK_CNT_THIRD',	'TWO_PARK_CNT_THIRD',	'THREE_PARK_CNT_THIRD',	'BUMP_CNT_THIRD',	'ONE_INSUR_AMOUNT_CUST',	'ONE_INSUR_CNT_CUST',	'TWO_INSUR_AMOUNT_CUST',	'TWO_INSUR_CNT_CUST',	'THREE_INSUR_AMOUNT_CUST',	'THREE_INSUR_CNT_CUST',	'FOUR_INSUR_AMOUNT_CUST',	'FOUR_INSUR_CNT_CUST',	'FIVE_INSUR_AMOUNT_CUST',	'FIVE_INSUR_CNT_CUST',	'ONE_PARK_CNT_CUST',	'TWO_PARK_CNT_CUST',	'THREE_PARK_CNT_CUST',	'BUMP_CNT_CUST',	'ONE_INSUR_AMOUNT_FORCE',	'ONE_INSUR_CNT_FORCE',	'TWO_INSUR_AMOUNT_FORCE',	'TWO_INSUR_CNT_FORCE',	'THREE_INSUR_AMOUNT_FORCE',	'THREE_INSUR_CNT_FORCE',	'FOUR_INSUR_AMOUNT_FORCE',	'FOUR_INSUR_CNT_FORCE',	'FIVE_INSUR_AMOUNT_FORCE',	'FIVE_INSUR_CNT_FORCE',	'ONE_PARK_CNT_FORCE')
integer_col2 =c('TWO_PARK_CNT_FORCE',	'THREE_PARK_CNT_FORCE',	'BUMP_CNT_FORCE',	'ONE_INSUR_AMOUNT_OTHER',	'ONE_INSUR_CNT_OTHER',	'TWO_INSUR_AMOUNT_OTHER',	'TWO_INSUR_CNT_OTHER',	'THREE_INSUR_AMOUNT_OTHER',	'THREE_INSUR_CNT_OTHER',	'FOUR_INSUR_AMOUNT_OTHER',	'FOUR_INSUR_CNT_OTHER',	'FIVE_INSUR_AMOUNT_OTHER',	'FIVE_INSUR_CNT_OTHER',	'ONE_PARK_CNT_OTHER',	'TWO_PARK_CNT_OTHER',	'THREE_PARK_CNT_OTHER',	'BUMP_CNT_OTHER',	'ONE_INSUR_AMOUNT_COM',	'ONE_INSUR_CNT_COM',	'TWO_INSUR_AMOUNT_COM',	'TWO_INSUR_CNT_COM',	'THREE_INSUR_AMOUNT_COM',	'THREE_INSUR_CNT_COM',	'FOUR_INSUR_AMOUNT_COM',	'FOUR_INSUR_CNT_COM',	'FIVE_INSUR_AMOUNT_COM',	'FIVE_INSUR_CNT_COM',	'ONE_PARK_CNT_COM',	'TWO_PARK_CNT_COM',	'THREE_PARK_CNT_COM',	'BUMP_CNT_COM',	'ONE_INSUR_AMOUNT_BURG_C',	'ONE_INSUR_CNT_BURG_C',	'TWO_INSUR_AMOUNT_BURG_C',	'TWO_INSUR_CNT_BURG_C',	'THREE_INSUR_AMOUNT_BURG_C',	'THREE_INSUR_CNT_BURG_C',	'FOUR_INSUR_AMOUNT_BURG_C',	'FOUR_INSUR_CNT_BURG_C',	'FIVE_INSUR_AMOUNT_BURG_C',	'FIVE_INSUR_CNT_BURG_C',	'ONE_PARK_CNT_BURG_C',	'TWO_PARK_CNT_BURG_C',	'THREE_PARK_CNT_BURG_C',	'BUMP_CNT_BURG_C',	'ONE_INSUR_AMOUNT_THIRD_C',	'ONE_INSUR_CNT_THIRD_C',	'TWO_INSUR_AMOUNT_THIRD_C',	'TWO_INSUR_CNT_THIRD_C',	'THREE_INSUR_AMOUNT_THIRD_C',	'THREE_INSUR_CNT_THIRD_C',	'FOUR_INSUR_AMOUNT_THIRD_C',	'FOUR_INSUR_CNT_THIRD_C',	'FIVE_INSUR_AMOUNT_THIRD_C',	'FIVE_INSUR_CNT_THIRD_C',	'ONE_PARK_CNT_THIRD_C',	'TWO_PARK_CNT_THIRD_C',	'THREE_PARK_CNT_THIRD_C',	'BUMP_CNT_THIRD_C',	'ONE_INSUR_AMOUNT_CUST_C',	'ONE_INSUR_CNT_CUST_C',	'TWO_INSUR_AMOUNT_CUST_C',	'TWO_INSUR_CNT_CUST_C',	'THREE_INSUR_AMOUNT_CUST_C',	'THREE_INSUR_CNT_CUST_C',	'FOUR_INSUR_AMOUNT_CUST_C',	'FOUR_INSUR_CNT_CUST_C',	'FIVE_INSUR_AMOUNT_CUST_C',	'FIVE_INSUR_CNT_CUST_C',	'ONE_PARK_CNT_CUST_C',	'TWO_PARK_CNT_CUST_C',	'THREE_PARK_CNT_CUST_C',	'BUMP_CNT_CUST_C',	'ONE_INSUR_AMOUNT_FORCE_C',	'ONE_INSUR_CNT_FORCE_C',	'TWO_INSUR_AMOUNT_FORCE_C',	'TWO_INSUR_CNT_FORCE_C',	'THREE_INSUR_AMOUNT_FORCE_C',	'THREE_INSUR_CNT_FORCE_C',	'FOUR_INSUR_AMOUNT_FORCE_C',	'FOUR_INSUR_CNT_FORCE_C',	'FIVE_INSUR_AMOUNT_FORCE_C',	'FIVE_INSUR_CNT_FORCE_C',	'ONE_PARK_CNT_FORCE_C',	'TWO_PARK_CNT_FORCE_C',	'THREE_PARK_CNT_FORCE_C',	'BUMP_CNT_FORCE_C',	'ONE_INSUR_AMOUNT_OTHER_C',	'ONE_INSUR_CNT_OTHER_C',	'TWO_INSUR_AMOUNT_OTHER_C',	'TWO_INSUR_CNT_OTHER_C',	'THREE_INSUR_AMOUNT_OTHER_C',	'THREE_INSUR_CNT_OTHER_C',	'FOUR_INSUR_AMOUNT_OTHER_C',	'FOUR_INSUR_CNT_OTHER_C',	'FIVE_INSUR_AMOUNT_OTHER_C',	'FIVE_INSUR_CNT_OTHER_C',	'ONE_PARK_CNT_OTHER_C',	'TWO_PARK_CNT_OTHER_C',	'THREE_PARK_CNT_OTHER_C',	'BUMP_CNT_OTHER_C',	'ONE_INSUR_AMOUNT_COM_C',	'ONE_INSUR_CNT_COM_C',	'TWO_INSUR_AMOUNT_COM_C',	'TWO_INSUR_CNT_COM_C',	'THREE_INSUR_AMOUNT_COM_C',	'THREE_INSUR_CNT_COM_C',	'FOUR_INSUR_AMOUNT_COM_C',	'FOUR_INSUR_CNT_COM_C',	'FIVE_INSUR_AMOUNT_COM_C',	'FIVE_INSUR_CNT_COM_C',	'ONE_PARK_CNT_COM_C',	'TWO_PARK_CNT_COM_C',	'THREE_PARK_CNT_COM_C',	'BUMP_CNT_COM_C',	'INS_IS_ONE_YEAR_REPAIR')
factor_col = c('OCCU_CATCODE_INS',	'OCCU_CATCODE_APC',	'INS_SCHOOL_DEGREE',	'INS_ZIP_CODE',	'APC_MARRIAGE',	'APC_RELATION_INS',	'APC_SEX',	'INS_MARRIAGE',	'INS_SEX',	'IS_SAME',	'IS_MAIN_INSURED',	'IS_ADD_OUTFIT',	'AGENT_DIV_NO',	'UNIT_NO',	'CATEGORY_CODE',	'AGENT_KIND',	'BROKER_NO',	'BODY_MAIN',	'INTRDUCE_KIND',	'IS_FORCE_DEGREE_CH',	'LIMIT_DRIVER',	'IS_NEW_CAR',	'IS_CHANGE_CARNO',	'IS_TRADE_FORCE',	'VEHICLE_KIND_NO',	'IS_BUMP',	'IS_BUMP_BODY',	'IS_BUMP_BODY_C',	'IS_UNCLEAR',	'IS_WEATHER',	'IS_HUMAN',	'IS_DEPRECIATION',	'IS_RECOVERY',	'INS_SIP',	'APC_SIP',	'INS_IS_INSUR_STRICT',	'APC_IS_INSUR_STRICT',	'INS_IS_REPAIR',	'INS_IS_MAIN_CAUSE',	'INS_IS_CLAIM_MANY',	'INS_IS_BAD_CUSTOMER',	'INS_IS_MORALS_RISK',	'INS_IS_FRAUD_GROUP',	'IS_HPRICE_CAR',	'IS_CONTINUE_POLICY',	'IS_NEW_POLICY',	'CATEGORY_KIND',	'IS_BUMP_BURG',	'IS_BUMP_THIRD',	'IS_BUMP_CUST',	'IS_BUMP_FORCE',	'IS_BUMP_OTHER',	'IS_BUMP_COM',	'IS_BUMP_BURG_C',	'IS_BUMP_THIRD_C',	'IS_BUMP_CUST_C',	'IS_BUMP_FORCE_C',	'IS_BUMP_OTHER_C',	'IS_BUMP_COM_C',	'INS_IS_TERR',	'APC_IS_TERR',	'INS_SAN',	'APC_SAN',	'INS_AM',	'APC_AM',	'INS_ML_TF',	'APC_ML_TF',	'INS_IS_DRUNK_DRIVING')
Date_col = c('START_DATETIME')
character_col = c('')

numeric_col = numeric_col[!numeric_col %in% c(droplist1,droplist2,droplist3,droplist4)]
integer_col1 = integer_col1[!integer_col1 %in% c(droplist1,droplist2,droplist3,droplist4)]
integer_col2 = integer_col2[!integer_col2 %in% c(droplist1,droplist2,droplist3,droplist4)]
factor_col = factor_col[!factor_col %in% c(droplist1,droplist2,droplist3,droplist4)]
Date_col = Date_col[!Date_col %in% c(droplist1,droplist2,droplist3,droplist4)]

dataset[numeric_col] = lapply(dataset[numeric_col], as.numeric) 
dataset[integer_col1] = lapply(dataset[integer_col1], as.integer) 
dataset[integer_col2] = lapply(dataset[integer_col2], as.integer) 
dataset[factor_col] = lapply(dataset[factor_col], as.factor) 
dataset[Date_col] = lapply(dataset[Date_col], as.Date) 
str(dataset)

# 特徵轉換
dataset['IS_NEW_CAR'] = as.integer(ifelse(dataset['IS_NEW_CAR']=='Y',1,0))
dataset['INS_SEX'] = as.integer(ifelse(dataset['INS_SEX']=='M',1,0))
dataset['APC_SEX'] = as.integer(ifelse(dataset['APC_SEX']=='M',1,0))

# 轉換函數
# label_encoder = function(vec){
#   levels = sort(unique(vec))
#   function(x){
#     match(x, levels)
#   }
# }
# LE.UNIT_NO = label_encoder(dataset[['UNIT_NO']])
# LE.VEHICLE_KIND_NO = label_encoder(dataset[['VEHICLE_KIND_NO']])
# LE.AGENT_KIND = label_encoder(dataset[['AGENT_KIND']])
# LE.BROKER_NO = label_encoder(dataset[['BROKER_NO']])
load('LE.RData')
dataset[['UNIT_NO']] = LE.UNIT_NO(dataset[['UNIT_NO']])
dataset[['VEHICLE_KIND_NO']] = LE.VEHICLE_KIND_NO(dataset[['VEHICLE_KIND_NO']])
dataset[['AGENT_KIND']] = LE.AGENT_KIND(dataset[['AGENT_KIND']])
dataset[['BROKER_NO']] = LE.BROKER_NO(dataset[['BROKER_NO']])

save(LE.UNIT_NO,LE.VEHICLE_KIND_NO,LE.AGENT_KIND,LE.BROKER_NO, file = 'LE.RData')
# load('LE.RData')

# 處理Y變數
Y = ifelse(dataset['LOSS_RATE']>0,'1','0')
dataset$LOSS_RATE = as.integer(Y)

# 切割訓練資料及測試資料 並打亂順序
dataset = dataset[dataset[['START_DATETIME']]<'2017-01-01',] # 690874 * 283
set.seed(1001)
train_data = dataset[dataset[['START_DATETIME']]<'2016-01-01',]
train_data = train_data[sample(nrow(train_data)),]
test_data = dataset[dataset[['START_DATETIME']]>='2016-01-01',]
test_data = test_data[sample(nrow(test_data)),]

train_data$START_DATETIME = NULL
test_data$START_DATETIME = NULL
dim(train_data) #513639*280
dim(test_data) #177235*280

# write.table(train_data,'train_data.csv',sep=',', row.names=F)
# write.table(test_data,'test_data.csv',sep=',', row.names=F)

train_data = read_csv('train_data.csv')
test_data = read_csv('test_data.csv')

train_data = as.data.frame(train_data)
test_data = as.data.frame(test_data)
#################################################################################
#################################################################################
type_list = sapply(train_data,function(x) class(x))
table(type_list)
# colnames(train_data)[type_list=='logical']
#################################################################################
#################################################################################
cl<-makeCluster(3) #建議不要超過4，自行根據硬體設備設定
registerDoParallel(cl)

nrounds = 5
set.seed(1001)
folds = createFolds(factor(train_data$LOSS_RATE), k = 5, list = FALSE)

train_x = train_data[,-which(colnames(test_data)=='LOSS_RATE')]
train_y = train_data$LOSS_RATE
test_x = test_data[,-which(colnames(test_data)=='LOSS_RATE')]
test_y = test_data$LOSS_RATE
rm(dataset,train_data,test_data,Y)
gc()

tefinal = data.matrix(test_x)

# XGB
dev.result <-  rep(0, nrow(train_x)) 
pred_te <- rep(0, nrow(test_x))



for (this.round in 1:nrounds){      
  valid <- c(1:length(train_y)) [folds == this.round]
  dev <- c(1:length(train_y)) [folds != this.round]
  
  dtrain<- xgb.DMatrix(data= as.matrix(train_x[dev,]), 
                       label= train_y[dev])
  #weight = w[dev])
  dvalid <- xgb.DMatrix(data= as.matrix(train_x[valid,]) , 
                        label= train_y[valid])
  valids <- list(val = dvalid)
  #### parameters are far from being optimal ####  
  param = list(objective = "binary:logistic", 
               eval_metric = "auc",
               max_depth = 4,
               eta = 0.05,
               gamma = 5,
               subsample = 0.7,   
               colsample_bytree = 0.7,
               min_child_weight = 50,  
               colsample_bylevel = 0.7,
               lambda = 1, 
               alpha = 0,
               booster = "gbtree",
               silent = 0
  ) 
  model<- xgb.train(data = dtrain,
                    params= param, 
                    nrounds = 5000, 
                    verbose = T, 
                    list(val1=dtrain , val2 = dvalid) ,       
                    early_stopping_rounds = 50 , 
                    print_every_n = 10,
                    maximize = T
  )
  
  saveRDS(model, file = paste0("./model_201904/model_xgb_",this.round,".rda"))
  pred = predict(model,as.matrix(train_x[valid,]))
  dev.result[valid] = pred  
  pred_test  = predict(model,tefinal)
  pred_te = pred_te +pred_test
}

library(pROC)
auc(train_y,dev.result)
pred_test = pred_te/nrounds
# 2016測試資料AUC
auc(test_y,pred_test)

pred_y = ifelse(pred_test>0.1446511,'1','0')

confusion_matrix = table(pred_y,test_y,dnn = c("預測", "實際"))
confusion_matrix
accuracy = sum(diag(confusion_matrix)) / sum(confusion_matrix)
LH=diag(confusion_matrix)/colSums(confusion_matrix)
accuracy
LH


threshold <- function(predict, response) {
  r <- pROC::roc(response, predict)
  r$thresholds[which.max(r$sensitivities + r$specificities)]
}

threshold(pred_test,test_y)
threshold(dev.result,train_y)



model1 = readRDS("./model_201904/xgb_model_1.rda")
model2 = readRDS("./model_201904/xgb_model_2.rda")
model3 = readRDS("./model_201904/xgb_model_3.rda")
model4 = readRDS("./model_201904/xgb_model_4.rda")
model5 = readRDS("./model_201904/xgb_model_5.rda")

# stacking

dev_model1 = predict(model1,as.matrix(train_x))
dev_model2 = predict(model2,as.matrix(train_x))
dev_model3 = predict(model3,as.matrix(train_x))
dev_model4 = predict(model4,as.matrix(train_x))
dev_model5 = predict(model5,as.matrix(train_x))

dev_df = data.frame(x1=dev_model1,x2=dev_model2,x3=dev_model3,x4=dev_model4,x5=dev_model5,y=train_y)
glm_fit = glm(y~x1+x2+x3+x4+x5,data=dev_df,family=binomial())
summary(glm_fit) 
pred_glm = predict(glm_fit, dev_df, type="response")

auc(train_y,pred_glm)
# 2016測試資料AUC
test_model1 = predict(model1,as.matrix(test_x))
test_model2 = predict(model2,as.matrix(test_x))
test_model3 = predict(model3,as.matrix(test_x))
test_model4 = predict(model4,as.matrix(test_x))
test_model5 = predict(model5,as.matrix(test_x))
test_df = data.frame(x1=test_model1,x2=test_model2,x3=test_model3,x4=test_model4,x5=test_model5,y=test_y)
pred_glm_test = predict(glm_fit, test_df, type="response")

auc(test_y,pred_glm_test)

pred_glm_y = ifelse(pred_glm_test>0.1022812,'1','0')

confusion_matrix = table(pred_glm_y,test_y,dnn = c("預測", "實際"))
confusion_matrix
accuracy = sum(diag(confusion_matrix)) / sum(confusion_matrix)
LH=diag(confusion_matrix)/colSums(confusion_matrix)
accuracy
LH

threshold(pred_glm_test,test_y)
threshold(pred_glm,train_y)


# > accuracy
# [1] 0.7818659
# > LH
# 0         1 
# 0.8077168 0.5956019 

