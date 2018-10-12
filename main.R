library('data.table')
library(cathayR)
inicathayR()
# ?cathayR

# CAR_ABT_GROUP2.csv	(IS_CONTUNE_POLICY = 1 續保VEHICLE_KIND_NO = 03.07 小客車)	842600


#######################################################################
############################## 續保小客車 #############################
#######################################################################



#########################################
############### 資料前處理 ##############
#########################################

# 讀取資料
ABT_CONTRACT <- fread('~/R/ML/CAR/dataset/CAR_ABT_GROUP2.csv', header = TRUE,na.strings="")
# 觀察欄位數292個欄位 與ABT相符
# 觀察欄位數322個欄位 與ABT相符
dim(ABT_CONTRACT)
# 簡單探勘
str(ABT_CONTRACT)
names(ABT_CONTRACT)

# 欄位名稱統一轉大寫 此步驟很重要 避免找不到欄位
colnames(ABT_CONTRACT)<-toupper(names(ABT_CONTRACT))
# 抓取至2016年的資料 632385筆
# 抓取至2016年的資料 415799筆 new
ABT_CONTRACT_2017=ABT_CONTRACT[which(START_DATETIME>='2017-01-01'),]
ABT_CONTRACT=ABT_CONTRACT[which(START_DATETIME<'2017-01-01'),]


#########################################
################ 資料探勘 ###############
#########################################

# 因為已經作過一次 只需載入探勘結果即可
# DM_ABT_CONTRACT <- autoDataMining(ABT_CONTRACT)
# 讀取修正後探勘結果
DM_ABT_CONTRACT<-read.csv('~/R/ML/CAR/data_mining_contract.csv', header = TRUE)
# 智能形態轉換
ABT_CONTRACT <- autoTypeConv(ABT_CONTRACT,DM_ABT_CONTRACT$COLNAME,DM_ABT_CONTRACT$TYPE)
# 再次確保沒有欄位全為空 並刪除他
nacol_list<-as.character(findNullCol(ABT_CONTRACT))
nacol_list # 三個欄位需刪除"OCCU_CATCODE_INS" "OCCU_CATCODE_APC" "INS_SCHOOL_DEGREE"
remove<-names(ABT_CONTRACT) %in% nacol_list
ABT_CONTRACT=data.frame(ABT_CONTRACT) # 必須轉換才能直接刪除
ABT_CONTRACT<-ABT_CONTRACT[!remove]

#人工刪除無意義欄位
nacol_list<-c("APC_AGE","APC_MARRIAGE","INTRDUCE_DIV_NO","INS_ZIP_CODE")
remove<-names(ABT_CONTRACT) %in% nacol_list
ABT_CONTRACT<-ABT_CONTRACT[!remove]
#315個欄位

#########################################
################ 補缺失值 ###############
#########################################
# 
# sum(is.na(ABT_CONTRACT$IS_SAME))
# sum(is.na(ABT_CONTRACT$APC_RELATION_INS))
# 
# table(ABT_CONTRACT$IS_SAME)
# table(ABT_CONTRACT$APC_RELATION_INS)

# 人工補值
# 加裝配備註記補0無
ABT_CONTRACT$IS_ADD_OUTFIT[is.na(ABT_CONTRACT$IS_ADD_OUTFIT)]=0
# 被保人婚姻補1未婚
ABT_CONTRACT$INS_MARRIAGE[is.na(ABT_CONTRACT$INS_MARRIAGE)]=1
# 要保人與被保人關係補1本人
ABT_CONTRACT$APC_RELATION_INS[is.na(ABT_CONTRACT$APC_RELATION_INS)]=1
# 保額倍數補0
ABT_CONTRACT$AMOUNT_TIMES[is.na(ABT_CONTRACT$AMOUNT_TIMES)]=0
# 承保型態 都是1或NA 占比87% 刪除
nacol_list<-c("INSRNCE_TYPE_CODE")
remove<-names(ABT_CONTRACT) %in% nacol_list
ABT_CONTRACT<-ABT_CONTRACT[!remove]
#314欄位


# 開始進行缺失值處理 若有需人工補值的部份請先行處理
# 把剩下的一般欄位 使用智能缺失函數補值 此函數也會將字串及日期欄位做刪除處理
ABT_CONTRACT<-autoTransMissValue(ABT_CONTRACT)
# 補完值後將整個欄位factor都是同一元素的欄位剔除 這樣的欄位分析沒有意義
# 同樣使用智能踢除函數處理
ABT_CONTRACT<-removeRepFac(ABT_CONTRACT)
# VIEW一下資料有無問題
head(ABT_CONTRACT,5)
# 欄位數剩114

# 找出高度共線性欄位
# CollineList<-findCollineList(ABT_CONTRACT,0.9)
# 移除高度共線性欄位
# remove<-names(ABT_CONTRACT) %in% as.character(CollineList$column)
# ABT_CONTRACT<-ABT_CONTRACT[!remove]
# 欄位數剩145個

#########################################
# 折衷方法 刪除3年以上記錄
CollineList<-c('FIVE_INSUR_AMOUNT','FIVE_INSUR_CNT','FIVE_INSUR_AMOUNT_BODY','FIVE_INSUR_CNT_BODY','FIVE_INSUR_AMOUNT_BURG','FIVE_INSUR_CNT_BURG','FIVE_INSUR_AMOUNT_THIRD','FIVE_INSUR_CNT_THIRD','FIVE_INSUR_AMOUNT_CUST','FIVE_INSUR_CNT_CUST','FIVE_INSUR_AMOUNT_FORCE','FIVE_INSUR_CNT_FORCE','FIVE_INSUR_AMOUNT_OTHER','FIVE_INSUR_CNT_OTHER','FIVE_INSUR_AMOUNT_COM','FIVE_INSUR_CNT_COM','FIVE_INSUR_AMOUNT_C','FIVE_INSUR_CNT_C','FIVE_INSUR_AMOUNT_BODY_C','FIVE_INSUR_CNT_BODY_C','FIVE_INSUR_AMOUNT_BURG_C','FIVE_INSUR_CNT_BURG_C','FIVE_INSUR_AMOUNT_THIRD_C','FIVE_INSUR_CNT_THIRD_C','FIVE_INSUR_AMOUNT_CUST_C','FIVE_INSUR_CNT_CUST_C','FIVE_INSUR_AMOUNT_FORCE_C','FIVE_INSUR_CNT_FORCE_C','FIVE_INSUR_AMOUNT_OTHER_C','FIVE_INSUR_CNT_OTHER_C','FIVE_INSUR_AMOUNT_COM_C','FIVE_INSUR_CNT_COM_C','FOUR_INSUR_AMOUNT','FOUR_INSUR_CNT','FOUR_INSUR_AMOUNT_BODY','FOUR_INSUR_CNT_BODY','FOUR_INSUR_AMOUNT_BURG','FOUR_INSUR_CNT_BURG','FOUR_INSUR_AMOUNT_THIRD','FOUR_INSUR_CNT_THIRD','FOUR_INSUR_AMOUNT_CUST','FOUR_INSUR_CNT_CUST','FOUR_INSUR_AMOUNT_FORCE','FOUR_INSUR_CNT_FORCE','FOUR_INSUR_AMOUNT_OTHER','FOUR_INSUR_CNT_OTHER','FOUR_INSUR_AMOUNT_COM','FOUR_INSUR_CNT_COM','FOUR_INSUR_AMOUNT_C','FOUR_INSUR_CNT_C','FOUR_INSUR_AMOUNT_BODY_C','FOUR_INSUR_CNT_BODY_C','FOUR_INSUR_AMOUNT_BURG_C','FOUR_INSUR_CNT_BURG_C','FOUR_INSUR_AMOUNT_THIRD_C','FOUR_INSUR_CNT_THIRD_C','FOUR_INSUR_AMOUNT_CUST_C','FOUR_INSUR_CNT_CUST_C','FOUR_INSUR_AMOUNT_FORCE_C','FOUR_INSUR_CNT_FORCE_C','FOUR_INSUR_AMOUNT_OTHER_C','FOUR_INSUR_CNT_OTHER_C','FOUR_INSUR_AMOUNT_COM_C','FOUR_INSUR_CNT_COM_C')
# 移除3年以上記錄
remove<-names(ABT_CONTRACT) %in% CollineList
ABT_CONTRACT<-ABT_CONTRACT[!remove]
# 欄位數剩98個

#########################################
############### 應變數處理 ##############
#########################################

# 風險等級閥值 0.87 / 7.01
# L 91%
# M 6%
# H 3%
sum(ABT_CONTRACT$LOSS_RATE<0.87) # 379705
sum(ABT_CONTRACT$LOSS_RATE>=0.87 & ABT_CONTRACT$LOSS_RATE<7.01) # 24071
sum(ABT_CONTRACT$LOSS_RATE>=7.01) # 12023
# 應變數轉換
PAYOUT_RATE=NULL
for(i in 1:length(ABT_CONTRACT$LOSS_RATE)){
  if (ABT_CONTRACT$LOSS_RATE[i]<0.87){PAYOUT_RATE[i]='L'}
  else if (ABT_CONTRACT$LOSS_RATE[i]>= 0.87 & ABT_CONTRACT$LOSS_RATE[i]<7.01){PAYOUT_RATE[i]='M'}
  else {PAYOUT_RATE[i]='H'}
}
# 確認資料筆數正確
table(PAYOUT_RATE)
# 轉換factor導入
ABT_CONTRACT$LOSS_RATE=as.factor(PAYOUT_RATE)

#########################################
################ 資料抽樣 ###############
#########################################


######################## 變數處理 #####################################

# 金額及次數+1取log轉換
names_list=c('DISCOUNT_PREMIUM',	'INS_ISSUE_YEAR',	'ONE_INSUR_AMOUNT',	'ONE_INSUR_CNT',	'TWO_INSUR_AMOUNT',	'TWO_INSUR_CNT',	'BUMP_CNT',	'ONE_INSUR_AMOUNT_BODY',	'ONE_INSUR_CNT_BODY',	'TWO_INSUR_AMOUNT_BODY',	'TWO_INSUR_CNT_BODY',	'BUMP_CNT_BODY',	'ONE_INSUR_AMOUNT_BURG',	'ONE_INSUR_CNT_BURG',	'TWO_INSUR_AMOUNT_BURG',	'TWO_INSUR_CNT_BURG',	'THREE_INSUR_CNT_BURG',	'BUMP_CNT_BURG',	'ONE_INSUR_AMOUNT_THIRD',	'ONE_INSUR_CNT_THIRD',	'TWO_INSUR_AMOUNT_THIRD',	'TWO_INSUR_CNT_THIRD',	'ONE_PARK_CNT_THIRD',	'TWO_PARK_CNT_THIRD',	'BUMP_CNT_THIRD',	'ONE_INSUR_AMOUNT_CUST',	'ONE_INSUR_CNT_CUST',	'TWO_INSUR_AMOUNT_CUST',	'TWO_INSUR_CNT_CUST',	'THREE_INSUR_CNT_CUST',	'BUMP_CNT_CUST',	'ONE_INSUR_AMOUNT_FORCE',	'ONE_INSUR_CNT_FORCE',	'TWO_INSUR_AMOUNT_FORCE',	'TWO_INSUR_CNT_FORCE',	'THREE_INSUR_AMOUNT_FORCE',	'THREE_INSUR_CNT_FORCE',	'ONE_PARK_CNT_FORCE',	'TWO_PARK_CNT_FORCE',	'THREE_PARK_CNT_FORCE',	'BUMP_CNT_FORCE',	'ONE_INSUR_AMOUNT_OTHER',	'ONE_INSUR_CNT_OTHER',	'TWO_INSUR_AMOUNT_OTHER',	'TWO_INSUR_CNT_OTHER',	'ONE_PARK_CNT_OTHER',	'TWO_PARK_CNT_OTHER',	'BUMP_CNT_OTHER',	'ONE_INSUR_AMOUNT_COM',	'ONE_INSUR_CNT_COM',	'TWO_INSUR_AMOUNT_COM',	'TWO_INSUR_CNT_COM',	'BUMP_CNT_COM',	'ONE_INSUR_AMOUNT_C',	'ONE_INSUR_CNT_C',	'TWO_INSUR_AMOUNT_C',	'TWO_INSUR_CNT_C',	'BUMP_CNT_C',	'ONE_INSUR_AMOUNT_BODY_C',	'TWO_INSUR_AMOUNT_BODY_C',	'ONE_INSUR_AMOUNT_BURG_C',	'ONE_INSUR_CNT_BURG_C',	'TWO_INSUR_CNT_BURG_C',	'BUMP_CNT_BURG_C',	'ONE_INSUR_AMOUNT_THIRD_C',	'ONE_INSUR_CNT_THIRD_C',	'TWO_INSUR_AMOUNT_THIRD_C',	'TWO_INSUR_CNT_THIRD_C',	'ONE_INSUR_AMOUNT_CUST_C',	'ONE_INSUR_CNT_CUST_C',	'TWO_INSUR_CNT_CUST_C',	'THREE_INSUR_CNT_CUST_C',	'BUMP_CNT_CUST_C',	'ONE_INSUR_AMOUNT_FORCE_C',	'ONE_INSUR_CNT_FORCE_C',	'TWO_INSUR_AMOUNT_FORCE_C',	'TWO_INSUR_CNT_FORCE_C',	'THREE_INSUR_AMOUNT_FORCE_C',	'THREE_INSUR_CNT_FORCE_C',	'ONE_PARK_CNT_FORCE_C',	'BUMP_CNT_FORCE_C',	'ONE_INSUR_AMOUNT_OTHER_C',	'TWO_INSUR_AMOUNT_OTHER_C',	'TWO_INSUR_CNT_OTHER_C',	'ONE_PARK_CNT_OTHER_C',	'TWO_PARK_CNT_OTHER_C',	'BUMP_CNT_OTHER_C',	'ONE_INSUR_AMOUNT_COM_C',	'ONE_INSUR_CNT_COM_C',	'TWO_INSUR_AMOUNT_COM_C',	'TWO_INSUR_CNT_COM_C',	'BUMP_CNT_COM_C',	'DRIVER_CONCLUDE_AMOUNT_1Y',	'DRIVER_TIMES_1Y',	'DRIVER_CONCLUDE_AMOUNT_2Y',	'DRIVER_TIMES_2Y',	'DRIVER_CONCLUDE_AMOUNT_3Y')
for (col in names_list){
  print(paste("處理欄位 ",col))
  tryCatch({
    ABT_CONTRACT[[col]]=log(ABT_CONTRACT[[col]]+1)
    },error= function(e) {
      print(paste("欄位無法處理 ",col))
  })
}

#######################################################################


# 使用比例抽樣 將三個風險等級分開
ABT_CONTRACT_L<-ABT_CONTRACT[ABT_CONTRACT$LOSS_RATE=='L',]
ABT_CONTRACT_M<-ABT_CONTRACT[ABT_CONTRACT$LOSS_RATE=='M',]
ABT_CONTRACT_H<-ABT_CONTRACT[ABT_CONTRACT$LOSS_RATE=='H',]

set.seed(1000)
# 抽出測試資料 5000筆
# 根據比例 L:M:H = 4311:2161:3528 
# 根據比例 L:M:H = 2500:1500:1000 
# 抽出 L
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_L)),replace = F)
testing_data<-ABT_CONTRACT_L[data_idx[1:2500],] # 測試資料
ABT_CONTRACT_L<-ABT_CONTRACT_L[data_idx[2500:nrow(ABT_CONTRACT_L)],] # 訓練資料
# 抽出 M
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_M)),replace = F)
testing_data<-rbind(testing_data,ABT_CONTRACT_M[data_idx[1:1500],]) # 測試資料
ABT_CONTRACT_M<-ABT_CONTRACT_M[data_idx[1500:nrow(ABT_CONTRACT_M)],] # 訓練資料
# 抽出 H
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_H)),replace = F)
testing_data<-rbind(testing_data,ABT_CONTRACT_H[data_idx[1:1000],]) # 測試資料
ABT_CONTRACT_H<-ABT_CONTRACT_H[data_idx[1000:nrow(ABT_CONTRACT_H)],] # 訓練資料

# 儲存測試資料 需要時才使用
# save.image('~/R/ML/CAR/testing_data.Rdata')

set.seed(100)
# 抽出訓練資料 20000筆
# 根據比例 L:M:H = 10000:6000:4000 
# 抽出 L
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_L)),10000,replace = F)
training_data<-ABT_CONTRACT_L[data_idx,]
# 抽出 M
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_M)),6000,replace = F)
training_data<-rbind(training_data,ABT_CONTRACT_M[data_idx,])
# 抽出 H
data_idx<-sample(seq_len(nrow(ABT_CONTRACT_H)),4000,replace = F)
training_data<-rbind(training_data,ABT_CONTRACT_H[data_idx,])

# data=training_data
testing_data=testing_data[,1:97]
training_data=training_data[,1:97]


# 儲存修正後原始資料
# save.image('~/R/ML/CAR/ABT_CONTRACT.Rdata')
# 讀取修正後原始資料
# load('~/R/ML/CAR/ABT_CONTRACT.Rdata')

###############################################################################
######################### 隨機森林RandomForest ################################
###############################################################################

library(randomForest)

#RandomForest建模 importane顯示重要因子
randomforestM <- randomForest(LOSS_RATE ~ ., data = training_data , importane = T, proximity = T, do.trace =500)

################## 補充區一 ######################
#由於因子不能大於53的元素 可能導致演算法失敗
#找出大於53個元素的因子欄位
col.list<-names(training_data)
sol<-NULL
#找出因子數目53個以上
for(col_name in col.list){
  if(length(names(table(data[[col_name]])))>53 && class(data[[col_name]])=='factor') sol<-rbind(sol,col_name)
}
sol
##################################################

# 顯示重要的因子 並儲存pca_test.csv觀察
sol<-data.frame(MeanDecreaseGini=round(importance(randomforestM), 2),Col_name=rownames(round(importance(randomforestM), 2)))
write.table(sol, file = "~/R/ML/CAR/pca_RF.csv",row.names = F,sep = ",")
# 檢視誤差收斂狀況
plot(randomforestM)

#預測
result <- predict(randomforestM, newdata = testing_data)
#建立混淆矩陣(confusion matrix)觀察模型表現
cm <- table(testing_data$LOSS_RATE, result, dnn = c("實際", "預測"))
cm

#正確率
#計算正確率
accuracy <- sum(diag(cm)) / sum(cm)
accuracy

#各別準確率
l<-c(1:4)
diag(cm)[l]/rowSums(cm)[l]



saveRDS(randomforestM, paste0('R/ML/CAR/model/RF_fin.model'))
# RFM<-readRDS('R/ML/CAR/model/RF_fin.model')

save(randomforestM,file = "R/ML/CAR/model/RF_fin.RData")




###############################################################################
################################ XGboost ######################################
###############################################################################

library(xgboost)
library(Matrix)

#轉換稀疏矩陣
# xdata = sparse.model.matrix(LOSS_RATE ~ .-1, data = training_data)
xdata = model.matrix(~.-1,training_data[names(training_data)!='LOSS_RATE'])
#計算應變數幾個元素
m = nlevels(training_data$LOSS_RATE)
Y = as.integer(training_data$LOSS_RATE)-1

# xgboost 參數設定 (xgboost parameters setup)
param = list("objective" = "multi:softprob",
             "eval_metric" = "mlogloss",
             "num_class" = m
)

# 進行建模
result = xgboost(param=param, data=xdata, label=Y, nrounds=500)

# 計算預測值 (get prediction)
# xdata = sparse.model.matrix(PAYOUT_RATE ~ .-1, data = data)
Ypred = predict(result,xdata)
Ypred = t(matrix(Ypred,m,length(Ypred)/m))
# colnames(Ypred) = levels(data$PAYOUT_RATE)
Ypred = levels(training_data$LOSS_RATE)[max.col(Ypred)]

# 混淆矩陣 (confusion matrix)
t0 = table(training_data$LOSS_RATE,Ypred,dnn = c("實際", "預測"))
t0

# 預測正確率 (accuracy)
accuracy <- sum(diag(t0)) / sum(t0)
accuracy

#各別準確率
l<-c(1:3)
diag(t0)[l]/rowSums(t0)[l]

# 解釋變數重要性 (variable importance)
imp = xgb.importance(names(training_data[,-1]),model=result)
print(imp)
write.table(imp, file = "~/R/ML/CAR/pca_xg.csv",row.names = F,sep = ",")


#===================================================================================
testing_data_fin=testing_data
data_idx<-sample(seq_len(nrow(ABT_CONTRACT)),10000,replace = F)
testing_data<-ABT_CONTRACT[data_idx,] # 測試資料
testing_data=testing_data[1:97]

data_idx<-sample(seq_len(nrow(ABT_CONTRACT_L)),replace = F)
testing_data<-ABT_CONTRACT_L[data_idx[1:4311],] # 測試資料

xdata = model.matrix(~.-1,testing_data[names(testing_data)!='LOSS_RATE'])

# 計算預測值 (get prediction)
# xdata = sparse.model.matrix(PAYOUT_RATE ~ .-1, data = data)
Ypred = predict(result,xdata)
Ypred = t(matrix(Ypred,m,length(Ypred)/m))
# colnames(Ypred) = levels(data$PAYOUT_RATE)
Ypred = levels(testing_data$LOSS_RATE)[max.col(Ypred)]

# 混淆矩陣 (confusion matrix)
t0 = table(testing_data$LOSS_RATE,Ypred,dnn = c("實際", "預測"))
t0

# 預測正確率 (accuracy)
accuracy <- sum(diag(t0)) / sum(t0)
accuracy

#各別準確率
l<-c(1:3)
diag(t0)[l]/rowSums(t0)[l]




data_idx<-sample(seq_len(nrow(ABT_CONTRACT)),replace = F)
testing_data<-ABT_CONTRACT[data_idx[1:10000],] # 測試資料

xgb.save(result, paste0('R/ML/CAR/model/xgb_fin.model'))
# XBGM=xgb.load('R/ML/CAR/model/xgb_fin.model')



