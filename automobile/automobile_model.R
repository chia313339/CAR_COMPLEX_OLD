library(readr)
library(cathayR)
inicathayR()

# Initialization environment
library(h2o)
library(h2oEnsemble) 
h2o.init(nthreads = 50)

print(paste(Sys.time(),"車體險預測模型腳本開始"))

# Extract example dataset by multiple data
dataset=read_csv('/home/cxl_etl/datastage/ABT_CAT_TABLE_ALL.txt')
colnames(dataset)<-toupper(names(dataset))
print(paste(Sys.time(),"輸入資料集維度"))
print(dim(dataset))

# Extract example dataset by one data
# dataset = read_csv('/home/cxl_etl/datastage/ABT_CAT_TABLE_ALL.txt')[1,]
# print(paste(Sys.time(),"輸入資料集維度"))
# print(dim(dataset))

# 判斷批次或單一
if (dim(dataset)[1]>1) {
  print(paste(Sys.time(),"輸入資料為批次資料"))
  dataset_type='2'
} else {
  print(paste(Sys.time(),"輸入資料為單筆資料"))
  dataset_type='1'
}

# 根據不同資料型態跑對應腳本
switch(dataset_type,
       '1'={
         print(paste(Sys.time(),"單筆資料判斷分群"))
         if(dataset$IS_CONTINUE_POLICY=='0' & dataset$VEHICLE_KIND_NO %in% c('03','07')) {script='group1';print(paste(Sys.time(),"資料為新件小客車"))}
         if(dataset$IS_CONTINUE_POLICY=='1' & dataset$VEHICLE_KIND_NO %in% c('03','07')) {script='group2';print(paste(Sys.time(),"資料為續保件小客車"))}
         if(dataset$IS_CONTINUE_POLICY=='0' & !(dataset$VEHICLE_KIND_NO %in% c('03','07'))) {script='group3';print(paste(Sys.time(),"資料為新件非小客車"))}
         if(dataset$IS_CONTINUE_POLICY=='1' & !(dataset$VEHICLE_KIND_NO %in% c('03','07'))) {script='group4';print(paste(Sys.time(),"資料為續保件非小客車"))}
         dataset = batchGroup.cardataprocess(dataset)
         model.result = one.carmodel(dataset,script)
       },
       {
         print(paste(Sys.time(),"輸入資料為批次資料"))
         # 模型資料處理
         dataset = batchGroup.cardataprocess(dataset)
         model.result = batchGroup.carmodel(dataset)
         script='batch_group'
       }
)

print(paste(Sys.time(),"檢視預測結果"))
head(model.result)
print(paste(Sys.time(),"輸出模型預測資料 PRED_RESULT.TXT"))
write.table(model.result, file = "/home/9400200/public/automobile/PRED_RESULT.TXT",row.names = F,sep = ",")

print(paste(Sys.time(),"車體險預測模型腳本結束"))

