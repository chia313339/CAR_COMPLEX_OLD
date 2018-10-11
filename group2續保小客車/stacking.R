# https://segmentfault.com/a/1190000008580737
# https://codertw.com/%E4%BA%BA%E5%B7%A5%E6%99%BA%E6%85%A7/8024/

dataset=copy

dim(dataset)
dataset=dataset[which(dataset[,'START_DATETIME']<'2017-01-01'),]

# xlevs <- lapply(dataset[,sapply(dataset, is.factor), drop = F], function(j){levels(j)})
dataset[,'LOSS_RATE']=as.factor(ifelse(dataset[,'LOSS_RATE'] == 0 , '0', '1'))
prop.table(table(dataset$LOSS_RATE))

# 只改training時間區間
testdata=dataset[intersect(which(dataset[,'START_DATETIME']>='2016-01-01'),which(dataset[,'START_DATETIME']<'2017-01-01')),]
traindata=dataset[intersect(which(dataset[,'START_DATETIME']>='2012-01-01'),which(dataset[,'START_DATETIME']<'2016-01-01')),]
dim(traindata)

traindata=traindata[,-which(colnames(traindata) %in% 'START_DATETIME')]
testdata=testdata[,-which(colnames(testdata) %in% 'START_DATETIME')]

H=subset(traindata,traindata$LOSS_RATE=='1')
L=subset(traindata,traindata$LOSS_RATE=='0')

set.seed(100)
new_train = rbind(H[sample(1:nrow(H),20000,replace=F),],L[sample(1:nrow(L),30000,replace=F),])
new_train = new_train[sample(1:nrow(new_train),replace=F),]
new_test = testdata

#####################################################################################################
library(h2o)
library(h2oEnsemble) 
h2o.init(nthreads = 50)

train=as.h2o(new_train)
test=as.h2o(new_test)
y <- "LOSS_RATE"
x <- setdiff(names(train), y) 

family <- "binomial"

nfolds <- 5  

#glm2
glm2 <- h2o.glm(x = x, y = y, family = family,
                training_frame = train,
                nfolds = nfolds,alpha=0.25,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)
glm5 <- h2o.glm(x = x, y = y, family = family,
                training_frame = train,
                nfolds = nfolds,alpha=1,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)

# RF
d=20
m=14
rf1 <- h2o.randomForest(x = x, y = y, ntrees=10 ,mtries = 21,
                        training_frame = train,
                        seed = 1, # max_depth=d,
                        nfolds = nfolds,
                        fold_assignment = "Modulo",
                        keep_cross_validation_predictions = TRUE)
rf4 <- h2o.randomForest(x = x, y = y, ntrees=200 ,mtries = m,
                        training_frame = train,
                        seed = 1,max_depth=d,
                        nfolds = nfolds,
                        fold_assignment = "Modulo",
                        keep_cross_validation_predictions = TRUE)
d=5
r=0.8
gbm1 <- h2o.gbm(x = x, y = y, distribution = "bernoulli",
                training_frame = train, max_depth=10,
                seed = 1,ntrees = 50,
                nfolds = nfolds,col_sample_rate=0.5,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)

gbm5 <- h2o.gbm(x = x, y = y, distribution = "bernoulli",
                training_frame = train,max_depth=d,
                seed = 1,ntrees = 500,
                nfolds = nfolds,col_sample_rate=r,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)

nb1 <- h2o.naiveBayes(x = x,y = y,  
                      training_frame = train,
                      nfolds = nfolds,
                      fold_assignment = "Modulo",
                      keep_cross_validation_predictions = TRUE)

xgb1 <- h2o.xgboost(x = x,y = y,training_frame = train,
                    distribution = "bernoulli",
                    ntrees = 50,
                    max_depth = 10,eta=0.5,gamma=1, #default
                    # min_rows = 2,learn_rate = 0.2,
                    nfolds = nfolds,
                    fold_assignment = "Modulo",
                    keep_cross_validation_predictions = TRUE,seed = 1)

xgb4 <- h2o.xgboost(x = x,y = y,training_frame = train,
                    distribution = "bernoulli",
                    ntrees = 200,
                    max_depth = 6,
                    #                     min_rows = 2,learn_rate = 0.2,
                    nfolds = nfolds,
                    fold_assignment = "Modulo",
                    keep_cross_validation_predictions = TRUE,seed = 1)

# fit <- h2o.ensemble(x = x, y = y, 
#                     training_frame = train,
#                     family = family, 
#                     learner = learner, 
#                     metalearner = metalearner,
#                     cvControl = list(V = 5))

models <- list(glm2,rf4,gbm5,xgb4,nb1,glm5,rf1,gbm1,xgb1) 
metalearner <- "h2o.glm.wrapper" #"h2o.glm.wrapper","h2o.randomForest.wrapper","h2o.gbm.wrapper","h2o.deeplearning.wrapper"
set.seed(0)
stack <- h2o.stack(models = models,seed=1,
                   response_frame = train[,y],
                   metalearner = metalearner, 
                   keep_levelone_data = TRUE)
perf <- h2o.ensemble_performance(stack, newdata = test);perf
perf_t <- predict(stack, newdata = test);perf_t

confusion_matrix = table(as.data.frame(perf_t$pred[,1])[,1],new_test$LOSS_RATE,dnn = c("預測", "實際")); 
colnames(confusion_matrix)=c('L','H')
rownames(confusion_matrix)=c('L','H')
accuracy = sum(diag(confusion_matrix)) / sum(confusion_matrix)
LH=diag(confusion_matrix)/colSums(confusion_matrix)
confusion_matrix
accuracy
LH

#######################################################################
stack <- h2o.load_ensemble(path = "/home/9400200/9400215/h2o-ensemble-model-loadtest-group2-續保小客車")
# load("/home/9400200/9400215/h2o-ensemble-model-loadtest-group2/ensemble.RData")


# perf <- h2o.ensemble_performance(stack, newdata = test);perf

perf_t <- predict(stack, newdata = test);perf_t

t0 = table(new_test$LOSS_RATE,as.data.frame(perf_t$pred[,1])[,1],dnn = c("實際", "預測"))
t0
# 預測正確率  (accuracy)
sum(diag(t0)) / sum(t0)
diag(t0)/rowSums(t0)

saved_model <- h2o.loadModel("/home/9400200/9400215/h2o-ensemble-model-loadtest-group2/DRF_model_R_1534745861279_135")
write.csv(h2o.varimp(saved_model),'group2_importance_rf.csv')


dataset_INSURE=dataset_INSURE[intersect(which(dataset_INSURE[,'START_DATETIME']>='2016-01-01'),which(dataset_INSURE[,'START_DATETIME']<'2017-01-01')),]
tmp=data.frame(group=rep(2,nrow(new_test)),as.data.frame(perf_t$pred),dataset_INSURE)
test2016=rbind(test2016,tmp)


########################################
######################################
#以下是資料統計不用跑
######################################
# 過核保準則但模型預測錯誤
pred=as.data.frame(perf_t$pred[,1])[,1]
table(new_test$POLICY_RESULT_CODE)
h_l=Reduce(intersect, list(which(pred==1),which(new_test$POLICY_RESULT_CODE==6),which(new_test$LOSS_RATE==0)))
l_h=Reduce(intersect, list(which(pred==0),which(new_test$POLICY_RESULT_CODE==1),which(new_test$LOSS_RATE==1)))
l_l=Reduce(intersect, list(which(pred==0),which(new_test$POLICY_RESULT_CODE==6),which(new_test$LOSS_RATE==0)))
h_h=Reduce(intersect, list(which(pred==1),which(new_test$POLICY_RESULT_CODE==1),which(new_test$LOSS_RATE==1)))

library(cluster)
i=3
tmp=rbind(new_test[h_l,][i,],new_test[h_h,])
head(tmp)
dist=daisy(tmp[,-which(colnames(tmp) %in% c('POLICY_RESULT_CODE','LOSS_RATE'))],metric = "gower")
dist_mat <- as.matrix(dist)
rbind(tmp[1,],tmp[(which.min(dist_mat[1,-1])+1),])

f=rbind(f,tmp[1,],tmp[(which.min(dist_mat[1,-1])+1),])
write.csv(f,'fff.csv')

##########
i=3
tmp=rbind(new_test[l_h,][i,],new_test[l_l,])
head(tmp)
dist=daisy(tmp[,-which(colnames(tmp) %in% c('POLICY_RESULT_CODE','LOSS_RATE'))],metric = "gower")
dist_mat <- as.matrix(dist)
rbind(tmp[1,],tmp[(which.min(dist_mat[1,-1])+1),])
f=NULL
f=rbind(f,tmp[1,],tmp[(which.min(dist_mat[1,-1])+1),])
write.csv(f,'fff.csv')


write.csv(new_test[l_h,],'l_h.csv')

write.csv(new_test[l_h,],'L_H_contract_no.csv')

w=new_test[l_h,]

tmp=NULL
for(i in 1:nrow(w)) tmp[i]=all(w[i,24:187]==0)
write.csv(w[which(tmp==TRUE),],'L_H_contract_no_insure0.csv')

i=Reduce(intersect,list(which(new_test$IS_DEPRECIATION==1),which(new_test$IS_RECOVERY==1),which(new_test$INS_IS_REPAIR==1)))
length(l_h)

h=which(as.data.frame(perf_t$pred[,1])[,1]==1)
l=which(as.data.frame(perf_t$pred[,1])[,1]==0)
hh=intersect(which(as.data.frame(perf_t$pred[,1])[,1]==1),which(new_test$LOSS_RATE==1))
hl=intersect(which(as.data.frame(perf_t$pred[,1])[,1]==1),which(new_test$LOSS_RATE==0))
lh=intersect(which(as.data.frame(perf_t$pred[,1])[,1]==0),which(new_test$LOSS_RATE==1))
ll=intersect(which(as.data.frame(perf_t$pred[,1])[,1]==0),which(new_test$LOSS_RATE==0))
sum(new_test[ll,'DISCOUNT_PREMIUM'])

sum(new_test[h,'INSURE'])/sum(new_test[h,'DISCOUNT_PREMIUM'])
sum(new_test[l,'INSURE'])/sum(new_test[l,'DISCOUNT_PREMIUM'])

sum(new_test[ll,'INSURE'])

t0 = table(new_test$LOSS_RATE,as.data.frame(perf_t$pred[,1])[,1],dnn = c("實際", "預測"));t0
# write.csv(cbind(new_test$LOSS_RATE,as.data.frame(perf_t$pred[,2])[,1]),'auc.csv')
# 預測正確率 (accuracy)
sum(diag(t0)) / sum(t0)
diag(t0)/rowSums(t0)

h2o.varimp(xgb1)
write.csv(h2o.varimp(rf4),'rf4_importance.csv')
h2o.varimp_plot(rf4)

# #AUC
# library(pROC)
category=new_test$LOSS_RATE
prediction=as.data.frame(perf_t$pred[,2])[,1]
roc_obj <- roc(category, prediction)
auc(roc_obj)

h2o.save_ensemble(stack, path = "./h2o-ensemble-model-loadtest-servermodel+xgb", force = FALSE, export_levelone = FALSE)

# 件數金額統計
i=intersect(which(new_test$LOSS_RATE==0),which(tmp==0))
length(i)
sum(new_test[i,'INSURE'])
sum(new_test[i,'DISCOUNT_PREMIUM'])

########################################
# 預測錯誤案件
pred=as.data.frame(perf_t$pred[,1])[,1]
real=new_test$LOSS_RATE
rule=new_test$POLICY_RESULT_CODE
table(real)

wrong_pred1=Reduce(intersect,list(which(rule==1),which(pred==0),which(real==1)))
wrong_pred2=Reduce(intersect,list(which(rule==6),which(pred==1),which(real==0)))

wrong_pred1=Reduce(intersect,list(which(rule==1),which(pred==1),which(real==0)))
wrong_pred2=Reduce(intersect,list(which(rule==6),which(pred==0),which(real==1)))
length(wrong_pred2)
############################################
dim(new_train)
tmp=prop.table(table(new_train$CATEGORY_CODE,new_train$LOSS_RATE),2)
write.csv(tmp,'cate.csv')
rownames(tmp)=c('甲式車體損失險',
                '颱風、洪水或因雨積水',
                '罷工、暴動、民眾騷擾險',
                '乙式車體損失險',
                '免自負額限額車對車碰撞損失險',
                '丙式車體損失險',
                '乙式限定駕駛人',
                '車體險全損理賠無折舊',
                '丙式限定駕駛人',
                '車體損失保險許可使用免追償',
                '限額不明刮損',
                '乙式車體險(專案型)',
                '限額不明刮損',
                '限額不明刮損')
plot(1:nrow(tmp),tmp[,1],type='b')
points(1:nrow(tmp),tmp[,2],col=2,type='b')

dataset$DISCOUNT_PREMIUM=as.numeric(dataset$DISCOUNT_PREMIUM)
aggregate(dataset$DISCOUNT_PREMIUM,list(dataset$LOSS_RATE),mean)
boxplot(dataset[which(dataset$LOSS_RATE==0),'DISCOUNT_PREMIUM'])


tmp=round(prop.table(table(dataset$BODY_MAIN,dataset$LOSS_RATE),2),2)
tmp=prop.table(table(dataset$UNIT_NO,dataset$LOSS_RATE),2)
tmp=round(prop.table(table(dataset$INS_IS_MAIN_CAUSE,dataset$LOSS_RATE),2),4)

tmp=round(prop.table(table(dataset$INS_AGE,dataset$LOSS_RATE),2),4)

aggregate(dataset$INS_ISSUE_YEAR,list(dataset$LOSS_RATE),mean)
# t1=dataset[which(dataset$LOSS_RATE==1),'INS_AGE']
# names(table(t1))[table(t1)==max(table(t1))]

boxplot(INS_ISSUE_YEAR~LOSS_RATE,
        data=dataset,
        ylab="INS_ISSUE_YEAR",horizontal = TRUE
        #         ,col="orange",border="brown"
)

tmp=round(prop.table(table(dataset$BODY_CLAIM_CODE,dataset$LOSS_RATE),2),4)

aggregate(dataset$TWO_INSUR_AMOUNT_BODY_C,list(dataset$LOSS_RATE),mean)
tmp=dataset[which(dataset$LOSS_RATE==1),'TWO_INSUR_AMOUNT_BODY_C']
prop.table(c(length(which(tmp==0)),length(which(tmp>0))))


aggregate(dataset$INSRNCE_AMOUNT,list(dataset$LOSS_RATE),mean)

aggregate(dataset$BUMP_CNT_C,list(dataset$LOSS_RATE),mean)
unique(dataset$BUMP_CNT_C)
tmp=prop.table(table(dataset$BUMP_CNT_C,dataset$LOSS_RATE),2)

aggregate(dataset$BUMP_CNT_BODY_C,list(dataset$LOSS_RATE),mean)

unique(dataset$CONCLUDE_NTD_AMOUNT_AGN)
tmp=aggregate(dataset$DISCOUNT_PREMIUM_AGN,list(dataset$LOSS_RATE),mean)
tmp=dataset[which(dataset$LOSS_RATE==0),'CONCLUDE_NTD_AMOUNT_AGN']
prop.table(c(length(which(tmp==0)),length(which(tmp>0))))

###############################
dataset$DISCOUNT_PREMIUM=as.numeric(dataset$DISCOUNT_PREMIUM)
v=dataset$BODY_CLAIM_CODE
t1=aggregate(dataset$LOSS_RATE,list(v),length)
t2=aggregate(dataset$INSURE,list(v),sum)
t3=aggregate(dataset$DISCOUNT_PREMIUM,list(v),sum)
write.csv(cbind(t1,t2,t3),'ttt.csv')


tmp=dataset[1:20000,]
plot(tmp$BUMP_CNT_BODY_C,tmp$LOSS_RATE) 
cor(dataset$BUMP_CNT_BODY_C,dataset$LOSS_RATE)
text(15,120,'cor=0.0616',cex=2)

# 被保人年齡 以5年一個分檻計算損率 保費、理賠金額、件數
l=c(0,seq(21,96,5))
h=c(20,seq(25,100,5))
age=matrix(0,17,4)
for(i in 1:16){
  sub=dataset[intersect(which(dataset$INS_AGE>=l[i]),which(dataset$INS_AGE<=h[i])),]
  age[i,]=c(nrow(sub),sum(sub$INSURE),sum(sub$DISCOUNT_PREMIUM),sum(sub$INSURE)/sum(sub$DISCOUNT_PREMIUM))
}
write.csv(age,'ins_age.csv')
rownames(age)=c()


