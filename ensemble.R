# https://segmentfault.com/a/1190000008580737
# https://codertw.com/%E4%BA%BA%E5%B7%A5%E6%99%BA%E6%85%A7/8024/

dataset=copy
dim(dataset)

# xlevs <- lapply(dataset[,sapply(dataset, is.factor), drop = F], function(j){levels(j)})
dataset[,'LOSS_RATE']=as.factor(ifelse(dataset[,'LOSS_RATE'] == 0 , '0', '1'))
prop.table(table(dataset$LOSS_RATE))

# 只改training時間區間
testdata=dataset[intersect(which(dataset[,'START_DATETIME']>='2016-01-01'),which(dataset[,'START_DATETIME']<'2017-01-01')),]
traindata=dataset[intersect(which(dataset[,'START_DATETIME']>='2014-01-01'),which(dataset[,'START_DATETIME']<'2017-01-01')),]

traindata=traindata[,-which(colnames(traindata) %in% 'START_DATETIME')]
testdata=testdata[,-which(colnames(testdata) %in% 'START_DATETIME')]

# # 全部轉為dummy
# x_train=model.matrix(LOSS_RATE~., traindata, xlev=xlevs)[,-1]
# x_test=model.matrix(LOSS_RATE~., testdata, xlev=xlevs)[,-1]
# traindata=cbind.data.frame(x_train, LOSS_RATE=(traindata$LOSS_RATE))
# testdata=cbind.data.frame(x_test, LOSS_RATE=(testdata$LOSS_RATE))

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

# # GLM
# glm1 <- h2o.glm(x = x, y = y, family = family,
#                 training_frame = train,
#                 nfolds = nfolds,alpha=1,
#                 fold_assignment = "Modulo",
#                 keep_cross_validation_predictions = TRUE)
#glm2
glm2 <- h2o.glm(x = x, y = y, family = family,
                training_frame = train,
                nfolds = nfolds,alpha=0.25,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)
# glm3 <- h2o.glm(x = x, y = y, family = family,
#                 training_frame = train,
#                 nfolds = nfolds,alpha=0.5,
#                 fold_assignment = "Modulo",
#                 keep_cross_validation_predictions = TRUE)
# glm4 <- h2o.glm(x = x, y = y, family = family,
#                 training_frame = train,
#                 nfolds = nfolds,alpha=0.75,
#                 fold_assignment = "Modulo",
#                 keep_cross_validation_predictions = TRUE)
# glm5 <- h2o.glm(x = x, y = y, family = family,
#                 training_frame = train,
#                 nfolds = nfolds,alpha=1,
#                 fold_assignment = "Modulo",
#                 keep_cross_validation_predictions = TRUE)

# # RF
d=20
m=14
# rf1 <- h2o.randomForest(x = x, y = y, ntrees=10 ,mtries = m,
#                         training_frame = train,
#                         seed = 1,max_depth=d,
#                         nfolds = nfolds,
#                         fold_assignment = "Modulo",
#                         keep_cross_validation_predictions = TRUE)
# rf2 <- h2o.randomForest(x = x, y = y, ntrees=50 ,mtries = m,
#                         training_frame = train,
#                         seed = 1,max_depth =d,
#                         nfolds = nfolds,
#                         fold_assignment = "Modulo",
#                         keep_cross_validation_predictions = TRUE)
# rf3 <- h2o.randomForest(x = x, y = y, ntrees=100 ,mtries = m,
#                         training_frame = train,
#                         seed = 1,max_depth=d,
#                         nfolds = nfolds,
#                         fold_assignment = "Modulo",
#                         keep_cross_validation_predictions = TRUE)
#rf4
rf4 <- h2o.randomForest(x = x, y = y, ntrees=200 ,mtries = m,
                        training_frame = train,
                        seed = 1,max_depth=d,
                        nfolds = nfolds,
                        fold_assignment = "Modulo",
                        keep_cross_validation_predictions = TRUE)
# rf5 <- h2o.randomForest(x = x, y = y, ntrees=500 ,mtries = m,
#                         training_frame = train,
#                         seed = 2,max_depth=d,
#                         nfolds = nfolds,
#                         fold_assignment = "Modulo",
#                         keep_cross_validation_predictions = TRUE)

d=5
r=0.8
# gbm1 <- h2o.gbm(x = x, y = y, distribution = "bernoulli",
#                 training_frame = train,#max_depth=d,
#                 seed = 1,ntrees = 10,
#                 nfolds = nfolds,#col_sample_rate=0.5,
#                 fold_assignment = "Modulo",
#                 keep_cross_validation_predictions = TRUE)
# gbm2 <- h2o.gbm(x = x, y = y, distribution = "bernoulli",
#                 training_frame = train,#max_depth=d,
#                 seed = 1,ntrees = 50,
#                 nfolds = nfolds,#col_sample_rate=r,
#                 fold_assignment = "Modulo",
#                 keep_cross_validation_predictions = TRUE)
# gbm3 <- h2o.gbm(x = x, y = y, distribution = "bernoulli",
#                 training_frame = train,max_depth=d,
#                 seed = 1,ntrees = 100,
#                 nfolds = nfolds,#col_sample_rate=r,
#                 fold_assignment = "Modulo",
#                 keep_cross_validation_predictions = TRUE)
# gbm4 <- h2o.gbm(x = x, y = y, distribution = "bernoulli",
#                 training_frame = train,#max_depth=d,
#                 seed = 1,ntrees = 200,
#                 nfolds = nfolds,#col_sample_rate=r,
#                 fold_assignment = "Modulo",
#                 keep_cross_validation_predictions = TRUE)
# gbm5
gbm5 <- h2o.gbm(x = x, y = y, distribution = "bernoulli",
                training_frame = train,max_depth=d,
                seed = 1,ntrees = 500,
                nfolds = nfolds,col_sample_rate=r,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)

# r=0.01
# rho=0.9
# e=1e-4
# dl1 <- h2o.deeplearning(x = x, y = y, distribution = "bernoulli",
#                         training_frame = train,
#                         nfolds = nfolds,
#                         fold_assignment = "Modulo",
#                         keep_cross_validation_predictions = TRUE)
# dl2 <- h2o.deeplearning(x = x, y = y, distribution = "bernoulli",
#                         training_frame = train,
#                         nfolds = nfolds,
#                         fold_assignment = "Modulo",
#                         keep_cross_validation_predictions = TRUE)
# dl3 <- h2o.deeplearning(x = x, y = y, distribution = "bernoulli",
#                         training_frame = train,
#                         nfolds = nfolds,
#                         fold_assignment = "Modulo",
#                         keep_cross_validation_predictions = TRUE)
# dl4 <- h2o.deeplearning(x = x, y = y, distribution = "bernoulli",
#                         training_frame = train,
#                         nfolds = nfolds,
#                         fold_assignment = "Modulo",
#                         keep_cross_validation_predictions = TRUE)
# dl5 <- h2o.deeplearning(x = x, y = y, distribution = "bernoulli",
#                         training_frame = train,
#                         nfolds = nfolds,
#                         fold_assignment = "Modulo",
#                         keep_cross_validation_predictions = TRUE)
#dl6
# dl6 <- h2o.deeplearning(x = x, y = y, distribution = "bernoulli",
#                         training_frame = train,
#                         nfolds = nfolds,
#                         fold_assignment = "Modulo",
#                         keep_cross_validation_predictions = TRUE,
#                         activation = c("MaxoutWithDropout"),
#                         hidden = c(50, 50, 50),
#                         epochs = 198,
#                         l1 = 0,
#                         l2 = 0.00001,
#                         rate = 1,
#                         rate_annealing = 1e-6,
#                         rho = 0.99,
#                         epsilon = 1e-10,
#                         momentum_start = 0,
#                         momentum_stable = 0,
#                         input_dropout_ratio = 0.1,
#                         max_w2 = 1000)

# nb1 <- h2o.naiveBayes(x = x,y = y,  
#                       training_frame = train,
#                       nfolds = nfolds,
#                       fold_assignment = "Modulo",
#                       keep_cross_validation_predictions = TRUE)
# 
# nb2 <- h2o.naiveBayes(x = x,y = y, laplace = 2, 
#                       training_frame = train,
#                       nfolds = nfolds,
#                       fold_assignment = "Modulo",
#                       keep_cross_validation_predictions = TRUE)

# models <- list(glm1,glm2,glm3,glm4,glm5)
# models <- list(rf1,rf2,rf3,rf4)
# models <- list(gbm1,gbm2,gbm3,gbm4,gbm5)
# models <- list(glm2,rf4,gbm3)      
models <- list(glm2,rf4,gbm5)
# models <- list(glm2,rf4,gbm3,nb1)  
# models <- list(glm2,rf4,gbm3,gbm5,rf1,nb1,dl6)
# models <- list(dl1,dl2,dl3,dl4,dl5)
# models <- list(glm2,rf4,gbm5,dl6,glm1,rf1,gbm1,dl1,glm3,rf3,gbm3,dl3)

metalearner <- "h2o.glm.wrapper" #"h2o.glm.wrapper","h2o.randomForest.wrapper","h2o.gbm.wrapper","h2o.deeplearning.wrapper"
set.seed(0)
stack <- h2o.stack(models = models,seed=1,
                   response_frame = train[,y],
                   metalearner = metalearner, 
                   keep_levelone_data = TRUE)

# ensemble <- h2o.stackedEnsemble(x = x,
#                                 y = y,
#                                 training_frame = train,
#                                 model_id = "my_ensemble_binomial",
#                                 base_models = list(glm1, glm2))

perf <- h2o.ensemble_performance(stack, newdata = test);perf

perf_t <- predict(stack, newdata = test);perf_t

t0 = table(new_test$LOSS_RATE,as.data.frame(perf_t$pred[,1])[,1],dnn = c("實際", "預測"))
t0
# 預測正確率 (accuracy)
sum(diag(t0)) / sum(t0)
diag(t0)/rowSums(t0)

h2o.save_ensemble(stack, path = "./h2o-ensemble-model-loadtest", force = FALSE, export_levelone = FALSE)

# 風險與非風險的理賠金額及件數
i=intersect(which(new_test$LOSS_RATE==0),which(tmp==0))
length(i)
sum(new_test[i,'INSURE'])
sum(new_test[i,'DISCOUNT_PREMIUM'])

#######################################################################

library(h2o)
library(h2oEnsemble) 
h2o.init(nthreads = 50)

saved_model <- h2o.loadModel("/home/9400200/9400215/h2o-ensemble-model-loadtest/")
saved_model <- h2o.loadModel("~/h2o-ensemble-model-loadtest/GLM_model_R_1532402269672_64")

stack <- h2o.load_ensemble(path = "/home/9400200/9400215/h2o-ensemble-model-loadtest")
load("/home/9400200/9400215/h2o-ensemble-model-loadtest/ensemble.RData")


perf <- h2o.ensemble_performance(stack, newdata = test);perf

perf_t <- predict(stack, newdata = test);perf_t

t0 = table(new_test$LOSS_RATE,as.data.frame(perf_t$pred[,1])[,1],dnn = c("實際", "預測"))
t0
# 預測正確率  (accuracy)
sum(diag(t0)) / sum(t0)
diag(t0)/rowSums(t0)

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
