dataset=copy

dim(dataset)
dataset=dataset[which(dataset[,'START_DATETIME']<'2017-01-01'),]

# xlevs <- lapply(dataset[,sapply(dataset, is.factor), drop = F], function(j){levels(j)})
dataset[,'LOSS_RATE']=as.factor(ifelse(dataset[,'LOSS_RATE'] == 0 , '0', '1'))
prop.table(table(dataset$LOSS_RATE))

# 只改training時間區間
testdata=dataset[intersect(which(dataset[,'START_DATETIME']>='2016-01-01'),which(dataset[,'START_DATETIME']<'2017-01-01')),]
traindata=dataset[intersect(which(dataset[,'START_DATETIME']>='2012-01-01'),which(dataset[,'START_DATETIME']<'2016-01-01')),]

traindata=traindata[,-which(colnames(traindata) %in% 'START_DATETIME')]
testdata=testdata[,-which(colnames(testdata) %in% 'START_DATETIME')]

H=subset(traindata,traindata$LOSS_RATE=='1')
L=subset(traindata,traindata$LOSS_RATE=='0')

set.seed(100)
new_train = rbind(H,L[sample(1:nrow(L),6500,replace=F),]) #6500
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
# glm2 <- h2o.glm(x = x, y = y, family = family,
#                 training_frame = train,
#                 nfolds = nfolds,alpha=0.25,
#                 fold_assignment = "Modulo",
#                 keep_cross_validation_predictions = TRUE)
# glm5 <- h2o.glm(x = x, y = y, family = family,
#                 training_frame = train,
#                 nfolds = nfolds,alpha=1,
#                 fold_assignment = "Modulo",
#                 keep_cross_validation_predictions = TRUE)


# RF
d=20
m=14
rf1 <- h2o.randomForest(x = x, y = y, ntrees=10 ,mtries = 14,
                        training_frame = train,
                        seed = 1,  max_depth=20,
                        nfolds = nfolds,
                        fold_assignment = "Modulo",
                        keep_cross_validation_predictions = TRUE)

gbm1 <- h2o.gbm(x = x, y = y, distribution = "bernoulli",
                training_frame = train, max_depth=5,
                seed = 1,ntrees = 50,
                nfolds = nfolds,col_sample_rate=0.5,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)
gbm2 <- h2o.gbm(x = x, y = y, distribution = "bernoulli",
                training_frame = train,max_depth=5,
                seed = 1,ntrees = 50,
                nfolds = nfolds,col_sample_rate=0.6,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)
#gbm3
gbm3 <- h2o.gbm(x = x, y = y, distribution = "bernoulli",
                training_frame = train,max_depth=5,
                seed = 1,ntrees = 50,
                nfolds = nfolds,col_sample_rate=0.7,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)
gbm4 <- h2o.gbm(x = x, y = y, distribution = "bernoulli",
                training_frame = train,max_depth=5,
                seed = 1,ntrees = 50,
                nfolds = nfolds,col_sample_rate=0.8,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)
gbm5 <- h2o.gbm(x = x, y = y, distribution = "bernoulli",
                training_frame = train,max_depth=5,
                seed = 1,ntrees = 50,
                nfolds = nfolds,col_sample_rate=0.9,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)

nb1 <- h2o.naiveBayes(x = x,y = y,  
                      training_frame = train,
                      nfolds = nfolds,
                      fold_assignment = "Modulo",
                      keep_cross_validation_predictions = TRUE)


xgb1 <- h2o.xgboost(x = x,y = y,training_frame = train,
                    distribution = "bernoulli",
                    #ntrees = 10,
                    #max_depth = 5,#eta=0.5,gamma=1, #default
                    # min_rows = 2,learn_rate = 0.2,
                    nfolds = nfolds,
                    fold_assignment = "Modulo",
                    keep_cross_validation_predictions = TRUE,seed = 1)

xgb2 <- h2o.xgboost(x = x,y = y,training_frame = train,
                    distribution = "bernoulli",
                    ntrees = 10,
                    max_depth = 10,
                    #                     min_rows = 2,learn_rate = 0.2,
                    nfolds = nfolds,
                    fold_assignment = "Modulo",
                    keep_cross_validation_predictions = TRUE,seed = 1)
xgb3 <- h2o.xgboost(x = x,y = y,training_frame = train,
                    distribution = "bernoulli",
                    ntrees = 10,
                    max_depth = 15,
                    #                     min_rows = 2,learn_rate = 0.2,
                    nfolds = nfolds,
                    fold_assignment = "Modulo",
                    keep_cross_validation_predictions = TRUE,seed = 1)
xgb4 <- h2o.xgboost(x = x,y = y,training_frame = train,
                    distribution = "bernoulli",
                    ntrees = 10,
                    max_depth = 20,
                    #                     min_rows = 2,learn_rate = 0.2,
                    nfolds = nfolds,
                    fold_assignment = "Modulo",
                    keep_cross_validation_predictions = TRUE,seed = 1)
xgb5 <- h2o.xgboost(x = x,y = y,training_frame = train,
                    distribution = "bernoulli",
                    ntrees = 10,
                    max_depth = 50,
                    #                     min_rows = 2,learn_rate = 0.2,
                    nfolds = nfolds,
                    fold_assignment = "Modulo",
                    keep_cross_validation_predictions = TRUE,seed = 1)


t=seq(10,500,50)
d=seq(10,100,10)
m=matrix(0,length(t)*length(d),5)
k=1
for(i in 1:length(t)){
  for(j in 1:length(d)){
    xgb1 <- h2o.xgboost(x = x,y = y,training_frame = train,
                        distribution = "bernoulli",
                        ntrees = 260,
                        max_depth = 100,
                        nfolds = nfolds,
                        fold_assignment = "Modulo",
                        keep_cross_validation_predictions = TRUE,seed = 1)
    
    models <- list(gbm1,gbm2,gbm3,gbm4,gbm5,rf1,xgb1)

# models <- list(glm2,rf4,gbm5,xgb4,nb1,glm5,rf1,gbm1,xgb1)
# models[[i]]=NULL
# models=list(rf4,glm2) #70/71/63

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

confusion_matrix = table(as.data.frame(perf_t$pred[,1])[,1],new_test$LOSS_RATE,dnn = c("預測", "實際")); 
colnames(confusion_matrix)=c('L','H')
rownames(confusion_matrix)=c('L','H')
accuracy = sum(diag(confusion_matrix)) / sum(confusion_matrix)
LH=diag(confusion_matrix)/colSums(confusion_matrix)
confusion_matrix
accuracy
LH
m[k,]=c(t[i],d[j],accuracy,LH)
k=k+1
 }
}

#######################################################################
# load model
h2o.save_ensemble(stack, path = "./h2o-ensemble-model-loadtest-group4-0903", force = FALSE, export_levelone = FALSE)

h2o.varimp(gbm3)
write.csv(h2o.varimp(gbm3),'gbm3_importance_group4.csv')

