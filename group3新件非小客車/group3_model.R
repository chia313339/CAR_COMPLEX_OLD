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
new_train = rbind(H,L[sample(1:nrow(L),5500,replace=F),])
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
rf4 <- h2o.randomForest(x = x, y = y, ntrees=200 ,mtries = 14,
                        training_frame = train,
                        seed = 1,max_depth=d,
                        nfolds = nfolds,
                        fold_assignment = "Modulo",
                        keep_cross_validation_predictions = TRUE)
d=5
r=0.8
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
                    nfolds = nfolds,
                    fold_assignment = "Modulo",
                    keep_cross_validation_predictions = TRUE,seed = 1)

d=5
r=0.8
gbm1 <- h2o.gbm(x = x, y = y, distribution = "bernoulli",
                training_frame = train, max_depth=10,
                seed = 1,ntrees = 50,
                nfolds = nfolds,col_sample_rate=0.5,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)


models <- list(glm2,rf4,gbm5,xgb4,nb1,glm5,gbm1,xgb1) 

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
m[i,]=c(accuracy,LH)

#######################################################################
# load model
h2o.save_ensemble(stack, path = "./h2o-ensemble-model-loadtest-group3-0829", force = FALSE, export_levelone = FALSE)

h2o.varimp(glm2)
write.csv(h2o.varimp(glm2),'glm2_importance_group3.csv')

