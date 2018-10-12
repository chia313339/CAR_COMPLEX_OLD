library(AER)
library(Matrix)

# ���Jcreditcard��ƶ�(�]�t1,319���[����աA�@��12���ܼ�)
data(CreditCard)
#�[�������
head(CreditCard)
CreditCard <- subset(CreditCard, select = c(card, reports, age, income, owner,months))

# �����ưϤ��� train=0.8, test=0.2 
set.seed(22)
train.index <- sample(x=1:nrow(CreditCard), size=ceiling(0.8*nrow(CreditCard) ))

train = CreditCard[train.index, ]
test = CreditCard[-train.index, ]

require(xgboost)
# dtrain = xgb.DMatrix(data = as.matrix(train[,2:12]),label = train$card)
# dtest = xgb.DMatrix(data = as.matrix(test[,2:12]),label = test$card)
# dtrain = sparse.model.matrix(card~., data = train)[,-1]
# dtest = sparse.model.matrix(card~., data = test)[,-1]
dtrain = xgb.DMatrix(data = sparse.model.matrix(card~., data = train)[,-1],label = as.integer(train$card)-1)
dtest = xgb.DMatrix(data = sparse.model.matrix(card~., data = test)[,-1],label = as.integer(test$card)-1)


xgb.params = list(
  colsample_bytree = 0.5,                    
  subsample = 0.5,                      
  booster = "gbtree",
  max_depth = 2,           
  eta = 0.03,
  "num_class" = 2,
  eval_metric = "mlogloss",                      
  objective = "multi:softprob",
  gamma = 0)               

cv.model = xgb.cv(
  params = xgb.params, 
  data = dtrain,
  nfold = 5,     # 5-fold cv
  nrounds=400,   # ����1-100�A�U�Ӿ��`�ƤU���ҫ�
  # �p�G��nrounds < 30 �ɡA�N�w�g��overfitting���p�o�͡A�����ܤ����~��tune�U�h�F�A�i�H��������                
  early_stopping_rounds = 30, 
  print_every_n = 20 # �C20�ӳ��~��ܤ@�����G�A
) 

# �e�ϡA�[�� CV �L�{��Train �� Validation ��ƪ����{(����GTrain�A�Ŧ�GValidation)
tmp = cv.model$evaluation_log

plot(x=1:nrow(tmp), y= tmp$train_rmse_mean, col='red', xlab="nround", ylab="rmse", main="Avg.Performance in CV") 
points(x=1:nrow(tmp), y= tmp$test_rmse_mean, col='blue') 
legend("topright", pch=1, col = c("red", "blue"), 
       legend = c("Train", "Validation") )

best.nrounds = cv.model$best_iteration 
best.nrounds

xgb.model = xgb.train(paras = xgb.params, 
                      data = dtrain,
                      nrounds = best.nrounds) 
xgb_y = predict(xgb.model, dtest)
y = t(matrix(xgb_y,2,length(xgb_y)/2))

levels(test$card)[max.col(xgb_y)]






