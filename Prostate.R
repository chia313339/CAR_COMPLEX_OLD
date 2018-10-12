data(Prostate, package="lasso2")
str(Prostate)

# �����ưϤ��� train=0.8, test=0.2 
set.seed(22)
train.index <- sample(x=1:nrow(Prostate), size=ceiling(0.8*nrow(Prostate) ))

train = Prostate[train.index, ]
test = Prostate[-train.index, ]

################################################################################################

# Bagging ���@�}�l�A�O�n�����ư����Ʃ�ˡA�]�����N Train �����T�Ӥl��ƶ�(n=33)
# Subset-1
set.seed(1)
ind_1 = sample(1:nrow(train), 33)
subset_1 = train[ind_1, ]

# Subset-1
set.seed(2)
ind_2 = sample(1:nrow(train), 33)
subset_2 = train[ind_2, ]

# Subset-3
set.seed(3)
ind_3 = sample(1:nrow(train), 33)
subset_3 = train[ind_3, ]

# ��subset_1�Asubset_2�Asubset_3���O�إ߽u�ʰj�k�ҫ��A�A�N Test ���w�����G�����_��
# Model-1 : linear regression
model_1 = lm(lpsa~., subset_1)
y1 = predict(model_1, test)

# Model-2 : linear regression
model_2 = lm(lpsa~., subset_2)
y2 = predict(model_2, test)

# Model-3 : linear regression
model_3 = lm(lpsa~., subset_3)
y3 = predict(model_3, test)

# Average Prediction Results
ave_y = (y1+y2+y3)/3

# MSE Comparision between three models and the bagging model
c(mean((y1 - test$lpsa)^2),   # linear regression of subset_1
  mean((y2 - test$lpsa)^2),   # linear regression of subset_2
  mean((y3 - test$lpsa)^2),   # linear regression of subset_3
  mean((ave_y - test$lpsa)^2))  # bagging model

################################################################################################

# randomForest�M��i�H���ڭ̫إ��H���˪L���ҫ�
require(randomForest)

rf_model = randomForest(lpsa~.,
                        data=train,
                        ntree=150        # �M���𪺼ƶq
)
# �w��
rf_y = predict(rf_model, test)
mean((rf_y - test$lpsa)^2) # MSE

# �o�̦��@�ӫؼҤ����|�J�쪺���nĳ�D�G�u�n�M�w�h�֨M����H�v(�]�N�Ontree�n�]�w�h�֡H)
# �ھڤU�ϡA�ڭ̥i�H���D�A��ntree = 70����A���骺�~�t�j���N�ͩ�í�w
# Observe that what is the best number of trees
plot(rf_model)

# �t�@�ӰѼ�mtry���]�ݭn�h Tune�A�o�ӰѼƥN���C����ˮɻݭn��u�h�֭��ܼơv���N��
# �i�H�ϥ�tuneRF()�� tune mtry���ȡA�îھڤU�������G�P�ϡA�o���C����ˮɡu��4���ܼơv�|�O����n�����
tuneRF(train[,-9], train[,9])


# �̨Τƪ��Ѽƫ�i�H���g���o��
rf_model = randomForest(lpsa~.,
                        data = train,
                        ntree = 70,        # �M���𪺼ƶq
                        mtry = 4
)
# �w��
rf_y = predict(rf_model, test)
mean((rf_y - test$lpsa)^2) # MSE

# �����ܼƬO�|��l�����(Loss Function)�̦��^�m��
# Variable Importance of Random Forest
rf_model$importance
varImpPlot(rf_model)

################################################################################################

# �ϥ� xgboost �|�ΤU���o�˪�SOP�A���ѷ��@�ѦҡG
# 1.�N��Ʈ榡(Data.frame)�A��xgb.DMatrix()�ഫ�� xgboost ���}���x�}
# 2.�]�wxgb.params�A�]�N�O xgboost �̭����Ѽ�
# 3.�ϥ�xgb.cv()�Atune �X�̨Ϊ��M����ƶq(nrounds)
# 4.��xgb.train()�إ߼ҫ�

# 1. �N��Ʈ榡(Data.frame)�A��`xgb.DMatrix()`�ഫ�� xgboost ���}���x
require(xgboost)
dtrain = xgb.DMatrix(data = as.matrix(train[,1:8]),
                     label = train$lpsa)
dtest = xgb.DMatrix(data = as.matrix(test[,1:8]),
                    label = test$lpsa)

# 2. �]�wxgb.params�A�]�N�O xgboost �̭����Ѽ�
xgb.params = list(
  #col����ˤ�ҡA�V�����ܨC�ʾ�ϥΪ�col�V�h�A�|�W�[�C�ʤp�𪺽�����
  colsample_bytree = 0.5,                    
  # row����ˤ�ҡA�V�����ܨC�ʾ�ϥΪ�col�V�h�A�|�W�[�C�ʤp�𪺽�����
  subsample = 0.5,                      
  booster = "gbtree",
  # �𪺳̤j�`�סA�V�����ܼҫ��i�H���o�V�`�A�ҫ������׶V��
  max_depth = 2,           
  # boosting�|�W�[�Q����������v���A�Ӧ��ѼƬO���v�����|�W�[������֡A�]���V�j�|���ҫ��U�O�u
  eta = 0.03,
  # �Υ�'mae'�]�i�H
  eval_metric = "rmse",                      
  objective = "reg:linear",
  # �V�j�A�ҫ��|�V�O�u�A�۹諸�ҫ������פ���C
  gamma = 0)               

# �ϥ�xgb.cv()���禡�A�f�t cross validation ���ޥ��A��X�̨Ϊ��M����ƶqnrounds
# 3. �ϥ�xgb.cv()�Atune �X�̨Ϊ��M����ƶq
cv.model = xgb.cv(
  params = xgb.params, 
  data = dtrain,
  nfold = 5,     # 5-fold cv
  nrounds=200,   # ����1-100�A�U�Ӿ��`�ƤU���ҫ�
  # �p�G��nrounds < 30 �ɡA�N�w�g��overfitting���p�o�͡A�����ܤ����~��tune�U�h�F�A�i�H��������                
  early_stopping_rounds = 30, 
  print_every_n = 20 # �C20�ӳ��~��ܤ@�����G�A
) 

# �e�ϡA�[�� CV �L�{��Train �� Validation ��ƪ����{(����GTrain�A�Ŧ�GValidation)
tmp = cv.model$evaluation_log
# tmp = data.frame(train_rmse_mean=cv.model$train.rmse.mean,test_rmse_mean=cv.model$test.rmse.mean)

plot(x=1:nrow(tmp), y= tmp$train_rmse_mean, col='red', xlab="nround", ylab="rmse", main="Avg.Performance in CV") 
points(x=1:nrow(tmp), y= tmp$test_rmse_mean, col='blue') 
legend("topright", pch=1, col = c("red", "blue"), 
       legend = c("Train", "Validation") )

# �p�G Train �� Validation �۪�A���ܼҫ�����٥i�H�V�m�o��n(�����)
# �i�H�եH�U�ѼơA�H�����ҫ������ת������i��
# max_depth �հ� 1 ��� (�̫�ĳ�ճo��)
# colsample_bytree�Bsubsample�հ���� (�ճo�Ӥ]����)
# eta�էC (���o�H�~�ճo��)
# gamma �էC (���o�H�~�ճo��)
# �p�G Train �� Validation �n�Ӧh�A�N���ܦ� ovrfitting�����D�o�͡A�o�ɭԤW�����ѼƴN�n�ϹL�ӽաA�H���C�ҫ������ת������Ӷi��

# �i�H�Q�Χ�cv.model����best_iteration���X��
# �۰ʧP�_�̦n�� nrounds�O�h�֡A���k�O�[�� Train �� Validation ���������{�t��
# ��o best nround
best.nrounds = cv.model$best_iteration 
best.nrounds

# 4. ��xgb.train()�إ߼ҫ�
xgb.model = xgb.train(paras = xgb.params, 
                      data = dtrain,
                      nrounds = best.nrounds) 

# �p�G�n�e�X xgb �����Ҧ��M����A�i�H�ΥH�U�禡(���]���|�ܦh�A�o�̴N���e�F)
# library("DiagrammeR")
# xgb.plot.tree(model = xgb.model)

# �w��
xgb_y = predict(xgb.model, dtest)
mean((xgb_y - test$lpsa)^2) # MSE

################################################################################################

# R Code for Stacking Implementg
# �Ĥ@���q(Stacking)
# �@�}�l�A�ڭ̧�V�m��� Train �����T��(3-folds)
# 3-folds
n = 3
n.folds = rep(1:n, each=nrow(train)/n)
train.folds = split(train, n.folds)

# �� linear regression ���ҡA���g�@�� stacking �[�c�A�åB�x�smeta-x �� meta-y����
meta.x = vector()
meta.y = list()

# 1st fold for validation
stacking.train = rbind(train.folds[[2]], train.folds[[3]])
stacking.valid = train.folds[[1]]
stacking.test = test

model_1 = lm(lpsa~., stacking.train)

tmp.meta.x = predict(model_1, stacking.valid)
tmp.meta.y = predict(model_1, stacking.test)

meta.x = c(meta.x, tmp.meta.x)
meta.y[[1]] = tmp.meta.y

# 2nd fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[3]])
stacking.valid = train.folds[[2]]
stacking.test = test

model_1 = lm(lpsa~., stacking.train)

tmp.meta.x = predict(model_1, stacking.valid)
tmp.meta.y = predict(model_1, stacking.test)

meta.x = c(meta.x, tmp.meta.x)
meta.y[[2]] = tmp.meta.y

# 3rd fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[2]])
stacking.valid = train.folds[[3]]
stacking.test = test

model_1 = lm(lpsa~., stacking.train)

tmp.meta.x = predict(model_1, stacking.valid)
tmp.meta.y = predict(model_1, stacking.test)

meta.x = c(meta.x, tmp.meta.x)
meta.y[[3]] = tmp.meta.y

# �Ĥ@�ӽu�ʼҫ��]���T������A�N�|�o��@��meta.x��meta.y
# �A�򥻨Ӫ���ڭ�(train$lpsa, test$lpsa)���X�A�����Ĥ@�Ӽҫ���X�� meta.train.1�� meta.test.1
# Average Meta.X of Test
mean.meta.y = (meta.y[[1]] + meta.y[[2]] + meta.y[[3]]) / 3

meta.train.1 = data.frame('meta.x' = meta.x, 
                          y=train$lpsa)

meta.test.1 = data.frame('mete.y' = mean.meta.y, 
                         y = test$lpsa)

# ���U�ӡA�ĤG�ҫ���support vector regression�A�]�O�@�˪��y�{
require(e1071)
meta.x = vector()
meta.y = list()

# 1st fold for validation
stacking.train = rbind(train.folds[[2]], train.folds[[3]])
stacking.valid = train.folds[[1]]
stacking.test = test

model_2 = svm(lpsa~., stacking.train)

tmp.meta.x = predict(model_2, stacking.valid)
tmp.meta.y = predict(model_2, stacking.test)

meta.x = c(meta.x, tmp.meta.x)
meta.y[[1]] = tmp.meta.y

# 2nd fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[3]])
stacking.valid = train.folds[[2]]
stacking.test = test

model_2 = svm(lpsa~., stacking.train)

tmp.meta.x = predict(model_2, stacking.valid)
tmp.meta.y = predict(model_2, stacking.test)

meta.x = c(meta.x, tmp.meta.x)
meta.y[[2]] = tmp.meta.y

# 3rd fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[2]])
stacking.valid = train.folds[[3]]
stacking.test = test

model_2 = svm(lpsa~., stacking.train)

tmp.meta.x = predict(model_2, stacking.valid)
tmp.meta.y = predict(model_2, stacking.test)

meta.x = c(meta.x, tmp.meta.x)
meta.y[[3]] = tmp.meta.y

# Average Meta.X of Test
mean.meta.y = (meta.y[[1]] + meta.y[[2]] + meta.y[[3]]) / 3

meta.train.2 = data.frame('meta.x' = meta.x, 
                          y=train$lpsa)

meta.test.2 = data.frame('mete.y' = mean.meta.y, 
                         y = test$lpsa)


# ��ӤW�����y�{�A�ĤT�Ӽҫ��h�� CART �M����
require(rpart)
meta.x = vector()
meta.y = list()

# 1st fold for validation
stacking.train = rbind(train.folds[[2]], train.folds[[3]])
stacking.valid = train.folds[[1]]
stacking.test = test

model_3 = rpart(lpsa~., stacking.train)

tmp.meta.x = predict(model_3, stacking.valid)
tmp.meta.y = predict(model_3, stacking.test)

meta.x = c(meta.x, tmp.meta.x)
meta.y[[1]] = tmp.meta.y

# 2nd fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[3]])
stacking.valid = train.folds[[2]]
stacking.test = test

model_3 = rpart(lpsa~., stacking.train)

tmp.meta.x = predict(model_3, stacking.valid)
tmp.meta.y = predict(model_3, stacking.test)

meta.x = c(meta.x, tmp.meta.x)
meta.y[[2]] = tmp.meta.y

# 3rd fold for validation
stacking.train = rbind(train.folds[[1]], train.folds[[2]])
stacking.valid = train.folds[[3]]
stacking.test = test

model_3 = rpart(lpsa~., stacking.train)

tmp.meta.x = predict(model_3, stacking.valid)
tmp.meta.y = predict(model_3, stacking.test)

meta.x = c(meta.x, tmp.meta.x)
meta.y[[3]] = tmp.meta.y

# Average Meta.X of Test
mean.meta.y = (meta.y[[1]] + meta.y[[2]] + meta.y[[3]]) / 3

meta.train.3 = data.frame(`meta.x` = meta.x, 
                          y=train$lpsa)

meta.test.3 = data.frame(`mete.y` = mean.meta.y, 
                         y = test$lpsa)


# �ĤG���q(Blending)
# �⤤���T�� Meta-Train �� �T�� Meta-Test
c(dim(meta.train.1), dim(meta.test.1))

# �n�غc�ĤG���q�� Meta-Model�A�ڳo�̮�xgboost���ҫ��Ӱ�

### Meta- Model Construction 
# ����T�� Meta-Train�X�֤@�_�G
big.meta.train = rbind(meta.train.1, meta.train.2, meta.train.3)

# �ഫ�� xgboost ���榡
dtrain = xgb.DMatrix(data = as.matrix(big.meta.train[,1]), label = big.meta.train[, 2])

# �V�m XGboost �ҫ�
# �䤤xgb.params �������ĤG�`(Boosting)���]�w
# ²��� nrounds = 100
xgb.model = xgb.train(paras = xgb.params, data = dtrain, nrounds = 100) 

# ��T�� Meta-Test�i��w���G
dtest.1 = xgb.DMatrix(data = as.matrix(meta.test.1[,1]), label = meta.test.1[, 2])
final_1 = predict(xgb.model, dtest.1)

dtest.2 = xgb.DMatrix(data = as.matrix(meta.test.2[,1]), label = meta.test.2[, 2])
final_2 = predict(xgb.model, dtest.2)

dtest.3 = xgb.DMatrix(data = as.matrix(meta.test.3[,1]), label = meta.test.3[, 2])
final_3 = predict(xgb.model, dtest.3)

# ��T�յ��G�����_�ӡA�M��� MSE
final_y = (final_1 + final_2 + final_3)/3
mean((final_y - test$lpsa)^2) # MSE

























