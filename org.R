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

x_train=model.matrix(card~.,train)[,-1]


#�p�����ܼƴX�Ӥ���
# m = nlevels(new_train$LOSS_RATE)
m= 2
Y = as.integer(train$card)-1

# xgboost �ѼƳ]�w (xgboost parameters setup)
param = list("objective" = "multi:softprob",
             "eval_metric" = "mlogloss",
             "num_class" = m,
             eta=0.3,
             gamma=0.2
)
print(paste(Sys.time(), "�}�l�ؼ�"))
# �i��ؼ� nrounds���|�N���� �i�H�W�[ �����]���|�����
result = xgboost(param=param, data=x_train, label=Y, nrounds=150)

x_test=model.matrix(card~.,test)[,-1]

Ypred_LH = predict(result,x_test)
Ypred_LH = t(matrix(Ypred_LH,m,length(Ypred_LH)/m))
Ypred_LH = levels(test$card)[max.col(Ypred_LH)]

t0 = table(test$card,Ypred_LH,dnn = c("���", "�w��"))
t0
l<-c(1:2)
TESTACC <- sum(diag(t0)) / sum(t0)
TESTACCH <-(diag(t0)[l]/rowSums(t0)[l])[1]
TESTACC
diag(t0)[l]/rowSums(t0)[l]

sparse.model.matrix(card~.,test)[,-1]


