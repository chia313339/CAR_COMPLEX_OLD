# 載入creditcard資料集(包含1,319筆觀察測試，共有12個變數)
data(CreditCard)
#觀察資料欄位
head(CreditCard)
CreditCard <- subset(CreditCard, select = c(card, reports, age, income, owner,months))

# 先把資料區分成 train=0.8, test=0.2 
set.seed(22)
train.index <- sample(x=1:nrow(CreditCard), size=ceiling(0.8*nrow(CreditCard) ))

train = CreditCard[train.index, ]
test = CreditCard[-train.index, ]

x_train=model.matrix(card~.,train)[,-1]


#計算應變數幾個元素
# m = nlevels(new_train$LOSS_RATE)
m= 2
Y = as.integer(train$card)-1

# xgboost 參數設定 (xgboost parameters setup)
param = list("objective" = "multi:softprob",
             "eval_metric" = "mlogloss",
             "num_class" = m,
             eta=0.3,
             gamma=0.2
)
print(paste(Sys.time(), "開始建模"))
# 進行建模 nrounds為疊代次數 可以增加 本機跑的會比較快
result = xgboost(param=param, data=x_train, label=Y, nrounds=150)

x_test=model.matrix(card~.,test)[,-1]

Ypred_LH = predict(result,x_test)
Ypred_LH = t(matrix(Ypred_LH,m,length(Ypred_LH)/m))
Ypred_LH = levels(test$card)[max.col(Ypred_LH)]

t0 = table(test$card,Ypred_LH,dnn = c("實際", "預測"))
t0
l<-c(1:2)
TESTACC <- sum(diag(t0)) / sum(t0)
TESTACCH <-(diag(t0)[l]/rowSums(t0)[l])[1]
TESTACC
diag(t0)[l]/rowSums(t0)[l]

sparse.model.matrix(card~.,test)[,-1]



