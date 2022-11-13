
library(caret, quietly=T); library(xgboost)

set.seed(42)
df <- data.frame(
  Hours = c(0.50,0.75,1.00,1.25,1.50,1.75,1.75,2.00,2.25,2.50,2.75,3.00,3.25,3.50,4.00,4.25,4.50,4.75,5.00,5.50),
  Pass = c(0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1)
)

'%=%' = zeallot::`%<-%`

idx <- createDataPartition(df$Pass, list=F, p=0.8)
c(train,test) %=% list(df[idx,],df[-idx,])

#split train and test data
c(train.data, test.data) %=% list(
  as.matrix(train[,names(df)!="Pass"]),
  as.matrix(test[,names(df)!="Pass"])
)

#split lables
c(train.label, test.label) %=% list(train$Pass, test$Pass)

#rendering model
model = xgboost(
  data = train.data,
  label = train.label,
  max.depth = 2,
  eta = 0.5,
  nthread=2,
  nrounds=2
)

#test
pred = predict(model,test.data) 
pred = ifelse(pred > 0.5,1,0)

#accuracy
sum(pred==test.label)/length(pred)




