library(quantmod); library(TTR); library(xgboost)

'%=%' = zeallot::`%<-%`
'%+%' = paste0

#preparing data
aapl = getSymbols(
  Symbols = "AAPL",
  from='2020-01-01',
  auto.assign = TRUE
) %>% get() 

price = aapl[,6]

#RSI
rsi = RSI(price,n=14,maType = "WMA") 
adx = aapl %>% HLC() %>% ADX(.,)  
sar = aapl %>% HLC() %>% SAR(.,)  
trend = aapl[,6]  - sar

pr = (aapl$AAPL.Adjusted - aapl$AAPL.Open) 
class = ifelse(pr > 0, 1, 0)

model_df = data.frame(class, rsi, adx$ADX, trend)
model = model_df %>% as.matrix(.,nrow = length(class))
model = na.omit(model)
colnames(model) = c("class","rsi","adx","trend")


#split data 
train_size = 2/3
breakpoint = nrow(model) * train_size

c(training_data,test_data) %=% list(model[1:breakpoint,],
                                    model[(breakpoint+1):nrow(model),])

c(x_train,y_train) %=% list(training_data[,2:4],
                            training_data[,1])

c(x_test,y_test) %=% list(test_data[,2:4],
                          test_data[,1])

#trainig datasets using XGBoost
dtrain = xgb.DMatrix(
  data = x_train, label = y_train
)

xgModel = xgboost(
  data = dtrain, nrounds = 5,
  objective = "binary:logistic"
)

#prediction
pred = (predict(xgModel,x_test) >.5 ) %>% as.numeric()

#using cross validation
dtrain = xgb.DMatrix(
  data= x_train, label = y_train
)

cv = xgb.cv(
  data = dtrain,
  nrounds = 10,
  nfold = 5,
  objective = "binary:logistic"
)

#error valuation
err_value = mean(pred != y_test)
('Test Error : ' %+% err_value) %>% print()

#View feature importance from the learnt model
importance_matrix = xgb.importance(model = xgModel)
importance_matrix

#View the trees from a model
xgb.plot.tree(model = xgModel) #require DiagrammeR package
xgb.plot.tree(model = xgModel,n_first_tree = 1)

