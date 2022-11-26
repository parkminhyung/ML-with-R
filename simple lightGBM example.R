library(lightgbm); library('caret'); library(dplyr); library(plotly)
set.seed(12)
'%=%' = zeallot::`%<-%`

boston = MASS::Boston
dim(boston)

#split train and test sets
indexes = createDataPartition(boston$medv, list = FALSE, p = .85)
c(train,test) %=% list(boston[indexes,], boston[-indexes,])


## scale function is a function standardizing data in data frame.
## Y = X + ... x data is factors and y data is label
c(train_x, train_y) %=% list(train[,-14] %>% scale(.) %>% .[,],
                             train[,14])

c(test_x, test_y) %=% list(test[,-14] %>% scale(.) %>% .[,],
                           test[,14])


dtrain = lgb.Dataset(train_x,
                     label = train_y)
dtest  =lgb.Dataset.create.valid(dtrain,
                                 test_x,
                                 label = test_y)

#rendering model and prediction

params = list(
  objective = "regression", 
  metric = "l2",
  min_data = 1L,
  learning_rate = .3
)

# validataion data
valids = list(test = dtest)

model = lgb.train(
  params = params,
  data = dtrain,
  nrounds = 5L,
  valids = valids
)

#model evalutaion
lgb.get.eval.result(model, 'test', '12')

#prediction
pred = predict(model,test_x)
pred

#check accuracy 
MSE = mean((test_y-pred)^2)
MAE = MAE(test_y,pred)
RMSE = RMSE(test_y,pred)

cat("MSE: ", MSE,"\n",
    "MAE: ", MAE,"\n",
    "RMSE: ", RMSE,"\n")

df.result = data.frame(test_y,pred)
df.result %>% plot_ly(
  x = 1:nrow(df.result),
  y = df.result[,1],
  mode = 'line',
  type = 'scatter', 
  name = "actual value"
) %>% 
  add_lines(y= df.result[,2],
            name = "prediction",
            mode='line')
