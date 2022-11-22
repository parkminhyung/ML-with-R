library(quantmod); library(TTR); library(caret); library(corrplot); library(corrplot); library(pROC); library(FSelector);library(dplyr)
'%=%' = zeallot::`%<-%`

set.seed(5)
c(df_stock,df_index) %=% list(
  getSymbols(Symbols = 'AAPL',
             from = '2015-01-01',
             to = '2021-01-01',
             auto.assign = TRUE) %>% 
    get() %>% 
    na.omit(),
  getSymbols(Symbols = '^IXIC',
             from = '2015-01-01',
             to = '2021-01-01',
             src = 'yahoo',
             auto.assign = FALSE) %>% na.omit()
)

price = df_stock$AAPL.Close - df_stock$AAPL.Open
class = ifelse(price > 0, "UP", "DOWN")

#Force index indicator
forceindex = (df_stock$AAPL.Close - df_stock$AAPL.Open)*df_stock$AAPL.Volume ; forceindex = c(NA,head(forceindex,-1))

# buy and sell signal indicators williams R% and RSI
c(hlc,cl,hlcidx,clidx) %=% list(df_stock %>% HLC(),
                                df_stock %>% Cl(),
                                df_index %>% HLC(),
                                df_index %>% Cl())

c(willR5,willR10,willR15) %=% list(hlc %>% WPR(.,n=5) %>% head(.,-1) %>% c(NA,.),
                                   hlc %>% WPR(.,n=10) %>% head(.,-1) %>% c(NA,.),
                                   hlc %>% WPR(.,n=15) %>% head(.,-1) %>% c(NA,.))

c(ATR5,ATR10) %=% list(hlc %>% ATR(.,n=5,maType = "WMA") %>% .[,1] %>% head(.,-1) %>% c(NA,.),
                       hlc %>% ATR(.,n=10,maType = "WMA") %>% .[,1] %>% head(.,-1) %>% c(NA,.))

c(ATR5indx,ATR10indx) %=% list(hlcidx %>% ATR(.,n=5,maType = "WMA") %>% .[,1] %>% head(.,-1) %>% c(NA,.),
                               hlcidx %>% ATR(.,n=10,maType = "WMA") %>% .[,1] %>% head(.,-1) %>% c(NA,.))

c(RSI5,RSI10,RSI15) %=% list(cl %>% RSI(.,n=5) %>% head(.,-1) %>% c(NA,.),
                             cl %>% RSI(.,n=10) %>% head(.,-1) %>% c(NA,.),
                             cl %>% RSI(.,n=15) %>% head(.,-1) %>% c(NA,.))

c(ROC5,ROC10) %=% list(ROC(cl,n=5,type = "discrete")*100,
                       ROC(cl,n=10,type = "discrete")*100)

c(ROC5,ROC10) %=% list(c(NA,head(ROC5,-1)),
                       c(NA,head(ROC10,-1)))

c(MOM5,MOM10) %=% list(cl %>% momentum(.,n=5,na.pad = TRUE)  %>% head(.,-1) %>% c(NA,.),
                       cl %>% momentum(.,n=10,na.pad = TRUE)  %>% head(.,-1) %>% c(NA,.))

c(MOM5indx,MOM10indx) %=% list(clidx %>% momentum(.,n=5,na.pad = TRUE) %>% head(.,-1) %>% c(NA,.),
                               clidx %>% momentum(.,n=10,na.pad = TRUE) %>% head(.,-1) %>% c(NA,.))

dataset = data.frame(class,forceindex,willR5,willR10,willR15,RSI5,RSI10,RSI15,ROC5,ROC10,MOM5,MOM10,
                     ATR5,ATR10,MOM5indx,MOM10indx,ATR5indx,ATR10indx) %>% na.omit()
colnames(dataset)[1] = "class"

dim(dataset) # dimension of dataframe

y = dataset[1] #class Up and down
cbind(freq = table(y),percentage = prop.table(table(y))*100)
summary(dataset)

#visualizing the dataset using a correlation matrix
correlations = cor(dataset[,-1])
corrplot(correlations)

#select features using the random forest, importance function from the FSelector package
set.seed(5)
weights = random.forest.importance(class~.,dataset,importance.type = 1)
weights

#use 'cutoff.k' function which provide k features with the highest importance value
subset = cutoff.k(weights,10)
subset

#recreate a dataframe using the selected features
dataset_rf = data.frame(dataset[1],dataset[subset]) %>% na.omit()

#split train and test sets
idx = createDataPartition(dataset_rf$class,list=F,p=0.8)

c(trainsets,testsets) %=% list(
  dataset_rf[idx,],dataset_rf[-idx,]
)

#resampling method used 10-fold-cross validation with "ACCuracy" as the model evaluation metrics
set.seed(5)
c(trainControl, metric) %=% list(
  trainControl(method = 'cv',number=10),
  "Accuracy"
)

#K-nearest Neighbors
fit.knn = train(
  class~., 
  data = trainsets,
  method = "knn",
  metric = metric,
  preProc = c("range"),
  trControl = trainControl
)

#Classification and Regression Trees(CARTS)
fit.cart = train(
  class~., 
  data = trainsets,
  method = "rpart",
  metric = metric,
  preProc = c("range"),
  trControl = trainControl
)

#Naive Bayes (NB)
fit.nb = train(
  class~., 
  data = trainsets,
  method = "nb",
  metric = metric,
  preProc = c("range"),
  trControl = trainControl
)

#Support Vector Machine with Radial Basis Funciton (SVM)
fit.svm = train(
  class~., 
  data = trainsets,
  method = "svmRadial",
  metric = metric,
  preProc = c("range"),
  trControl = trainControl
)

#Evaluating the algorithms using the "ACCURACY" metric
results = resamples(list(KNN = fit.knn,
                         CART = fit.cart,
                         NB = fit.nb,
                         SVM = fit.svm))
summary(results)
dotplot(results)


#For "Accuracy", NB model shows the worst performance among models so we will select "NB"model and try to improve its accuracy by tuning the parameters,
grid = expand.grid(.fL=seq(0,by=0.5),.usekernel=c(TRUE),.adjust =seq(0,by=0.5))
fit.nb = train(
  class~., 
  data = trainsets,
  method = "nb",
  metric = metric,
  tuneGrid = grid,
  preProc = c("range"),
  trControl = trainControl
)

results = resamples(list(KNN = fit.knn,
                         CART = fit.cart,
                         NB = fit.nb,
                         SVM = fit.svm))
summary(results)
dotplot(results)

#Improving KNN model 
grid = expand.grid(.k=seq(1,10,by=1))
fit.knn = train(
  class~., 
  data = trainsets,
  method = "knn",
  metric = metric,
  tuneGrid = grid,
  preProc = c("range"),
  trControl = trainControl
)


'%+%' = paste0

### Predict Up or Down with machinelearning models and compute accuracy 

#KNN accuracy
df_result = cbind(testsets[1],predict(
  fit.knn,
  newdata = testsets[,-1]
))
total = df_result %>% nrow()
acc.knn = (which(df_result[1]==df_result[2]) %>%
             length())/total

#accuracy CART
df_result = cbind(testsets[1],predict(
  fit.cart,
  newdata = testsets[,-1]
))
total = df_result %>% nrow()
acc.cart = (which(df_result[1]==df_result[2]) %>%
              length())/total

#accuracy NB
df_result = cbind(testsets[1],predict(
  fit.nb,
  newdata = testsets[,-1]
))
total = df_result %>% nrow()
acc.nb = (which(df_result[1]==df_result[2]) %>%
            length())/total

#accuracy SVM
df_result = cbind(testsets[1],predict(
  fit.svm,
  newdata = testsets[,-1]
))
#accuracy
total = df_result %>% nrow()
acc.svm = (which(df_result[1]==df_result[2]) %>%
             length())/total


cat("\n",
    "KNN Accuracy :" %+% round(acc.knn*100,digits=4) %+% "%","\n",
    "CART Accuracy :" %+% round(acc.cart*100,digits=4) %+% "%","\n",
    "NB Accuracy :" %+% round(acc.nb*100,digits=4) %+% "%","\n",
    "SVM : Accuracy :" %+% round(acc.svm*100,digits=4) %+% "%","\n"
)
