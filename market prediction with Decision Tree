library(quantmod); library(rpart); library(rpart.plot); library(rvest);library(caret)

'%=%' = zeallot::`%<-%`

#data preparaiton
{
  initial_equity = 100000
  symbol = 'PNB.NS'
  data = getSymbols(
    Symbols = symbol,
    from = '2010-01-01',
    auto.assign = FALSE
  )
  
  colnames(data) = c(
    "Open","High","Low","Close","Volume","Adjusted"
  )
  
  data = na.omit(data)
  closeprice = Cl(data)
  
  
  #RSI
  rsi = closeprice %>% 
    RSI(.,n=14,maType = "WMA") %>% 
    round(.,1)
  
  rsi = c(NA,head(rsi,-1))
  
  #SMA 
  sma = closeprice %>% 
    SMA(.,20) %>% 
    round(.,1)
  sma = c(NA,head(sma,-1))
  
  #LMA
  lma = closeprice %>% 
    SMA(.,50) %>% 
    round(.,1)
  lma = c(NA,head(lma,-1))
  
  data22 = ADX(data[,c("High","Low","Close")]) %>% 
    as.data.frame()
  
  adx = data22$ADX %>% round(.,1)
  adx = c(NA,head(adx,-1))
  
  data$Return = data$Close %>% dailyReturn(.,type = 'arithmetic') %>% 
    round(.,2)
  colnames(data) = c("Open","High","Low","Close","Volume","Adj","Return")
  
  class = nrow(data) %>% character()
  class = ifelse(coredata(data$Return) >=0, "up","down")
  
  data2 = data.frame(data,class,rsi,sma,lma,adx)
  data = data.frame(class,rsi,sma,lma,adx) %>% na.omit()
  
}

#
df = data
colnames(df) = c("Class","RSI","SMA","LMA","ADX")

idx = createDataPartition(df$Class, list = F, p=0.8)
c(train,test) %=% list(
  df[idx,],df[-idx,])

DecisionTree = rpart(
  Class~., data = train, cp=0.001
)
prp(DecisionTree,type = 2, extra = 8)
rpart.plot(DecisionTree, type= 2,
           clip.right.labs = FALSE, under = TRUE)
rpart.rules(DecisionTree,cover = TRUE) %>% 
  View()


table(predict(DecisionTree,test,type = 'class'),
      test[,5],dnn=list('predicted','actual'))


predict(DecisionTree,test[,-1])
