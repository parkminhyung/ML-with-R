library(caret, quietly = T); library(rpart)
'%=%' = zeallot::`%<-%`

df = data.frame(
  Hours = c(0.50,0.75,1.00,1.25,1.50,1.75,1.75,2.00,2.25,2.50,2.75,3.00,3.25,3.50,4.00,4.25,4.50,4.75,5.00,5.50),
  Pass = c(0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1)
)

idx = createDataPartition(df$Pass, list=F, p=0.8)
c(train,test) %=% list(df[idx,],df[-idx,])
model = rpart(Pass ~., data = train)

options(echo=T)

model

#test
test$pass_predicted = ifelse(predict(model,test) > 0.5,1,0)

#accuracy
sum(test$Pass==test$Pass_predicted)/nrow(test)

#plot
rpart.plot::prp(model,type = 4, extra =3)
