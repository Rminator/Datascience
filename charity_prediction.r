dataSet <- read.csv("/Users/ngaonkar/Downloads/charity.csv",header=TRUE)

train <- subset(dataSet,part=="train")
test<-subset(dataSet,part=="test")
valid<-subset(dataSet,part=="valid")

names(which(sapply(.GlobalEnv, is.data.frame))) # list of dataframes in the workspace


# load the data
charity <- read.csv(file.choose()) # load the "charity.csv" file

# predictor transformations

charity.t <- charity
charity.t$avhv <- log(charity.t$avhv)
# add further transformations if desired
# for example, some statistical methods can struggle when predictors are highly skewed

# set up data for analysis

data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,2:21]
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:21]

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)



# logistic regression

logistic_model1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c, family=binomial("logit"))

#AIC 2189
logistic_model1 <- glm(donr ~ reg1 + reg2  + home + chld  + I(hinc^2)  + wrat + 
                  + incm + plow + npro + genf + tgif + tdon + tlag , 
                  data.train.std.c, family=binomial("logit"))


#backward step elimination

step(lm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
data=data.train.std.c),direction="backward")

logistic_model2 <- glm(donr ~ reg1 + reg2 + home + chld + hinc + I(hinc^2) + 
genf + wrat + incm + plow + npro + tgif + tdon + tlag,
data.train.std.c, family=binomial("logit"))

##forward direction profit 11646.5

step(lm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
data=data.train.std.c),direction="forward")

logistic_model3 <- glm(donr ~ summaryreg1 + reg2 + reg3 + reg4 + home + chld + 
hinc + I(hinc^2) + genf + wrat + avhv + incm + inca + plow + 
npro + tgif + lgif + rgif + tdon + tlag + agif, data.train.std.c, family=binomial("logit"))

#both

step(lm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat +
avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
data=data.train.std.c),direction="both")

logistic_model4 <- glm(donr ~ reg1 + reg2 + home + chld + hinc + I(hinc^2) + 
    genf + wrat + incm + plow + npro + tgif + tdon + tlag, data.train.std.c, family=binomial("logit"))


#Nearest neighbours

library(class)
 set.seed(1)
 post.valid.knn=knn(x.train.std,x.valid.std,c.train,k=10)
 profit.knn <- cumsum(14.5*c.valid[order(post.valid.knn, decreasing=T)]-2)
 n.mail.valid <- which.max(profit.knn)
 c(n.mail.valid, max(profit.knn))
 table(post.valid.knn,c.valid)
 
 > c(n.mail.valid, max(profit.knn))
 [1]  1247 11284
 > table(post.valid.knn,c.valid)
 c.valid
 post.valid.knn   0   1
 0 711  47
 1 308 952
 > 1-mean(post.valid.knn==c.valid)
 [1] 0.1759167

#classification trees:
# this gives around 84% accuracy
 library(tree)
 attach(data.train.std.c)
 donr_class=ifelse(donr==1,"yes","no")
 data.train.std.c.tree=data.frame(data.train.std.c,donr_class)
 tree.model=tree(donr_class~.-donr,data.train.std.c.tree)
 
 #validation
 donr_class_valid=ifelse(data.valid.std.c$donr==1,"yes","no")
 data.valid.std.c.tree=data.frame(data.valid.std.c,donr_class_valid)
 summary(tree.model)
 #prediction
 model_pred=predict(tree.model,data.valid.std.c.tree,type="class")
 table(model_pred ,donr_class_valid)
 
 ## cross validation
 
cv.donr =cv.tree(tree.model,FUN=prune.misclass)
names(cv.donr)
cv.donr
#prune
prune.donr=prune.misclass(tree.model,best=5)
tree.pred=predict(prune.donr,data.valid.std.c.tree,type="class")
table(tree.pred ,donr_class_valid)

#regression trees
#please work on tuning this model

reg.tree=tree(damt ~ .,data.valid.std.y)
summary(reg.tree)
plot(reg.tree)
text(reg.tree,pretty=0)
cv.reg=cv.tree(reg.tree)
plot(cv.reg$size ,cv.reg$dev ,type='b')
mean((yhat-data.valid.std.y$damt)^2)
yhat.test.tree <- predict(reg.tree, newdata = data.test.std)



#classification model: bagging and random forest
set.seed(1)
library(randomForest)
library(tree)
attach(data.train.std.c)
donr_class=ifelse(donr==1,"yes","no")
data.train.std.c.tree=data.frame(data.train.std.c,donr_class)
bag.model.class=randomForest(donr_class~.-donr,data=data.train.std.c.tree,mtry=20,importance=TRUE)
bag.model.class

#validation
donr_class_valid=ifelse(data.valid.std.c$donr==1,"yes","no")
data.valid.std.c.tree=data.frame(data.valid.std.c,donr_class_valid)
summary(bag.model.class)
#prediction
set.seed(1)
model_pred_bag_class=predict(bag.model.class,data.valid.std.c.tree,type="class")
table(model_pred_bag_class,donr_class_valid)

##results
#table(model_pred_bag_class,donr_class_valid)
#donr_class_valid
#model_pred_bag_class  no yes
##no  887  93
#yes 132 906
#> 1793/2018
#[1] 0.8885035


#classification RAndom forest:

set.seed(1)
library(randomForest)
library(tree)
rf.model.class=randomForest(donr_class~.-donr,data=data.train.std.c.tree,importance=TRUE)
rf.model.class

#prediction
set.seed(1)
model_pred_rf_class=predict(rf.model.class,data.valid.std.c.tree,type="class")
table(model_pred_rf_class,donr_class_valid)

#results:
#table(model_pred_rf_class,donr_class_valid)
#donr_class_valid
#model_pred_rf_class  no yes
#no  881  83
#yes 138 916
#> 1797/2018
#[1] 0.8904856



#predictive model/regression: bagging and random forest
library(randomForest)
bag.model=randomForest(damt~.,data=data.train.std.y,mtry=20,importance=TRUE)
bag.model
yhat.bag = predict(bag.model,newdata=data.valid.std.y)
mean((yhat.bag-data.valid.std.y$damt)^2)
yhat.test.bag <- predict(bag.model, newdata = data.test.std)
yhat.test
length(yhat.test)

#random forest
rf.model=randomForest(damt~.,data=data.train.std.y,importance=TRUE)
rf.model
yhat.rf=predict(rf.model,newdata=data.valid.std.y)
mean((yhat.rf-data.valid.std.y$damt)^2)
yhat.test.rf <- predict(rf.model, newdata = data.test.std)


#boosting
#gaussian distribution for regression and bernoulli distribution for the clssification problem
#tuning required
library(gbm)
boost.model=gbm(damt~.,data=data.train.std.y,distribution = "gaussian",n.trees=5000,interaction.depth =4)
yhat.boost=predict(boost.model,newdata=data.valid.std.y, n.trees=5000)
mean((yhat.boost-data.valid.std.y$damt)^2)
yhat.test.boost <- predict(boost.model, newdata = data.test.std,n.trees=5000)






