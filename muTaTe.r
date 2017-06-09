# bonus assigment2
# install.packages("caret")
# install.packages("lattice")
# install.packages("ggplot2")
# 

#### Below part of code loads the required libraries and laods the source data required for analysis ####
library(dplyr)
library(dummies)
library(caret)
data("GermanCredit")
attach(GermanCredit)
GC <- data.frame(GermanCredit)
head(GC)
nrow(GC)
dim(GC)
new_data<- dummy.data.frame(GC,names=c("Class"))

### Below is the function to find the missing data ###

missing_func<- function(df){
  col <- NCOL(GC)
  rows<- nrow(GC)
  var<- c()
  missing_var<- nrow(GC)
  
  for (i in 1:col) {
    var[i] <- colnames(df[i])
    missing_var[i] <- sum(is.na(df[i]))
    
  }
  dframe <- data.frame(var, missing_var)
  dframe
}

#### There is no missing data in our dataset####

missing_func(GC)


## Below function is used to remove the columns containing only zeros in them #####

remove_zero_cols <- function(df) {
  rem_vec <- NULL
  for(i in 1:ncol(df)){
    this_sum <- summary(df[,i])
    zero_test <- length(which(this_sum == 0))
    if(zero_test == 6) {
      rem_vec[i] <- names(df)[i]
    }
  }
  features_to_remove <- rem_vec[!is.na(rem_vec)]
  rem_ind <- which(names(df) %in% features_to_remove)
  df <- df[,-rem_ind]
  return(df)
}

## running the above function to remove columns with only zeros###
new_data <- remove_zero_cols(GC)


###convert the coloumns into numeric for PCA#####

new_data<- dummy.data.frame(new_data,names=c("Class"))

####dividing data into test(holdout) and train based on the given requirement ####

set.seed(123)
indexes = sample(1:nrow(new_data), size=0.368*nrow(GC))
test = new_data[indexes,]
train=new_data[-indexes,]
dim(test)
dim(train)
indexes

### Running PCA on the data#######


pca_train<- subset(train,select =-c(Amount))
prin_comp<- prcomp(pca_train,scale.=T)
names(prin_comp)
biplot(prin_comp,scale=0)
std_dev<- prin_comp$sdev
pr_var<- std_dev^2
prop_varex<-pr_var/sum(pr_var)
par(family="mono")
par(family="mono")
plot(prop_varex, xlab = "Principal Component",ylab = "Proportion of Variance Explained",type = "b")
plot(cumsum(prop_varex), xlab = "Principal Component",ylab = "Cumulative Proportion of Variance Explained",type = "b")
train.data <- data.frame(Amount = train$Amount, prin_comp$x)
data<-prin_comp$x

############# stepwise backward and forward selection ######

model_full<- lm (train$Amount~.,data=train)
summary(model_full)
model.aic.backward <- step(model_full, direction = "backward", trace = 1)
summary(model.aic.backward)

model.aic.forward <- step(model_full, direction = "forward", trace = 1)
summary(model.aic.forward)

model.aic.both <- step(model_full, direction = "both", trace = 1)
summary(model.aic.both)

model_lm<- lm(formula = train$Amount ~ Duration + InstallmentRatePercentage + Telephone + Job.Management.SelfEmp.HighlyQualified 
            +     Personal.Male.Single + CreditHistory.NoCredit.AllPaid ,data = train)

summary(model_lm)




####### now 50 PCA components almost explains more than 90 % of the variance####
train.data <- train.data[,1:51]
model1<- lm(Amount~.,data=train.data)
summary(model1)  ##r2=0.5932


######## running model on the holdout data##########
pca_test<- subset(test,select =-c(Amount))
test.data <- predict(prin_comp, newdata = pca_test)
test.data <- as.data.frame(test.data)
test.data <- test.data[,1:51]
linear.prediction <- predict(model1, test.data)
linear.prediction
cor(test$Amount,linear.prediction)^2  ## 0.5517428
sqrt(mean((test$Amount - linear.prediction)^2)) ## 1759.787


###### now using lasso regression to identify the best predictors##

library(glmnet)
grid <- 10 ^ seq(from = 10, to = -2, length = 100) 
x <- model.matrix(Amount~., data = GC)[, -2]
y <- GC$Amount

set.seed(123)
train_lasso <- sample(nrow(x), size =  round(0.632 * nrow(x)))
y.test <- y[-train_lasso]

set.seed(1)
lasso.cv <- cv.glmnet(x[train_lasso, ], y[train_lasso], alpha = 1, lambda = grid)
plot(lasso.cv)
bestlam <- lasso.cv$lambda.min
bestlam
lasso.cv$nzero
lasso.cv$glmnet.fit

lasso.pred <- predict(object = lasso.cv, s = bestlam, newx = x[-train_lasso, ])
mean((y.test - lasso.pred)^2) # MSE
sqrt(mean((y.test - lasso.pred)^2)) # residual standard error
cor(y.test, lasso.pred)^2 # r2holdout 



###### Mutate

intercept <- c()
Duration <- c()
InstallmentRatePercentage <- c()
Telephone<-c()
Job.Management.SelfEmp.HighlyQualified <- c()
Personal.Male.Single <- c()
CreditHistory.NoCredit.AllPaid <- c()
rsquared.train <- c()
rsquared.holdout <- c()  

# create 1000 separate OLS fits using different 63.32/36.8 training/validation sets.
for (i in 1:1000) {
  
  # create reproducible, randomly sampled training vs validation data.
  set.seed(i + 1)
  indices <- sample(nrow(GC), size = round(0.632 * nrow(GC)))
  data.train <- GC[indices, ]
  data.holdout <- GC[-indices, ] 
  
  # linear regression model fit with training data. Predictors chosen from prior EDA models.
  model_lm<- lm(formula = Amount ~ Duration + InstallmentRatePercentage + Telephone + Job.Management.SelfEmp.HighlyQualified 
                +     Personal.Male.Single + CreditHistory.NoCredit.AllPaid ,data = data.train)
  
  
  # capture fitted coefficient values.
  coeff <- coefficients(model_lm)
  intercept[i] <- coeff[1]
  Duration[i] <- coeff[2]
  InstallmentRatePercentage[i] <- coeff[3]
  Telephone[i] <- coeff[4]
  Job.Management.SelfEmp.HighlyQualified[i] <- coeff[5]
  Personal.Male.Single[i]<-coeff[6]
  CreditHistory.NoCredit.AllPaid<-coeff[7]
  
  
  # capture r-squared from training
  rsquared.train[i] <- summary(model_lm)$r.squared
  
  # predict model_lm on unseen records, store predictions.
  predicted.amount <- predict(model_lm,newdata=data.holdout)
  # calculate the r-squared on holdout data.
  # square the correlation between actual vs predicted amount.
  rsquared.holdout[i] <- cor(data.holdout$Amount, predicted.amount)^2
  
} 


##get all the output in the dataframe

results_mutate <- data.frame(model_number = rep(1:1000), rsquared.train, rsquared.holdout,intercept, 
                            + Duration, InstallmentRatePercentage,Telephone,Job.Management.SelfEmp.HighlyQualified,
                            + Personal.Male.Single, CreditHistory.NoCredit.AllPaid)



stats_mutate <- results_mutate %>%
summarise(intercept.mean.coef = mean(intercept),
            intercept.sd.coef = sd(intercept),
            Duration.mean.coef = mean(Duration),
            Duration.sd.coef = sd(Duration),
            InstallmentRatePercentage.mean.coef = mean(InstallmentRatePercentage),
            InstallmentRatePercentage.sd.coef = sd(InstallmentRatePercentage),
            Telephone.mean.coef = mean(Telephone),
            Telephone.sd.coef = sd(Telephone),
            Job.Management.SelfEmp.HighlyQualified.mean.coef = mean(Job.Management.SelfEmp.HighlyQualified),
            Job.Management.SelfEmp.HighlyQualified.sd.coef = sd(Job.Management.SelfEmp.HighlyQualified),
            Personal.Male.Single.mean.coef=mean(Personal.Male.Single),
            Personal.Male.Single.sd.coef=sd(Personal.Male.Single),
            CreditHistory.NoCredit.AllPaid.mean.coef=mean(CreditHistory.NoCredit.AllPaid),
            CreditHistory.NoCredit.AllPaid.mean.coef=sd(CreditHistory.NoCredit.AllPaid),
            rsquared.train.mean = mean(rsquared.train),
            rsquared.train.sd = sd(rsquared.train),
            rsquared.holdout.mean = mean(rsquared.holdout),
            rsquared.holdout.sd = sd(rsquared.holdout),
            rsquared.difference.mean = mean(rsquared.holdout.mean - rsquared.train.mean)
  )


#########


coef.means <- rbind(stats_mutate[, c(1,3,5,7,9)])
coef.sd <- rbind(stats_mutate[, c(2,4,6,8,10)])

reshape.coef.stats <- data.frame(coefficient = c("intercept", "Duration", 
                                                 "Job.Management.SelfEmp.HighlyQualified", 
                                                 "InstallmentRatePercentage",
                                                 "Purpose.UsedCar"),
                                 coef.means = as.numeric(coef.means), coef.sd = as.numeric(coef.sd))
reshape.coef.stats



######plots of the result

## distribution of the test raquared values

par(mfrow = c(2,1))
# distribution of test Rsquared.
hist(results_mutate$rsquared.holdout, xlab = "R-Squared Holdout", col = "grey",
     main = "Distribution of R-Squared values from Holdout Data")
abline(v = results_mutate$rsquared.holdout.mean, lty = "dashed", lwd = "3", col = "blue")  



hist(results_mutate$rsquared.holdout - results_mutate$rsquared.train, xlab = "R-Squared % Decrease",
     col = "grey", main = "Distribution of R-Squared % Decrease in Holdout")
abline(v = results_mutate$rsquared.difference.mean, lty = "dashed", lwd = "3", col = "blue")  



# distribution of coefficient

hist(results_mutate$intercept, col = "grey", xlab = "Intercept",
     main = paste("Coefficient Distribution of", "Intercept"))
abline(v = results_mutate$intercept.mean.coef, lty = "dashed", lwd = "3", col = "blue")  

# distribution of duration predictor coef.
hist(results_mutate$X.Duration, col = "grey", main = paste("Coefficient Distribution of", "Duration"),
     xlab = "Duration")
abline(v = results_mutate$X.Duration.mean.coef, lty = "dashed", lwd = "3", col = "blue")  

##installement rate percentage

hist(results_mutate$InstallmentRatePercentage, col = "grey",
     main = paste("Coefficient Distribution of", "InstallmentRatePercentage"),
     xlab = "InstallmentRatePercentage")
abline(v = results_mutate$InstallmentRatePercentage.mean.coef, lty = "dashed", lwd = "3", 
       col = "blue") 


# distribution of Job.Management.SelfEmp.HighlyQualified predictor coef.

graphics.off() 
par("mar") 
par(mar=c(1,1,1,1))
hist(results_mutate$Job.Management.SelfEmp.HighlyQualified, col = "grey",
     main = paste("Coefficient Distribution of", "Job.Management.SelfEmp.HighlyQualified"),
     xlab = "Job.Management.SelfEmp.HighlyQualified")
abline(v = results_mutate$Job.Management.SelfEmp.HighlyQualified.mean.coef, 
       lty = "dashed", lwd = "3", col = "blue") 
 
## distribution of telephone coeff
hist(results_mutate$Telephone, col = "grey",
     main = paste("Coefficient Distribution of", "Purpose.UsedCar"), xlab = "Purpose.UsedCar")
abline(v = results_mutate$Telephone, lty = "dashed", lwd = "3", 
       col = "blue") 

#### distribution of Personal.Male.Single

hist(results_mutate$X.Personal.Male.Single, col = "grey",
     main = paste("Coefficient Distribution of", "Purpose.UsedCar"), xlab = "Purpose.UsedCar")


#### distribution of CreditHistory.NoCredit.AllPaid
graphics.off() 
par("mar") 
par(mar=c(1,1,1,1))

hist(results_mutate$CreditHistory.NoCredit.AllPaid, col = "grey",
     main = paste("Coefficient Distribution of", "Purpose.UsedCar"), xlab = "Purpose.UsedCar")
abline(v = results_mutate$CreditHistory.NoCredit.AllPaid, lty = "dashed", lwd = "3", 
       col = "blue") 




# Compare these resampling results to a single model built on entire sample #######################

full_model_lmfit <- lm(Amount ~ Duration + InstallmentRatePercentage + Telephone + Job.Management.SelfEmp.HighlyQualified 
                 +     Personal.Male.Single + CreditHistory.NoCredit.AllPaid, data = GC)
summary(full_model_lmfit)

# names of different objects saved with the fitted model.
names(full_model_lmfit) 

# Names of the objects within the function
names(summary(model_lm)) 

# model coefficients  ## full model r squared value
full_model_lmfit.coef <- data.frame(coefficients(full_model_lmfit))
full_model_lmfit.coef
round(summary(full_model_lmfit)$r.squared, 3) 


