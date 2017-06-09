# Below is the function for cluterwise regression:
# Data: i have used the Auto data from the ISLR package
# K= is the no of clusters
# t is no of iterations
# q is the seed for ramdom sampling.


clustreg = function(data,k,t,q) {
  
  rsq = c()       # array of R-Squared values
  data0 = data[,-1]     # data set without response variable
  n = dim(data)[1]   # number of observations
  p = dim(data)[2]   # number of columns
  ybar = mean(data[,1])
  xnames = names(data) 
  
 # Regression formula for the multi regression

  formula = paste(xnames[1],'~.') 
  
  # Initializing variables
  # initial size of each cluster
  # remainder
  # set seed for reproducible results
  # randomized indices
  
  s = floor(n/k)  
  r = n%%k        
  set.seed(q)     
  ss = sample(1:n,n,replace=FALSE)   
  
  
  # Setting up initial cluster indices 
  # Initial clusters each have n/k observations from the random sample array
  # The last cluster starts with n/k+r observations if n/k is not an integer
  c = list()
  for(i in 1:k) {
    if(i==k) {
      c[[i]] = ss[((i-1)*s+1):(n)]
    } else {
      c[[i]] = ss[((i-1)*s+1):(i*s)]
    }
  }
  
  # Setting up initial clusters
  cc = list()         # data in k clusters
  ccc = list()        # data in k clusters without response variable
  for(i in 1:k) {
    cc[[i]] = data[c[[i]],]
    ccc[[i]] = data0[c[[i]],]
  }
  
  for(ii in 1:t) {    # runs loop t times
    
    # Storing multiple regression models in a list
    fit = list()
    for(i in 1:k) {
      fit[[i]]=lm(formula,data=cc[[i]])
    }
    
    # Calculating Squared Residual Error (SRE) for all observations
    pred = matrix(data = NA, n, k)
    res = matrix(data = NA, n, k)
    actual = matrix(data = NA, n, k)
    sre = matrix(data = NA, n, k)
    for(i in 1:k) {
      pred[,i] = predict(fit[[i]], data0)
      actual[,i] = data[,1]
      res[,i] = actual[,i]-pred[,i]
      sre[,i] = res[,i]^2
    }
    
    # Reclassifying observations to the cluster with the minimum SRE
    # Computing overall R-Squared value for all cluster regression models combined
    a = c()
    rsqnum = c()
    rsqden = c()
    for(i in 1:n) {
      for(j in 1:k) {
        if(min(sre[i,])==sre[i,j]) {
          a[i] = j
          rsqnum[i] = (pred[i,j]-ybar)^2
          rsqden[i] = (actual[i,j]-ybar)^2
        }
      }
    }
    rsq[ii] = sum(rsqnum)/sum(rsqden)
    
    # Reforming new clusters for next iteration
    # Refitting k linear regression models
    c = list()
    fit = list()
    crsq = c()
    for(i in 1:k) {
      c[[i]] = which(a==i)
      cc[[i]] = data[c[[i]],]
      ccc[[i]] = data0[c[[i]],]
      fit[[i]] = lm(formula,data=cc[[i]])
      crsq[i] = summary(fit[[i]])$r.squared
    }
    
    # If the overall R-Squared value is the same as in last iteration,
    # the procedure has converged and we will break out of the loop
    if(ii>1) {
      if(rsq[ii]==rsq[ii-1]) break
    }
    
    # End of iteration loop
  }
  
  #output = list()
  #output[[1]] = fit  # return final regression models for each cluster
  #output[[2]] = rsq  # return overall R-Squared value for each iteration (combined from all clusters)
  #output[[3]] = crsq # return R-Squared value for the final regression model for each cluster
  #output[[4]] = c    # return cluster list of observation assignments
  #output[[5]] = a    # return observation list of cluster classifications
  #output[[6]] = ii   # total number of iterations
  output = list("fit" = fit, 
                "rsq" = rsq, 
                "crsq" = crsq, 
                "clusters" = c, 
                "obs" = a, 
                "i" = ii)
  return(output)
}

###### applying the function on the data####

#first i have used the Auto data to run the clusterwise regression function 
# The coloumns other than numeric datatype have been excluded.
# Some EDA with the data to identify if any values are missing


library(ISLR)
data("Auto")
data<- Auto
data<- Auto[-9]
head(data)

#Now use the clusterwise function
data<-data
k=15
t=20
q=1

#call the function
clustreg(data,k,t,q)



##### run the function on the car.test.frame data from rpart 

library(rpart)
data("car.test.frame")
data1<-car.test.frame
rownames(data1) <- NULL
data1<-data1[-2]
data1<-data1[-4]
head(data1)

#removed rows with NA values in them
data1<-na.exclude(data1) 

data<-data1
k=5
t=15
q=1

#call the function:

clustreg(data,k,t,q)





