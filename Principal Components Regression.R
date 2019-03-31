install.packages("pls")
library(pls)
set.seed(2)

Hitters=na.omit(Hitters)
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

# Setting scale=TRUE has the effect of standardizing each predictor, 
#prior to generating the principal components, so that the scale on which each variable is measured will not have an effect. Setting validation="CV" causes pcr() to compute the ten-fold cross-validation error for each possible value of M ,
pcr.fit=pcr(Salary~.,data=Hitters,scale=TRUE,validation="CV")

# pcr() reports the root mean squared error; 
#in order to obtain the usual MSE, we must square this quantity. 
#For instance, a root mean squared error of 352.8 corresponds to an MSE of 352.82 = 124,468

# We see that the smallest cross-validation error occurs when M = 16 com- ponents are used. 
#This is barely fewer than M = 19, which amounts to simply performing least squares, because when all of the components are used in PCR no dimension reduction occurs. However, from the plot we also see that the cross-validation error is roughly the same when only one component is included in the model. This suggests that a model that uses just a small number of components might suffice.

# The summary() function also provides the percentage of variance explained in the predictors and in the response using different numbers of components
# Briefly, we can think of this as the amount of information about the predictors or the response that is captured using M principal components. For example, setting M = 1 only captures 38.31 % of all the variance, or information, in the predictors. In contrast, using M = 6 increases the value to 88.63 %. If we were to use all M = p = 19 components, this would increase to 100 %.
summary(pcr.fit)

validationplot(pcr.fit,val.type="MSEP")


set.seed(1)
pcr.fit=pcr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type = "MSEP")
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)

pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)
