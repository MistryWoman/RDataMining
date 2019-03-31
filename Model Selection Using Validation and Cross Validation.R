library(ISLR)
library(leaps)
View(Hitters)
names(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)

set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
test=(!train)

regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)

# Command to create an "X" matrix. Does not include response, Y.
# Includes intercept term.
test.mat=model.matrix(Salary~.,data=Hitters[test,])
sum(test==TRUE)

val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  # Matrix multiplication nxi * ix1 --> 1, end up
  # with one row per single prediction column. 
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,10)

predict.regsubsets = function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)


# Cross validation
# We must perform best subset selection within each of the k training sets.
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19,dimnames=list(NULL,paste(1:19)))

# For loop that performs cross-validation. In the jth fold, the elements of folds that equal j are in the test set, and the remainder are in the training set. We make our predictions for each model size (using our new predict() method), compute the test errors on the appropriate subset, and store them in the appropriate slot in the matrix cv.errors.
#Outer loop over k folds
for(j in 1:k){
  #Fit on all observations but those with j as our training set
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  #Inner loop over 19 sizes
  for(i in 1:19){
    # Predict all 19 versions using j'th fold as test set
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i] = mean((Hitters$Salary[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')

# perform best subset selection on the full data set 
reg.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(reg.best,11)