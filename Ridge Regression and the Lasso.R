require(ISLR)
require(glmnet)

Hitters=na.omit(Hitters)
# model.matrix() function
# produce a matrix corresponding to the 19 predictors but it also automatically transforms any qualitative variables into dummy variables.
# glmnet() can only take numerical, quantitative inputs.
# Don't need the intercept supplied by model.matrix
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

#If alpha=0 then a ridge regression model is fit, and if alpha=1 then a lasso model is fit
# By default, the glmnet() function standardizes the variables so that they are on the same scale
grid=10^seq(10,-2,length=100)
ridge.mod = glmnet(x,y,alpha=0,lambda=grid)

# Associated with each value of ?? is a vector of ridge regression coefficients, stored in a matrix that can be accessed by coef()
# rows (one for each predictor, plus an intercept)
# columns (one for each value of ??)
dim(coef(ridge.mod))

# We expect the coefficient estimates to be much smaller, in terms of l2 norm, when a large value of ?? is used, as compared to when a small value of ?? is used. These are the coefficients when ?? = 11,498, along with their l2 norm
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))




grid=10^seq(10,-2,length=100)
lasso.mod = glmnet(x,y,alpha=1,lambda=grid)

# Associated with each value of ?? is a vector of ridge regression coefficients, stored in a matrix that can be accessed by coef()
# rows (one for each predictor, plus an intercept)
# columns (one for each value of ??)
dim(coef(lasso.mod))

# We expect the coefficient estimates to be much smaller, in terms of l2 norm, when a large value of ?? is used, as compared to when a small value of ?? is used. These are the coefficients when ?? = 11,498, along with their l2 norm
lasso.mod$lambda[50]
coef(lasso.mod)[,50]
sqrt(sum(coef(lasoo.mod)[-1,50]^2))






# In contrast, here are the coefficients when ?? = 705, along with their l2 norm. Note the much larger l2 norm of the coefficients associated with this smaller value of ??.
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

# We can use the predict() function for a number of purposes. For instance, we can obtain the ridge regression coefficients for a new value of ??, say 50:
predict(ridge.mod,s=50,type="coefficients")[1:20,]


set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)

# if we had instead simply fit a model with just an intercept, we would have predicted each test observation using the mean of the training observations. In that case, we could compute the test set MSE like this
mean((mean(y[train])-y.test)^2)

# We could also get the same result by fitting a ridge regression model with a very large value of ??
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])

# In order for glmnet() to yield the exact least squares coefficients when ?? = 0, we use the argument exact=T when calling the predict() function. Otherwise, the predict() function will interpolate over the grid of ?? values used in fitting the glmnet() model, yielding approximate results. When we use exact=T, there remains a slight discrepancy in the third decimal place between the output of glmnet() when ?? = 0 and the output of lm(); this is due to numerical approximation on the part of glmnet().
# In the mathematical field of numerical analysis, interpolation is a method of constructing new data points within the range of a discrete set of known data points.
# In engineering and science, one often has a number of data points, obtained by sampling or experimentation, which represent the values of a function for a limited number of values of the independent variable. It is often required to interpolate (i.e. estimate) the value of that function for an intermediate value of the independent variable. This may be achieved by curve fitting or regression analysis.
# In general, if we want to fit a (unpenalized) least squares model, then we should use the lm() function, since that function provides more useful outputs, such as standard errors and p-values for the coefficients.
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x,subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]


set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)

# Finally, we refit our ridge regression model on the full data set, using the value of ?? chosen by cross-validation, and examine the coefficient estimates.
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)



#=================Lasso======================================#

require(ISLR)
require(glmnet)

Hitters=na.omit(Hitters)
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef

