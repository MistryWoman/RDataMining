#appendix: ridge regression
#effect of shrinkage constant lambda on coefficients in presence of collinearity
#this is really useful for estimating coefficients in the presence of multicollinearity, for example if you want to put both systolic bp & diastolic bp in a regression as predictors (their corr is about 0.8)

library(MASS) #this loads package containing lm.ridge function
set.seed(2120) #this is just to make the data reproducible
x1<-rnorm(100)
x2<-rnorm(100, mean = x1, sd = 0.01)
x3<-rexp(100,10)
y <- rnorm(100, mean = 3+x1+x2) 
#true b0=3, b1=1, b2=1, x3 doesn't matter w/r/t y
#and note that x1 and x2 are highly correlated


cor(y,x2) ; cor(y,x3)
cor(x1,x2) #this multicollinearity can wreak havok with estimates
plot(x1,y)
library(scatterplot3d) 
s3d <-scatterplot3d(x1,x2,y, pch=16, highlight.3d=TRUE, type="h", main="")

#since the data are simulated, we know the true values of betas are (3,1,1)
#here are the OLS estimates from a linear model
lm(y~x1+x2+x3)$coef
summary(lm(y~x1+x2+x3))


#for an explanation of the math and interpretation behind ridge regression, choosing a lambda parameter, etc.

ridge<-lm.ridge(y~x1+x2+x3,lambda=seq(0,50,.1))
#we want to choose the lambda that best predicts the data. We can do this via generalized cross-validation. This way we figure out which lambda seems to be the best, and apply it anew to the model. The GCV tosses out k (usually 10) observations, recalculates the model under the given lambda, and then calculates how well that model predicts the 10 values. It does this for every subsection of the data (every random 10 values), then averages the Mean Squared Error (MSE) for that lambda. We want the lambda that minimizes the MSE, and is most generalizable to other datasets. 

which.min(ridge$GCV) #this is the optimal tuning parameter lambda 
which(ridge$GCV==min(ridge$GCV))

lm.ridge(y~x1+x2+x3,lambda=as.numeric(names(which.min(ridge$GCV))))
#now, notice the parameter estimates for beta are all closer to their true values!




#another function to fit ridge regression, taken from ESL by Tibshirani
library(glmnet)
grid=10^seq(10,-2,length=100) 
#function automatically selects lambda range, but we can choose our own
#need to make input X as matrix:
X<-cbind(x1,x2,x3)
ridge.mod=glmnet(X,y,alpha=0)  #alpha=0 is ridge, =1 is lasso
cv.out<-cv.glmnet(X,y,alpha=0)  
plot(cv.out)
bestlam=cv.out$lambda.min #selects best lambda from leave-10-out cross validation
predict(ridge.mod,type="coefficients", s=bestlam)

#note that if we make alpha=1, making it LASSO, not ridge regression, we eliminate 
#x2!!!, which we know is in fact a true predictor of Y in the model. Thus, in this case, we get very different answers from LASSO vs. Ridge Regression

