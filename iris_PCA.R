load(iris) #loading iris dataset

head(iris) #visualizing first 6 datapoints of the dataset

str(iris) #Compactly display the internal structure of an R object

class(iris) #Returns the data type of R

iris_pred <- iris[,-5] #Keeping all col except 5:predictor set
iris_resp <- iris[,5]  #Keeping only 5th col:Response set

library(caret)
log_pred <- log(iris_pred)
iris_pca <- prcomp(log_pred,center = FALSE)
iris_pca
summary(iris_pca)

plot(iris_pca,type="l")
pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}
pcaCharts(iris_pca)#plots several charts to explain adequacy of PCA


