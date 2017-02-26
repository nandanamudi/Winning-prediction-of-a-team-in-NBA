
install.packages('ISLR')
install.packages('leaps')
library (ISLR)
library (leaps)

dataFile = read.csv("C:/Users/jnyc8/Downloads/team.csv",header= TRUE);

Data = dataFile[, c(1:32),];

head(Data, 10)


k=10 
set.seed(17) 
folds=sample (1:k,nrow(Data),replace=TRUE) 
cv.errors=matrix(NA,k,31, dimnames =list(NULL , paste(1:31)))
head(cv.errors, 10)

predict.regsubsets =function (object ,newdata ,id,...){ 
 form=as.formula (object$call [[2]]) 
 mat=model.matrix(form ,newdata ) 
 coefi=coef(object ,id=id) 
 xvars=names(coefi) 
 mat[,xvars]%*%coefi 
}

best.fit=regsubsets (win_per~. , Data)
summary(best.fit)


for(j in 1:k){ 
	best.fit=regsubsets (win_per~.,data=Data[folds!=j,], nvmax=31) 
 for(i in 1:31){ 
 	pred=predict(best.fit ,Data[folds==j,],id=i) 
 	cv.errors[j,i]=mean( (Data$win_per[folds==j]-pred)^2) 
} 
}



mean.cv.errors=apply(cv.errors ,2,mean)
mean.cv.errors


par(mfrow=c(1,1))
plot(mean.cv.errors,type= 'b')

reg.best=regsubsets (win_per~.,data=Data , nvmax=31)
coef(reg.best ,13) 


#LDA

trainData = dataFile[c(1:900), c(3, 5, 8, 9, 13, 16, 19, 23, 27, 28, 29, 31, 32, 35)];
length(trainData)

head(trainData, 10)

library(MASS) 

attach(trainData)

lda.fit = lda(Class~., data=trainData) 
lda.fit 
plot(lda.fit)



testData= dataFile[c(901:1337), c(3, 5, 8, 9, 13, 16, 19, 23, 27, 28, 29, 31, 32, 35)];
lda.pred=predict(lda.fit , testData)
names(lda.pred)

lda.class =lda.pred$class
lda.class


head(lda.pred, 10)
table(lda.class ,testData$Class)

length(lda.class)
dim(lda.class)

length(testData)
dim(testData)

mean(lda.class == testData$Class)

sum(lda.pred$posterior [ ,1] >=.4)
sum(lda.pred$posterior [,1]<.4)

lda.pred$posterior [1:20 ,1]
lda.class [1:20]




### Confusion Matrix and statistics

install.packages('caret')
library(caret)

modelFit<- train(Class~. , method='lda',preProcess=c('scale', 'center'), data=trainData)

confusionMatrix(testData$Class, predict(modelFit, testData))







####ROC


 confusion <- function(actual, predicted, names = NULL, printit = TRUE, prior = NULL) {                    
  if (is.null(names))
  names <- levels(actual)
  tab <- table(actual, predicted)
  acctab <- t(apply(tab, 1, function(x) x/sum(x)))
  dimnames(acctab) <- list(Actual = names, "Predicted (cv)" = names)
  if (is.null(prior)) {
  relnum <- table(actual)
  prior <- relnum/sum(relnum)
  acc <- sum(tab[row(tab) == col(tab)])/sum(tab)
  }
 else {
  acc <- sum(prior * diag(acctab))
  names(prior) <- names
  }
  if (printit)
  print(round(c("Overall accuracy" = acc, "Prior frequency" = prior),
                + 4))
  if (printit) {
  cat("\nConfusion matrix", "\n")
  print(round(acctab, 4))
  }
  invisible(acctab)
  }


prior <- c(0.7, 0.3)
  lda.70.30 <- lda(Class~., data = testData, CV=TRUE, prior=prior)
  confusion(testData$Class, predict(modelFit, testData), prior = c(0.7, 0.3))



truepos <- numeric(19)
 falsepos <- numeric(19)
 p1 <- (1:19)/20
 for (i in 1:19) {
 p <- p1[i]
 Family.ROC <- lda(Class~., data = testData, CV = TRUE, prior = c(p,  1 - p))

 confmat <- confusion(testData$Class, Family.ROC$class, printit = FALSE)
 falsepos[i] <- confmat[1, 2]
 truepos[i] <- confmat[2, 2]
 }



 windows(width=10, height=7)
 LDA.ROC<-plot(truepos~falsepos, type = "l", lwd=2, 
               xlab = "False positive rate (Specificity)", 
               ylab = "True positive rate (Sensitivity)", col ="green")
               abline(a=0,b=1, col="red")


install.packages('pROC')
library(pROC)
library(rpart)

AUC1<- auc(LDA.ROC)




##Pearson correlation coefficient far away from 0, and much closer to 1 or -1.

library(corrplot)

x <- seq(0, 100, 1)

# colinear with x
y <- x + 2.3 

# almost colinear with x / some small gaussian noise 
z <- x + rnorm(mean = 0, sd = 5, n = 101)

# uncorrrelated gaussian 
w <- rnorm(mean = 0, sd = 1, n = 101)

# this frame is made to exemplify the procedure
df <- data.frame(x = x, y = y, z = z, w = w)

corr.matrix <- cor(df)
corrplot.mixed(corr.matrix)



> tmp <- cor(trainData)
tmp[upper.tri(tmp)] <- 0
 diag(tmp) <- 0
# Above two commands can be replaced with 
# tmp[!lower.tri(tmp)] <- 0
#
> 
> data.new <- data[,!apply(tmp,2,function(x) any(x > 0.99))]
> head(data.new)




