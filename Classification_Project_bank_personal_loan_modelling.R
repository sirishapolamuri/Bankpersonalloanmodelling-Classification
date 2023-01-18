#################################
###  CASE STUDY ANALSYSIS
#################################
##### Importing data
data1=read.csv("C:/Users/HPP/Desktop/R programs/CLASSIFICATION PROJ/archive (5)/Bank_Personal_Loan_Modelling - Copy.csv")

N=ncol(data1)
N

#### Data preprocessing according to your data and problem statement
######### Filtering relevant columns needed for analysis dropping zipcode column
########################################################
data_an=data1[,c(1,2,3,4,6,7,8,9,10,11,12,13,14)]
n=ncol(data_an)
n
head(data_an)
nn=nrow(data1)
nn

### Install and activate package 'ggplot2' needed for histogram and box plot
install.packages("ggplot2")
library(ggplot2)

### Histogram of the response variable ###
qplot(data1$Personal.Loan,
      geom="histogram",
      binwidth=1,  
      main="Histogram for Personal.Loan ", 
      xlab="Personal.Loan",
      xlim=c(-1,2),
      fill=I("gray"), 
      col=I("red"))+theme_bw()

### Obtaining descriptive statistics ###

install.packages("pastecs") # Install package 'pastecs' needed for obtaining descriptive stats 
library(pastecs)

stat.desc(data1$Personal.Loan) # stat_desc(): function for displaying the descriptive statistics - mean, median, SD etc.

###perfrom shapiro test
shapiro.test(data1$Personal.Loan)

###perform t test
t.test(data1$Personal.Loan)

### Creating training and test set
set.seed(123)
indx=sample(1:nn,0.9*nn)
traindata=data_an[indx,]
testdata=data_an[-indx,]

#### Fitting full logistic regression (LR) model with all features
fullmod=glm(Personal.Loan~ID+Age +Experience +Income +Family +CCAvg 
            +Education +Mortgage +Securities.Account+CD.Account+Online 
            +CreditCard,data=traindata,family="binomial")
summary(fullmod)


#### Selecting features for fitting reduced logistic regression model
library(MASS)
step=stepAIC(fullmod)

mod2=glm(Personal.Loan ~ Experience +Income +Family +CCAvg 
         +Education +Securities.Account+CD.Account+Online 
         +CreditCard,data=traindata,family="binomial")
summary(mod2)

### predicting success probabilities using the LR model
head(testdata)

testdata_new=testdata[,c(3,4,5,6,7,10,11,12,13)]
pred_prob=predict(mod2,testdata_new,type="response")
hist(pred_prob)



### predicting success probability for an individual
sampletest=data.frame(t(c(10,175,2,8.5,2,1,0,1,1)))

colnames(sampletest)=c("Experience","Income","Family","CCAvg","Education","Securities.Account","CD.Account","Online","CreditCard")
sampletest
predict(mod2,sampletest,type="response")

#### Plotting ROC 
library(pROC)
roc1=roc(testdata[,9],pred_prob,plot=TRUE,legacy.axes=TRUE)
plot(roc1)
roc1$auc


#### Using ROC in deciding threshold
thres=data.frame(sen=roc1$sensitivities, spec=roc1$specificities,thresholds=roc1$thresholds)
thres[thres$sen>0.94&thres$spec>0.68,]
thres

library(caret)
pred_Y=ifelse(pred_prob > 0.02,1,0)
pred_Y
confusionMatrix(as.factor(testdata[,9]), as.factor(pred_Y))
thres



###############################
## Random Forest
###############################
library(randomForest)
###create train data###create train data
head(data_an)
data_an$Personal.Loan=as.factor(data_an$Personal.Loan)
data_an$Family=as.factor(data_an$Family)
data_an$Education=as.factor(data_an$Education)
data_an$Securities.Account=as.factor(data_an$Securities.Account)
data_an$CD.Account=as.factor(data_an$CD.Account)
data_an$Online=as.factor(data_an$Online)
data_an$CreditCard=as.factor(data_an$CreditCard)



###RF model
modRF=randomForest(Personal.Loan~ ., data=data_an,ntree=500, mtry=6)
modRF

###create test data
head(testdata)
nrow(testdata)
testdata$Personal.Loan=as.factor(testdata$Personal.Loan)
testdata$Family=as.factor(testdata$Family)
testdata$Education=as.factor(testdata$Education)
testdata$Securities.Account=as.factor(testdata$Securities.Account)
testdata$CD.Account=as.factor(testdata$CD.Account)
testdata$Online=as.factor(testdata$Online)
testdata$CreditCard=as.factor(testdata$CreditCard)

print(testdata[4,])


predict(modRF,testdata[4,-9],type="response")
