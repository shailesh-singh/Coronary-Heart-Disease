library(car)
library(caTools)
library(GGally)
library(ROCR)
library(dplyr)
library(MASS)
chd_df <- read.csv("framingham.csv")

set.seed(100)

#test/train split
split <- sample.split(chd_df$TenYearCHD, SplitRatio  = .7)
training_set <- subset(chd_df, split == TRUE)
test_set <-subset(chd_df,split == FALSE)



chd_mod<-glm(TenYearCHD ~ male + age + cigsPerDay + totChol + glucose + sysBP,data=training_set,family="binomial")

print (summary(chd_mod))
print (vif(chd_mod))

# Prediction on test set
pred<-predict(chd_mod,newdata=test_set,type="response")
print( summary(pred))
print(str(pred))


table(test_set$TenYearCHD)
167/(930+176)


table(test_set$TenYearCHD, pred > .5)
#Accuracy:
#(924+8)/(924+6+159+8) = 0.8495898

#True Positive Rate:
#8/(8+159) = 0.04790419

#False Positive Rate:
#6/(6+924) = 0.006451613



#threshold probability of .289
table(test_set$TenYearCHD, pred > 0.289)

#Accuracy:
#(845+45)/(845+85+122+45) = 0.8113036

#True Positive:
#45/(45+122) = 0.2694611

#False Positive:
#85/(85+845) = 0.09139785


# Predicting an observation
obs<-data.frame(male=c(0),age=c(51),cigsPerDay=c(20),totChol=c(220),glucose=c(78),sysBP=c(140.))
outcome <- predict(chd_mod, newdata = obs , type="response")
#outcome value = 0.1412832



# ROC Curves

#logistic
rocr.lr.pred<-prediction(pred,test_set$TenYearCHD)
logperformance<-performance(rocr.lr.pred,"tpr","fpr")
plot(logperformance,colorize=TRUE, main="Logistic Regression ROC")
points(0.091397,0.269411, col="red",cex=1.5)
points(0.4,0.74, col="red",cex=1.5)
points(0.8,0.98, col="red",cex=1.5)
abline(0,1)
as.numeric(performance(rocr.lr.pred, "auc")@y.values)


#lda 
chd_mod_lda <- lda(TenYearCHD ~.,data=training_set)
pred_lda <- predict(chd_mod_lda, newdata = test_set, type="responds")
pred_lda_probs <- pred_lda$posterior[,2]

rocr.lda.pred <- prediction(pred_lda_probs, test_set$TenYearCHD)
ldaPerformance <- performance(rocr.lda.pred, "tpr", "fpr")
plot(ldaPerformance, colorize = TRUE, main= "LDA ROC")
abline(0, 1)
as.numeric(performance(rocr.lda.pred, "auc")@y.values)

plot(logperformance, col="blue", main= 'Logistic Regression and LDA', sub="ROC")
plot(ldaPerformance, col="red", add=TRUE)
abline(0,1)
