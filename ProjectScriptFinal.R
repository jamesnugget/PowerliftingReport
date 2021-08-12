#Project Stuff
#Let me know if you actually want the dataset, it is huge, like almost 400MB large.

#Libraries used.
library(tree)
library(boot)
library(ISLR)

#Open and format table
opl <- read.csv("openpowerlifting-2020-10-16.csv")            #Open file  
oplMod <- opl[ -c(1, 3, 6:8, 11:14, 16:19, 21:24, 27:40) ]    #Remove the irrelevant columns
oplMod <- oplMod[!(oplMod$Sex=="Mx"),]                        #Remove all the genders that are not M/F
oplMod <- oplMod[which(oplMod$Age > 24 & oplMod$Age < 40), ]  #Remove everyone outside of the open age range
oplMod$Sex <- as.factor(oplMod$Sex)                           #Set gender as a factor
oplMod$Equipment <- as.factor(oplMod$Equipment)               #Set Equipment as factor
oplMod$WeightClassKg <- as.numeric(oplMod$WeightClassKg)      #Weight class to numeric (currently String)
oplMod <- na.omit(oplMod)                                     #Remove all empty rows
oplMod <- oplMod[-c(10001:nrow(oplMod)),]                     #Remove all rows after 10k
#Males = 2, Female = 1
#Wraps = 5, Unlimited = 4, Single-PLy = 3, Raw = 2, Multi-ply = 1


#To make the names in the graph easier to read.
gender <- as.numeric(oplMod$Sex)
equip <- as.numeric(oplMod$Equipment)

##Deadlift linear model (y = gender * x1 + equipment * x2 + bodtweight * x3 + weightclass * x4 + age * x5)
lm_opl_dl <- lm(formula = oplMod$Best3DeadliftKg ~ as.numeric(oplMod$Sex) + as.numeric(oplMod$Equipment) + oplMod$BodyweightKg + oplMod$WeightClassKg + oplMod$Age, data = oplMod)
summary(lm_opl_dl)

#I am limiting this project to deadlifting as of Nov 5, 2020 because of time constraints and it took too long to find a higher order regression.

##K-fold Crossvalidation
##Deadlifting
#Over 1 attempt
set.seed(1)   #Set the Seed.
glm.fit.dl=glm(oplMod$Best3DeadliftKg ~ as.numeric(oplMod$Sex) + as.numeric(oplMod$Equipment) + oplMod$BodyweightKg + oplMod$WeightClassKg + oplMod$Age, data = oplMod)
lm_cv_dl = cv.glm(oplMod,glm.fit.dl,K=10)$delta[1]
lm_cv_dl #Error after 10-fold CV. 6595

##Average over 10 attempts (Run this after running all the necessary stuff)
cv.error.lm=rep(0,10)
for (i in 1:10){  #Repeat the attempt 10 times and save to a list. 
  set.seed(i)
  glm.temp=glm(oplMod$Best3DeadliftKg ~ as.numeric(oplMod$Sex) + as.numeric(oplMod$Equipment) + oplMod$BodyweightKg + oplMod$WeightClassKg + oplMod$Age, data = oplMod)
  cv.error.lm[i]=cv.glm(oplMod,glm.temp,K=10)$delta[1]
}
mean(cv.error.lm)#Average the errors


#Trees for linear regression. (Run after running necessary stuff)
tree.opl_dl = tree(formula = oplMod$Best3DeadliftKg ~ gender + equip + oplMod$BodyweightKg + oplMod$WeightClassKg + oplMod$Age, data = oplMod)
summary(tree.opl_dl)
plot(tree.opl_dl) #Plot tree and labels
title(main = "Deadlifting Tree Plot")
text(tree.opl_dl,pretty = 0)


#Optimal(After a lot of testing, y = weightclass * x1 + bodyweight * x2 + (bodyweight * weightclass) * x3) (Run after running necessary stuff)
best_opl_dl <- lm(formula = oplMod$Best3DeadliftKg ~ oplMod$WeightClassKg:oplMod$BodyweightKg + oplMod$WeightClassKg, data = oplMod)
summary(best_opl_dl)

set.seed(1)
##CV 10-fold over 1 attempt.
glm.fit.dlop=glm(oplMod$Best3DeadliftKg ~ oplMod$WeightClassKg:oplMod$BodyweightKg + oplMod$WeightClassKg, data = oplMod)
lm_cv_dlop = cv.glm(oplMod,glm.fit.dlop,K=10)$delta[1]
lm_cv_dlop #Error after 10-fold cv. 6006. Wow I guess we were using too many variables

##Average over 10 attempts at 10-fold CV. (Run after running necessary stuff)
cv.error.best=rep(0,10)
for (i in 1:10){  #Repeat the attempt 10 times and save to a list. 
  set.seed(i)
  glm.temp=glm(oplMod$Best3DeadliftKg ~ oplMod$WeightClassKg + oplMod$BodyweightKg, data = oplMod)
  cv.error.best[i]=cv.glm(oplMod,glm.fit.dlop,K=10)$delta[1]
}
mean(cv.error.best)  #Average the errors

#Tree better model (Run after running necessary stuff)
tree.opl_dl = tree(formula = oplMod$Best3DeadliftKg ~ oplMod$WeightClassKg:oplMod$BodyweightKg + oplMod$WeightClassKg, data = oplMod)
plot(tree.opl_dl) #Plot tree and labels
title(main = "Deadlifting Tree Plot")
text(tree.opl_dl,pretty = 0)



#Classification Portion. We are going to be predicting gender based on stuff! We use the two models we went over.
##Create the test/train sets (Important, must run)
smp_siz = floor(0.8*nrow(oplMod))   #80-20 Split
set.seed(123)                       #Setting Seed
train_ind = sample(seq_len(nrow(oplMod)), size = smp_siz) 
train_set = oplMod[train_ind,]
test_set = oplMod[-train_ind,]   #Putting it into rows.

##Linear Model testing. (Ignore the code, just run it. It works)
cls_lm_dl <- glm(as.numeric(train_set$Sex) - 1 ~ Equipment + BodyweightKg + WeightClassKg + Age, data = train_set, family = binomial)
cls_lm_p1 <- predict(cls_lm_dl, test_set, type = "response")
glm_predictions1 <- rep("F", length(cls_lm_p1))
glm_predictions1[cls_lm_p1 > 0.5] <- "M"
table(glm_predictions1, test_set$Sex)
mean(glm_predictions1 == test_set$Sex)#Attempted correctly      --86.15%
mean(glm_predictions1 != test_set$Sex)#Attempted incorrectly    --13.85%

##Better Model testing. (Ignore the code, just run it. It works)
cls_bm_dl <- glm(as.numeric(train_set$Sex) - 1 ~ BodyweightKg:WeightClassKg + WeightClassKg, data = train_set, family = binomial)
cls_bm_p1 <- predict(cls_bm_dl, test_set, type = "response")
glm_predictions2 <- rep("F", length(cls_lm_p1))
glm_predictions2[cls_bm_p1 > 0.5] <- "M"
table(glm_predictions2, test_set$Sex)
mean(glm_predictions2 == test_set$Sex)#Attempted correctly      --85.80%
mean(glm_predictions2 != test_set$Sex)#Attempted incorrectly    --14.20% Close but not quite.
