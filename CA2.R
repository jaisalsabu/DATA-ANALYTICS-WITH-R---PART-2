#https://github.com/jaisalsabu/DATAANALYTICS-CA2
#PART 1 - LOADING THE DATASET

#we load the data in heart.csv
heartattack <- read.csv("heart.csv", na = "")
heartattack
#schema of dataframe
str(heartattack)

#getting to know the column names 
colnames(heartattack)
#understanding the type of values
sapply(heartattack, class)
#we can understand that every value is of integer/numerical type 


#listing the first 20 rows, to understand the data better
head(heartattack,n=20)

#PART 2 - ANALYSIS AND TRANSFORMATION

library(dplyr)
#classify the variables into categorical and numerical variables 
#select the numerical variables
numeric_var <-heartattack %>% 
  select("age","trtbps","chol","thalachh","oldpeak")
numeric_var
#select the categorical values and factorising 
library(dplyr)
categorical_var<- heartattack %>%
  select("sex","cp","fbs","restecg","exng","slp","caa",
         "thall","output")%>%
  mutate_if(is.numeric, as.factor)
categorical_var

#combine the categorical and numerical values
heartattack_prediction = cbind(categorical_var,numeric_var)

#to find the missing values , complete and incomplete cases , we use the library DataExplorer
#we also get to know the continuous and discrete variables in detail
install.packages("DataExplorer")
library(DataExplorer)
plot_intro(heartattack_prediction,title="Dataset Information")

#summary of dataset
summary(heartattack_prediction)

#descriptive statistics
#we will use the pastecs package that will provide us a clear 
#statistical analysis of the numerical variables
install.packages("pastecs")
library(pastecs)
stat.desc(heartattack_prediction, norm = TRUE)
#from this we conculde that our dataset is highly skewed

install.packages("psych")
library(psych)
#pairplot
pairs(heartattack_prediction)
# Initial investigation of data variable
pairs.panels(heartattack_prediction,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

#we will implement a correlation plot to check the correlation betweeen variables
correlation_tab <- cor(heartattack)
install.packages("corrplot")
library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
#png(file="corr2.png",res=150,width=900,height=700)                        
corrplot(correlation_tab, method = "color", shade.col = NA, tl.col = "black", tl.srt = 45,tl.cex =1,cl.cex=1,col = col(200), addCoef.col = "black", order = "AOE",number.cex = .5)

#we now plot our output to understand the balance of our data set
install.packages("ggplot2")
library(ggplot2)
table(heartattack$output)
ggplot(heartattack_prediction, aes(x = output)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#around 138 people have the no chances of a heart attack
#while 165 people from our observation have the risk of heart attack
#around 54% of people have the chances of heart attack in our dataset

#we check the normality with qqplot
#age variable
with(heartattack_prediction, {
  qqnorm(heartattack_prediction$age, 
         main = "Normality analysis of age data")
  qqline(heartattack_prediction$age)
})
#resting blood pressure variable
with(heartattack_prediction, {
  qqnorm(heartattack_prediction$Resting_blood_pressure, 
         main = "Normality analysis of Resting Blood Pressure data")
  qqline(heartattack_prediction$Resting_blood_pressure)
})
#cholesterol variable
with(heartattack_prediction, {
  qqnorm(heartattack_prediction$Cholesterol, 
         main = "Normality analysis of Cholesterol data")
  qqline(heartattack_prediction$Cholesterol)
})
#maximum heart rate variable
with(heartattack_prediction, {
  qqnorm(heartattack_prediction$Maximum_heart_rate, 
         main = "Normality analysis of Maximum heart rate data")
  qqline(heartattack_prediction$Maximum_heart_rate)
})
#Old peak variable
with(heartattack_prediction, {
  qqnorm(heartattack_prediction$oldpeak, 
         main = "Normality analysis of Maximum heart rate data")
  qqline(heartattack_prediction$oldpeak)
})

#PART 3 - OUTLIER DETETCTION AND SOLUTION
#There are multiple ways to detect outliers within a dataset. 
#scatter plots and bar plots are quite commonly used
#we will be trying to use the boxplot analysis to detect outliers.

opar <- par(no.readonly = TRUE)
par(mfrow = c(2,3))
attach(heartattack_prediction)
boxplot(heartattack_prediction$age,
        main = "Age",
        sub = paste("Outlier rows: ", boxplot.stats(heartattack_prediction$age)$out)
)

boxplot(heartattack_prediction$chol,
        main = "Cholesterol",
        sub = paste("Outlier rows: ", boxplot.stats(heartattack_prediction$chol)$out)
)
#from the boxplot for chol variable we understand that outliers are present in our dataset
#to view the outliers we use the boxplot.stats()function
boxplot.stats(heartattack_prediction$chol)$out

boxplot(heartattack_prediction$oldpeak,
        main = "Oldpeak",
        sub = paste("Outlier rows: ", boxplot.stats(heartattack_prediction$oldpeak)$out)
)
#from the boxplot for variable we understand that outliers are present in our dataset
#to view the outliers we use the boxplot.stats()function
boxplot.stats(heartattack_prediction$oldpeak)$out

boxplot(heartattack_prediction$thalachh,
        main = "Maximum heart rate achieved",
        sub = paste("Outlier rows: ", boxplot.stats(heartattack_prediction$thalachh)$out)
)
#from the boxplot for thalachh variable we understand that outliers are present in our dataset
#to view the outliers we use the boxplot.stats()function
boxplot.stats(heartattack_prediction$thalachh)$out

boxplot(heartattack_prediction$trtbps,
        main = "Resting Blood pressure",
        sub = paste("Outlier rows: ", boxplot.stats(heartattack_prediction$trtbps)$out)
)
#from the boxplot for trtbps variable we understand that outliers are present in our dataset
#to view the outliers we use the boxplot.stats()function
detach(heartattack_prediction)
par <- opar
#outliers
#age
boxplot.stats(heartattack_prediction$age)$out
#cholesterol
boxplot.stats(heartattack_prediction$chol)$out
#oldpeak
boxplot.stats(heartattack_prediction$oldpeak)$out
#Maximum heart rate achieved(thalachh)
boxplot.stats(heartattack_prediction$thalachh)$out
#Resting blood Pressure(trtbps)
boxplot.stats(heartattack_prediction$trtbps)$out


#since we have found out all the outliers in the variables
#we have to remove them from the data prepared for prediction.

#from chol variable we have to remove 417,564,394,407,409 that were identified as outliers
heartattack_prediction <- subset(heartattack_prediction, heartattack_prediction$chol!=417)
heartattack_prediction <- subset(heartattack_prediction, heartattack_prediction$chol!=564)
heartattack_prediction <- subset(heartattack_prediction, heartattack_prediction$chol!=394)
heartattack_prediction <- subset(heartattack_prediction, heartattack_prediction$chol!=407)
heartattack_prediction <- subset(heartattack_prediction, heartattack_prediction$chol!=409)
#after removing the outliers , we will use the boxplot to confirm the deletion 
boxplot(heartattack_prediction$chol,
        main = "Cholesterol",
        sub = paste("Outlier rows: ", boxplot.stats(heartattack_prediction$chol)$out)
)
#from the boxplot we have confirmed the absence of outliers
#next we take the oldpeak variable
#we have to remove 4.2, 6.2, 5.6, 4.2, 4.4
heartattack_prediction <- subset(heartattack_prediction, heartattack_prediction$oldpeak!=4.2)
heartattack_prediction <- subset(heartattack_prediction, heartattack_prediction$oldpeak!=6.2)
heartattack_prediction <- subset(heartattack_prediction, heartattack_prediction$oldpeak!=5.6)
heartattack_prediction <- subset(heartattack_prediction, heartattack_prediction$oldpeak!=4.4)
#after removing the outliers , we confirm the deletion with boxplot
boxplot(heartattack_prediction$oldpeak,
        main = "Oldpeak",
        sub = paste("Outlier rows: ", boxplot.stats(heartattack_prediction$oldpeak)$out)
)
#we have confirmed the deletion of outliers from oldpeak variable
#next we take the thalachh variable
#we have to remove 71
heartattack_prediction <- subset(heartattack_prediction, heartattack_prediction$thalachh!=71)
#we confirm this as we did for the earlier variables
boxplot(heartattack_prediction$thalachh,
        main = "Maximum heart rate",
        sub = paste("Outlier rows: ", boxplot.stats(heartattack_prediction$thalachh)$out)
)
#we move on to the trtbps variable 
#172, 178, 180, 200, 174, 192 are the outliers in this variable
heartattack_prediction <- subset(heartattack_prediction, heartattack_prediction$trtbps!=172)
heartattack_prediction <- subset(heartattack_prediction, heartattack_prediction$trtbps!=178)
heartattack_prediction <- subset(heartattack_prediction, heartattack_prediction$trtbps!=180)
heartattack_prediction <- subset(heartattack_prediction, heartattack_prediction$trtbps!=200)
heartattack_prediction <- subset(heartattack_prediction, heartattack_prediction$trtbps!=174)
heartattack_prediction <- subset(heartattack_prediction, heartattack_prediction$trtbps!=192)
#confirming the deletion of outliers
boxplot(heartattack_prediction$trtbps,
        main = "Resting Blood Pressure",
        sub = paste("Outlier rows: ", boxplot.stats(heartattack_prediction$trtbps)$out)
)

summary(heartattack_prediction)
#we change the column names to prepare our dataset for building the model
names(heartattack_prediction)
names(heartattack_prediction)[names(heartattack_prediction)=="sex"] <- "Gender"
names(heartattack_prediction)[names(heartattack_prediction)=="cp"] <- "Chest_pain"
names(heartattack_prediction)[names(heartattack_prediction)=="trtbps"] <- "Resting_blood_pressure"
names(heartattack_prediction)[names(heartattack_prediction)=="chol"] <- "Cholesterol"
names(heartattack_prediction)[names(heartattack_prediction)=="fbs"] <- "Fasting_blood_sugar"
names(heartattack_prediction)[names(heartattack_prediction)=="thalachh"] <- "Maximum_heart_rate"
names(heartattack_prediction)[names(heartattack_prediction)=="exng"] <- "agina_exercise"
names(heartattack_prediction)[names(heartattack_prediction)=="restecg"] <- "Resting_ecocardiograph"
names(heartattack_prediction)[names(heartattack_prediction)=="caa"] <- "number_of_vessels"
#dataset after changing names
names(heartattack_prediction)

print(ncol(heartattack_prediction))
print(nrow(heartattack_prediction))

#import caTools library
install.packages("caTools")
install.packages("pscl")
library(caTools)
#splitting the dataset
set.seed(100)
split = sample.split(heartattack_prediction$output, SplitRatio = 0.80)
train_set = subset(heartattack_prediction, split == TRUE)
test_set = subset(heartattack_prediction, split == FALSE)
#Model ran with all variables
#implementing logistic regression
full.model <- glm(formula = `output` ~ ., 
                  family = "binomial", 
                  data = heartattack_prediction)
summary(full.model)
coef(full.model)
library(pscl)
pR2(model)

#re-building the model again with significant factors 
model1 <- glm( output ~ Gender + Chest_pain + Resting_ecocardiograph  + number_of_vessels + Cholesterol + Maximum_heart_rate + oldpeak,
               data = train_set, family = binomial)
summary(model1)
pR2(model1)
#prediction for model built with significant variables 
probabilities <- predict(model1, test_set, type = "response")
predicted.classes <- ifelse(probabilities > 0.5,1,0)
# Prediction accuracy
observed.classes <- test_set$output
mean(predicted.classes == observed.classes)


#RANDOM FOREST ESTIMATION
install.packages("randomForest")
set.seed(100)
#create the subsets for sizes
subsets <- c(1:8,10,13)
# define the control using random forest selection 
library(caret)
ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   number = 10,
                   verbose = FALSE)

#run the RFE
library(randomForest)
results <- rfe(x=heartattack_prediction[, c(1:8,10:14)], y=heartattack_prediction$output,
               sizes = subsets,
               rfeControl = ctrl)
# Print the selected features
print(predictors(results))
#next we build the model based on these features
rfe.model = glm(formula = `output` ~ number_of_vessels + `Chest_pain` 
                + `agina_exercise` + oldpeak + `Gender` 
                + thall + Maximum_heart_rate,  
                family = "binomial", 
                data = train_set)
summary(rfe.model)
library(pscl)
pR2(rfe.model)
#prediction based on rfe estimator values
probabilities <- predict(rfe.model, test_set, type = "response")
predicted.classes <- ifelse(probabilities > 0.5,1,0)
# Prediction accuracy
observed.classes <- test_set$output
mean(predicted.classes == observed.classes)
#_____________________________________________________________________________
#**--MODEL VALIDATION--**
#collinearity
# we check collineairty
library(car)
vif(model1)
vif(rfe.model)

#Likehood ratio test 
anova(model1, rfe.model, test ="Chisq")
library(lmtest)
lrtest(model1, rfe.model)

#Variable Importance - statistical test for individual predictor
install.packages("caret")
library(caret)
varImp(model1)
varImp(rfe.model)

#**----FORECASTING------**#
predicted <- predict(rfe.model, test_set, type="response")
install.packages("InformationValue")
library(InformationValue)

#convert defaults from "Yes" and "No" to 1's and 0's
test_set$output <- ifelse(test_set$output=="Yes", 1, 0)

#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test_set$output, predicted)[1]
optimal
#from this we understand that the optimal probability cut off is 0.9899968
#using this cutoff , we will create a confusion matrix
confusionMatrix(test_set$output, predicted)
#calculate total misclassification error rate
misClassError(test_set$output, predicted, threshold=optimal)

