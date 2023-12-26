##################################################################
######################   LOAD LIBRARY   ##########################
###################################################################
library(summarytools)
library(mice)
library(ggplot2) 
library(caret)
library(dplyr)
library(GGally)
library(ROSE)
library(randomForest)
library(e1071)
library(DT)
library(tidyverse)
library(rpart)	
library(rpart.plot)	
library(plotly)


##################################################################
################   IMPORT DATASET         ##################
###################################################################
stroke <- read_excel("Documents/Stroke_Data.xlsx")

######To show my dataset within the table#######
View(stroke)


##################################################################
##########DATA PREPARATION/PREPROCESSING#############
################################################################### 
#check data class and structure
str(stroke)

#convert all chr to factor
stroke <- as.data.frame(unclass(stroke),stringsAsFactors=TRUE)
str(stroke)


#change levels from 0 and 1 to Yes or No
stroke$stroke<- factor(stroke$stroke, levels = c(0,1), labels = c("No", "Yes")) 
stroke$hypertension<- factor(stroke$hypertension, levels = c(0,1), labels = c("No", "Yes"))
stroke$heart_disease<- factor(stroke$heart_disease, levels = c(0,1), labels = c("No", "Yes")) 
 
#Remove outliers
stroke$bmi[stroke$bmi == "N/A"]  <- NA
stroke<- na.omit(stroke)
str(stroke)
stroke$bmi = as.numeric(stroke$bmi)
stroke$bmi[stroke$bmi >50]<-NA
ImputedData<-mice(stroke, m = 5, method = ifelse(colnames(stroke) == "bmi", "pmm", ""), maxit = 20)
stroke_final<-complete(ImputedData, 5)
stroke<- na.omit(stroke)

##################################################################
##############      DATA EXPLORATION        #####################
###################################################################

##########View the distribution of the subjects with stroke within the set

table(stroke$stroke)
###   0    1 
###  4861  249 

######### To view the percentage distribution of subjects with stroke within the dataset

prop.table(table(stroke$stroke))*100

###        0         1 
###     95.127202  4.872798 


##########View the distribution of the subjects with heart disease within the set

table(stroke$heart_disease)
##  0    1 
## 4834  276

######### To view the percentage distribution of subjects with stroke within the dataset

prop.table(table(stroke$heart_disease))*100

##     0         1 
##   94.598826  5.401174 


##########View the distribution of the subjects with hypertension within the set

table(stroke$hypertension)
##     0    1 
##    4612  498
######### To view the percentage distribution of subjects with hypertension within the dataset

prop.table(table(stroke$hypertension))*100
###        0         1 
###     90.254403  9.745597 

##########View the distribution of  gender within the set

table(stroke$gender)
###   Female   Male  Other 
###    2994   2115      1 
######### To view the percentage distribution of gender within the dataset

prop.table(table(stroke$gender))*100
####    Female        Male       Other 
####    58.59099804 41.38943249  0.01956947 

##########View the distribution of  smoking status within the set

table(stroke$smoking_status)
##          formerly smoked    never smoked          smokes         Unknown 
##                   885            1892             789            1544 
######### To view the percentage distribution of smoking status within the dataset

prop.table(table(stroke$smoking_status))*100
##          formerly smoked    never smoked          smokes         Unknown 
##                  17.31898        37.02544        15.44031        30.21526 

# locate  missing value
is.na(stroke)
which(is.na(stroke))

#pie chart representation of the the target variable

new_df = data.frame(prop.table(table(stroke$stroke))) 
fig = plot_ly(new_df, labels = ~Var1, values = ~Freq, type = 'pie')
fig
# From the prop.table or pie chart analysis we see that there's a huge bias to the majority class
# this bias in the data can influence the ML algorithm to ignore minority class 
# the phenomena is called class imbalance and will be solved by oversampling the minority class

# basic data/variable descriptive analysis
descr(stroke,
      headings = FALSE, # remove headings
      stats = "common" # most common descriptive statistics
) 
 
##################################################################
#################        DATA VISUALIZATION       ################
################################################################### 

#This is to view work type against stroke
ggplot(stroke_final, aes(x = work_type, fill = stroke))+
  geom_bar(position = "fill")+
  stat_count(geom = "text",
             aes(label = stat(count)), 
             position = position_fill(vjust = 0.5), color = "black")+ coord_flip()

#This is to view ever married against stroke
ggplot(stroke_final, aes(x = stroke, fill = ever_married))+
  geom_bar(position = "fill")+
  stat_count(geom = "text",
             aes(label = stat(count)), 
             position = position_fill(vjust = 0.5), color = "black")

#This is to view gender against stroke
ggplot(stroke_final, aes(x = stroke, fill = gender))+
  geom_bar(position = "fill")+
  stat_count(geom = "text",
             aes(label = stat(count)), 
             position = position_fill(vjust = 0.5), color = "black")

#This is to view Hypertension against stroke
ggplot(stroke_final, aes(x = stroke, fill = hypertension))+
  geom_bar(position = "fill")+
  stat_count(geom = "text",
             aes(label = stat(count)), 
             position = position_fill(vjust = 0.5), color = "black")

#This is to view bmi against stroke for heart disease indicator and individual gender
ggplot(stroke_final, aes(x = stroke, y = bmi, fill = heart_disease, color = gender))+
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5, position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))

#This is to view resident type against stroke
ggplot(stroke_final, aes(x = stroke, fill = Residence_type))+
  geom_bar(position = "fill")+
  stat_count(geom = "text",
             aes(label = stat(count)), 
             position = position_fill(vjust = 0.5), color = "black")

#This is to view smoking status against stroke for each resident type and glucose level
ggplot(stroke_final, aes(x = stroke, y = avg_glucose_level))+
  geom_bar(aes(fill = Residence_type),stat = "identity", position = position_dodge(0.9))+
  facet_wrap(~smoking_status, scales = "free")

#This is to view heart disease against stroke
ggplot(stroke_final, aes(x = stroke, fill = heart_disease))+
  geom_bar(position = "fill")+
  stat_count(geom = "text",
             aes(label = stat(count)), 
             position = position_fill(vjust = 0.5), color = "black")

#This is to view smoking status against stroke for each gender and bmi
ggplot(stroke_final, aes(x = stroke, y = bmi))+
  geom_bar(aes(fill = gender),stat = "identity", position = position_dodge(0.9))+
  facet_wrap(~smoking_status, scales = "free")


#This is to view Smoking Status against stroke for bmi indicator and individual gender
ggplot(stroke_final, aes(x = stroke, y = smoking_status, fill = bmi, color = gender))+
  geom_boxplot(alpha = 0.5) +
  geom_point(alpha = 0.5, position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0.1))

##################################################################
#############          MODEL BUILDING          ###################
################################################################### 

stroke_final<-stroke_final[2:12]


#Splitting data into training and testing set 
part<-createDataPartition(stroke_final$stroke, p = 0.75, list = FALSE)
train<-stroke_final[part,]
test<-stroke_final[-part,]
 
# target is used as a target variable and the rest of variables as features to predict TARGET
# We control the growth of the tree by setting the minimum amount of information gained (cp) needed to justify the split
prop.table(table(stroke_final$stroke))
#Satisfied Unsatisfied 
#0.95742514  0.04257486 

#theres a massive bias in the TrainingSet

# Random Oversampling for imbalanced classification
over<- ovun.sample(stroke~., data = train, method = "over", N= NROW(train)*1.3)$data
prop.table(table(over$stroke))

#Satisfied Unsatisfied 
#0.7365232   0.2634768

train = over%>%
  relocate(stroke)

test = test%>%
  relocate(stroke) 

# DECISION TREES MODEL
#------- model fitting/training------------
DecisionTree = rpart(
  stroke ~ .,
  data = train,
  na.action = na.omit,
  cp = .001
)

# LOGISTIC REGRESSION MODEL
#------- model fitting/training------------
mod_fit = train(stroke ~ .
                ,data = train, 
                method = "glm",  
                family = "binomial")

# SUPPORT VECTOR MACHINE
#------- model fitting/training------------
SVM = svm(stroke ~ ., 
          data = train, 
          kernel = "radial",
          type = "C-classification",
          na.action = na.omit,
          cost = 1,
          gamma = 1/5)
#--------------MODEL COMPARISON/REPORTING--------------------------
##################################################################
####################   DECISION TREE    ##########################
###################################################################
# plotting tool with a couple parameters
# this plot shows a visual structure of how the trees are generated
prp(DecisionTree, type =2, extra =8)

# shows the minimal cp for each trees of each size
# shows Which variables contribute the most to the prediction? (List variable numbers).
# check Variables actually used in tree construction:
fit = printcp(DecisionTree) 

# building confusion matrix
confmat = table(predict(DecisionTree, test,type="class"),
                unlist(test[,1]),
                dnn = list('predicted', 'actual')) 

print(confmat)
# Compute classification metrics and comment on the accuracy of your results.
# calculate the accuracy by dividing the sum diagonal of the matrix - which are the correct predictions - by the total sum of the matrix
acc = sum(diag(confmat))/sum(confmat)

paste('Accuracy =', acc, sep = ' ')

##################################################################
###############  LOGISTIC REGRESSION   ###########################
###################################################################
prediction = predict(mod_fit, test)
# confusion matrix
logit_pred_table = table(prediction, unlist(test[,1]) )
print(mod_fit)
# model accuracy
#logistic_pred_accuracy <- sum(diag(logit_pred_table)) / nrow(test)
#logistic_pred_accuracy

 
##################################################################
####################     SVM              #########################
###################################################################
# building confusion matrix
confmat = table(predict(SVM, test,type="class"),
                unlist(test[,1]),
                dnn = list('predicted', 'actual')) 

# calculate the accuracy by dividing the sum diagonal of the matrix - which are the correct predictions - by the total sum of the matrix
acc = sum(diag(confmat))/sum(confmat)
paste('Accuracy =', acc, sep = ' ')

##################################################################
###   K-fold cross validation and Naive Bayes    ##########
###################################################################

ctrl <- trainControl(method = "cv", number = 8)

model1 <- train(factor(stroke) ~. , data = train, method = "glm", trControl = ctrl)

#view summary of k-fold CV               
print(model1)
summary(model1)
