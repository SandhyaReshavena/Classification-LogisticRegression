# Import data set
data = read.csv('Loan Status DataSet.csv')
# summary 
head(data);#tail(data)
summary(data)
# install 'Psych' package and active it to do descriptive statistics  
library(psych)
describe(data)
head(data)
# Remove unnecessary columns - Loan ID is not unnecessary as because it just unique id 
Data = subset (data, select = -1)
# Check for Missing values - Percentage
p <- function(x) {sum(is.na(x))/length(x)*100}
apply(Data,2,p)
# Check for Missing values - Count
p <- function(x) {sum(is.na(x))}
apply(Data,2,p)
#Impute Missing values With Mean
for(i in 1:ncol(Data)){
  Data[is.na(Data[,i]), i] <- mean(Data[,i], na.rm = TRUE)
}
#Check for Categorical variables and dealt with categorical features
Data$Gender=factor(Data$Gender)
Data$Married=factor(Data$Married)
Data$Dependent=factor(Data$Dependent)
Data$Education=factor(Data$Education)
Data$Self_Employed=factor(Data$Self_Employed)
Data$Property_Area=factor(Data$Property_Area)
Data$Loan_Status=factor(Data$Loan_Status)

# Change Y values to 1's and 0's
Data$Loan_Status <- ifelse(Data$Loan_Status == "Y", 1, 0)

# Train and test split
#df = sort(sample(nrow(Data), nrow(Data)*.8))
#train<-Data[df,]
#test<-Data[-df,]

library(caTools)
sample <- sample.split(Data$Loan_Status, SplitRatio = 0.8)
train_data <- subset(Data, sample == TRUE)
test_data <- subset(Data, sample == FALSE)
dim(train_data)
dim(test_data)
table(train_data$Loan_Status)
head(train_data)
# Build Logistic Model
set.seed(1)
logitmod <- glm(Loan_Status ~ ., family = "binomial", data=train_data)
summary(logitmod)
logistics = glm(Loan_Status~Credit_History+Property_Area, data = train_data, family = "binomial")
summary(logistics)
pred <- predict(logistics, newdata = test_data, type = "response")
pred
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- test_data$Loan_Status
mean(y_pred == y_act)  
library(caret)
#confusionMatrix(reference=test_data$Loan_Status, data=y_pred)
y_act = factor(y_act)
y_pred = factor(y_pred)
confusionMatrix(reference=y_act, data=y_pred)

