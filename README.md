# Churn
Churn assignment script

EDA of churn dataset

data<-read.csv("C:\\Users\\RYAN\\Desktop\\Churn capstone.csv")

data$Area.Code <- NULL

data$State <- NULL

data$Phone <- NULL


cor(churn_data)


cor(churn_data)

mylogit <- glm(Churn ~ Account.Length + VMail.Message + Day.Mins + Eve.Mins + Night.Mins + Intl.Mins + Day.Calls + Day.Charge + Eve.Calls + Eve.Charge + Night.Calls + Night.Charge + Intl.Calls + Intl.Charge + CustServ.Calls, data = data, family = "binomial")

summary(mylogit)

histogram(data$Account.Length,col="red" , main="Histogram of Customer Account Length", xlab="Account Length Label")

histogram(data$Day.Mins,col="red" , main="Histogram of Day Minutes Consumed", xlab="Day Minutes")


histogram(data$CustServ.Calls,col="red" , main="Histogram of Customer Service Calls", xlab="Customer Service Calls")

summary(data)


Naive bayes script

data<-read.csv("C:\\Users\\RYAN\\Desktop\\Churn capstone.csv")

data$Area.Code <- NULL

data$State <- NULL

data$Phone <- NULL


data$Churn<-factor(data$Churn)

rn_train <- sample(nrow(data), floor(nrow(data)*0.9))
train <- data[rn_train,]
test <- data[-rn_train,]

model <- NaiveBayes(Churn~., data=train)

predictions <- predict(model, test)

# summarize results

confusionMatrix(predictions$class, test$Churn)

KNN Script

data<-read.csv("C:\\Users\\RYAN\\Desktop\\Churn capstone.csv")


head(data)

sapply(data, class)

data$Area.Code <- NULL

data$State <- NULL

data$Phone <- NULL

head(data)

data$Churn<-factor(data$Churn)


rn_train <- sample(nrow(data), floor(nrow(data)*0.8))
train <- data[rn_train,]
test <- data[-rn_train,]

#KNN analysis using WEKA

classifier <- IBk(Churn ~., data = train,
                  control = Weka_control(K = 20))
evaluate_Weka_classifier(classifier, numFolds = 10)

classifier <- IBk(Churn ~., data = train,
                  control = Weka_control(K = 5))
evaluate_Weka_classifier(classifier, numFolds = 10)

p<-predict(classifier, test)

results<-data.frame(test, p)

count (results$Churn==results$p)

logistic regression

churn_data<-read.csv("C:\\Users\\RYAN\\Desktop\\Churn capstone.csv")
churn_data$Area.Code <- NULL

churn_data$State <- NULL

churn_data$Phone <- NULL

churn_data$Churn<-factor(churn_data$Churn)

churn_data$Churn <- as.integer(churn_data$Churn)-1
fit <- glm(Churn~Day.Mins,data=churn_data,family=binomial())
summary(fit) # display fit results
