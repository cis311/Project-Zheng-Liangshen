
library(tidyverse) # metapackage with lots of helpful functions
library(Hmisc)
MyData <- read.csv(file='../input/Lottery_Cash_4_Life_Winning_Numbers__Beginning_20141.csv', header=TRUE, sep=",")
MyData

y <- MyData[, c(6)]
MyData <- MyData[, c(1,2,3,4,5)]

dfsum <- apply(MyData, 1, function(x) sum(x))
dfmax <- apply(MyData, 1, function(x) max(x))
dfmin <- apply(MyData, 1, function(x) min(x))
dfmean <- apply(MyData, 1, function(x) mean(x))

MyData['row-sum'] = dfsum
MyData['row-max'] = dfmax
MyData['row-min'] = dfmin
MyData['row-mean'] = dfmean

MyData

MyData_Scaled = scale(MyData)

MyData_Scaled

library(DataExplorer)

plot_str(MyData)

plot_missing(MyData)

plot_histogram(MyData)

plot_density(MyData)

plot_correlation(MyData, type = 'continuous','Review.Date')

plot_boxplot(MyData, by = "X0") 

train<-sample_frac(MyData, 0.7)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test<-MyData[-sid,]

y_train = y[sid]
y_test = y[-sid]


model <- glm(y_train ~. , data=train)

prediction <- predict(model, type = "response", newdata=test)
prediction <- round(prediction)
print ("Accuracy is : ")
print (length(prediction[prediction == y_test])/length(prediction))
