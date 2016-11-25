library("dplyr")
library("ggplot2")
library('corrplot') #package corrplot
library("magrittr")
library("scales")
library("rpart") # Recursive Partitioning and Regression Trees
library("rattle")
library("rpart.plot")
library("randomForest")
library("Amelia")

testData <- read.csv("data/test.csv")
trainData <- read.csv("data/train.csv")
str(testData)

dataDescrip <- read.csv("data/houseVarDescrip.csv")

numData <- trainData[sapply(trainData, function(x) !is.factor(x))]
str(numData)

numData %>% cor(use = "na.or.complete") %>% corrplot(type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


missmap(trainData,
        main = "Missing values in Housing Prices Dataset",
        y.labels = NULL,
        y.at = NULL)

#histogram of sales price
ggplot(data = trainData, aes(SalePrice)) +
  geom_histogram(bins = 15, aes(fill = ..count..)) + 
  scale_x_continuous(name="Sales Price", labels = comma)

#building first decision tree
fit <- rpart(SalePrice ~ OverallQual + YearBuilt, data=trainData, method="anova")
plot(fit)
fancyRpartPlot(fit, digits=2)

prediction <- predict(fit, testData, type = "vector")

submit <- data.frame(Id = testData$Id, SalePrice = prediction)

write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE, quote=FALSE)

levels(testData$HouseStyle) <- levels(trainData$HouseStyle)

#randomForest
fitForest <- randomForest(SalePrice ~., 
                    data=trainData,
                    importance=TRUE, 
                    ntree=20)

varImpPlot(fitForest)

predictionForest <- predict(fitForest, testData)

submit <- data.frame(Id = testData$Id, SalePrice = predictionForest)
write.csv(submit, file = "myfirstRandomForest.csv", row.names = FALSE, quote=FALSE)

