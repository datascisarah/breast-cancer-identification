#####################################################################
################# Breast Cancer Prediction ##########################
#####################################################################
library(randomForest)
require(caTools)

df <- read.csv("Downloads/data.csv")

df$diagnosis <- replace(df$diagnosis, df$diagnosis == 'M', 1)
df$diagnosis <- replace(df$diagnosis, df$diagnosis == 'B', 0)
df$diagnosis = as.factor(df$diagnosis)

set.seed(222)
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
train <- df[ind==1,]
test <- df[ind==2,]


rf <- randomForest(diagnosis~., data=train, proximity=TRUE)
print(rf)

p1 <- predict(rf, train)
confusionMatrix(p1, train$diagnosis)

p2 <- predict(rf, test)
confusionMatrix(p2, test$ diagnosis)

importance(rf)
varImpPlot(rf, col = "navy")

MDSplot(rf, df$diagnosis)
