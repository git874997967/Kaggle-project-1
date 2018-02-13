library(readr) # File read / write
library(ggplot2) # Data visualization
library(ggthemes) # Data visualization
library(scales) # Data visualization
library(plyr)
library(stringr) # String manipulation
library(InformationValue) # IV / WOE calculation
library(MLmetrics) # Mache learning metrics.e.g. Recall, Precision, Accuracy, AUC
library(rpart) # Decision tree utils
library(randomForest) # Random Forest
library(dplyr) # Data manipulation
library(e1071) # SVM
library(Amelia) # Missing value utils
library(party) # Conditional inference trees
library(gbm) # AdaBoost
library(class) # KNN
library(ranger)
library(scales)
library(randomForest)
library(kknn)

train <- read_csv("train.csv")
test <- read_csv("test.csv")
data <- bind_rows(train, test)
train.row <- 1:nrow(train)
test.row <- (1 + nrow(train)):(nrow(train) + nrow(test))
str(data)
data$Survived <- factor(data$Survived)
ggplot(data = data[1:nrow(train),], mapping = aes(x = Pclass, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='dodge') + 
  xlab('Pclass') + 
  ylab('Count') + 
  ggtitle('How Pclass impact survivor') + 
  scale_fill_manual(values=c("#FF0000", "#00FF00")) +
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

WOETable(X=factor(data$Pclass[1:nrow(train)]), Y=data$Survived[1:nrow(train)])
IV(X=factor(data$Pclass[1:nrow(train)]), Y=data$Survived[1:nrow(train)])
data$Title <- sapply(data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
data$Title <- sub(' ', '', data$Title)
data$Title[data$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
data$Title[data$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
data$Title[data$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
data$Title <- factor(data$Title)
ggplot(data = data[1:nrow(train),], mapping = aes(x = Title, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='stack') + 
  xlab('Title') + 
  ylab('Count') + 
  ggtitle('How Title impact survivor') + 
  scale_fill_discrete(name="Survived", breaks=c(0, 1), labels=c("Perish", "Survived")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
WOETable(X=data$Title[1:nrow(train)], Y=data$Survived[1:nrow(train)])
IV(X=data$Title[1:nrow(train)], Y=data$Survived[1:nrow(train)])

data$Sex <- as.factor(data$Sex)
ggplot(data = data[1:nrow(train),], mapping = aes(x = Sex, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('Sex') + 
  ylab('Count') + 
  ggtitle('How Sex impact survivo') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
###???
WOETable(X=data$Sex[1:nrow(train)], Y=data$Survived[1:nrow(train)])
IV(X=data$Sex[1:nrow(train)], Y=data$Survived[1:nrow(train)])
ggplot(data = data[(!is.na(data$Age)) & row(data[, 'Age']) <= 891, ], aes(x = Age, color=Survived)) + 
  geom_line(aes(label=..count..), stat = 'bin', binwidth=5)  + 
  labs(title = "How Age impact survivor", x = "Age", y = "Count", fill = "Survived")


ggplot(data = data[1:nrow(train),], mapping = aes(x = SibSp, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  labs(title = "How SibSp impact survivor", x = "Sibsp", y = "Count", fill = "Survived") + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

WOETable(X=as.factor(data$SibSp[1:nrow(train)]), Y=data$Survived[1:nrow(train)])

IV(X=as.factor(data$SibSp[1:nrow(train)]), Y=data$Survived[1:nrow(train)])

ggplot(data = data[1:nrow(train),], mapping = aes(x = Parch, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  labs(title = "How Parch impact survivor", x = "Parch", y = "Count", fill = "Survived") + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
WOETable(X=as.factor(data$Parch[1:nrow(train)]), Y=data$Survived[1:nrow(train)])

IV(X=as.factor(data$Parch[1:nrow(train)]), Y=data$Survived[1:nrow(train)])
data$FamilySize <- data$SibSp + data$Parch + 1
ggplot(data = data[1:nrow(train),], mapping = aes(x = FamilySize, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('FamilySize') + 
  ylab('Count') + 
  ggtitle('How FamilySize impact survivor') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
WOETable(X=as.factor(data$FamilySize[1:nrow(train)]), Y=data$Survived[1:nrow(train)])
IV(X=as.factor(data$FamilySize[1:nrow(train)]), Y=data$Survived[1:nrow(train)])
ticket.count <- aggregate(data$Ticket, by = list(data$Ticket), function(x) sum(!is.na(x)))
data$TicketCount <- apply(data, 1, function(x) ticket.count[which(ticket.count[, 1] == x['Ticket']), 2])
data$TicketCount <- factor(sapply(data$TicketCount, function(x) ifelse(x > 1, 'Share', 'Unique')))
ggplot(data = data[1:nrow(train),], mapping = aes(x = TicketCount, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('TicketCount') + 
  ylab('Count') + 
  ggtitle('How TicketCount impact survivor') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
WOETable(X=data$TicketCount[1:nrow(train)], Y=data$Survived[1:nrow(train)])
IV(X=data$TicketCount[1:nrow(train)], Y=data$Survived[1:nrow(train)])
ggplot(data = data[(!is.na(data$Fare)) & row(data[, 'Fare']) <= 891, ], aes(x = Fare, color=Survived)) + 
  geom_line(aes(label=..count..), stat = 'bin', binwidth=10)  + 
  labs(title = "How Fare impact survivor", x = "Fare", y = "Count", fill = "Survived")
ggplot(data[1:nrow(train), ], mapping = aes(x = as.factor(sapply(data$Cabin[1:nrow(train)], function(x) str_sub(x, start = 1, end = 1))), y = ..count.., fill = Survived)) +
  geom_bar(stat = 'count', position='dodge') + 
  xlab('Cabin') +
  ylab('Count') +
  ggtitle('How Cabin impact survivor') +
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
data$Cabin <- sapply(data$Cabin, function(x) str_sub(x, start = 1, end = 1))
WOETable(X=as.factor(data$Cabin[1:nrow(train)]), Y=data$Survived[1:nrow(train)])
IV(X=as.factor(data$Cabin[1:nrow(train)]), Y=data$Survived[1:nrow(train)])
ggplot(data[1:nrow(train), ], mapping = aes(x = Embarked, y = ..count.., fill = Survived)) +
  geom_bar(stat = 'count', position='dodge') + 
  xlab('Embarked') +
  ylab('Count') +
  ggtitle('How Embarked impact survivor') +
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
WOETable(X=as.factor(data$Embarked[1:nrow(train)]), Y=data$Survived[1:nrow(train)])
IV(X=as.factor(data$Embarked[1:nrow(train)]), Y=data$Survived[1:nrow(train)])
attach(data)
missing <- list(Pclass=nrow(data[is.na(Pclass), ]))
missing$Name <- nrow(data[is.na(Name), ])
missing$Sex <- nrow(data[is.na(Sex), ])
missing$Age <- nrow(data[is.na(Age), ])
missing$SibSp <- nrow(data[is.na(SibSp), ])
missing$Parch <- nrow(data[is.na(Parch), ])
missing$Ticket <- nrow(data[is.na(Ticket), ])
missing$Fare <- nrow(data[is.na(Fare), ])
missing$Cabin <- nrow(data[is.na(Cabin), ])
missing$Embarked <- nrow(data[is.na(Embarked), ])
for (name in names(missing)) {
  if (missing[[name]][1] > 0) {
    print(paste('', name, ' miss ', missing[[name]][1], ' values', sep = ''))
  }
}
detach(data)

age.model <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data=data[!is.na(data$Age), ], method='anova')
data$Age[is.na(data$Age)] <- predict(age.model, data[is.na(data$Age), ])
data[is.na(data$Embarked), c('PassengerId', 'Pclass', 'Fare', 'Embarked')]
ggplot(data[!is.na(data$Embarked),], aes(x=Embarked, y=Fare, fill=factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), color='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) + theme_few()
data$Embarked[is.na(data$Embarked)] <- 'C'
data$Embarked <- as.factor(data$Embarked)
data$Fare[is.na(data$Fare)] <- median(data$Fare, na.rm=TRUE)
data$Cabin <- as.factor(sapply(data$Cabin, function(x) ifelse(is.na(x), 'X', str_sub(x, start = 1, end = 1))))
data$Sex=as.factor(data$Sex)
set.seed(415)
str(data)
model1 <- cforest(Survived ~ Pclass + Title + Sex + Age + SibSp + Parch + FamilySize + TicketCount + Fare + Cabin + Embarked, data = data[train.row, ], controls=cforest_unbiased(ntree=3000, mtry=3))
model2<- ranger(Survived ~ Pclass + Title + Sex + Age + SibSp + Parch + FamilySize + TicketCount + Fare + Cabin + Embarked, data = data[train.row, ],num.trees = 2500,mtry=3,num.threads=8)
model3 <- randomForest(Survived ~ Pclass + Title + Sex + Age + SibSp + Parch + FamilySize + TicketCount + Fare + Cabin + Embarked, data = data[train.row, ])

cv.summarize <- function(data.true, data.predict) {
  print(paste('Recall:', Recall(data.true, data.predict)))
  print(paste('Precision:', Precision(data.true, data.predict)))
  print(paste('Accuracy:', Accuracy(data.predict, data.true)))
  print(paste('AUC:', AUC(data.predict, data.true)))
}
set.seed(415)
cv.test.sample <- sample(1:nrow(train), as.integer(0.3 * nrow(train)), replace = TRUE)
cv.test <- data[cv.test.sample,]
cv.prediction1 <- predict(model1, cv.test, OOB=TRUE, type = "response")
cv.prediction3 <- predict(model3, cv.test, OOB=TRUE, type = "response")
cv.prediction4=predict(model4,cv.test,OOB=TRUE, type = "response")
cv.prediction2 <- predict(model2, cv.test, interval = "prediction",level = .95,OOB=TRUE, type = "response")
cv.summarize(cv.test$Survived, cv.prediction1)
cv.summarize(cv.test$Survived, cv.prediction3)
cv.summarize(cv.test$Survived, cv.prediction2$predictions)
predict.result1 <- predict(model1, data[(1+nrow(train)):(nrow(data)), ], OOB=TRUE, type = "response")
predict.result1
predict.result2 <- predict(model2, data = data[test.row, ], interval = "prediction", OOB=TRUE, type = "response")
# length(predict.result
# length(predict.result2$predictions)
output6 <- data.frame(PassengerId = test$PassengerId, Survived = predict.result6$predictions)
write.csv(output6, file = "cit6.csv", row.names = FALSE)

#####optimization 
data$Name
data$Surname <- sapply(data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
data$FamilyID <- paste(as.character(data$FamilySize), data$Surname, sep="")
data$FamilyID[data$FamilySize <= 2] <- 'Small'
 
#？？？？ table function
famIDs <- data.frame(table(data$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
data$FamilyID[data$FamilyID %in% famIDs$Var1] <- 'Small'
 data$FamilyID=factor(data$FamilyID)
 model4 <- cforest(Survived ~ Pclass +FamilyID+Title + Sex + Age + SibSp + Parch + FamilySize + TicketCount + Fare + Cabin + Embarked, data = data[train.row, ], controls=cforest_unbiased(ntree=3000, mtry=3))
 cv.prediction4=predict(model4,cv.test,OOB=T,type='response')
 cv.summarize(cv.test$Survived, cv.prediction4)
 predict.result1 <- predict(model1, data[(1+nrow(train)):(nrow(data)), ], OOB=TRUE, type = "response")
predict.result4 <- predict(model4, data[(1+nrow(train)):(nrow(data)),], OOB=TRUE, type = "response")

data$Embarked[c(62,830)] = "S"
data$Embarked <- factor(data$Embarked)

model5 <- cforest(Survived ~ Pclass +FamilyID+Title + Sex + Age + SibSp + Parch + FamilySize + TicketCount + Fare + Cabin + Embarked, data = data[train.row, ], controls=cforest_unbiased(ntree=3000, mtry=3))
model6=ranger(Survived ~ Pclass +FamilyID+Title + Sex + Age + SibSp + Parch + FamilySize + TicketCount + Fare + Cabin + Embarked, data = data[train.row, ], num.trees=3000,mtry=5,min.node.size=1)
cv.prediction5=predict(model5,cv.test,OOB=T,type='response')
cv.summarize(cv.test$Survived, cv.prediction5)
# 从交叉验证的结果来看  ranger 的账面数据最漂亮  但是  得分却是 cforest 最高
#？？？？？
# cv.prediction6=predict(model6,cv.test,OOB=T,type='response')
# cv.summarize(cv.test$Survived, cv.prediction6$predictions)
#  predict.result6 <- predict(model6, data[(1+nrow(train)):(nrow(data)),], OOB=TRUE, type = "response")


