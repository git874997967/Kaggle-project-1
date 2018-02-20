library(glmnet)
library(dplyr)
library(Amelia)
library(VIM)
library(ggplot2)
library(ranger)
library(scales)
library(ggthemes)
train = read.csv('train.csv', stringsAsFactors = F)
test = read.csv('test.csv', stringsAsFactors = F)
str(train)
str(test)
test$Survived = 0


data = bind_rows(train, test)
missValue = function(data) {
  for (i in 1:ncol(data)) {
    if (nrow(data[is.na(data[, i]), ]) != 0) {
      print(paste(nrow(data[is.na(data[, i]), ]), colnames(data[i]), sep =
                    " "))
    }
  }
}
missValue(data)
aggr(data)
missmap(data)
data$Embarked
attach(data)
missing <- list(Pclass = nrow(data[is.na(Pclass),]))
missing$Name <- nrow(data[is.na(Name),])
missing$Sex <- nrow(data[is.na(Sex),])
missing$Age <- nrow(data[is.na(Age),])
missing$SibSp <- nrow(data[is.na(SibSp),])
missing$Parch <- nrow(data[is.na(Parch),])
missing$Ticket <- nrow(data[is.na(Ticket),])
missing$Fare <- nrow(data[is.na(Fare),])
missing$Cabin <- nrow(data[is.na(Cabin),])
missing$Embarked <- nrow(data[is.na(Embarked),])
for (name in names(missing)) {
  if (missing[[name]][1] > 0) {
    print(paste('', name, ' miss ', missing[[name]][1], ' values', sep = ''))
  }
}
detach(data)
sapply(data, function(x)
  sum(is.na(x)))
sapply(data, function(x)
  sum(x == ""))
#fix the embarked
which(data$Embarked %in% '')
data.frame(data[data$PassengerId %in% c(62, 830), ])
###Pclass=1 Fare=80
data$Embarked[c(62, 830)] = 'C'
###fix the fare
data[1044, ]
FareInfo = data$Fare[data$Embarked == 'S' & data$Pclass == 3]
mean(FareInfo)
summary(FareInfo)
data$Fare[1044] = 8.05
###Fix Age
hist(data$Age)
set.seed(129)
########fix with mice
ss <-
  c('PassengerId',
    'Name',
    'Ticket',
    'Cabin',
    'family',
    'Surname',
    'Survived')
mice_age <- mice(data[, !names(data) %in% ss], method = 'rf')
mice_output <- complete(mice_age)
mice_output$Age
data$Age[is.na(data$Age)] = mice_output$Age
hist(mice_output$Age)
data$Survived = as.factor(data$Survived)
### Pclass
ggplot(data = data[1:nrow(train), ],
       mapping = aes(x = Pclass, y = ..count.., fill = Survived)) +
  geom_bar(stat = "count", position = 'dodge') +
  xlab('Pclass') +
  ylab('Count') +
  ggtitle('Different Pclass impact survived') +
  scale_fill_manual(values = c("#FF0000", "#00FF00")) +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 1),
    ,
    vjust = -0.5
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
####Name
Others <-
  c('Capt',
    'Col',
    'Don',
    'Dona',
    'Jonkheer',
    'Lady',
    'Major',
    'Sir',
    'the Countess')
data$Title = sapply(
  data$Name,
  FUN = function(x) {
    strsplit(x, split = '[,.]')[[1]][2]
  }
)
data$Title = sub(" ", "", data$Title)
data$Title[data$Title == 'Mlle'] <- 'Miss'
data$Title[data$Title == 'Mme'] <- 'Mrs'
data$Title[data$Title == 'Ms'] <- 'Miss'
data$Title[data$Title %in% Others] <- 'Others'
table(data$Title)
## add new features
# family size
data$familySize = 0
data$familySize[(data$Sib + data$Parch + 1) < 2] = 1
data$familySize[(data$Sib + data$Parch + 1) %in% c(2:4)] = 2
data$familySize[(data$Sib + data$Parch + 1) > 4] = 3
##age
data$ageType = ''
data$ageType[data$Age <= 19] = 'child'
data$ageType[data$Age > 19] = 'Adult'
###Fare vs survived
ggplot(data = data[1:nrow(train), ],
       mapping = aes(x = Fare, y = ..count.., fill = Survived)) +
  geom_bar(stat = "count", position = 'dodge') +
  xlab('Fare') +
  ylab('Count') +
  ggtitle('Different Fare impact survived') +
  scale_fill_manual(values = c("#FF0000", "#00FF00")) +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 1),
    ,
    vjust = -0.5
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

ggplot(data = data, aes(x = Fare, color = Survived)) +
  geom_line(aes(label = ..count..), stat = 'bin', binwidth = 10)  +
  labs(title = "Different Fare impact survivor",
       x = "Fare",
       y = "Count",
       fill = "Survived") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
####as factor
data$Pclass <- factor(data$Pclass)
data$Title <- factor(data$Title)
data$Sex <- factor(data$Sex)
data$ageType <- factor(data$ageType)
data$familySize <- factor(data$familySize)
data$Embarked <- factor(data$Embarked)
str(data)
train <- data[1:891, ]
test <- data[892:1309, ]
###factorize

####build model with three models
## 1 forest 0.77
set.seed(1234)
rf.model = ranger(
  Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title +
    familySize + ageType,
  train,
  num.trees = 3000,
  mtry = 3,
  num.threads = 8
)
rf.predict = predict(rf.model, test, OOB = T, type = 'response')
test$Survived1 = rf.predict$predictions
test$Survived1
rf.result = data.frame(PassengerId = test$PassengerId,
                       Survived = test$Survived1)
write.csv(rf.result, 'rf1.csv')
###logic regression
# set.seed(4321)
# log.model=lm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+familySize+ageType,train,family = 'binomial')
# log.predict=predict(log.model,test,type='response')
###Lasso  regression failed
# formula=as.formula(Survived~.)
# x=model.matrix(formula,train)
# y=train$Survived
# set.seed(4321)
# log.model=cv.glmnet(x,y,alpha=1)
# x_test=model.matrix(formula,test)
#  train$Survived=as.integer(train$Survived)
