library(xgboost)
library(VIM)
library(ggthemes)
library(tidyverse)
library(mice)
train = read.csv(file.choose())
test = read.csv(file.choose())
# str(train)
# str(test)

target = train$Survived
train = train %>% select(-Survived)
# aggr(train)
# aggr(test)
sapply(data, function(x) {
  sum(is.na(x))
})
sapply(data, function(x) {
  sum(x == '')
})
aggr(data, combined = T)
data = bind_rows(train, test)
data$Embarked[c(62, 830)] = "C"
data[is.na(data$Fare), ]
# ggplot(data[data$Embarked == 'S' &
#               data$Pclass == 3,], aes(
#                 x = Embarked,
#                 y = Fare,
#                 fill = factor(Pclass)
#               )) +
#   geom_boxplot()
summary(data$Fare[data$Embarked == 'S' &
                    data$Pclass == 3 & data$Age > 50], na.rm = T)
data$Fare[1044] = 7.763
summary(data$Cabin)
data$familySize = data$SibSp + data$Parch + 1
###use mice to fix the Age
set.seed(124)
str(train)
data$Title = gsub("(.*,)|(\\..*)", "", data$Name)
data$Title = trimws(data$Title)
table(data$Title)
data$Title[data$Title == "Miss" |
             data$Title == 'Mlle' | data$Title == 'Mme'] = "Miss"
data$Title[data$Title == "Lady" |
             data$Title == 'the Countess' |
             data$Title == 'Ms'] = "Mrs"
data$Title[data$Title == "Capt" |
             data$Title == 'Don' | data$Title == 'Dona' |
             data$Title == 'Jonkheer'] = "Col"
data$Title[data$Title == "Major"] = "Col"
data$Age[is.na(data$Age)] = mean(data$Age, na.rm = T)
# miceE = c('PassengerId',
#           "Name",
#           "Ticket",
#           "Cabin",
#           "familySize",
#           "Sex",
#           "SibSp")
# mice_age = mice(data[,!names(data) %in% miceE], method = 'rf')
# mice_output = complete((mice_age))
# head(mice_output)
# data$Age = mice_output$Age
aggr(data)
data$Adult = 'Adult'
data$Adult[data$Age <= 18] = "Child"
####start one hot encoding
data_num = data %>% select_if(is.numeric)
str(data_num)
data$Title = as.factor(data$Title)
Tit = tibble(Title = data$Title)
data_Title = model.matrix( ~ -1 + Title, Tit)
data$Adult = as.factor(data$Adult)
Adu = tibble(Adult = data$Adult)
data_Adult = model.matrix( ~ -1 + Adult, Adu)
data$Embarked = as.factor(data$Embarked)
data_Embarked = model.matrix( ~ -1 + Embarked, tibble(Embarked = data$Embarked))
data$Pclass = as.factor(data$Pclass)
data_Pclass = model.matrix( ~ -1 + Pclass, tibble(Pclass = data$Pclass))
data$Sex = as.factor(data$Sex)
data_Sex = model.matrix( ~ -1 + Sex, tibble(Sex = data$Sex))
hist(data$familySize)
table(data$familySize)
data$famCat[data$familySize < 4] = 'Small'
data$famCat[data$familySize >= 4 & data$familySize < 6] = 'Mid'
data$famCat[data$familySize >= 6] = 'Large'
data$famCat = as.factor(data$famCat)
data_famCat = model.matrix( ~ -1 + famCat, tibble(famCat = data$famCat))
###combine togather
data_final = cbind(data_num,
                   data_Title,
                   data_Adult,
                   data_Embarked,
                   data_famCat,
                   data_Pclass,
                   data_Sex)
head(data_final)
train = data_final[1:nrow(train), ]
train = train[, -7]
str(train)
test = data_final[1 + nrow(train):nrow(data_final) - 1, ]
nrow(test)
head(test)
test = test[-1, -7]
length(target)

dTrain = xgb.DMatrix(as.matrix(train), label = target)
dTest = xgb.DMatrix(as.matrix(test))
### cv to find the best nround
cv.nround = 200
cv.nfold = 10
seed=4321
param = list("objective" = "binary:logistic",
             "metrics" = 'rmse',
             "seed"=seed,
             "eta" = 0.2)
bst.cv = xgb.cv(param, dTrain,
                nfold = cv.nfold,
                nround = cv.nround)
print(bst.cv)
nround = which(
  bst.cv$evaluation_log$test_error_mean == min(bst.cv$evaluation_log$test_error_mean)
)
neg=sum(target==0)
pos=sum(target==1)
nround
bst = xgboost(
  data = dTrain,
  param,
  eta=0.2,
  label = target,
  metrics='rmse',
  scale_pos_weight=neg/pos,
  seed = seed,
  nrounds = nround
)
A = predict(bst, dTest)
table(target)
 
output = tibble(passengerId = test$PassengerId, Survived = round(A))
write.csv(output,
          "xgboost_titanic.csv",
          row.names = F,
      
          quote = F)
