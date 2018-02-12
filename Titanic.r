#Titanic幸存预测是Kaggle上参赛人数最多的竞赛之一。它要求参赛选手通过训练数据集分析出什么类型的人更可能幸存，并预测出测试数据集中的所有乘客是否生还。

#该项目是一个二元分类问题  classificer
install.packages(c(
  'readr',
  'ggthemes',
  'InformationValue',
  'MLmetrics',
  'e1071',
  'Amelia',
  'gbm'
))

#在加载数据之前，先通过如下代码加载之后会用到的所有R库
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
library(scales)
library(sqldf)
# 通过如下代码将训练数据和测试数据分别加载到名为train和test的data.frame中
train = read.csv(file.choose())
test = read.csv(file.choose())
# combine the data togather
data = bind_rows(train, test)
train.row = 1:nrow(train)
test.row = 1:nrow(test)
# factorize the data set
data$Survived = factor(data$Survived)

#可通过如下方式统计出每个Pclass幸存和遇难人数，如下

ggplot(data = data[1:nrow(train), ],
       mapping = aes(x = Pclass, y = ..count.., fill = Survived)) +
  geom_bar(stat = "count", position = 'dodge') +
  xlab('Pclass') +
  ylab('Count') +
  ggtitle('How Pclass impact survivor') +
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

lived = sqldf("select count(*) as lived from data  where Survived =1 group by Pclass ")
dead = sqldf("select count(*) as dead from data  where Survived =0 group by Pclass")
total = cbind(lived, dead)
total = data.frame(total)
rownames(total) = c("1", "2", "3")
barplot(
  t(total),
  beside = T,
  xlab = 'Pclass',
  ylab = 'numbers',
  col = terrain.colors(2)
)
legend(
  "topleft",
  legend = c("Survived", "Dead"),
  fill = terrain.colors(2),
  box.col = "transparent"
)
title(main = "the relation between pclass and survived")
# WOE IV 值 很重要
#IV的全称是Information Value，中文意思是信息价值，或者信息量。用IV去衡量变量预测能力
#是将变量引入模型的重要一句  类似的指标还有 信息增益 和基尼系数
#WOE的全称是“Weight of Evidence”，即证据权重。WOE是对原始自变量的一种编码形式。
WOETable(X = factor(data$Pclass[1:nrow(train)]), Y = data$Survived[1:nrow(train)])
#
# 插播一段，从上面的计算结果中我们可以看一下WOE的基本特点：
# 当前分组中，响应的比例越大，WOE值越大；
# 当前分组WOE的正负，由当前分组响应和未响应的比例，与样本整体响应和未响应的比例的大小关系决定，
#当前分组的比例小于样本整体比例时，WOE为负，当前分组的比例大于整体比例时，WOE为正，当前分组的比例和整体比例相等时，WOE为0。
# WOE的取值范围是全体实数。

IV(X = factor(data$Pclass[1:nrow(train)]), Y = data$Survived[1:nrow(train)])
data$Title = sapply(
  data$Name,
  FUN = function(x) {
    strsplit(x, split = '[,.]')[[1]][2]
  }
)
data$Title = sub(' ', '', data$Title)
data$Title[data$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
data$Title[data$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <-
  'Sir'
data$Title[data$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <-
  'Lady'
data$Title <- factor(data$Title)

livedTitle = sqldf("select count(*) as lived,Title from data  where Survived =1 group by Title ")
data.frame(livedTitle)
deadTitle = sqldf("select count(*) as dead,Title from data  where Survived =0 group by Title")
deadTitle
totalTitle = sqldf(
  "select livedTitle.lived, deadTitle.dead from livedTitle  left outer  join deadTitle on livedTitle.title=deadTitle.title"
)
summary(data$Title)
totalTitle = data.frame(totalTitle)
totalTitle

ggplot(data = data[1:nrow(train), ],
       mapping = aes(x = Title, y = ..count.., fill = Survived)) +
  geom_bar(stat = "count", position = 'dodge') +
  xlab('Title') +
  ylab('Count') +
  ggtitle('How Title impact survivor') +
  scale_fill_discrete(
    name = "Survived",
    breaks = c(0, 1),
    labels = c("Perish", "Survived")
  ) +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 1),
    ,
    vjust = -0.5
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
totalTitle[is.na(totalTitle)] = 0
rownames(totalTitle) = levels(data$Title)
totalTitle = rbind(totalTitle[1:9, ], c(0, 6), totalTitle[10:nrow(totalTitle), ])
edit(totalTitle)
barplot(
  t(totalTitle),
  beside = T,
  xlab = 'Title',
  ylab = 'numbers',
  col = terrain.colors(2)
)
legend(
  "topleft",
  legend = c("Survived", "Dead"),
  fill = terrain.colors(2),
  box.col = "transparent"
)
title(main = "the relation between title and survived")
WOETable(X = factor(data$Title[1:nrow(train)]), Y = data$Survived[1:nrow(train)])
#
# 插播一段，从上面的计算结果中我们可以看一下WOE的基本特点：
# 当前分组中，响应的比例越大，WOE值越大；
# 当前分组WOE的正负，由当前分组响应和未响应的比例，与样本整体响应和未响应的比例的大小关系决定，
#当前分组的比例小于样本整体比例时，WOE为负，当前分组的比例大于整体比例时，WOE为正，当前分组的比例和整体比例相等时，WOE为0。
# WOE的取值范围是全体实数。

IV(X = factor(data$Title[1:nrow(train)]), Y = data$Survived[1:nrow(train)])

Male=sqldf("select count(*)  as lived,Sex from data where Survived=1 group by Sex")
Female=sqldf("select count(*)  as dead,Sex from data where Survived=0 group by Sex")
totalSex=sqldf("select Male.lived, Female.dead from Male left outer join Female on Male.Sex=Female.Sex  ")
totalTitle = sqldf(
  "select livedTitle.lived, deadTitle.dead from livedTitle  left outer  join deadTitle on livedTitle.title=deadTitle.title"
)

rownames(totalSex)=levels(data$Sex)
totalSex
barplot(t(totalSex),beside = T,col= terrain.colors(2))
legend(
  "topleft",
  legend = c("Survived", "Dead"),
  fill = terrain.colors(2),
  box.col = "transparent"
)
title("Relationship between sex and survived")
box()
data[is.na(data$Age),]=0
data[(!is.na(data$Age)),]
# ggplot(data = data[(!is.na(data$Age)) & row(data[, 'Age']) <= 891, ], aes(x = Age, color=Survived)) + 
#   geom_line(aes(label=..count..), stat = 'bin', binwidth=5)  + 
#   labs(title = "How Age impact survivor", x = "Age", y = "Count", fill = "Survived")
 


