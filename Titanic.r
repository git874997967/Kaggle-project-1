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
# 缺失值不要贸然处理  最后统一进行处理
# data[is.na(data$Age),]=0
# data[(!is.na(data$Age)),]
# ggplot(data = data[(!is.na(data$Age)) & row(data[, 'Age']) <= 891, ], aes(x = Age, color=Survived)) + 
#   geom_line(aes(label=..count..), stat = 'bin', binwidth=5)  + 
#   labs(title = "How Age impact survivor", x = "Age", y = "Count", fill = "Survived")
liveAge=sqldf("select count(Age) as num,Age from data where Survived=1 group by Age")
deadAge=sqldf("select count(Age) as num, Age from data where Survived=0 group by Age")
plot(density(deadAge$Age),
     main="how age infulence with survive",xlab="",ylab="",ylim=c(0,0.02))
polygon(density(liveAge$Age), col=rgb(1,1,0,0.4), border="blue", lwd=2) 
polygon(density(deadAge$Age), col=rgb(1,0,1,0.4), border="red", lwd=2) 
 
# 配偶和兄弟数量的关系
ggplot(data = data[1:nrow(train),], mapping = aes(x = SibSp, y = ..count.., fill=Survived)) +
  geom_bar(stat = 'count', position='dodge') +
  labs(title = "How SibSp impact survivor", x = "Sibsp", y = "Count", fill = "Survived") 
  # geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1),   vjust=-0.5,
  #           theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
 
sibLive=sqldf("select count(Sibsp) as live,Sibsp from data where Survived=1 group by SibSp ")
sibLive
sibDead=sqldf("select count(Sibsp) as dead,Sibsp from data where Survived=0 group by SibSp ")
sibDead
totalSib=sqldf("select sibLive.live, sibDead.dead,sibDead.SibSp from sibDead left outer join sibLive on sibLive.SibSp=sibDead.SibSp")
barplot(t(data.frame(totalSib$live,totalSib$dead)),beside = T,col= c(rgb(1,0,1,0.2),rgb(1,1,0,0.2)),  axes = TRUE, axisnames = TRUE)
legend(
  "topright",
  legend = c("Survived", "Dead"),
  fill =   c(rgb(1,0,1,0.2),rgb(1,1,0,0.2)),
  box.col = "transparent"
)
 
 
title("Relationship between sib and survived",xlab = " sib num",ylab="count")
box()
#  父母子女数量的关系
parchLive=sqldf("select count(Parch) as num, Parch from data where Survived=1 group by Parch")
parchLive
parchDead=sqldf("select count(Parch) as num, Parch from data where Survived=0 group by Parch")
parchDead
totalParch=sqldf("select parchDead.num as dead,parchlive.num as live,ParchDead.Parch from parchDead left outer join parchLive on parchDead.Parch=parchLive.parch")
totalParch
barplot(t(data.frame(totalParch$dead,totalParch$live)),col=c(rgb(1,1,0,0.2),rgb(1,0,1,0.2)),beside=T)
legend(
  "topright",
  c("dead","survived"),
  fill=c(rgb(1,1,0,0.2),rgb(1,0,1,0.2))
)
title("Relationship between Parch and survived",xlab = " Parch num",ylab="count",ylim=0.99)
box()
#family size
data$Fam=data$Parch+data$SibSp+1
famLive=sqldf("select count(Fam) as num, Fam from data where Survived=1 group by Fam")
famLive
famDead=sqldf("select count(Fam) as num, Fam from data where Survived=0 group by Fam")
famDead
totalfam=sqldf("select famDead.num as dead,famlive.num as live,famDead.fam from famDead left outer join famLive on famDead.fam=famLive.fam")
totalfam
barplot(t(data.frame(totalfam$dead,totalfam$live)),col=c(rgb(1,1,0,0.2),rgb(1,0,1,0.2)),beside=T)
legend(
  "topright",
  c("dead","survived"),
  fill=c(rgb(1,1,0,0.2),rgb(1,0,1,0.2))
)
title("Relationship between fam and survived",xlab = " fam num",ylab="count",ylim=0.99)
box()
# ticket
# with the same number I assume that they are a family
ticket.count <- aggregate(data$Ticket, by = list(data$Ticket), function(x) sum(!is.na(x)))
ticket.count
 
data$TicketCount <- apply(data, 1, function(x) ticket.count[which(ticket.count[, 1] == x['Ticket']), 2])
data$TicketCount <- factor(sapply(data$TicketCount, function(x) ifelse(x > 1, 'Share', 'Unique')))
livedTicket=sqldf("select count(TicketCount) as live,TicketCount from data where Survived=1 group by TicketCount")
deadTicket=sqldf("select count(TicketCount) as dead,TicketCount from data where Survived=0 group by TicketCount")
livedTicket
deadTicket
totalTicket=sqldf("select livedTicket.live,deadTicket.dead, livedTicket.TicketCount from livedTicket left outer join deadTicket on livedTicket.TicketCount=deadTicket.TicketCount")
totalTicket=t(data.frame(totalTicket$live,totalTicket$dead))
colnames(totalTicket)=c("Shared","Unique")
totalTicket
barplot(totalTicket,
        beside=T,
        col=c(rgb(1,1,0,0.2),rgb(1,0,1,0.2)),
        xlab="Ticket type",
        ylab="Count",
        axes=T,
        cex.axis = par("cex.axis"), cex.names = par("cex.axis")
        )
legend(
  "topright",
  c("survived","dead"),
  fill=c(rgb(1,1,0,0.2),rgb(1,0,1,0.2))
)
title("How familySize with survive")
box()
ggplot(data = data[1:nrow(train),], mapping = aes(x = TicketCount, y = ..count.., fill=Survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('TicketCount') + 
  ylab('Count') + 
  ggtitle('How TicketCount impact survivor') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
# how fare influence the survived
liveFare=sqldf("select Fare from data where Survived=1")
deadFare =sqldf("select Fare from data where Survived=0")
 plot(density(deadFare$Fare),main="How fare influence the survive",
      type = "l", ylim = c(0, 0.07), xlab = "",ylab="")
polygon(density(liveFare$Fare), col=rgb(1,1,0,0.4), border="blue", lwd=2) 
polygon(density(deadFare$Fare), col=rgb(1,0,1,0.4), border="red", lwd=2) 
legend(
  "topright",
  c("survived","dead"),
  fill=c(rgb(1,1,0,0.4),rgb(1,0,1,0.4))
) 
##CABIN

ggplot(data[1:nrow(train), ], mapping = aes(x = as.factor(sapply(data$Cabin[1:nrow(train)], function(x) str_sub(x, start = 1, end = 1))), y = ..count.., fill = Survived)) +
  geom_bar(stat = 'count', position='dodge') + 
  xlab('Cabin') +
  ylab('Count') +
  ggtitle('How Cabin impact survivor') +
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
livedEmbarked=sqldf("select count(Embarked) as num,Embarked from data where Survived=1 group by Embarked ")
deadEmbarked= sqldf("select count(Embarked) as num,Embarked from data where Survived=0 group by Embarked ")
deadEmbarked
edit(deadEmbarked)
totalEmbarked=sqldf("select livedEmbarked.num as live, deadEmbarked.num as dead, livedEmbarked.Embarked from livedEmbarked left outer join deadEmbarked on livedEmbarked.Embarked=deadEmbarked.Embarked")
totalEmbarked=t(data.frame(totalEmbarked$live,totalEmbarked$dead))
colnames(totalEmbarked)=c(NA,"C","Q","S")

barplot(totalEmbarked,beside=T,col=c(rgb(1,1,0,0.4),rgb(1,0,1,0.4)) )
legend("topleft",
       c("survived","dead"),
       fill=c(rgb(1,1,0,0.4),rgb(1,0,1,0.4)))
title("How embarked impacted the survive",xlab="Embarked")
box()
 
##### 处理缺失值
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
  
##use model to predict missing Sex
#缺失Sex信息的乘客数为263，缺失量比较大，不适合使用中位数或者平均值填补。
#一般通过使用其它变量预测或者直接将缺失值设置为默认值的方法填补，
#这里通过其它变量来预测缺失的年龄信息。

age.model <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + Fam, data=data[!is.na(data$Age), ], method='anova')

data$Age[is.na(data$Age)] <- predict(age.model, data[is.na(data$Age), ])
 
#  处理 Embarked
ggplot(data[!is.na(data$Embarked),], aes(x=Embarked, y=Fare, fill=factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), color='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) + theme_few()
data[is.na(data$Embarked), c('PassengerId', 'Pclass', 'Fare', 'Embarked')]
data$Embarked <- as.factor(data$Embarked)
## 处理  face 
data$Fare[is.na(data$Fare)] <- median(data$Fare, na.rm=TRUE)
 
 
# use cforest
 
set.seed(415)
model <- cforest(Survived ~ Pclass + Title + Sex + Age + SibSp + Parch + Fam + TicketCount + Fare + Cabin + Embarked, data = data[train.row, ], controls=cforest_unbiased(ntree=2000, mtry=3))
cv.summarize <- function(data.true, data.predict) {
  print(paste('Recall:', Recall(data.true, data.predict)))
  print(paste('Precision:', Precision(data.true, data.predict)))
  print(paste('Accuracy:', Accuracy(data.predict, data.true)))
  print(paste('AUC:', AUC(data.predict, data.true)))
}
set.seed(415)
cv.test.sample <- sample(1:nrow(train), as.integer(0.3 * nrow(train)), replace = TRUE)
cv.test <- data[cv.test.sample,]
cv.prediction <- predict(model, cv.test, OOB=TRUE, type = "response")
cv.summarize(cv.test$Survived, cv.prediction)


 














