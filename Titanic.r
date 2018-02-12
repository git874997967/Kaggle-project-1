#Titanic幸存预测是Kaggle上参赛人数最多的竞赛之一。它要求参赛选手通过训练数据集分析出什么类型的人更可能幸存，并预测出测试数据集中的所有乘客是否生还。

#该项目是一个二元分类问题  classificer
# install.packages(c('readr','ggthemes','InformationValue','MLmetrics','e1071','Amelia','gbm'))

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
train=read.csv(file.choose())
test=read.csv(file.choose())
# combine the data togather
data=bind_rows(train,test)
train.row=1:nrow(train)
test.row=1:nrow(test)
# factorize the data set
data$Survived=factor(data$Survived)

#可通过如下方式统计出每个Pclass幸存和遇难人数，如下

ggplot(data = data[1:nrow(train),], mapping = aes(x = Pclass, y = ..count.., fill=Survived)) + 
  geom_bar(stat = "count", position='dodge') + 
  xlab('Pclass') + 
  ylab('Count') + 
  ggtitle('How Pclass impact survivor') + 
  scale_fill_manual(values=c("#FF0000", "#00FF00")) +
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

 lived=sqldf("select count(*) as lived from data  where Survived =1 group by Pclass ")
dead=sqldf("select count(*) as dead from data  where Survived =0 group by Pclass")
total=cbind(lived,dead)
total=data.frame(total)
rownames(total)=c("1","2","3")
legs
barplot(t(total),beside=T,xlab='Pclass',ylab='numbers',col=terrain.colors(2))
legend("topleft", legend = c("Survived","Dead"), fill = terrain.colors(2), box.col = "transparent")
title(main = "the relation between pclass and survived")
