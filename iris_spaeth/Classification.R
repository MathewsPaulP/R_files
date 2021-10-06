setwd("H:/R/")
getwd()

#<-------------------ASSESSED------------------------>#
#1
iris = read.csv('H:/YEAR 3/AI/Data Sets/iris.csv', sep=",")
iris_real = read.csv('H:/YEAR 3/AI/Data Sets/iris_real.csv', sep=",")

#randomisation
testing <- data.frame(iris_real,iris)
iris_rand = testing[sample(149,149),]

iclass = iris_rand[,1]  
ivalues = iris_rand[,-1]

#set up a training set 
iclassTrain = iclass[1:100] 
ivaluesTrain = ivalues[1:100,] 

#setting up testset 
iclassTest = iclass[100:149] 
ivaluesTest = ivalues[100:149,]

#function install
#install.packages("rpart")

library(rpart)
#decision tree build
fit <- rpart(iclassTrain~., method="class", data=ivaluesTrain)

#learnt decision tree plotting
plot(fit, uniform=TRUE, main="Decision Tree for iris")
text(fit, use.n=TRUE, all=TRUE, cex=0.8)

#2
#test set 
treepred <-predict(fit, ivaluesTest, type = 'class')
#accuracy without pruning
n = length(iclassTest) #the number of test cases
ncorrect = sum(treepred==iclassTest) #the number of correctly predicted
accuracy=ncorrect/n
print(accuracy)

#3
#plotting two variables according to predicted tree values
df <- data.frame(id=1:n, x1=abs(ivaluesTest$X5.1),x2=abs(ivaluesTest$X3.5),x3=treepred)
plot(df$x1,df$x2,col=df$x3)


#prune accuracy
x<-0
p_acc <- c()
for(i in 1:10)
  {x=x+0.1
   pfit<- prune(fit, cp=x)
   treepred <-predict(pfit, ivaluesTest, type = 'class')

   n = length(iclassTest) #the number of test cases
   ncorrect = sum(treepred==iclassTest) #the number of correctly predicted
   accuracy=ncorrect/n
   print(accuracy)
   p_acc[[i]]<-accuracy
   #plot(pfit, uniform=TRUE, main="Pruned Decision Tree for iris")
   #text(pfit, use.n=TRUE, all=TRUE, cex=.8)
 }


#4
library(class)
y<-0
k_acc <- c()
for(j in 1:10)
   {y=y+1
    knn3pred =  knn(ivaluesTrain, ivaluesTest, iclassTrain, k=y)#calculating k nearest neighbour
    n = length(iclassTest) #the number of test cases 
    ncorrect = sum(knn3pred==iclassTest) #the number of correctly predicted 
    accuracy=ncorrect/n 
    print(accuracy)
    k_acc[[j]]<-accuracy
   }

#accuracy line plots
pt<-c(p_acc,k_acc)
plot(pt,type='l') #single line plot with prune accuracy followed by KNN accuracy

rn<-c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0)
plot(rn, p_acc, type = "o", col="blue",pch="0") #prune accuracy line graph
plot(rn, k_acc, type = "o",col="red",pch="+") #KNN accuracy line graph
