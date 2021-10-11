library(gridExtra)
library(ggplot2)
library(ggmosaic)

#data Import
titanic.train<-read.csv(file.choose())
titanic.test<-read.csv(file.choose())

#data checked
str(titanic.train)
str(titanic.test)

#summary
summary(titanic.train)

#Visualization [fare(요금)/age]
titanic_fare<-ggplot(titanic.train,aes(x=Fare,y=..density..),main="",xlab="fare distribution")+
  geom_density(fill="blue",alpha=0.2)
           
titanic_age<-ggplot(titanic.train,aes(x=Age,y=..density..),main="",xlab="age distribution")+
  geom_density(fill="blue",alpha=0.2)
grid.arrange(titanic_fare,titanic_age,nrow=1,ncol=2)

#모지이크 플롯 pclass<-칸등급
par(mfrow=c(1,2))
mosaicplot(table(ifelse(titanic.train$Survived==1,"Survived","Dead"),titanic.train$Sex),main="",cex=1.2,color=TRUE)
mosaicplot(table(ifelse(titanic.train$Survived==1,"Survived","Dead"),titanic.train$Pclass),main="",cex=1.2,color=TRUE)

#box plot/ jitter plot
par(mfrow=c(1,2))
boxplot(Age~Survived,titanic.train,xlab = "Survival",ylab = "Age",cex=1.2)
plot(Age~jitter(Survived),titanic.train,cex=1.2,xlab="Survival")

#scatter plot
ggplot(titanic.train,aes(Age,log(Fare),color=factor(Survived),shape=factor(Sex))) +
  geom_point()+geom_jitter()
