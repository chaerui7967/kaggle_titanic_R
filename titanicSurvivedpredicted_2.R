install.packages("randomForest")
library(gridExtra)
library(ggplot2)
library(ggmosaic)
library(randomForest)

#data Import/전처리 준비
titanic.train<-read.csv(file.choose())
titanic.test<-read.csv(file.choose())

head(titanic.train)
head(titanic.test)

str(titanic.train)
str(titanic.test)

##Train,Test 구분을 위한 새로운 칼럼 추가
titanic.train$IsTrainSet<-TRUE
titanic.test$IsTrainSet<-FALSE

##칼럼명 확인
names(titanic.train)
names(titanic.test)

##테스트셋 survived 칼럼 추가
titanic.test$Survived<-NA

##Train+Test
titanic.full<-rbind(titanic.train,titanic.test)
table(titanic.full$IsTrainSet)#checked

#데이터 전처리-1<-결측치 처리
##결측치 확인(Embarked)
table(titanic.full$Embarked)
##결측치에 임의값 부여
titanic.full[titanic.full$Embarked=='',"Embarked"]<-'S'

##결측치 확인(age)
table(titanic.full$Age)
##결측치에 중앙값 부여 /중앙값이 아닌 더 정교한 것으로 했을 시 예측치가 올라감
age.median<-median(titanic.full$Age,na.rm = TRUE)
titanic.full[is.na(titanic.full$Age),"Age"]<-age.median

##결측치 확인(Fare)/age결측치 처리와 같음
table(is.na(titanic.full$Fare))
fare.median<-median(titanic.full$Fare,na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare),"Fare"]<-fare.median

#데이터 전처리2<-범주형 데이터 변환
titanic.full$Pclass<-as.factor(titanic.full$Pclass)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)

##전처리 끝난 데이터셋 다시 원 데이터셋에 돌려놓기
titanic.train<- titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test<-titanic.full[titanic.full$IsTrainSet==FALSE,]

##survived 항목 데이터변환
titanic.train$Survived<-as.factor(titanic.train$Survived)

#randomForest Formula사용/모델 만들기
##modeling
survied.equation<-"Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survied.formula<-as.formula(survied.equation)
#library(randomForest)
titanic.model<-randomForest(formula=survied.formula, data=titanic.train,ntree=500, mtry=3, nodesize = 0.01 * nrow(titanic.test))

Survived <- predict(titanic.model,newdata = titanic.test)

PassengerId<-titanic.test$PassengerId
output.df<-as.data.frame(PassengerId)
output.df$Survived<-Survived

write.csv(output.df, file = "kaggle_submission.csv",row.names = FALSE)

#최종 예측치
table(output.df$Survived)
