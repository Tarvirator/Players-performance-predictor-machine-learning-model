messi_data<-read.csv("messi.csv")
ronaldo_data<-read.csv("ronaldo.csv")
neymar_data<-read.csv("neymar.csv")
aguero_data<-read.csv("aguero.csv")
salah_data<-read.csv("salah.csv")
mane_data<-read.csv("mane.csv")
suarez_data<-read.csv("suarez.csv")
lewandowski_data<-read.csv("lewandowski.csv")


#------player and season vector------
players<-c("messi","ronaldo","neymar","aguero","salah","mane","suarez","lewandowski","kane","immobile")
seasons<-c("2009-10","2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19")

#------naming colums--------
colnames(messi_data)<-c("appearances","goals","assists","minutesplayed","seasons")
rownames(messi_data)<-seasons

colnames(ronaldo_data)<-c("appearances","goals","assists","minutesplayed","seasons")
rownames(ronaldo_data)<-seasons

colnames(neymar_data)<-c("appearances","goals","assists","minutesplayed","seasons")
rownames(neymar_data)<-seasons

colnames(aguero_data)<-c("appearances","goals","assists","minutesplayed","seasons")
rownames(aguero_data)<-seasons

colnames(salah_data)<-c("appearances","goals","assists","minutesplayed","seasons")
rownames(salah_data)<-seasons

colnames(mane_data)<-c("appearances","goals","assists","minutesplayed","seasons")
rownames(mane_data)<-seasons

colnames(suarez_data)<-c("appearances","goals","assists","minutesplayed","seasons")
rownames(suarez_data)<-seasons

colnames(lewandowski_data)<-c("appearances","goals","assists","minutesplayed","seasons")
rownames(lewandowski_data)<-seasons

colnames(kane_data)<-c("appearances","goals","assists","minutesplayed","seasons")
rownames(kane_data)<-seasons

colnames(immobile_data)<-c("appearances","goals","assists","minutesplayed","seasons")
rownames(immobile_data)<-seasons


#---------------creating regression model--------------

#messi

library(caTools)
set.seed(123)
split<-sample.split(messi_data$goals,SplitRatio = 40/50)

training_set<-subset(messi_data,split==TRUE)
test_set<-subset(messi_data,split==FALSE)

regressor_messi<-lm(goals~seasons+minutesplayed+appearances,data = training_set)
library(ggplot2)
ggplot()+
  geom_point(aes(x=training_set$seasons,y=training_set$goals),color="red")+
  geom_line(aes(x=training_set$seasons,y=predict(regressor_messi,newdata = training_set)),color="blue")+
  xlab("seasons (2009-10 to 2018-19)")+
  ylab("goals")+
  ggtitle("regression model for messi")

#model testing
test_pred<-round(predict(regressor_messi,test_set))
test_pred
test_set$goals

error_percentage<-mean((abs(test_pred-test_set$goals))/test_set$goals*100)
error_percentage


#---------------it's predition time---------------------

next_season_prediction<-predict(regressor_messi,data.frame(seasons=11,minutesplayed=4500,appearances=55))
round(next_season_prediction)


#ronaldo

library(caTools)
set.seed(123)
split<-sample.split(ronaldo_data$goals,SplitRatio = 40/50)

training_set<-subset(ronaldo_data,split==TRUE)
test_set<-subset(ronaldo_data,split==FALSE)

regressor_ronaldo<-lm(goals~seasons+appearances,data = training_set)


ggplot()+
  geom_point(aes(x=training_set$seasons,y=training_set$goals),color="red")+
  geom_line(aes(x=training_set$seasons,y=predict(regressor_ronaldo,newdata = training_set)),color="blue")+
  xlab("seasons (2009-10 to 2018-19)")+
  ylab("goals")+
  ggtitle("regression model for ronaldo")

#model testing
test_pred<-round(predict(regressor_ronaldo,test_set))
test_pred
test_set$goals

error_percentage<-mean((abs(test_pred-test_set$goals))/test_set$goals*100)
error_percentage


#---------------it's predition time---------------------

next_season_prediction_ronaldo<-predict(regressor_ronaldo,data.frame(seasons=11,appearances=48))
round(next_season_prediction_ronaldo)

#neymar

library(caTools)
set.seed(123)
split<-sample.split(neymar_data$goals,SplitRatio = 40/50)

training_set<-subset(neymar_data,split==TRUE)
test_set<-subset(neymar_data,split==FALSE)

regressor_neymar<-lm(goals~seasons,data = training_set)


ggplot()+
  geom_point(aes(x=training_set$seasons,y=training_set$goals),color="red")+
  geom_line(aes(x=training_set$seasons,y=predict(regressor_neymar,newdata = training_set)),color="blue")+
  xlab("seasons (2009-10 to 2018-19)")+
  ylab("goals")+
  ggtitle("regression model for neymar")

#model testing
test_pred<-round(predict(regressor_neymar,test_set))
test_pred
test_set$goals

error_percentage<-mean((abs(test_pred-test_set$goals))/test_set$goals*100)
error_percentage


#---------------it's predition time---------------------

next_season_prediction_neymar<-predict(regressor_neymar,data.frame(seasons=11))
round(next_season_prediction_neymar)

#aguero
library(caTools)
set.seed(123)
split<-sample.split(aguero_data$goals,SplitRatio = 40/50)

training_set<-subset(aguero_data,split==TRUE)
test_set<-subset(aguero_data,split==FALSE)

regressor_aguero<-lm(goals~seasons+appearances,data = training_set)


ggplot()+
  geom_point(aes(x=training_set$seasons,y=training_set$goals),color="red")+
  geom_line(aes(x=training_set$seasons,y=predict(regressor_aguero,newdata = training_set)),color="blue")+
  xlab("seasons (2009-10 to 2018-19)")+
  ylab("goals")+
  ggtitle("regression model for aguero")

#model testing
test_pred<-round(predict(regressor_aguero,test_set))
test_pred
test_set$goals

error_percentage<-mean((abs(test_pred-test_set$goals))/test_set$goals*100)
error_percentage


#---------------it's predition time---------------------

next_season_prediction_aguero<-predict(regressor_aguero,data.frame(seasons=11,appearances=50))
round(next_season_prediction_aguero)


#salah
library(caTools)
set.seed(123)
split<-sample.split(salah_data$goals,SplitRatio = 40/50)

training_set<-subset(salah_data,split==TRUE)
test_set<-subset(salah_data,split==FALSE)

regressor_salah<-lm(goals~seasons+appearances,data = training_set)


ggplot()+
  geom_point(aes(x=training_set$seasons,y=training_set$goals),color="red")+
  geom_line(aes(x=training_set$seasons,y=predict(regressor_salah,newdata = training_set)),color="blue")+
  xlab("seasons (2009-10 to 2018-19)")+
  ylab("goals")+
  ggtitle("regression model for salah")

#model testing
test_pred<-round(predict(regressor_salah,test_set))
test_pred
test_set$goals

error_percentage<-mean((abs(test_pred-test_set$goals))/test_set$goals*100)
error_percentage


#---------------it's predition time---------------------

next_season_prediction_salah<-predict(regressor_salah,data.frame(seasons=11,appearances=55))
round(next_season_prediction_salah)

#lewandowski

library(caTools)
set.seed(123)
split<-sample.split(lewandowski_data$goals,SplitRatio = 40/50)

training_set<-subset(lewandowski_data,split==TRUE)
test_set<-subset(lewandowski_data,split==FALSE)

regressor_lewandowski<-lm(goals~seasons,data = training_set)


ggplot()+
  geom_point(aes(x=training_set$seasons,y=training_set$goals),color="red")+
  geom_line(aes(x=training_set$seasons,y=predict(regressor_lewandowski,newdata = training_set)),color="blue")+
  xlab("seasons (2009-10 to 2018-19)")+
  ylab("goals")+
  ggtitle("regression model for lewandowski")

#model testing
test_pred<-round(predict(regressor_lewandowski,test_set))
test_pred
test_set$goals

error_percentage<-mean((abs(test_pred-test_set$goals))/test_set$goals*100)
error_percentage


#---------------it's predition time---------------------

next_season_prediction_lewandowski<-predict(regressor_lewandowski,data.frame(seasons=11))
round(next_season_prediction_lewandowski)

#mane


library(caTools)
set.seed(123)
split<-sample.split(mane_data$goals,SplitRatio = 40/50)

training_set<-subset(mane_data,split==TRUE)
test_set<-subset(mane_data,split==FALSE)

regressor_mane<-lm(goals~seasons+appearances+minutesplayed,data = training_set)

ggplot()+
  geom_point(aes(x=training_set$seasons,y=training_set$goals),color="red")+
  geom_line(aes(x=training_set$seasons,y=predict(regressor_mane,newdata = training_set)),color="blue")+
  xlab("seasons (2009-10 to 2018-19)")+
  ylab("goals")+
  ggtitle("regression model for mane")

#model testing
test_pred<-round(predict(regressor_mane,test_set))
test_pred
test_set$goals

error_percentage<-mean((abs(test_pred-test_set$goals))/test_set$goals*100)
error_percentage


#---------------it's predition time---------------------

next_season_prediction_mane<-predict(regressor_mane,data.frame(seasons=11,appearances=60,minutesplayed=5000))
round(next_season_prediction_mane)

#suarez

library(caTools)
set.seed(123)
split<-sample.split(suarez_data$goals,SplitRatio = 40/50)

training_set<-subset(suarez_data,split==TRUE)
test_set<-subset(suarez_data,split==FALSE)

regressor_suarez<-lm(goals~seasons+assists,data = training_set)

ggplot()+
  geom_point(aes(x=training_set$seasons,y=training_set$goals),color="red")+
  geom_line(aes(x=training_set$seasons,y=predict(regressor_suarez,newdata = training_set)),color="blue")+
  xlab("seasons (2009-10 to 2018-19)")+
  ylab("goals")+
  ggtitle("regression model for suarez")

#model testing
test_pred<-round(predict(regressor_suarez,test_set))
test_pred
test_set$goals

error_percentage<-mean((abs(test_pred-test_set$goals))/test_set$goals*100)
error_percentage


#---------------it's predition time---------------------

next_season_prediction_suarez<-predict(regressor_suarez,data.frame(seasons=11,assists=10))
round(next_season_prediction_suarez)

