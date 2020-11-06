#패키지 다운
install.packages("randomForest")
install.packages("caret")
library(randomForest)
library(caret)


library(e1071)
install.packages("e1071")
getwd()
setwd("C:/R")
getwd()

x112 <-read.csv("x112.csv")

########날씨 전처리
weather <- read.csv("weather1.csv")

#시간범주화
hour0<-0;hour1<-0;hour2<-0;hour3<-0;hour4<-0;hour5<-0;hour6<-0;hour7<-0;hour8<-0;hour9<-0;hour10<-0;
hour11<-0;hour12<-0;hour13<-0;hour14<-0;hour15<-0;hour16<-0;hour17<-0;hour18<-0;hour19<-0;hour20<-0;hour21<-0;hour22<-0;hour23<-0;
weather<-cbind(weather,hour0,hour1,hour2,hour3,hour4,hour5,hour6,hour7,hour8,hour9,hour10,hour11,hour12,hour13,hour14,hour15,hour16,hour17,hour18,hour19,hour20,hour21,hour22,hour23)
weather$hour0[ weather$시간 ==0] <- 1
weather$hour1[ weather$시간 ==1] <- 1
weather$hour2[ weather$시간 ==2] <- 1
weather$hour3[ weather$시간 ==3] <- 1
weather$hour4[ weather$시간 ==4] <- 1
weather$hour5[ weather$시간 ==5] <- 1
weather$hour6[ weather$시간 ==6] <- 1
weather$hour7[ weather$시간 ==7] <- 1
weather$hour8[ weather$시간 ==8] <- 1
weather$hour9[ weather$시간 ==9] <- 1
weather$hour10[ weather$시간 ==10] <- 1
weather$hour11[ weather$시간 ==11] <- 1
weather$hour12[ weather$시간 ==12] <- 1
weather$hour13[ weather$시간 ==13] <- 1
weather$hour14[ weather$시간 ==14] <- 1
weather$hour15[ weather$시간 ==15] <- 1
weather$hour16[ weather$시간 ==16] <- 1
weather$hour17[ weather$시간 ==17] <- 1
weather$hour18[ weather$시간 ==18] <- 1
weather$hour19[ weather$시간 ==19] <- 1
weather$hour20[ weather$시간 ==20] <- 1
weather$hour21[ weather$시간 ==21] <- 1
weather$hour22[ weather$시간 ==22] <- 1
weather$hour23[ weather$시간 ==23] <- 1
head(weather)

#요일변수 범주화
MonThur <- 0
Fri<-0
Sat<-0
Sun<-0
weather<-cbind(weather,MonThur,Fri,Sat,Sun)
weather$MonThur[ weather$요일.코딩.월.목.1. ==1] <- 1
weather$Fri[ weather$요일.코딩.월.목.1. ==2] <- 1
weather$Sat[ weather$요일.코딩.월.목.1. ==3] <- 1
weather$Sun[ weather$요일.코딩.월.목.1. ==4] <- 1
head(weather)

tuning.rf112 <- tune.randomForest(data.one112[,-35], y = data.one112$datadata, data = data.one112, ntree = seq(90, 150, by = 10), mtry = 10:11)
tuning.rf112


#OOB : Out-of-Bag Error
data.one112[,35] <- factor(data.one112[,35])
fit.datadata.rf112 <- randomForest(datadata ~ ., ntree = 150, mtry = 11, do.trace = 30, nodesize = 10, importance = T, data = data.one112)
fit.datadata.rf112

###변수 중요도 출력
###importance(fit.datadata.rf)


#모형 출력값
pred_tr.datadata.rf112 <- predict(fit.datadata.rf112, newdata = data.one112[,-35])

#성능비교
compair112<-cbind(data.one112$datadata,pred_tr.datadata.rf112)

write.csv(compair112,file="모델성능112_2.csv")


CM_datadata.rf112 <- table(actual = data.one112$datadata, predicted = pred_tr.datadata.rf112)
CM_datadata.rf112




#################test
weather_test <- read.csv("weather_test11.csv")
w_weather_test <- read.csv("weather_test11.csv")

#시간범주화
hour0<-0;hour1<-0;hour2<-0;hour3<-0;hour4<-0;hour5<-0;hour6<-0;hour7<-0;hour8<-0;hour9<-0;hour10<-0;
hour11<-0;hour12<-0;hour13<-0;hour14<-0;hour15<-0;hour16<-0;hour17<-0;hour18<-0;hour19<-0;hour20<-0;hour21<-0;hour22<-0;hour23<-0;
weather_test<-cbind(weather_test,hour0,hour1,hour2,hour3,hour4,hour5,hour6,hour7,hour8,hour9,hour10,hour11,hour12,hour13,hour14,hour15,hour16,hour17,hour18,hour19,hour20,hour21,hour22,hour23)
weather_test$hour0[ weather_test$시간 ==0] <- 1
weather_test$hour1[ weather_test$시간 ==1] <- 1
weather_test$hour2[ weather_test$시간 ==2] <- 1
weather_test$hour3[ weather_test$시간 ==3] <- 1
weather_test$hour4[ weather_test$시간 ==4] <- 1
weather_test$hour5[ weather_test$시간 ==5] <- 1
weather_test$hour6[ weather_test$시간 ==6] <- 1
weather_test$hour7[ weather_test$시간 ==7] <- 1
weather_test$hour8[ weather_test$시간 ==8] <- 1
weather_test$hour9[ weather_test$시간 ==9] <- 1
weather_test$hour10[ weather_test$시간 ==10] <- 1
weather_test$hour11[ weather_test$시간 ==11] <- 1
weather_test$hour12[ weather_test$시간 ==12] <- 1
weather_test$hour13[ weather_test$시간 ==13] <- 1
weather_test$hour14[ weather_test$시간 ==14] <- 1
weather_test$hour15[ weather_test$시간 ==15] <- 1
weather_test$hour16[ weather_test$시간 ==16] <- 1
weather_test$hour17[ weather_test$시간 ==17] <- 1
weather_test$hour18[ weather_test$시간 ==18] <- 1
weather_test$hour19[ weather_test$시간 ==19] <- 1
weather_test$hour20[ weather_test$시간 ==20] <- 1
weather_test$hour21[ weather_test$시간 ==21] <- 1
weather_test$hour22[ weather_test$시간 ==22] <- 1
weather_test$hour23[ weather_test$시간 ==23] <- 1

#요일변수 범주화
MonThur <- 0;Fri<-0;Sat<-0;Sun<-0
weather_test<-cbind(weather_test,MonThur,Fri,Sat,Sun)
weather_test$MonThur[ weather_test$요일.코딩.월.목.1. ==1] <- 1
weather_test$Fri[ weather_test$요일.코딩.월.목.1. ==2] <- 1
weather_test$Sat[ weather_test$요일.코딩.월.목.1. ==3] <- 1
weather_test$Sun[ weather_test$요일.코딩.월.목.1. ==4] <- 1
head(weather_test)

test_data <- weather_test[,c(4:38)]
head(test_data)

#예측값_test
pred112 <- predict(fit.datadata.rf112, newdata = test_data)

pred_bike <- cbind(w_weather_test,pred112)
write.csv(pred_bike,file="예측자료.csv")
