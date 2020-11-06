b112 <-read.csv("s112.csv")
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

########112만들기
datadata<-station112$rent
weather112<- cbind(weather,datadata)
head(weather112)
#data.one 만들기
data.one112<- weather112[,c(5:39)]
#########randomForest 모델 생성
#파라미터 설정
tuning.rf112 <- tune.randomForest(data.one112[,-35], y = data.one112$datadata, 
                                  data = data.one112, ntree = seq(50, 150, by = 10), mtry = 10)

#OOB : Out-of-Bag Error
data.one112[,35] <- factor(data.one112[,35])
fit.datadata.rf112 <- randomForest(datadata ~ ., ntree = as.numeric(tuning.rf112$best.parameters[2]), 
                                   mtry = as.numeric(tuning.rf112$best.parameters[1]), do.trace = 30, nodesize = 10, 
                                   importance = T, data = data.one112)
fit.datadata.rf112
