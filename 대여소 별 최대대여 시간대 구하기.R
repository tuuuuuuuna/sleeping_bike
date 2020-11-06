#데이터 불러오기
getwd()
setwd("C:/R")
getwd()

first <- read.csv("3_2.csv",header = TRUE)
station <- read.csv("station.csv")
daytime <- read.csv("daytime.csv", header = TRUE)

time <- c(0:23)
day <- c(1:31)

colnames(daytime) <- day
colnames(daytime)

#maxtime 함수선언
maxtime_fun <- function(x){
  sumtime <- rbind(c(0:23),rep(0,24))
  for(i in 1:24){
    sumtime[2,i]<-sum(x[i,])
  }
  
  mmm=sumtime[1,1]
  mm=0
  m=0
  nnn=sumtime[2,1]
  nn=0
  n=0
  for(i in 2:24){
    if(nnn<sumtime[2,i]){
      n<-nn
      nn<-nnn
      nnn<-sumtime[2,i]
      m<-mm
      mm<-mmm
      mmm<-sumtime[1,i]
    }
    else if(nn<sumtime[2,i]&& sumtime[2,i]<nnn){
      n<-nn
      nn<-sumtime[2,i]
      m<-mm
      mm<-sumtime[1,i]
    }
    else if(n<sumtime[2,i] && sumtime[2,i]<nn){
      n<-sumtime[2,i]
      m<-sumtime[1,i]
    }
  }
  maxtime <- c(mmm,mm,m)
  return(maxtime)
}

min(station[,1]):max(station[,1])
data=0

first$대여소번호 <- as.integer(first$대여소번호)
first$반납대여소번호 <- as.integer(first$반납대여소번호)
min(first$대여소번호)
str(first$대여소번호)
for(id in min(station[,1]):max(station[,1])){
  print(id)
  #대여소번호
  first_re <- first[first$대여소번호==id,]
  #대여 데이타임 초기화
  daytime <- read.csv("daytime.csv", header = TRUE)
  #대여 데이타임
  for(i in 1:nrow(first_re)){
    for(k in 1:31){
      for(j in 1:24){
        if(first_re[i,3]==k){
          if(first_re[i,4]== j-1){
            daytime[j,k] =  daytime[j,k] + 1
          }
        }
      }
    }
  }
  daytime <- daytime - 1

  #최대 대여 빈도 시간 순서
  maxtime <- maxtime_fun(daytime)
  
  
  #반납대여소번호
  first_re <- first[first$반납대여소번호==id,]
  #대여 데이타임 초기화
  daytime <- read.csv("daytime.csv", header = TRUE)
  #대여 데이타임
  for(i in 1:nrow(first_re)){
    for(k in 1:31){
      for(j in 1:24){
        if(first_re[i,3]==k){
          if(first_re[i,4]== j-1){
            daytime[j,k] =  daytime[j,k] + 1
          }
        }
      }
    }
  }
  daytime <- daytime - 1

  #반납시간
  maxtime <- c(id,maxtime,maxtime_fun(daytime))
  data <- rbind(data,maxtime)
}
label<-c("대여소번호","1등대여","2등대여","3등대여","1등반납","2등반납","3등반납")
str(data)
data<- data[-1,]
colnames(data)<-label

data<-as.data.frame(data)

write.csv(data,"123대여.csv")

for(i in num1){
  print(i)
}

str(first)
first_rent <- first[-c(7:11)]
str(first_rent)
summary(is.na(first_rent$대여일))
str(first_rent)

num1<-as.numeric(rownames(table(first_rent$대여소번호)))
str(num1)
str(first_rent1)
summary(is.na(first_rent1$대여일))
data=0
for(id in num1){
  print(id)
  #대여소번호
  first_rent1 <- first_rent[first_rent$대여소번호==id,]
  #대여 데이타임 초기화
  daytime <- read.csv("daytime.csv", header = TRUE)
  #대여 데이타임
  for(i in 1:nrow(first_rent1)){
    for(k in 1:31){
      for(j in 1:24){
        if(first_rent1[i,3]==k){
          if(first_rent1[i,4]== j-1){
            daytime[j,k] =  daytime[j,k] + 1
          }
        }
      }
    }
  }
  daytime <- daytime - 1
  #최대 대여 빈도 시간 순서
  maxtime <- c(id,maxtime_fun(daytime))
  data <-rbind(data,maxtime)
}
write.csv(data,"rent_time_max.csv")

subset(first_rent, first_rent$대여소번호>101,sort=F)
str(first_rent$대여소번호[first_rent$대여소번호==101,])
str(first_rent$대여소번호)
summary(first_rent$대여소번호)

is.na(num1)

str(first)
first_return <- first[-c(2:6)]
str(first_return)
summary(is.na(first_return$반납일))
str(first_return)

num2<-as.numeric(rownames(table(first_return$반납대여소번호)))

summary(is.na(first_return$반납일))
data_return=0
for(id in num2){
  print(id)
  #반납대여소번호
  first_return1 <- first_return[first_return$반납대여소번호==id,]
  #반납 데이타임 초기화
  daytime <- read.csv("daytime.csv", header = TRUE)
  #반납 데이타임
  for(i in 1:nrow(first_return1)){
    for(k in 1:31){
      for(j in 1:24){
        if(first_return1[i,3]==k){
          if(first_return1[i,4]== j-1){
            daytime[j,k] =  daytime[j,k] + 1
          }
        }
      }
    }
  }
  daytime <- daytime - 1
  #최대 대여 빈도 시간 순서
  maxtime <- c(id,maxtime_fun(daytime))
  data_return <-rbind(data,maxtime)
}


summary(table(num2))
subset(first_rent, first_rent$대여소번호>101,sort=F)
str(first_rent$대여소번호[first_rent$대여소번호==101,])
str(first_rent$대여소번호)
summary(first_rent$대여소번호)

write.csv(data_return,"return_time_max.csv")
