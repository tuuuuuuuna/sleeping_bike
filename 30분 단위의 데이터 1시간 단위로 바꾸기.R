getwd()
setwd("C:/R")
getwd()



#################test
rent_return112 <- read.csv("2분크롤링112_대여반납.csv")

rent_return112 <-rent_return112[-1,]
rent_return112 <-rent_return112[-c(97,98),]
tail(rent_return112)


new_112<- c(0,0)
for(i in 1:96){
  if (i%%2){
    temp_rent <- rent_return112[i,5]
    temp_return <- rent_return112[i,6]
  }
  else{
    temp_rent = temp_rent + rent_return112[i,5]
    temp_return = temp_return + rent_return112[i,6]
    new_112<-rbind(new_112,c(temp_rent,temp_return))
  }
}
new_112<-new_112[-1,]


colnames(new_112) = c("대여량_112","반납량_112")

data<-cbind(new_112)
data
write.csv(data,"2분크롤링_1시간.csv")
