getwd()
setwd("C:/R")
getwd()

str(r)

install.packages('devtools')
library('devtools')

install_github('dkahle/ggmap')
install.packages('maps')
library('maps')
library('ggmap')
library('ggplot2')
register_google(key="AIzaSyA4HeeYra0Er_My4a5zkft2oSyl2U9Uqik")


install.packages("readxl")
library(readxl)
########################지도에 표시하기
station<-read.csv("Location_station1004_01.csv")
station<-station[,-1]

more_idx <- which(station[,1] > station[,2])
less_idx <- which(station[,1] < station[,2])

cen <- c(mean(station$lon),mean(station$lat))
map <- get_map(center=cen,zoom=13,size=c(240,240),maptype="roadmap",marker=station)
mapo.map <- ggmap(map)+geom_path(data= station,aes(x=lon,y=lat),size = 2, linetype = 1, col = "green")

mapo.map<-mapo.map+geom_point(data=station[-less_idx,],aes(x=lon,y=lat),size=3,alpha=0.7,col="red")
mapo.map<-mapo.map+geom_point(data=station[more_idx,],aes(x=lon,y=lat),size=3,alpha=0.7,col="blue")
###mapo.map<-mapo.map+geom_point(data=gc[1,],aes(x=lon,y=lat),size=4,alpha=1,col="yellow")
mapo.map

