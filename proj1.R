library(readxl)
ins1 <- read_excel("C:/Users/shrey/OneDrive/Desktop/ins1.xlsx")
View(ins1)
dt<- ins1
str(dt)
summary(dt)
dt <- na.omit(dt)
summary(dt)
View(dt)
dt$`Release Date`<- as.Date(dt$`Release Date`,"%d-%m-%Y")
unique(dt$`Release Date (N / LW / Festive)`)
dt$`Release Date (N / LW / Festive)`<- factor(dt$`Release Date (N / LW / Festive)`, levels = c("LW","N","HS","FS"), labels = c("Long Weekend","Normal","Holiday Season","Festive Season"))
str(dt)
unique(dt$`Genre - Defined`)
dt$`Genre - Defined`<- factor(dt$`Genre - Defined`, levels = c("Romance","Thriller","Comedy","Drama","Action"), labels = c("Romance","Thriller","Comedy","Drama","Action"))

install.packages("RCurl")
install.packages("bitops")
install.packages("Amelia")
install.packages("Rcpp")
install.packages("Hmisc")
install.packages("lattice")
install.packages("survival")
install.packages("Formula")
install.packages("ggplot2")

library(RCurl)
library(bitops)
library(Amelia)
library(Rcpp)
library(Hmisc)
library(lattice)
library(survival)
library(ggplot2)
library(Formula)

str(dt)

hist(dt$`Box Office Collection`)
hist(dt$`Box Office Collection`,main="Box Office Collection frequency",xlab="Box office Collection",col="lightblue",labels=TRUE)
hist(dt$`Box Office Collection`,main="Box Office Collection frequency",xlab="Box office Collection",col="lightblue",labels=TRUE, breaks = 10)

hist(dt$Budget,main="Budget",xlab="Budget",col="pink",labels=TRUE,xlim = c(0,200), ylim = c(0,100))

box_plot1<-ggplot(dt,aes(x= dt$`Genre - Defined`,y=dt$Budget))+geom_boxplot()+theme_classic()
box_plot1

dt %>% select(Movies, `Genre - Defined`, Budget) %>% filter(Budget>40 & `Genre - Defined` == `Thriller`) %>% summarize(MOvies)

box_plot2<-ggplot(dt,aes(x= `Genre - Defined`,y= Budget))+geom_point(aes(color=`Box Office Collection`))+theme_classic()
box_plot2

box_plot3<-ggplot(dt,aes(x=Budget,y= `Youtube Likes`))+geom_point(aes(color=`Genre - Defined`))+theme_classic()
box_plot3

box_plot3<-ggplot(dt,aes(x=Budget))+geom_histogram(aes(color=`Genre - Defined`))+theme_classic()
box_plot3

box_plot4<-ggplot(dt,aes(x=`Youtube Likes`))+geom_histogram(aes(color=`Genre - Defined`))+theme_classic()
box_plot4

box_plot5<-ggplot(dt,aes(x=Budget,y= `Youtube Dislikes`))+geom_point(aes(color=`Genre - Defined`))+theme_classic()
box_plot5

box_plot6<-ggplot(dt,aes(x=`Youtube Likes`,y= `Youtube Dislikes`))+geom_point(aes(color=`Genre - Defined`))+theme_classic()
box_plot6

box_plot7<-ggplot(dt,aes(x=`Youtube Likes`,y= `Youtube Dislikes`))+geom_point(aes(color=`Genre - Defined`))+theme_classic()
box_plot7

str(dt)

box_plot8<-ggplot(dt,aes(x=`Budget`))+geom_histogram(aes(color=`Release Date (N / LW / Festive)`))+theme_classic()
box_plot8

box_plot9<-ggplot(dt,aes(x=`Release Date (N / LW / Festive)`,y= Budget))+geom_point(aes(color= "pink"))+theme_classic()
box_plot9

hist(dt$`Youtube Views`)
