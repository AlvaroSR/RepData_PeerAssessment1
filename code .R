##### Peer Assessment 1 ####
# Reproducible research course
# Álvaro 
# 
#####System information####
info=R.Version()
info$platform
info$version.string
Sys.Date()

Sys.setlocale("LC_TIME", "en_US.UTF-8") # system english adaptation

####Loading libraries#########################
ipak <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg)) 
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
}

ipak(c("ggplot2","dplyr","lubridate"))


####Loading data##############################

setwd("/media/DATA2/cursos/R/Reproducible Research/Peer Assessment 1/RepData_PeerAssessment1/")
unzip("activity.zip")
data_act=read.csv("activity.csv")


####Procesing Data####
data_act$date=as.Date(data_act$date)
data_act$julian=julian(data_act$date,origin=min(data_act$date))


##### 1st Question#####
# What is mean total number of steps per day?

by_day=group_by(data_act,julian)
steps_per_day=summarise(by_day,sumatory=sum(steps, na.rm=TRUE),average=mean(steps))
steps_per_day=filter(steps_per_day, sumatory!=0)

ggplot(data=steps_per_day,aes(x=sumatory))+
        geom_histogram(fill="cadetblue",colour="black")+
        ylab("Count")+xlab("Total number of steps per day")+
        theme_bw()+
        theme(panel.grid.minor=element_blank(),
              axis.text.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=.5,face="bold"),
              axis.text.y = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=.5,face="bold"),
              axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=1,face="bold"),
              axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=-0.2,face="bold"),
              legend.key=element_blank()
        )


mean(steps_per_day$sumatory)
median(steps_per_day$sumatory)

#### 2nd Question####
#What is the average daily activity pattern?

by_interval=group_by(data_act,interval)
steps_per_interval=summarise(by_interval,media=mean(steps,na.rm=TRUE))

ggplot(data=steps_per_interval,aes(x=interval,y=media))+
        geom_line(colour="cadetblue")+
        ylab("Mean of steps taken every day")+xlab("Interval of the day (min)")+
        theme_bw()+
        theme(panel.grid.minor=element_blank(),
              axis.text.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=.5,face="bold"),
              axis.text.y = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=.5,face="bold"),
              axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=1,face="bold"),
              axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=-0.2,face="bold"),
              legend.key=element_blank()
        )

maximum=filter(steps_per_interval,media==max(media))
seconds_to_period(maximo$interval*60)

#### 3rd Question ####
#Imputing missing values

sum(is.na(data_act$steps))

steps_per_day=summarise(by_day,sumatory=sum(steps, na.rm=TRUE),average=mean(steps))
steps_per_day$average=ifelse(is.na(steps_per_day$average),0,steps_per_day$average)

data_act_na=merge(data_act,steps_per_day,by="julian")

data_act_na[is.na("steps"),"steps"]=data_act_na[is.na("steps"),"average"]

by_day_na=group_by(data_act_na,julian)
steps_per_day_na=summarise(by_day_na,sumatory=sum(steps, na.rm=TRUE),average=mean(steps))
steps_per_day_na=filter(steps_per_day, sumatory!=0)

ggplot(data=steps_per_day_na,aes(x=sumatory))+
        geom_histogram(fill="cadetblue",colour="black")+
        ylab("Count")+xlab("Total number of steps per day (NA included)")+
        theme_bw()+
        theme(panel.grid.minor=element_blank(),
              axis.text.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=.5,face="bold"),
              axis.text.y = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=.5,face="bold"),
              axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=1,face="bold"),
              axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=-0.2,face="bold"),
              legend.key=element_blank()
        )


mean(steps_per_day_na$sumatory)
median(steps_per_day_na$sumatory)

#### 4th Question ####

data_act$weekday=ifelse(weekdays(data_act$date)%in%c("Saturday","Sunday"),"weekend","weekday")
data_act$weekday=factor(data_act$weekday)

by_weekday=group_by(data_act,weekday,interval)
steps_per_day_int=summarise(by_weekday,steps_mean=mean(steps, na.rm = T),
                            steps_sum=sum(steps, na.rm = T))

ggplot(steps_per_day_int,aes(x=interval,y=steps_mean, colour=weekday))+
        geom_line()+
        facet_grid(weekday~.)+
        ylab("Interval (min)")+xlab("Total number of steps per day")+
        theme_bw()+
        theme(panel.grid.minor=element_blank(),
              axis.text.x = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=.5,face="bold"),
              axis.text.y = element_text(colour="grey20",size=11,angle=0,hjust=.5,vjust=.5,face="bold"),
              axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=1,face="bold"),
              axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=-0.2,face="bold"),
              strip.text.y= element_text(colour="grey20",size=11,vjust=.5,angle=0,face="bold"),
              legend.key=element_blank(),
              legend.position="none",
              strip.background=element_rect(fill="WhiteSmoke"),
              strip.background=element_blank()
        )
