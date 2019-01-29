library(readxl)
library(dplyr)
library(knitr)
library(ggplot2)
library(stringr)

df <- read_excel('~/info350S2/experimentResult.xlsx')

########################################
###Showing a table about the time points of participants who switched
#Participants who did not switch
df_none<-df[df$V6=="none",]
name=paste("Participants who did not switch")
total_number=nrow(df_none)
case1=kable(data.frame(name,total_number))

#Participants who switched
df_do<-df[df$V6!="none",]
#First switch
switch_1 <- sapply(strsplit(as.character(df_do$V6),','), "[", 1)
#Second switch
switch_2 <- sapply(strsplit(as.character(df_do$V6),','), "[", 2)
#Third switch
switch_3 <- sapply(strsplit(as.character(df_do$V6),','), "[", 3)
#Fourth switch
switch_4 <- sapply(strsplit(as.character(df_do$V6),','), "[", 4)

switch_1 <- substring(switch_1, 13)

switch_1 <- switch_1[!is.na(switch_1)]
switch_2 <- switch_2[!is.na(switch_2)]
switch_3 <- switch_3[!is.na(switch_3)]
switch_4 <- switch_4[!is.na(switch_4)]

#First switch is at 5min point
switched_at_5=length(which(switch_1=="5"))
#First switch is at 10min point
switched_at_10=length(which(switch_1=="10"))
#First switch is at 15min point
switched_at_15=length(which(switch_1=="15"))
#First switch is at 20min point
switched_at_20=length(which(switch_1=="20"))

name=paste("The first switch for all who switched")
case2<-kable(data.frame(name,switched_at_5,switched_at_10,switched_at_15,switched_at_20))

#Second switch is at 5min point
switched_at_5=length(which(switch_2=="5"))
#Second switch is at 10min point
switched_at_10=length(which(switch_2=="10"))
#Second switch is at 15min point
switched_at_15=length(which(switch_2=="15"))
#Second switch is at 20min point
switched_at_20=length(which(switch_2=="20"))

name=paste("The second switch for all who switched")
case3<-kable(data.frame(name,switched_at_5,switched_at_10,switched_at_15,switched_at_20))

#Third switch is at 5min point
switched_at_5=length(which(switch_3=="5"))
#Third switch is at 10min point
switched_at_10=length(which(switch_3=="10"))
#Third switch is at 15min point
switched_at_15=length(which(switch_3=="15"))
#Third switch is at 20min point
switched_at_20=length(which(switch_3=="20"))

name=paste("The Third switch for all who switched")
case4<-kable(data.frame(name,switched_at_5,switched_at_10,switched_at_15,switched_at_20))

#Fourth switch is at 5min point
switched_at_5=length(which(switch_4=="5"))
#Fourth switch is at 10min point
switched_at_10=length(which(switch_4=="10"))
#Fourth switch is at 15min point
switched_at_15=length(which(switch_4=="15"))
#Fourth switch is at 20min point
switched_at_20=length(which(switch_4=="20"))

name=paste("The Fourth switch for all who switched")
case5<-kable(data.frame(name,switched_at_5,switched_at_10,switched_at_15,switched_at_20))

list(case1,case2,case3,case4,case5)
########################################################
###Showing a table about the time points of participants who switched for different times
#Participants who switched once
df_once<-df[df$V5=="1 Time(s)",]
df_once_n<- df_once%>% group_by(V6)%>%summarise(count=n())
df_once_n<-rename(df_once_n,'participants who switched once in total'=V6)
df_once_n=kable(as.data.frame.matrix(df_once_n))
#Participants who switched twice
df_twice<-df[df$V5=="2 Time(s)",]
df_twice_n<- df_twice%>% group_by(V6)%>%summarise(count=n())
df_twice_n<-rename(df_twice_n,'participants who switched twice in total'=V6)
df_twice_n=kable(as.data.frame.matrix(df_twice_n))

#Participants who switched third times
df_third_times<-df[df$V5=="3 Time(s)",]
df_third_times_n<- df_third_times%>% group_by(V6)%>%summarise(count=n())
df_third_times_n<-rename(df_third_times_n,'participants who switched third times in total'=V6)
df_third_times_n=kable(as.data.frame.matrix(df_third_times_n))

#Participants who switched fourth times
df_fourth_times<-df[df$V5=="4 Time(s)",]
df_fourth_times_n<- df_fourth_times%>% group_by(V6)%>%summarise(count=n())
df_fourth_times_n<-rename(df_fourth_times_n,'participants who switched fourth times in total'=V6)
df_fourth_times_n=kable(as.data.frame.matrix(df_fourth_times_n))

list(df_once_n,df_twice_n,df_third_times_n,df_fourth_times_n)

########################################################
#Delta for first switch only
df_first_switch<- df_do%>% select(p_number,cogproc_before_first,cogproc_after_first,V5,V6)
df_first_switch$cogproc_before_first<- as.numeric(as.character(df_first_switch$cogproc_before_first),na.rm=TRUE)
df_first_switch$cogproc_after_first<- as.numeric(as.character(df_first_switch$cogproc_after_first),na.rm=TRUE)
df_first_switch$delta<-df_first_switch$cogproc_after_first-df_first_switch$cogproc_before_first

T1<- df_first_switch%>% group_by(p_number)%>%summarize(V5==('1 Time(s)'))

df_first_switch<- left_join(df_first_switch,T1,by="p_number")
df_first_switch<-rename(df_first_switch,'Stopped switching'='V5 == (\"1 Time(s)\")','When(including first switch)'=V6)

df_first_switch<- df_first_switch%>%select(p_number,cogproc_before_first,cogproc_after_first,delta,'Stopped switching','When(including first switch)')
df_first_switch <- df_first_switch[order(-df_first_switch$delta),]
View(df_first_switch)

#Delta for all switches combined
df_all_first<- df_do%>% select(p_number,cogproc_before_first,cogproc_after_first)
df_all_first<- rename(df_all_first,before='cogproc_before_first',after='cogproc_after_first')
df_all_second<- df_do%>% select(p_number,cogproc_before_second,cogproc_after_second)
df_all_second<- rename(df_all_second,before='cogproc_before_second',after='cogproc_after_second')
df_all_third<- df_do%>% select(p_number,cogproc_before_third,cogproc_after_third)
df_all_third<- rename(df_all_third,before='cogproc_before_third',after='cogproc_after_third')
df_all_fourth<- df_do%>% select(p_number,cogproc_before_fourth,cogproc_after_fourth)
df_all_fourth<- rename(df_all_fourth,before='cogproc_before_fourth',after='cogproc_after_fourth')

df_all<-rbind(df_all_first,df_all_second,df_all_third,df_all_fourth)
df_all<-df_all[complete.cases(df_all), ]
df_all$after<- as.numeric(as.character(df_all$after))
df_all$before<- as.numeric(as.character(df_all$before))

df_all$delta=df_all$after-df_all$before
df_all <- df_all[order(-df_all$delta),]
View(df_all)

########################################################
##table including age and time came at
questionnaire <- read_excel('~/info350S2/questionnaire_edited.xlsx')
questionnaire$time<- str_sub(questionnaire$"StartDateStart Date.x",-5,-1)
questionnaire$time<-gsub(":", ".", gsub("\\.", "", questionnaire$time))
questionnaire$time<- as.numeric(as.character(questionnaire$time))
questionnaire$time=round(questionnaire$time-6,0)
# questionnaire$time<- as.character(as.numeric(questionnaire$time))
questionnaire_personal_info <- questionnaire %>% select('participant number','Q8What is your age?','time')
questionnaire_7<- questionnaire_personal_info%>% filter(time==7)
questionnaire_not_7<- questionnaire_personal_info%>% filter(time!=7)
questionnaire_7$time=questionnaire_7$time+1
questionnaire_personal_info<-rbind(questionnaire_7,questionnaire_not_7)

##bar plot about the time participants came at
questionnaire_personal_info_n<- questionnaire_personal_info%>% group_by(time)%>% summarise(n=n())

print(ggplot(data=questionnaire_personal_info_n, aes(x=time, y=n)) +
  geom_bar(stat="identity")+geom_text(aes(label=n), vjust=-0.3, size=3.5)+
    theme_minimal()+labs(x="Time participants came at",y="Total")+ggtitle("Bar plot for the time participants came at"))

##bimodal distrubution about the time participants came at
questionnaire_personal_info_moring<- questionnaire_personal_info%>% filter(time<12)
questionnaire_personal_info_moring$time_type='moring'

questionnaire_personal_info_afternoon<- questionnaire_personal_info%>% filter(time>=12)
questionnaire_personal_info_afternoon$time_type='afternoon'

questionnaire_personal_info<- rbind(questionnaire_personal_info_moring,questionnaire_personal_info_afternoon)

print(ggplot(questionnaire_personal_info, aes(x=time, fill=time_type)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")+labs(x="Time participants came at",y="Total")+ggtitle("Bimodal distribution for the time participants came at"))

##bar plot about participants' age
questionnaire_personal_info$age_interval<- findInterval(questionnaire_personal_info$'Q8What is your age?', c(10, 20, 30))
questionnaire_age_n<- questionnaire_personal_info%>% group_by(age_interval)%>% summarise(n=n())
questionnaire_age_n$age_interval<- as.character(as.numeric(questionnaire_age_n$age_interval))
questionnaire_age_n$age_interval<- recode(questionnaire_age_n$age_interval, '1'='10-20','2'='20-30','3'='30+')

print(ggplot(data=questionnaire_age_n, aes(x=age_interval, y=n)) +
        geom_bar(stat="identity")+geom_text(aes(label=n), vjust=-0.3, size=3.5)+
        theme_minimal()+labs(y="Total")+ggtitle("Bar plot for ages"))

########################################################
###Comparing cogproc in the first slot with the last slot
questionnaire_personal_info<- rename(questionnaire_personal_info,p_number='participant number')
questionnaire_personal_info$p_number<- as.numeric(as.character(questionnaire_personal_info$p_number))
df_overall<- df%>%select(p_number,cogproc_all_with)
df_overall<- left_join(df_overall,questionnaire_personal_info,by='p_number')
df_overall$cogproc_all_with<- as.numeric(as.character(df_overall$cogproc_all_with))
df_overall$age_interval<- as.character(as.numeric(df_overall$age_interval))
df_overall$age_interval<- recode(df_overall$age_interval, '1'='10-20','2'='20-30','3'='30+')

##Overall scatter plot
print(ggplot(df_overall, aes(x=time, y=cogproc_all_with)) +
  geom_point(aes(color=age_interval),size=4, alpha=0.4)+ggtitle("Scatter plot about participants overall cognition and the time they came at") +
    labs(x='Time participants came at', y='Overall cogproc'))

##Scatter plot comparing cogproc between participants who came at the first and last slot
df_first_and_last_slot<- df_overall%>%filter(time %in% c(max(df_overall$time),min(df_overall$time)))
print(ggplot(df_first_and_last_slot, aes(x=time, y=cogproc_all_with)) +
          geom_point(aes(color=age_interval),size=4, alpha=0.4)+ggtitle("Comparing cogproc between participants who came at the first and last slot") +
            labs(x='Time participants came at', y='Overall cogproc'))

########################################################
##parallel plot about each participant cogproc and time they switched
df_cogproc<-df_do%>% select(p_number,V5,V6,cogproc_before_first,cogproc_after_first,cogproc_before_second,cogproc_after_second,cogproc_before_third,cogproc_after_third,cogproc_before_fourth,cogproc_after_fourth)
df_cogproc$first_switch <- sapply(strsplit(as.character(df_cogproc$V6),','), "[", 1)
df_cogproc$second_switch <- sapply(strsplit(as.character(df_cogproc$V6),','), "[", 2)
df_cogproc$third_switch <- sapply(strsplit(as.character(df_cogproc$V6),','), "[", 3)
df_cogproc$fourth_switch <- sapply(strsplit(as.character(df_cogproc$V6),','), "[", 4)
df_cogproc$first_switch <- substring(df_cogproc$first_switch, 13)
##first switch for all
df_cogproc_before_first<-df_cogproc%>%select(p_number,cogproc_before_first,first_switch)
df_cogproc_before_first$type='cogproc_before_first'
df_cogproc_before_first<- rename(df_cogproc_before_first,cogproc='cogproc_before_first',switch_at='first_switch')

df_cogproc_after_first<-df_cogproc%>%select(p_number,cogproc_after_first,first_switch)
df_cogproc_after_first$type='cogproc_after_first'
df_cogproc_after_first<- rename(df_cogproc_after_first,cogproc='cogproc_after_first',switch_at='first_switch')
##Second switch for all
df_cogproc_before_second<-df_cogproc%>%select(p_number,cogproc_before_second,second_switch)
df_cogproc_before_second$type='cogproc_before_second'
df_cogproc_before_second<- rename(df_cogproc_before_second,cogproc='cogproc_before_second',switch_at='second_switch')

df_cogproc_after_second<-df_cogproc%>%select(p_number,cogproc_after_second,second_switch)
df_cogproc_after_second$type='cogproc_after_second'
df_cogproc_after_second<- rename(df_cogproc_after_second,cogproc='cogproc_after_second',switch_at='second_switch')

##third switch for all
df_cogproc_before_third<-df_cogproc%>%select(p_number,cogproc_before_third,third_switch)
df_cogproc_before_third$type='cogproc_before_third'
df_cogproc_before_third<- rename(df_cogproc_before_third,cogproc='cogproc_before_third',switch_at='third_switch')

df_cogproc_after_third<-df_cogproc%>%select(p_number,cogproc_after_third,third_switch)
df_cogproc_after_third$type='cogproc_after_third'
df_cogproc_after_third<- rename(df_cogproc_after_third,cogproc='cogproc_after_third',switch_at='third_switch')

##fourth switch for all
df_cogproc_before_fourth<-df_cogproc%>%select(p_number,cogproc_before_fourth,fourth_switch)
df_cogproc_before_fourth$type='cogproc_before_fourth'
df_cogproc_before_fourth<- rename(df_cogproc_before_fourth,cogproc='cogproc_before_fourth',switch_at='fourth_switch')

df_cogproc_after_fourth<-df_cogproc%>%select(p_number,cogproc_after_fourth,fourth_switch)
df_cogproc_after_fourth$type='cogproc_after_fourth'
df_cogproc_after_fourth<- rename(df_cogproc_after_fourth,cogproc='cogproc_after_fourth',switch_at='fourth_switch')


df_cogproc_all<- rbind(df_cogproc_before_first,df_cogproc_after_first,df_cogproc_before_second,df_cogproc_after_second,df_cogproc_before_third,df_cogproc_after_third,df_cogproc_before_fourth,df_cogproc_after_fourth)
df_cogproc_all$cogproc<- as.numeric(as.character(df_cogproc_all$cogproc))
df_cogproc_all<-df_cogproc_all[complete.cases(df_cogproc_all$cogproc), ]
df_cogproc_all$type<-factor(df_cogproc_all$type,levels = c('cogproc_before_first','cogproc_after_first','cogproc_before_second','cogproc_after_second','cogproc_before_third','cogproc_after_third','cogproc_before_fourth','cogproc_after_fourth'))

print(ggplot(df_cogproc_all, aes(x=type, y=cogproc, group=p_number, color=switch_at)) + geom_point() + geom_line() + theme_bw() + labs(y="Cogproc")+ggtitle("Parallel plot")) 
########################################################
###Cogproc for each interval
sd <- read_excel('~/info350S2/LIWC2015Results(txt_for_each_interval).xlsx')
sd<- sd %>% select(Filename,cogproc)
##Split Filename to p_number and interval_type
sd$p_number <- sapply(strsplit(as.character(sd$Filename),'_'), "[", 4)
sd$p_number = substr(sd$p_number,1,nchar(sd$p_number)-4)

sd$interval_type <- sapply(strsplit(as.character(sd$Filename),'_'), "[", 1)

sd$p_number<- as.numeric(as.character(sd$p_number))

##join data with the time participants came
sd<- left_join(sd,questionnaire_personal_info,by='p_number')

##join data with switch points
sd_switches<- df_cogproc%>% select(p_number,first_switch,second_switch,third_switch,fourth_switch)

sd<- left_join(sd,sd_switches,by='p_number')

# ##Mark when switches happened
# if_switched_at_the_interval_first<- sd%>% group_by(Filename)%>%summarize(interval_type==first_switch)
# if_switched_at_the_interval_second<- sd%>% group_by(Filename)%>%summarize(interval_type==second_switch)
# if_switched_at_the_interval_third<- sd%>% group_by(Filename)%>%summarize(interval_type==third_switch)
# if_switched_at_the_interval_fourth<- sd%>% group_by(Filename)%>%summarize(interval_type==fourth_switch)
# 
# ##When it is false, it means pariticipants did not switch, which mean na
# if_switched_at_the_interval_first <- if_switched_at_the_interval_first%>%mutate_if(is.logical, as.character)
# if_switched_at_the_interval_second <- if_switched_at_the_interval_second%>%mutate_if(is.logical, as.character)
# if_switched_at_the_interval_third <- if_switched_at_the_interval_third%>%mutate_if(is.logical, as.character)
# if_switched_at_the_interval_fourth <- if_switched_at_the_interval_fourth%>%mutate_if(is.logical, as.character)
# 
# if_switched_at_the_interval<- cbind(if_switched_at_the_interval_first,if_switched_at_the_interval_second[,2],if_switched_at_the_interval_third[,2],if_switched_at_the_interval_fourth[,2])
# 
# if_switched_at_the_interval[ if_switched_at_the_interval == "FALSE" ] <- NA
# 
# 
# sd<- full_join(sd,if_switched_at_the_interval,by="Filename")

##Line plot for each interval
sd$interval_type<-factor(sd$interval_type,levels = c('5','10','15','20','25'))
print(ggplot(data=sd, aes(x=interval_type, y=cogproc, group=p_number)) +geom_line(aes(color=time_type))+geom_point(aes(color=first_switch))+ggtitle('Line plot of cogproc for participants in each interval'))




