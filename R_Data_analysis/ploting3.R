library(readxl)
library(dplyr)
library(knitr)
library(ggplot2)
library(stringr)
library(psych)

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
View(describe(df_first_switch$delta))
View(df_first_switch)

#Delta for all switches combined
df_all_first<- df_do%>% select(p_number,cogproc_before_first,cogproc_after_first)
df_all_first<- rename(df_all_first,before='cogproc_before_first',after='cogproc_after_first')
df_all_first$type='first'
df_all_second<- df_do%>% select(p_number,cogproc_before_second,cogproc_after_second)
df_all_second<- rename(df_all_second,before='cogproc_before_second',after='cogproc_after_second')
df_all_second$type='second'
df_all_third<- df_do%>% select(p_number,cogproc_before_third,cogproc_after_third)
df_all_third<- rename(df_all_third,before='cogproc_before_third',after='cogproc_after_third')
df_all_third$type='third'
df_all_fourth<- df_do%>% select(p_number,cogproc_before_fourth,cogproc_after_fourth)
df_all_fourth<- rename(df_all_fourth,before='cogproc_before_fourth',after='cogproc_after_fourth')
df_all_fourth$type='fourth'

df_all<-rbind(df_all_first,df_all_second,df_all_third,df_all_fourth)
df_all<-df_all[complete.cases(df_all), ]
df_all$after<- as.numeric(as.character(df_all$after))
df_all$before<- as.numeric(as.character(df_all$before))

df_all$delta=df_all$after-df_all$before
df_all <- df_all[order(-df_all$delta),]
View(df_all)
View(describe(df_all$delta))

print(ggplot(data=df_all,aes(x=p_number, y=delta)) + theme_bw()+
              geom_point(aes(color=type),size=4, alpha=0.4) + scale_x_continuous(breaks= seq(1,83,by = 1))+
              ggtitle("Delta for all switches combined"))

#Comparing the first delta and the second delta among people who switched at least twice
df_more_than_one<- df_do%>% filter(V5!='1 Time(s)')
df_more_than_one<- df_more_than_one%>% select(p_number,cogproc_before_first,cogproc_after_first,cogproc_before_second,cogproc_after_second,cogproc_all_with)

df_more_than_one$cogproc_before_first<- as.numeric(as.character(df_more_than_one$cogproc_before_first))
df_more_than_one$cogproc_after_first<- as.numeric(as.character(df_more_than_one$cogproc_after_first))
df_more_than_one$cogproc_before_second<- as.numeric(as.character(df_more_than_one$cogproc_before_second))
df_more_than_one$cogproc_after_second<- as.numeric(as.character(df_more_than_one$cogproc_after_second))
df_more_than_one$cogproc_all_with<- as.numeric(as.character(df_more_than_one$cogproc_all_with))


df_more_than_one$delta_one<- df_more_than_one$cogproc_after_first-df_more_than_one$cogproc_before_first
df_more_than_one$delta_two<- df_more_than_one$cogproc_after_second- df_more_than_one$cogproc_before_second
df_more_than_one$first_after_minors_overall<- df_more_than_one$cogproc_after_first-df_more_than_one$cogproc_all_with
df_more_than_one$second_after_minors_overall<- df_more_than_one$cogproc_after_second-df_more_than_one$cogproc_all_with

stat1=describe(df_more_than_one$delta_one)
stat2=describe(df_more_than_one$delta_two)
stat3=describe(df_more_than_one$first_after_minors_overall)
stat4=describe(df_more_than_one$second_after_minors_overall)
stat_delta<- rbind(stat1,stat2,stat3,stat4)
rownames(stat_delta)[1]<-'Delta one'
rownames(stat_delta)[2]<-'Delta two'
rownames(stat_delta)[3]<-'First_after_minors_overall'
rownames(stat_delta)[4]<-'Second_after_minors_overall'
View(stat_delta)

##Scatter plot displaying the distribution of delta one and delta two
df_delta_one<- df_more_than_one%>% select (p_number,delta_one)
df_delta_two<- df_more_than_one%>% select (p_number,delta_two)
df_delta_one$type='delta one'
df_delta_two$type='delta two'
df_delta_one<- rename(df_delta_one,delta='delta_one')
df_delta_two<- rename(df_delta_two,delta='delta_two')
df_delta<- rbind(df_delta_one,df_delta_two)

##displaying the distribution of delta retrieved by cogproc after minors overall cogproc
df_delta_minor_overall1<- df_more_than_one%>% select (p_number,first_after_minors_overall)
df_delta_minor_overall2<- df_more_than_one%>% select (p_number,second_after_minors_overall)
df_delta_minor_overall1$type='delta of first after&overall'
df_delta_minor_overall2$type='delta of second after&overall'
df_delta_minor_overall1<- rename(df_delta_minor_overall1,delta='first_after_minors_overall')
df_delta_minor_overall2<- rename(df_delta_minor_overall2,delta='second_after_minors_overall')
df_delta_overall<- rbind(df_delta_minor_overall1,df_delta_minor_overall2)

print(ggplot(data=df_delta,aes(x=p_number, y=delta)) + theme_bw()+
              geom_point(aes(color=type),size=4, alpha=0.4) + scale_x_continuous(breaks= seq(1,83,by = 1))+
              ggtitle("Comparing delta one and delta two retrieved from the first and second switches"))
             

print(ggplot(data=df_delta_overall,aes(x=p_number, y=delta)) + theme_bw()+
        geom_point(aes(color=type),size=4, alpha=0.4) + scale_x_continuous(breaks= seq(1,83,by = 1))+
        ggtitle("Comparing deltas retrieved from subtracting overall cogproc from cogproc after the first/second switch")) 
        


########################################################
##correlation between cogproc previous and switch opportunity
sd <- read_excel('~/info350S2/LIWC2015Results(txt_for_each_interval).xlsx')
sd<- sd %>% select(Filename,cogproc)
##Split Filename to p_number and interval_type
sd$p_number <- sapply(strsplit(as.character(sd$Filename),'_'), "[", 4)
sd$p_number = substr(sd$p_number,1,nchar(sd$p_number)-4)

sd$interval_type <- sapply(strsplit(as.character(sd$Filename),'_'), "[", 1)

sd$p_number<- as.numeric(as.character(sd$p_number))
sd<- sd%>% filter(!p_number %in% c(7,8,9,10,11,12,24))
df_switch_oppo<- df%>% select(p_number,V6)
df_switch_oppo<- left_join(sd,df_switch_oppo,by='p_number')

df_switch_oppo$first_switch <- sapply(strsplit(as.character(df_switch_oppo$V6),','), "[", 1)
df_switch_oppo$second_switch <- sapply(strsplit(as.character(df_switch_oppo$V6),','), "[", 2)
df_switch_oppo$third_switch <- sapply(strsplit(as.character(df_switch_oppo$V6),','), "[", 3)
df_switch_oppo$fourth_switch <- sapply(strsplit(as.character(df_switch_oppo$V6),','), "[", 4)
df_switch_oppo$first_switch <- substring(df_switch_oppo$first_switch, 13)

##First 0-5 interval
df_switch_oppo_5<- df_switch_oppo%>% filter(interval_type=='5')
if_switched_5<- df_switch_oppo_5%>% group_by(Filename)%>%summarize(interval_type==first_switch)
# if_switched_5[is.na(if_switched_5)] <- FALSE
if_switched_5<- rename(if_switched_5,switch_opportunity='interval_type == first_switch')
if_switched_5<- left_join(if_switched_5,df_switch_oppo_5[,1:4],by='Filename')

##5-10 interval
df_switch_oppo_10<- df_switch_oppo%>% filter(interval_type=='10')
if_switched_10<- df_switch_oppo_10%>% group_by(Filename)%>%summarize(interval_type==first_switch | interval_type==second_switch |interval_type==third_switch |interval_type==fourth_switch)
if_switched_10[is.na(if_switched_10)] <- FALSE
if_switched_10<- rename(if_switched_10,switch_opportunity=`interval_type == first_switch | interval_type == second_switch | 
    interval_type == third_switch | interval_type == fourth_switch`)
if_switched_10<- left_join(if_switched_10,df_switch_oppo_10[,1:4],by='Filename')

##10-15 interval
df_switch_oppo_15<- df_switch_oppo%>% filter(interval_type=='15')
if_switched_15<- df_switch_oppo_15%>% group_by(Filename)%>%summarize(interval_type==first_switch | interval_type==second_switch |interval_type==third_switch |interval_type==fourth_switch)
if_switched_15[is.na(if_switched_15)] <- FALSE
if_switched_15<- rename(if_switched_15,switch_opportunity=`interval_type == first_switch | interval_type == second_switch | 
    interval_type == third_switch | interval_type == fourth_switch`)
if_switched_15<- left_join(if_switched_15,df_switch_oppo_15[,1:4],by='Filename')

##15-20 interval
df_switch_oppo_20<- df_switch_oppo%>% filter(interval_type=='20')
if_switched_20<- df_switch_oppo_20%>% group_by(Filename)%>%summarize(interval_type==first_switch | interval_type==second_switch |interval_type==third_switch |interval_type==fourth_switch)
if_switched_20[is.na(if_switched_20)] <- FALSE
if_switched_20<- rename(if_switched_20,switch_opportunity=`interval_type == first_switch | interval_type == second_switch | 
    interval_type == third_switch | interval_type == fourth_switch`)
if_switched_20<- left_join(if_switched_20,df_switch_oppo_20[,1:4],by='Filename')


##20-25 interval
df_switch_oppo_25<- df_switch_oppo%>% filter(interval_type=='25')
if_switched_25<- df_switch_oppo_25%>% group_by(Filename)%>%summarize(interval_type==first_switch | interval_type==second_switch |interval_type==third_switch |interval_type==fourth_switch)
if_switched_25[is.na(if_switched_25)] <- FALSE
if_switched_25<- rename(if_switched_25,switch_opportunity=`interval_type == first_switch | interval_type == second_switch | 
    interval_type == third_switch | interval_type == fourth_switch`)
if_switched_25<- left_join(if_switched_25,df_switch_oppo_25[,1:4],by='Filename')

if_switched<- rbind(if_switched_5,if_switched_10,if_switched_15,if_switched_20,if_switched_25)
if_switched[ if_switched == "FALSE" ] <- '0'
if_switched[ if_switched == "TRUE" ] <- '1'

##Stats about True and False with previous cogproc
if_switched_true<- if_switched%>% filter(switch_opportunity=='1')
stat_true= describe(if_switched_true$cogproc)

if_switched_false<- if_switched%>% filter(switch_opportunity=='0')
stat_false= describe(if_switched_false$cogproc)

stat_combined<- rbind(stat_true,stat_false)
rownames(stat_combined)[1]<-'Switched'
rownames(stat_combined)[2]<-'Not Switched'
View(stat_combined)

##Scatter plot of the dirtribution of previous cogproc and switch opportunity
print(ggplot(data=if_switched,aes(x=p_number, y=cogproc)) + theme_bw()+
              geom_point(aes(color=switch_opportunity),size=4, alpha=0.4) + scale_x_continuous(breaks= seq(1,83,by = 1))+
              ggtitle("Distribution of previous cogproc associated with switch opportunity(Overall)"))
##violin plot of previous cogproc and switch opportunity
print(ggplot(if_switched, aes(switch_opportunity, cogproc)) +
        geom_violin(scale="count",aes(fill = switch_opportunity)) + scale_fill_brewer(palette="RdBu") + theme_minimal() + geom_boxplot(width=0.1)+labs(title="Relationship between previous cogproc and switch opportunity(Overall)"))


###previous cogproc and switch opportunity(only consider first 5 mins)
if_switched_5[ if_switched_5== "FALSE" ] <- '0'
if_switched_5[ if_switched_5 == "TRUE" ] <- '1'

##Scatter plot of the dirtribution of previous cogproc and switch opportunity(only consider first 5 mins)
print(ggplot(data=if_switched_5,aes(x=p_number, y=cogproc)) + theme_bw()+
              geom_point(aes(color=switch_opportunity),size=4, alpha=0.4) + scale_x_continuous(breaks= seq(1,83,by = 1))+
              ggtitle("Distribution of previous cogproc associated with switch opportunity(First 5 mins)"))

##violin plot of previous cogproc and switch opportunity(only consider first 5 mins)
print(ggplot(if_switched_5, aes(switch_opportunity, cogproc)) +
        geom_violin(scale="count",aes(fill = switch_opportunity)) + scale_fill_brewer(palette="Set1") + theme_minimal() + geom_boxplot(width=0.1)+labs(title="Relationship between previous cogproc and switch opportunity(First 5 mins)"))

##
########################################################
##table including the time participants came at and ages
questionnaire <- read_excel('~/info350S2/questionnaire_edited.xlsx')
questionnaire$time<- str_sub(questionnaire$"StartDateStart Date.x",-5,-1)
questionnaire$time<-gsub(":", ".", gsub("\\.", "", questionnaire$time))
questionnaire$time<- as.numeric(as.character(questionnaire$time))
questionnaire$time=round(questionnaire$time-6,0)
questionnaire_personal_info <- questionnaire %>% select('participant number','Q8What is your age?','time','Q28_2The writing task has ended.\r\n\r\n \r\n\r\nHow did it go? - It was no fun at all:It was fun')
questionnaire_7<- questionnaire_personal_info%>% filter(time==7)
questionnaire_not_7<- questionnaire_personal_info%>% filter(time!=7)
questionnaire_7$time=questionnaire_7$time+1
questionnaire_personal_info<-rbind(questionnaire_7,questionnaire_not_7)

questionnaire_personal_info$'Q28_2The writing task has ended.\r\n\r\n \r\n\r\nHow did it go? - It was no fun at all:It was fun'<- as.numeric(as.character(questionnaire_personal_info$'Q28_2The writing task has ended.\r\n\r\n \r\n\r\nHow did it go? - It was no fun at all:It was fun'))

questionnaire_personal_info$ifmore_than_3<- questionnaire_personal_info$'Q28_2The writing task has ended.\r\n\r\n \r\n\r\nHow did it go? - It was no fun at all:It was fun'>3

questionnaire_personal_info_enjoyable<- subset(questionnaire_personal_info,ifmore_than_3==TRUE)
questionnaire_personal_info_enjoyable$type='enjoyable'

questionnaire_personal_info_not_enjoyable<- subset(questionnaire_personal_info,ifmore_than_3==FALSE)
questionnaire_personal_info_not_enjoyable$type='not_enjoyable'

questionnaire_personal_info<- rbind(questionnaire_personal_info_enjoyable,questionnaire_personal_info_not_enjoyable)

print(ggplot(questionnaire_personal_info, aes(x=time, fill=type)) +
        geom_histogram(binwidth=.5, alpha=.5, position="identity")+labs(x="Time participants came at",y="Total")+ggtitle("Bimodal distribution for the time participants came at by how enjoyable they felt"))



















