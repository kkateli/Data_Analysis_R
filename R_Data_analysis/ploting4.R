##plots for more than two dependent variables
##f(x,y) = x*y

# dependent variable: cogproc
# Independent variables:  education, language, how well rested, mood, 
#                       ages, the time participants came to the lab, posemo, negemo

# dependent variable: switch opportunity
# Indenpendent variables: cogproc previous, reasons of switching,enjoyment of the writing task, enjoyment of the videos


library(readxl)
library(dplyr)
library(knitr)
library(ggplot2)
library(stringr)
library(plotly)
library(gridExtra)
library(grid)

Sys.setenv("plotly_username"="kateli24")
Sys.setenv("plotly_api_key"="tS3trRLX2kduAQZ0tDLc")

df <- read_excel('~/info350S2/experimentResult.xlsx')
questionnaire <- read_excel('~/info350S2/questionnaire_edited.xlsx')

# Independent variable: cogproc
# dependent variables:  education, language, how well rested, mood, 
#                       ages, the time participants came to the lab, posemo, negemo

questionnaire$time<- str_sub(questionnaire$"StartDateStart Date.x",-5,-1)
questionnaire$time<-gsub(":", ".", gsub("\\.", "", questionnaire$time))
questionnaire$time<- as.numeric(as.character(questionnaire$time))
questionnaire$time=round(questionnaire$time-6,0)
# questionnaire$time<- as.character(as.numeric(questionnaire$time))
questionnaire_cogproc <- questionnaire %>% select('participant number','Q8What is your age?','time',"Q1Are you a native English speaker? - Selected Choice",
                                                        "Q3How many years have you studied here?","Q23Right now, are you well rested?","Q37How would you rate your mood right now?")
questionnaire_7<- questionnaire_cogproc%>% filter(time==7)
questionnaire_not_7<- questionnaire_cogproc%>% filter(time!=7)
questionnaire_7$time=questionnaire_7$time+1
questionnaire_cogproc<-rbind(questionnaire_7,questionnaire_not_7)

questionnaire_cogproc<- rename(questionnaire_cogproc,p_number='participant number')
questionnaire_cogproc$p_number<- as.numeric(as.character(questionnaire_cogproc$p_number))
df_overall<- df%>%select(p_number,cogproc_all_with,posemo_all_with,negemo_all_with)
df_overall<- left_join(df_overall,questionnaire_cogproc,by='p_number')
df_overall$`Q3How many years have you studied here?`<- as.numeric(as.character(df_overall$`Q3How many years have you studied here?`))
df_overall$`Q8What is your age?`<- as.numeric(as.character(df_overall$`Q8What is your age?`))
df_overall$`Q23Right now, are you well rested?` <- as.numeric(as.character(df_overall$`Q23Right now, are you well rested?`))
df_overall$`Q37How would you rate your mood right now?` <- as.numeric(as.character(df_overall$`Q37How would you rate your mood right now?`))
df_overall$cogproc_all_with<- as.numeric(as.character(df_overall$cogproc_all_with))
df_overall<- rename(df_overall, number_of_years="Q3How many years have you studied here?",ages='Q8What is your age?', 
                    if_englishspeaker="Q1Are you a native English speaker? - Selected Choice",
                    mood=`Q37How would you rate your mood right now?`,if_well_rested=`Q23Right now, are you well rested?`)
df_overall<-df_overall[complete.cases(df_overall),]

df_overall$if_englishspeaker[which(df_overall$if_englishspeaker == '1')] <- 'English_speaker'
df_overall$if_englishspeaker[which(df_overall$if_englishspeaker == '2')] <- 'Non_english_speaker'
df_overall$if_englishspeaker <- as.factor(df_overall$if_englishspeaker)

################################################################################################
##cogproc vs education, language, and ages
df_overall$education_ages=df_overall$ages*df_overall$number_of_years

print(ggplot(df_overall, aes(x=education_ages, y=cogproc_all_with,color=if_englishspeaker)) +
        geom_point(size=4, alpha=0.4)+ggtitle("Correlation between cogproc, education, language, and ages ") +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+labs(x='the_number_of_years*age', y='Overall cogproc'))

# p_multi_dimentional1 <- plot_ly(df_overall, x = ~number_of_years, y = ~ages, z = ~cogproc_all_with, color = ~if_englishspeaker, colors = c('#BF382A', '#0C4B8E')) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'X:number_of_years'),
#                       yaxis = list(title = 'Y:ages'),
#                       zaxis = list(title = 'Z:cogproc')))
# 
# cogproc1 = api_create(p_multi_dimentional1, filename="cogproc vs education, language, and ages")

################################################################################################
##cogproc vs education,how well rested, languages
df_overall$education_ifrested=df_overall$if_well_rested*df_overall$number_of_years

print(ggplot(df_overall, aes(x=education_ifrested, y=cogproc_all_with,color=if_englishspeaker)) +
        geom_point(size=4, alpha=0.4)+ggtitle("Correlation between cogproc, education, language, and how well rested ") +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+labs(x='the_number_of_years*how_well_rested', y='Overall cogproc'))

# p_multi_dimentional2 <- plot_ly(df_overall, x = ~number_of_years, y = ~if_well_rested, z = ~cogproc_all_with, color = ~if_englishspeaker, colors = c('#BF382A', '#0C4B8E')) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'X:number_of_years'),
#                       yaxis = list(title = 'Y:how_well_rested'),
#                       zaxis = list(title = 'Z:cogproc')))
# 
# cogproc2 = api_create(p_multi_dimentional2, filename="cogproc vs education, language, and how well rested")

################################################################################################
##cogproc vs education, mood, languages
df_overall$education_mood=df_overall$mood*df_overall$number_of_years

print(ggplot(df_overall, aes(x=education_mood, y=cogproc_all_with,color=if_englishspeaker)) +
        geom_point(size=4, alpha=0.4)+ggtitle("Correlation between cogproc, education, language, and mood ") +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+labs(x='the_number_of_years*mood', y='Overall cogproc'))

# p_multi_dimentional3 <- plot_ly(df_overall, x = ~number_of_years, y = ~mood, z = ~cogproc_all_with, color = ~if_englishspeaker, colors = c('#BF382A', '#0C4B8E')) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'X:number_of_years'),
#                       yaxis = list(title = 'Y:mood'),
#                       zaxis = list(title = 'Z:cogproc')))
# 
# cogproc3 = api_create(p_multi_dimentional3, filename="cogproc vs education, language, and mood")

##############################################################################################
##cogproc vs education,time, languages
df_overall$education_time=df_overall$time*df_overall$number_of_years

print(ggplot(df_overall, aes(x=education_time, y=cogproc_all_with,color=if_englishspeaker)) +
        geom_point(size=4, alpha=0.4)+ggtitle("Correlation between cogproc, education, language, and time ") +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+labs(x='the_number_of_years*time', y='Overall cogproc'))

# p_multi_dimentional4 <- plot_ly(df_overall, x = ~number_of_years, y = ~time, z = ~cogproc_all_with, color = ~if_englishspeaker, colors = c('#BF382A', '#0C4B8E')) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'X:number_of_years'),
#                       yaxis = list(title = 'Y:time'),
#                       zaxis = list(title = 'Z:cogproc')))
# 
# cogproc4 = api_create(p_multi_dimentional4, filename="cogproc vs education, language, and time")

###########################################################################################
##cogproc vs how well rested,mood, languages
df_overall$ifrested_mood=df_overall$if_well_rested*df_overall$mood

print(ggplot(df_overall, aes(x=ifrested_mood, y=cogproc_all_with,color=if_englishspeaker)) +
        geom_point(size=4, alpha=0.4)+ggtitle("Correlation between cogproc, how well rested, language, and mood") +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+labs(x='how_well_rested*mood', y='Overall cogproc'))

# p_multi_dimentional5 <- plot_ly(df_overall, x = ~if_well_rested, y = ~mood, z = ~cogproc_all_with, color = ~if_englishspeaker, colors = c('#BF382A', '#0C4B8E')) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'X:how_well_rested'),
#                       yaxis = list(title = 'Y:mood'),
#                       zaxis = list(title = 'Z:cogproc')))
# 
# cogproc5 = api_create(p_multi_dimentional5, filename="cogproc vs how well rested, language, and mood")

##########################################################################################
##cogproc vs how well rested,ages, languages
df_overall$ifrested_ages=df_overall$if_well_rested*df_overall$ages

print(ggplot(df_overall, aes(x=ifrested_ages, y=cogproc_all_with,color=if_englishspeaker)) +
        geom_point(size=4, alpha=0.4)+ggtitle("Correlation between cogproc, how well rested, language, and ages") +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+labs(x='how_well_rested*ages', y='Overall cogproc'))

# p_multi_dimentional6 <- plot_ly(df_overall, x = ~if_well_rested, y = ~ages, z = ~cogproc_all_with, color = ~if_englishspeaker, colors = c('#BF382A', '#0C4B8E')) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'X:how_well_rested'),
#                       yaxis = list(title = 'Y:ages'),
#                       zaxis = list(title = 'Z:cogproc')))
# 
# cogproc6 = api_create(p_multi_dimentional6, filename="cogproc vs how well rested, language, and ages")

########################################################################################
##cogproc vs how well rested,time, languages
df_overall$ifrested_time=df_overall$if_well_rested*df_overall$time

print(ggplot(df_overall, aes(x=ifrested_time, y=cogproc_all_with,color=if_englishspeaker)) +
        geom_point(size=4, alpha=0.4)+ggtitle("Correlation between cogproc, how well rested, language, and time") +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+labs(x='how_well_rested*time came to the lab', y='Overall cogproc'))

# p_multi_dimentional7 <- plot_ly(df_overall, x = ~if_well_rested, y = ~time, z = ~cogproc_all_with, color = ~if_englishspeaker, colors = c('#BF382A', '#0C4B8E')) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'X:how_well_rested'),
#                       yaxis = list(title = 'Y:time'),
#                       zaxis = list(title = 'Z:cogproc')))
# 
# cogproc7 = api_create(p_multi_dimentional7, filename="cogproc vs how well rested, language, and time")

###########################################################################################
##cogproc vs ages,mood, languages
df_overall$ages_mood=df_overall$ages*df_overall$mood

print(ggplot(df_overall, aes(x=ages_mood, y=cogproc_all_with,color=if_englishspeaker)) +
        geom_point(size=4, alpha=0.4)+ggtitle("Correlation between cogproc, ages, language, and mood") +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+labs(x='ages*mood', y='Overall cogproc'))

# p_multi_dimentional8 <- plot_ly(df_overall, x = ~mood, y = ~ages, z = ~cogproc_all_with, color = ~if_englishspeaker, colors = c('#BF382A', '#0C4B8E')) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'X:mood'),
#                       yaxis = list(title = 'Y:ages'),
#                       zaxis = list(title = 'Z:cogproc')))
# 
# cogproc8 = api_create(p_multi_dimentional8, filename="cogproc vs mood, language, and ages")

####################################################################################
##cogproc vs time,mood, languages
df_overall$time_mood=df_overall$time*df_overall$mood

print(ggplot(df_overall, aes(x=time_mood, y=cogproc_all_with,color=if_englishspeaker)) +
        geom_point(size=4, alpha=0.4)+ggtitle("Correlation between cogproc, time, language, and mood") +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+labs(x='time*mood', y='Overall cogproc'))

# p_multi_dimentional9 <- plot_ly(df_overall, x = ~mood, y = ~time, z = ~cogproc_all_with, color = ~if_englishspeaker, colors = c('#BF382A', '#0C4B8E')) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'X:mood'),
#                       yaxis = list(title = 'Y:time came to the lab'),
#                       zaxis = list(title = 'Z:cogproc')))
# 
# cogproc9 = api_create(p_multi_dimentional9, filename="cogproc vs mood, language, and time")

##################################################################################
##cogproc vs ages,time, languages
df_overall$time_ages=df_overall$time*df_overall$ages

print(ggplot(df_overall, aes(x=time_ages, y=cogproc_all_with,color=if_englishspeaker)) +
        geom_point(size=4, alpha=0.4)+ggtitle("Correlation between cogproc, time, language, and ages") +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+labs(x='time*ages', y='Overall cogproc'))

# p_multi_dimentional10 <- plot_ly(df_overall, x = ~ages, y = ~time, z = ~cogproc_all_with, color = ~if_englishspeaker, colors = c('#BF382A', '#0C4B8E')) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'X:ages'),
#                       yaxis = list(title = 'Y:time came to the lab'),
#                       zaxis = list(title = 'Z:cogproc')))
# 
# cogproc10 = api_create(p_multi_dimentional10, filename="cogproc vs ages, language, and time")

##################################################################################
##cogproc vs posemo,negemo,languages
df_overall$posemo_all_with <- as.numeric(as.character(df_overall$posemo_all_with))
df_overall$negemo_all_with <- as.numeric(as.character(df_overall$negemo_all_with))

df_overall$posemo_negemo=df_overall$posemo_all_with*df_overall$negemo_all_with

print(ggplot(df_overall, aes(x=posemo_negemo, y=cogproc_all_with,color=if_englishspeaker)) +
        geom_point(size=4, alpha=0.4)+ggtitle("Correlation between cogproc, posemo, negemo,and languages") +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+labs(x='posemo*negemo', y='Overall cogproc'))

# p_multi_dimentional11 <- plot_ly(df_overall, x = ~posemo_all_with, y = ~negemo_all_with, z = ~cogproc_all_with, color = ~if_englishspeaker, colors = c('#BF382A', '#0C4B8E')) %>%
#   add_markers() %>%
#   layout(scene = list(xaxis = list(title = 'X:posemo'),
#                       yaxis = list(title = 'Y:negemo'),
#                       zaxis = list(title = 'Z:cogproc')))
# 
# cogproc11 = api_create(p_multi_dimentional11, filename="cogproc vs posemo, negemo,and languages")
# 
# 

##########################################################################################################################
# Independent variable: switch opportunity
# denpendent variables: cogproc previous, reasons of switching,enjoyment of the writing task, enjoyment of the videos
questionnaire_switch_opportunity<- questionnaire%>% select("participant number","Q102Why did you watch the video(s)?\r\nPlease mark all that apply. - Selected Choice",
                                                           "Q4_1What do you think of the video(s) you watched? - Boring:Interesting",
                                                           "Q28_1The writing task has ended.\r\n\r\n \r\n\r\nHow did it go? - It was boring:It was interesting")

questionnaire_switch_opportunity<- rename(questionnaire_switch_opportunity,p_number='participant number')
questionnaire_switch_opportunity$p_number<- as.numeric(as.character(questionnaire_switch_opportunity$p_number))

questionnaire_switch_opportunity<- rename(questionnaire_switch_opportunity,switch_reasons="Q102Why did you watch the video(s)?\r\nPlease mark all that apply. - Selected Choice",
                   how_interesting_writing="Q28_1The writing task has ended.\r\n\r\n \r\n\r\nHow did it go? - It was boring:It was interesting",
                   how_interesting_videos="Q4_1What do you think of the video(s) you watched? - Boring:Interesting")

t1<- length(questionnaire_switch_opportunity$switch_reasons[which(questionnaire_switch_opportunity$switch_reasons == '1')])
t2<- length(questionnaire_switch_opportunity$switch_reasons[which(questionnaire_switch_opportunity$switch_reasons == '2')])
t3<- length(questionnaire_switch_opportunity$switch_reasons[which(questionnaire_switch_opportunity$switch_reasons == '3')])
t4<- length(questionnaire_switch_opportunity$switch_reasons[which(questionnaire_switch_opportunity$switch_reasons == '6')])
t5<- length(questionnaire_switch_opportunity$switch_reasons[which(questionnaire_switch_opportunity$switch_reasons == '1,3')])
t6<- length(questionnaire_switch_opportunity$switch_reasons[which(questionnaire_switch_opportunity$switch_reasons == '1,5')])
t7<- length(questionnaire_switch_opportunity$switch_reasons[which(questionnaire_switch_opportunity$switch_reasons == '1,2,4')])
t8<- length(questionnaire_switch_opportunity$switch_reasons[which(questionnaire_switch_opportunity$switch_reasons == '1,2,5')])
t9<- length(questionnaire_switch_opportunity$switch_reasons[which(questionnaire_switch_opportunity$switch_reasons == '1,3,5,6')])
t10<- length(questionnaire_switch_opportunity$switch_reasons[which(questionnaire_switch_opportunity$switch_reasons =='1,2,4,5,6')])

bar_chart <- data.frame(reasons=c('1','2','3','6','1,3','1,5','1,2,4','1,2,5','1,3,5,6','1,2,4,5,6'),count=c(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10))

print(ggplot(data=bar_chart, aes(x=reasons, y=count)) +geom_bar(stat="identity")+
        ggtitle("The number of participants and the reasons of switches")+
        geom_text(aes(label=count), vjust=-0.3, size=3.5)+
        labs(x='reasons(1--I was curious, 2--I was bored, 3--I acted on impulse, 4--I did not like the writing task, 5--I like watching videos, 6-- Others)'))


##Getting cogproc for each interval
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
##Previous cogproc
df_switch_oppo_5<- df_switch_oppo%>% filter(interval_type=='5')
df_switch_5<- df_switch_oppo_5%>% group_by(Filename)%>%summarize(interval_type==first_switch)
# df_switch_5[is.na(df_switch_5)] <- FALSE
df_switch_5<- rename(df_switch_5,switch_opportunity='interval_type == first_switch')
df_switch_5<- left_join(df_switch_5,df_switch_oppo_5[,1:4],by='Filename')
df_switch_5<- rename(df_switch_5,previous_cogproc='cogproc')

##After cogproc
df_switch_after_5<- df_switch_oppo%>% filter(interval_type=='10')
df_switch_after_5<-rename(df_switch_after_5,after_cogproc='cogproc')
df_switch_5<- cbind(df_switch_5,df_switch_after_5[,2])

##5-10 interval
##Previous cogproc
df_switch_oppo_10<- df_switch_oppo%>% filter(interval_type=='10')
df_switch_10<- df_switch_oppo_10%>% group_by(Filename)%>%summarize(interval_type==first_switch | interval_type==second_switch |interval_type==third_switch |interval_type==fourth_switch)
df_switch_10[is.na(df_switch_10)] <- FALSE
df_switch_10<- rename(df_switch_10,switch_opportunity=`interval_type == first_switch | interval_type == second_switch | 
    interval_type == third_switch | interval_type == fourth_switch`)
df_switch_10<- left_join(df_switch_10,df_switch_oppo_10[,1:4],by='Filename')
df_switch_10<- rename(df_switch_10,previous_cogproc='cogproc')

##After cogproc
df_switch_after_10<- df_switch_oppo%>% filter(interval_type=='15')
df_switch_after_10<-rename(df_switch_after_10,after_cogproc='cogproc')
df_switch_10<- cbind(df_switch_10,df_switch_after_10[,2])

##10-15 interval
##Previous cogproc
df_switch_oppo_15<- df_switch_oppo%>% filter(interval_type=='15')
df_switch_15<- df_switch_oppo_15%>% group_by(Filename)%>%summarize(interval_type==first_switch | interval_type==second_switch |interval_type==third_switch |interval_type==fourth_switch)
df_switch_15[is.na(df_switch_15)] <- FALSE
df_switch_15<- rename(df_switch_15,switch_opportunity=`interval_type == first_switch | interval_type == second_switch | 
    interval_type == third_switch | interval_type == fourth_switch`)
df_switch_15<- left_join(df_switch_15,df_switch_oppo_15[,1:4],by='Filename')
df_switch_15<- rename(df_switch_15,previous_cogproc='cogproc')

##After cogproc
df_switch_after_15<- df_switch_oppo%>% filter(interval_type=='20')
df_switch_after_15<-rename(df_switch_after_15,after_cogproc='cogproc')
df_switch_15<- cbind(df_switch_15,df_switch_after_15[,2])

##15-20 interval
##Previous cogproc
df_switch_oppo_20<- df_switch_oppo%>% filter(interval_type=='20')
df_switch_20<- df_switch_oppo_20%>% group_by(Filename)%>%summarize(interval_type==first_switch | interval_type==second_switch |interval_type==third_switch |interval_type==fourth_switch)
df_switch_20[is.na(df_switch_20)] <- FALSE
df_switch_20<- rename(df_switch_20,switch_opportunity=`interval_type == first_switch | interval_type == second_switch | 
    interval_type == third_switch | interval_type == fourth_switch`)
df_switch_20<- left_join(df_switch_20,df_switch_oppo_20[,1:4],by='Filename')
df_switch_20<- rename(df_switch_20,previous_cogproc='cogproc')

##After cogproc
df_switch_after_20<- df_switch_oppo%>% filter(interval_type=='25')
df_switch_after_20<-rename(df_switch_after_20,after_cogproc='cogproc')
df_switch_20<- cbind(df_switch_20,df_switch_after_15[,2])

df_switch<- rbind(df_switch_5,df_switch_10,df_switch_15,df_switch_20)
df_switch[ df_switch == "FALSE" ] <- '0'
df_switch[ df_switch == "TRUE" ] <- '1'


###Since aprticipants only answer the reason why they switched, enjoyment of writing and videos once, in the meanwhile we have 5 intervals. 
#so Reasons and enjoyment of videos and writing repeat for 5 times.  
##reason, enjoyment of videos = '0' when switch_opportunity == '0'. 

df_switch<- left_join(df_switch,questionnaire_switch_opportunity[,c(1,4)],by='p_number')

df_switched<- df_switch%>% filter(switch_opportunity=='1')
df_not_switched<-df_switch%>% filter(switch_opportunity=='0')
df_switched<- left_join(df_switched,questionnaire_switch_opportunity[,c(1,2,3)],by='p_number')
df_not_switched$switch_reasons='0'
df_not_switched$how_interesting_videos='0'

df_switch<- rbind(df_switched,df_not_switched)


df_switch$how_interesting_writing <- as.numeric(as.character(df_switch$how_interesting_writing))
df_switch$how_interesting_videos <- as.numeric(as.character(df_switch$how_interesting_videos))

df_switch<-df_switch[complete.cases(df_switch),]
# Independent variable: switch opportunity, cogproc after a time point(after_cogproc)
# denpendent variables: cogproc previous, reasons of switching,enjoyment of the writing task, enjoyment of the videos

#cogproc after a time point vs cogproc previous, reasons of switching,enjoyment of the writing task

df_switch$cogprocPrevious_writingEnjoyment=df_switch$previous_cogproc*df_switch$how_interesting_writing

print(ggplot(df_switch, aes(x=cogprocPrevious_writingEnjoyment, y=after_cogproc,color=switch_reasons)) +
        geom_point(size=4, alpha=0.4)+ggtitle("Correlation between cogproc after a time point vs cogproc previous, reasons of switching,enjoyment of the writing task") +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+labs(x='cogprocPrevious*writingEnjoyment\nReasons(0--Did not switch,1--I was curious, 2--I was bored, 3--I acted on impulse, 4--I did not like the writing task, 5--I like watching videos, 6-- Others)', y='after cogproc'))


# p_multi_dimentional12 <- plot_ly(df_switch, x = ~previous_cogproc, y = ~how_interesting_writing, z = ~after_cogproc, color = ~switch_reasons, colors = c('#808080','#BF382A', '#0C4B8E','#66ff33','#ff99ff','#990099','#ff9900','#cccc00','#3333cc','#008080','#993300')) %>%
#   add_markers() %>%
#   layout(title = "Reasons(0--Did not switch,1--I was curious, 2--I was bored, 3--I acted on impulse, 4--I did not like the writing task, 5--I like watching videos, 6-- Others)",scene = list(xaxis = list(title = 'X:cogprocPrevious'),
#                       yaxis = list(title = 'Y:writingEnjoyment'),
#                       zaxis = list(title = 'Z:cogprocAfter')))
# 
# cogproc12 = api_create(p_multi_dimentional12, filename="cogproc after vs cogproc previous,reasons,writing enjoyment")

#cogproc after a time point vs cogproc previous, reasons of switching,enjoyment of the videos

df_switch$cogprocPrevious_videoEnjoyment=df_switch$previous_cogproc*df_switch$how_interesting_videos

print(ggplot(df_switch, aes(x=cogprocPrevious_videoEnjoyment, y=after_cogproc,color=switch_reasons)) +
        geom_point(size=4, alpha=0.4)+ggtitle("Correlation between cogproc after a time point vs cogproc previous, reasons of switching,enjoyment of the Videos") +
        geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+labs(x='cogprocPrevious*VideosEnjoyment\nReasons(0--Did not switch,1--I was curious, 2--I was bored, 3--I acted on impulse, 4--I did not like the writing task, 5--I like watching videos, 6-- Others)', y='after cogproc'))


# p_multi_dimentional13 <- plot_ly(df_switch, x = ~previous_cogproc, y = ~how_interesting_videos, z = ~after_cogproc, color = ~switch_reasons, colors = c('#808080','#BF382A', '#0C4B8E','#66ff33','#ff99ff','#990099','#ff9900','#cccc00','#3333cc','#008080','#993300')) %>%
#   add_markers() %>%
#   layout(title = "Reasons(0--Did not switch,1--I was curious, 2--I was bored, 3--I acted on impulse, 4--I did not like the writing task, 5--I like watching videos, 6-- Others)",scene = list(xaxis = list(title = 'X:cogprocPrevious'),
#                                                                                                                                                                                   yaxis = list(title = 'Y:videosEnjoyment'),
#                                                                                                                                                                                   zaxis = list(title = 'Z:cogprocAfter')))
# 
# cogproc13 = api_create(p_multi_dimentional13, filename="cogproc after vs cogproc previous,reasons,video enjoyment")
# 
#                                                                                                                                                                              




