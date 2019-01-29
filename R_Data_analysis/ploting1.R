library(readxl)
library(dplyr)
library(ggplot2)

##Box plot for cogproc and emotions based on the number of times
##paticipents switched and the time points they switched

df <- read_excel('~/info350S2/experimentResult.xlsx')
########################################
##Q1 The cognitive levels/emotions regarding the number of switches
df$cogproc_all_with<- as.numeric(as.character(df$cogproc_all_with))
df$posemo_all_with<- as.numeric(as.character(df$posemo_all_with))
df$negemo_all_with<- as.numeric(as.character(df$negemo_all_with))


Result_cogproc=c()
tomain_cogproc=c()
Result_posemo=c()
tomain_posemo=c()
Result_negemo=c()
tomain_negemo=c()

df_switch_0<- df %>% filter(V5=="0 Time(s)")

df_switch_0_cogproc<-df_switch_0$cogproc_all_with
Result_cogproc=c(Result_cogproc,df_switch_0_cogproc)
tomain_cogproc=c(tomain_cogproc,rep(paste("0 time"),length(df_switch_0_cogproc)))

df_switch_0_posemo <- df_switch_0$posemo_all_with
Result_posemo=c(Result_posemo,df_switch_0_posemo)
tomain_posemo=c(tomain_posemo,rep(paste("0 time"),length(df_switch_0_posemo)))

df_switch_0_negemo <- df_switch_0$negemo_all_with
Result_negemo=c(Result_negemo,df_switch_0_negemo)
tomain_negemo=c(tomain_negemo,rep(paste("0 time"),length(df_switch_0_negemo)))


df_switch_1<- df %>% filter(V5=="1 Time(s)")
df_switch_1_cogproc<-df_switch_1$cogproc_all_with
Result_cogproc=c(Result_cogproc,df_switch_1_cogproc)
tomain_cogproc=c(tomain_cogproc,rep(paste("1 time"),length(df_switch_1_cogproc)))

df_switch_1_posemo <- df_switch_1$posemo_all_with
Result_posemo=c(Result_posemo,df_switch_1_posemo)
tomain_posemo=c(tomain_posemo,rep(paste("1 time"),length(df_switch_1_posemo)))

df_switch_1_negemo <- df_switch_1$negemo_all_with
Result_negemo=c(Result_negemo,df_switch_1_negemo)
tomain_negemo=c(tomain_negemo,rep(paste("1 time"),length(df_switch_1_negemo)))

df_switch_2<- df %>% filter(V5=="2 Time(s)")
df_switch_2_cogproc<-df_switch_2$cogproc_all_with
Result_cogproc=c(Result_cogproc,df_switch_2_cogproc)
tomain_cogproc=c(tomain_cogproc,rep(paste("2 times"),length(df_switch_2_cogproc)))

df_switch_2_posemo <- df_switch_2$posemo_all_with
Result_posemo=c(Result_posemo,df_switch_2_posemo)
tomain_posemo=c(tomain_posemo,rep(paste("2 times"),length(df_switch_2_posemo)))

df_switch_2_negemo <- df_switch_2$negemo_all_with
Result_negemo=c(Result_negemo,df_switch_2_negemo)
tomain_negemo=c(tomain_negemo,rep(paste("2 times"),length(df_switch_2_negemo)))

df_switch_3<- df %>% filter(V5=="3 Time(s)")
df_switch_3_cogproc<-df_switch_3$cogproc_all_with
Result_cogproc=c(Result_cogproc,df_switch_3_cogproc)
tomain_cogproc=c(tomain_cogproc,rep(paste("3 times"),length(df_switch_3_cogproc)))

df_switch_3_posemo <- df_switch_3$posemo_all_with
Result_posemo=c(Result_posemo,df_switch_3_posemo)
tomain_posemo=c(tomain_posemo,rep(paste("3 times"),length(df_switch_3_posemo)))

df_switch_3_negemo <- df_switch_3$negemo_all_with
Result_negemo=c(Result_negemo,df_switch_3_negemo)
tomain_negemo=c(tomain_negemo,rep(paste("3 times"),length(df_switch_3_negemo)))

df_switch_4<- df %>% filter(V5=="4 Time(s)")
df_switch_4_cogproc<-df_switch_4$cogproc_all_with
Result_cogproc=c(Result_cogproc,df_switch_4_cogproc)
tomain_cogproc=c(tomain_cogproc,rep(paste("4 times"),length(df_switch_4_cogproc)))

df_switch_4_posemo <- df_switch_4$posemo_all_with
Result_posemo=c(Result_posemo,df_switch_4_posemo)
tomain_posemo=c(tomain_posemo,rep(paste("4 times"),length(df_switch_4_posemo)))

df_switch_4_negemo <- df_switch_4$negemo_all_with
Result_negemo=c(Result_negemo,df_switch_4_negemo)
tomain_negemo=c(tomain_negemo,rep(paste("4 times"),length(df_switch_4_negemo)))

bar_chart <- data.frame(switches=c("0 switch", "1 switch", "2 switches","3 switches","4 switches"),count=c(length(df_switch_0_cogproc),length(df_switch_1_cogproc),length(df_switch_2_cogproc),length(df_switch_3_cogproc),length(df_switch_3_cogproc)))

print(ggplot(data=bar_chart, aes(x=switches, y=count)) +geom_bar(stat="identity")+ggtitle("The number of participants and times they switched"))
#bar plot

boxplot1=data.frame(Result_cogproc,tomain_cogproc)
boxplot2=data.frame(Result_posemo,tomain_posemo)
boxplot3=data.frame(Result_negemo,tomain_negemo)

#Box plot 
# print(boxplot(Result_cogproc~tomain_cogproc,boxplot1,main="Cogproc for participants switched for different times",cex.axis = 0.6))
# print(boxplot(Result_posemo~tomain_posemo,boxplot2,main="Posemo for participants switched for different times",cex.axis = 0.6))
# print(boxplot(Result_negemo~tomain_negemo,boxplot3,main="Negemo for participants switched for different times",cex.axis = 0.6))

#violin plot
print(ggplot(boxplot1, aes(tomain_cogproc, Result_cogproc)) +
        geom_violin(scale="count",aes(fill = tomain_cogproc)) + scale_fill_brewer(palette="RdBu") + theme_minimal() + geom_boxplot(width=0.1)+labs(title="Cogproc for participants switched for different times",x="Switches", y = "Cogproc"))

print(ggplot(boxplot2, aes(tomain_posemo, Result_posemo)) +
        geom_violin(scale="count",aes(fill = tomain_posemo)) + scale_fill_brewer(palette="Dark2") + theme_minimal() + geom_boxplot(width=0.1)+labs(title="Posemo for participants switched for different times",x="Switches", y = "Posemo"))

print(ggplot(boxplot3, aes(tomain_negemo, Result_negemo)) +
        geom_violin(scale="count",aes(fill = tomain_negemo)) + scale_fill_brewer(palette="BuPu") + theme_minimal() + geom_boxplot(width=0.1)+labs(title="Negemo for participants switched for different times",x="Switches", y = "Negemo"))
########################################
##Q2
##Cogproc
df$cogproc_before_first<- as.numeric(as.character(df$cogproc_before_first))
cogproc_before_first<-df[!is.na(df$cogproc_before_first), ]
df$cogproc_before_second<- as.numeric(as.character(df$cogproc_before_second))
cogproc_before_second<-df[!is.na(df$cogproc_before_second), ]
df$cogproc_before_third<- as.numeric(as.character(df$cogproc_before_third))
cogproc_before_third<-df[!is.na(df$cogproc_before_third), ]
df$cogproc_before_fourth<- as.numeric(as.character(df$cogproc_before_fourth))
cogproc_before_fourth<-df[!is.na(df$cogproc_before_fourth), ]

df$cogproc_after_first<- as.numeric(as.character(df$cogproc_after_first))
cogproc_after_first<-df[!is.na(df$cogproc_after_first), ]
df$cogproc_after_second<- as.numeric(as.character(df$cogproc_after_second))
cogproc_after_second<-df[!is.na(df$cogproc_after_second), ]
df$cogproc_after_third<- as.numeric(as.character(df$cogproc_after_third))
cogproc_after_third<-df[!is.na(df$cogproc_after_third), ]
df$cogproc_after_fourth<- as.numeric(as.character(df$cogproc_after_fourth))
cogproc_after_fourth<-df[!is.na(df$cogproc_after_fourth), ]


Result_Cogproc_Be_Af=c()
tomain_cogproc_Be_Af=c()
##Before and after for the first switch
Result_Cogproc_Be_Af=c(Result_Cogproc_Be_Af,cogproc_before_first$cogproc_before_first)
Result_Cogproc_Be_Af=c(Result_Cogproc_Be_Af,cogproc_after_first$cogproc_after_first)
tomain_cogproc_Be_Af=c(tomain_cogproc_Be_Af,rep(paste("Before-1"),length(cogproc_before_first$cogproc_before_first)))
tomain_cogproc_Be_Af=c(tomain_cogproc_Be_Af,rep(paste("After-1"),length(cogproc_after_first$cogproc_after_first)))

##Before and after for the second switch
Result_Cogproc_Be_Af=c(Result_Cogproc_Be_Af,cogproc_before_second$cogproc_before_second)
Result_Cogproc_Be_Af=c(Result_Cogproc_Be_Af,cogproc_after_second$cogproc_after_second)
tomain_cogproc_Be_Af=c(tomain_cogproc_Be_Af,rep(paste("Before-2"),length(cogproc_before_second$cogproc_before_second)))
tomain_cogproc_Be_Af=c(tomain_cogproc_Be_Af,rep(paste("After-2"),length(cogproc_after_second$cogproc_after_second)))

##Before and after for the third switch
Result_Cogproc_Be_Af=c(Result_Cogproc_Be_Af,cogproc_before_third$cogproc_before_third)
Result_Cogproc_Be_Af=c(Result_Cogproc_Be_Af,cogproc_after_third$cogproc_after_third)
tomain_cogproc_Be_Af=c(tomain_cogproc_Be_Af,rep(paste("Before-3"),length(cogproc_before_third$cogproc_before_third)))
tomain_cogproc_Be_Af=c(tomain_cogproc_Be_Af,rep(paste("After-3"),length(cogproc_after_third$cogproc_after_third)))

##Before and after for the fourth switch
Result_Cogproc_Be_Af=c(Result_Cogproc_Be_Af,cogproc_before_fourth$cogproc_before_fourth)
Result_Cogproc_Be_Af=c(Result_Cogproc_Be_Af,cogproc_after_fourth$cogproc_after_fourth)
tomain_cogproc_Be_Af=c(tomain_cogproc_Be_Af,rep(paste("Before-4"),length(cogproc_before_fourth$cogproc_before_fourth)))
tomain_cogproc_Be_Af=c(tomain_cogproc_Be_Af,rep(paste("After-4"),length(cogproc_after_fourth$cogproc_after_fourth)))

##Cogproc-Before and after for switches
boxplot4 = data.frame(Result_Cogproc_Be_Af,tomain_cogproc_Be_Af)
boxplot4$tomain_cogproc_Be_Af=factor(boxplot4$tomain_cogproc_Be_Af , levels=levels(boxplot4$tomain_cogproc_Be_Af)[c(5,1,6,2,7,3,8,4)])
#Box plot 
# print(boxplot(Result_Cogproc_Be_Af~tomain_cogproc_Be_Af,boxplot4,main="Cogproc-Before and after for switches",cex.axis = 0.6))
#Violin plot
print(ggplot(boxplot4, aes(tomain_cogproc_Be_Af, Result_Cogproc_Be_Af)) +
        geom_violin(scale="count",aes(fill = tomain_cogproc_Be_Af)) + scale_fill_brewer(palette="Set1") + theme_minimal() + geom_boxplot(width=0.1)+labs(title="Cogproc-Before and after for switches", x= "Switches",y = "Cogproc"))
#####################################
##Posemo
df$posemo_before_first<- as.numeric(as.character(df$posemo_before_first))
posemo_before_first<-df[!is.na(df$posemo_before_first), ]
df$posemo_before_second<- as.numeric(as.character(df$posemo_before_second))
posemo_before_second<-df[!is.na(df$posemo_before_second), ]
df$posemo_before_third<- as.numeric(as.character(df$posemo_before_third))
posemo_before_third<-df[!is.na(df$posemo_before_third), ]
df$posemo_before_fourth<- as.numeric(as.character(df$posemo_before_fourth))
posemo_before_fourth<-df[!is.na(df$posemo_before_fourth), ]

df$posemo_after_first<- as.numeric(as.character(df$posemo_after_first))
posemo_after_first<-df[!is.na(df$posemo_after_first), ]
df$posemo_after_second<- as.numeric(as.character(df$posemo_after_second))
posemo_after_second<-df[!is.na(df$posemo_after_second), ]
df$posemo_after_third<- as.numeric(as.character(df$posemo_after_third))
posemo_after_third<-df[!is.na(df$posemo_after_third), ]
df$posemo_after_fourth<- as.numeric(as.character(df$posemo_after_fourth))
posemo_after_fourth<-df[!is.na(df$posemo_after_fourth), ]


Result_posemo_Be_Af=c()
tomain_posemo_Be_Af=c()
##Before and after for the first switch
Result_posemo_Be_Af=c(Result_posemo_Be_Af,posemo_before_first$posemo_before_first)
Result_posemo_Be_Af=c(Result_posemo_Be_Af,posemo_after_first$posemo_after_first)
tomain_posemo_Be_Af=c(tomain_posemo_Be_Af,rep(paste("Before-1"),length(posemo_before_first$posemo_before_first)))
tomain_posemo_Be_Af=c(tomain_posemo_Be_Af,rep(paste("After-1"),length(posemo_after_first$posemo_after_first)))

##Before and after for the second switch
Result_posemo_Be_Af=c(Result_posemo_Be_Af,posemo_before_second$posemo_before_second)
Result_posemo_Be_Af=c(Result_posemo_Be_Af,posemo_after_second$posemo_after_second)
tomain_posemo_Be_Af=c(tomain_posemo_Be_Af,rep(paste("Before-2"),length(posemo_before_second$posemo_before_second)))
tomain_posemo_Be_Af=c(tomain_posemo_Be_Af,rep(paste("After-2"),length(posemo_after_second$posemo_after_second)))

##Before and after for the third switch
Result_posemo_Be_Af=c(Result_posemo_Be_Af,posemo_before_third$posemo_before_third)
Result_posemo_Be_Af=c(Result_posemo_Be_Af,posemo_after_third$posemo_after_third)
tomain_posemo_Be_Af=c(tomain_posemo_Be_Af,rep(paste("Before-3"),length(posemo_before_third$posemo_before_third)))
tomain_posemo_Be_Af=c(tomain_posemo_Be_Af,rep(paste("After-3"),length(posemo_after_third$posemo_after_third)))

##Before and after for the fourth switch
Result_posemo_Be_Af=c(Result_posemo_Be_Af,posemo_before_fourth$posemo_before_fourth)
Result_posemo_Be_Af=c(Result_posemo_Be_Af,posemo_after_fourth$posemo_after_fourth)
tomain_posemo_Be_Af=c(tomain_posemo_Be_Af,rep(paste("Before-4"),length(posemo_before_fourth$posemo_before_fourth)))
tomain_posemo_Be_Af=c(tomain_posemo_Be_Af,rep(paste("After-4"),length(posemo_after_fourth$posemo_after_fourth)))

##posemo-Before and after for switches
boxplot5 = data.frame(Result_posemo_Be_Af,tomain_posemo_Be_Af)
boxplot5$tomain_posemo_Be_Af=factor(boxplot5$tomain_posemo_Be_Af , levels=levels(boxplot5$tomain_posemo_Be_Af)[c(5,1,6,2,7,3,8,4)])
#Box plot 
# print(boxplot(Result_posemo_Be_Af~tomain_posemo_Be_Af,boxplot5,main="Posemo-Before and after for switches",cex.axis = 0.6))
#Violin plot
print(ggplot(boxplot5, aes(tomain_posemo_Be_Af, Result_posemo_Be_Af)) +
        geom_violin(scale="count",aes(fill = tomain_posemo_Be_Af)) + scale_fill_brewer(palette="PiYG") + theme_minimal() + geom_boxplot(width=0.1)+labs(title="Posemo-Before and after for switches", x= "Switches",y = "posemo"))
###############################################
##Negemo
df$negemo_before_first<- as.numeric(as.character(df$negemo_before_first))
negemo_before_first<-df[!is.na(df$negemo_before_first), ]
df$negemo_before_second<- as.numeric(as.character(df$negemo_before_second))
negemo_before_second<-df[!is.na(df$negemo_before_second), ]
df$negemo_before_third<- as.numeric(as.character(df$negemo_before_third))
negemo_before_third<-df[!is.na(df$negemo_before_third), ]
df$negemo_before_fourth<- as.numeric(as.character(df$negemo_before_fourth))
negemo_before_fourth<-df[!is.na(df$negemo_before_fourth), ]

df$negemo_after_first<- as.numeric(as.character(df$negemo_after_first))
negemo_after_first<-df[!is.na(df$negemo_after_first), ]
df$negemo_after_second<- as.numeric(as.character(df$negemo_after_second))
negemo_after_second<-df[!is.na(df$negemo_after_second), ]
df$negemo_after_third<- as.numeric(as.character(df$negemo_after_third))
negemo_after_third<-df[!is.na(df$negemo_after_third), ]
df$negemo_after_fourth<- as.numeric(as.character(df$negemo_after_fourth))
negemo_after_fourth<-df[!is.na(df$negemo_after_fourth), ]


Result_negemo_Be_Af=c()
tomain_negemo_Be_Af=c()
##Before and after for the first switch
Result_negemo_Be_Af=c(Result_negemo_Be_Af,negemo_before_first$negemo_before_first)
Result_negemo_Be_Af=c(Result_negemo_Be_Af,negemo_after_first$negemo_after_first)
tomain_negemo_Be_Af=c(tomain_negemo_Be_Af,rep(paste("Before-1"),length(negemo_before_first$negemo_before_first)))
tomain_negemo_Be_Af=c(tomain_negemo_Be_Af,rep(paste("After-1"),length(negemo_after_first$negemo_after_first)))

##Before and after for the second switch
Result_negemo_Be_Af=c(Result_negemo_Be_Af,negemo_before_second$negemo_before_second)
Result_negemo_Be_Af=c(Result_negemo_Be_Af,negemo_after_second$negemo_after_second)
tomain_negemo_Be_Af=c(tomain_negemo_Be_Af,rep(paste("Before-2"),length(negemo_before_second$negemo_before_second)))
tomain_negemo_Be_Af=c(tomain_negemo_Be_Af,rep(paste("After-2"),length(negemo_after_second$negemo_after_second)))

##Before and after for the third switch
Result_negemo_Be_Af=c(Result_negemo_Be_Af,negemo_before_third$negemo_before_third)
Result_negemo_Be_Af=c(Result_negemo_Be_Af,negemo_after_third$negemo_after_third)
tomain_negemo_Be_Af=c(tomain_negemo_Be_Af,rep(paste("Before-3"),length(negemo_before_third$negemo_before_third)))
tomain_negemo_Be_Af=c(tomain_negemo_Be_Af,rep(paste("After-3"),length(negemo_after_third$negemo_after_third)))

##Before and after for the fourth switch
Result_negemo_Be_Af=c(Result_negemo_Be_Af,negemo_before_fourth$negemo_before_fourth)
Result_negemo_Be_Af=c(Result_negemo_Be_Af,negemo_after_fourth$negemo_after_fourth)
tomain_negemo_Be_Af=c(tomain_negemo_Be_Af,rep(paste("Before-4"),length(negemo_before_fourth$negemo_before_fourth)))
tomain_negemo_Be_Af=c(tomain_negemo_Be_Af,rep(paste("After-4"),length(negemo_after_fourth$negemo_after_fourth)))

##negemo-Before and after for switches
boxplot6 = data.frame(Result_negemo_Be_Af,tomain_negemo_Be_Af)
boxplot6$tomain_negemo_Be_Af=factor(boxplot6$tomain_negemo_Be_Af , levels=levels(boxplot6$tomain_negemo_Be_Af)[c(5,1,6,2,7,3,8,4)])
#Box plot 
# print(boxplot(Result_negemo_Be_Af~tomain_negemo_Be_Af,boxplot6,main="Negemo-Before and after for switches",cex.axis = 0.6))
#Violin plot
print(ggplot(boxplot6, aes(tomain_negemo_Be_Af, Result_negemo_Be_Af)) +
        geom_violin(scale="count",aes(fill = tomain_negemo_Be_Af)) + scale_fill_brewer(palette="BrBG") + theme_minimal() + geom_boxplot(width=0.1)+labs(title="Negemo-Before and after for switches", x= "Switches",y = "negemo"))



