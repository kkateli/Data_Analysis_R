library(readxl)
library(dplyr)

##Finding ourliers
df <- read_excel('~/info350S2/questionnaire_edited.xlsx')

df$duration_total = df$"Duration..in.seconds.Duration (in seconds).x"+df$"Duration..in.seconds.Duration (in seconds).y"
mean_total = mean(df$duration_total)
min(df$duration_total)
sd = sd(df$duration_total)
outlier = mean_total-sd*2
##Finding outliers
#Question about the benchmark
df$ifLess<-df$duration_total <  outlier
outliers_participant <- subset(df, ifLess==TRUE)
df <- df[order(df$`participant number`),] 
print(ggplot(df, aes(x=df$`participant number`, y=df$duration_total)) + geom_point())

##################################
###outliers of 10 participants who spent lowest time
df <- df[order(df$duration_total),] 
df_10_participant_spent_least_time<-df[1:10,]

mean_total_10_participant_spent_least_time = mean(df_10_participant_spent_least_time$duration_total)
sd_10_participant_spent_least_time = sd(df_10_participant_spent_least_time$duration_total)
outlier_10_participant_spent_least_time = mean_total_10_participant_spent_least_time-sd_10_participant_spent_least_time*2

df_10_participant_spent_least_time$ifLess<-df_10_participant_spent_least_time$duration_total <  outlier_10_participant_spent_least_time
outliers_participant_10_participant_spent_least_time <- subset(df_10_participant_spent_least_time, ifLess==TRUE)

# library(xlsx)
# write.xlsx(df, "questionnaire.xlsx")