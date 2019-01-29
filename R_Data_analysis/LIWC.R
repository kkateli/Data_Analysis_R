library(readxl)
library(dplyr)
df <- read_xlsx('~/info350S2/LIWCParticipants.xlsx')

##Slightly tidying LIWC result file
##Filtering out "Filename" containing "txt"
df <-df[grepl("txt", df[["Filename"]]), ]
#Revoming '.txt' from every file name
df$Filename = substr(df$Filename,1,nchar(df$Filename)-4)

df<- df %>% select(Filename,posemo,negemo,cogproc)

before <-df[grepl("before", df[["Filename"]]), ]
before_spread <- as.data.frame(t(before))


# library(WriteXLS)
# # Extract Table
# WriteXLS(df, "LIWCEdited.xlsx")