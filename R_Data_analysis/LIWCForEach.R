library(readxl)
library(dplyr)
library(reshape2)

##Generate LIWSForEach file and combine it with experiment file as experimentResult
####################################################################
##Reading data
df_1 <- read_excel('~/Downloads/350s2/Participants/1/LIWCParticipant1.xlsx')

for (i in 1:6) {
  a <- paste0("df_",i," <- read_excel('~/Downloads/350s2/Participants/",i,"/LIWCParticipant",i,".xlsx')")
  print(a)
  eval(parse(text = a))
}

for (i in 13:23) {
  a <- paste0("df_",i," <- read_excel('~/Downloads/350s2/Participants/",i,"/LIWCParticipant",i,".xlsx')")
  print(a)
  eval(parse(text = a))
}

for (i in 25:83) {
  a <- paste0("df_",i," <- read_excel('~/Downloads/350s2/Participants/",i,"/LIWCParticipant",i,".xlsx')")
  print(a)
  eval(parse(text = a))
}

####################################################################
##Selecting data we need and sort by alphabetic order
for (i in 1:6) {
  a <- paste0("df_",i," <- df_",i,"%>% select(Filename,posemo,negemo,cogproc)") 
  print(a)
  eval(parse(text = a))
  b <- paste0("df_",i," <- df_",i,"[order(df_",i,"$Filename),]")
  print(b)
  eval(parse(text = b))
}

for (i in 13:23) {
  a <- paste0("df_",i," <- df_",i,"%>% select(Filename,posemo,negemo,cogproc)") 
  print(a)
  eval(parse(text = a))
  b <- paste0("df_",i," <- df_",i,"[order(df_",i,"$Filename),]")
  print(b)
  eval(parse(text = b))
}

for (i in 25:83) {
  a <- paste0("df_",i," <- df_",i,"%>% select(Filename,posemo,negemo,cogproc)") 
  print(a)
  eval(parse(text = a))
  b <- paste0("df_",i," <- df_",i,"[order(df_",i,"$Filename),]")
  print(b)
  eval(parse(text = b))
}

####################################################################
##Disassembling
for (i in 1:6) {
 a <-  paste0("df_",i,"_posemo <- df_",i,"%>% select(Filename,posemo)") 
  print(a)
  eval(parse(text = a))
  b <- paste0("df_",i,"_negemo <- df_",i,"%>% select(Filename,negemo)") 
  print(b)
  eval(parse(text = b))
  c <- paste0("df_",i,"_cogproc <- df_",i,"%>% select(Filename,cogproc)") 
  print(c)
  eval(parse(text = c))
}

for (i in 13:23) {
  a <-  paste0("df_",i,"_posemo <- df_",i,"%>% select(Filename,posemo)") 
  print(a)
  eval(parse(text = a))
  b <- paste0("df_",i,"_negemo <- df_",i,"%>% select(Filename,negemo)") 
  print(b)
  eval(parse(text = b))
  c <- paste0("df_",i,"_cogproc <- df_",i,"%>% select(Filename,cogproc)") 
  print(c)
  eval(parse(text = c))
}

for (i in 25:83) {
  a <-  paste0("df_",i,"_posemo <- df_",i,"%>% select(Filename,posemo)") 
  print(a)
  eval(parse(text = a))
  b <- paste0("df_",i,"_negemo <- df_",i,"%>% select(Filename,negemo)") 
  print(b)
  eval(parse(text = b))
  c <- paste0("df_",i,"_cogproc <- df_",i,"%>% select(Filename,cogproc)") 
  print(c)
  eval(parse(text = c))
}

####################################################################
##Reshaping

for (i in 1:6) {
  a <-  paste0("df_",i,"_posemo <- as.data.frame(t(df_",i,"_posemo))")
  print(a)
  eval(parse(text = a))
  b <- paste0("df_",i,"_negemo <- as.data.frame(t(df_",i,"_negemo))")
  print(b)
  eval(parse(text = b))
  c <- paste0("df_",i,"_cogproc <- as.data.frame(t(df_",i,"_cogproc))")
  print(c)
  eval(parse(text = c))
}

for (i in 13:23) {
  a <-  paste0("df_",i,"_posemo <- as.data.frame(t(df_",i,"_posemo))")
  print(a)
  eval(parse(text = a))
  b <- paste0("df_",i,"_negemo <- as.data.frame(t(df_",i,"_negemo))")
  print(b)
  eval(parse(text = b))
  c <- paste0("df_",i,"_cogproc <- as.data.frame(t(df_",i,"_cogproc))")
  print(c)
  eval(parse(text = c))
}

for (i in 25:83) {
  a <-  paste0("df_",i,"_posemo <- as.data.frame(t(df_",i,"_posemo))")
  print(a)
  eval(parse(text = a))
  b <- paste0("df_",i,"_negemo <- as.data.frame(t(df_",i,"_negemo))")
  print(b)
  eval(parse(text = b))
  c <- paste0("df_",i,"_cogproc <- as.data.frame(t(df_",i,"_cogproc))")
  print(c)
  eval(parse(text = c))
}



####################################################################
##Renaming posemo

for (i in 1:6) {
  a <-  paste0("if(nrow(df_",i,")==2){ df_",i,"_posemo <- rename(df_",i,"_posemo, posemo_all_with='V1', posemo_all_without='V2')} 
               else if (nrow(df_",i,")==4){ df_",i,"_posemo <- rename(df_",i,"_posemo, posemo_after_first='V1', posemo_all_with='V2',posemo_all_without ='V3',posemo_before_first='V4')} 
               else if (nrow(df_",i,")==6){ df_",i,"_posemo <- rename(df_",i,"_posemo, posemo_after_first='V1', posemo_after_second='V2',posemo_all_with='V3',posemo_all_without ='V4',posemo_before_first='V5',posemo_before_second='V6')}
               else if (nrow(df_",i,")==8){ df_",i,"_posemo <- rename(df_",i,"_posemo, posemo_after_first='V1', posemo_after_second='V2',posemo_after_third='V3',posemo_all_with='V4',posemo_all_without ='V5',posemo_before_first='V6',posemo_before_second='V7',posemo_before_third='V8')}
               else { df_",i,"_posemo <- rename(df_",i,"_posemo, posemo_after_first='V1', posemo_after_second='V2',posemo_after_third='V3',posemo_after_fourth='V4',posemo_all_with='V5',posemo_all_without ='V6',posemo_before_first='V7',posemo_before_second='V8',posemo_before_third='V9',posemo_before_fourth='V10')}
")
  a<- gsub("[\r\n]", "", a)      
  print(a)
  eval(parse(text = a))
 
}

for (i in 13:23) {
  a <-  paste0("if(nrow(df_",i,")==2){ df_",i,"_posemo <- rename(df_",i,"_posemo, posemo_all_with='V1', posemo_all_without='V2')} 
               else if (nrow(df_",i,")==4){ df_",i,"_posemo <- rename(df_",i,"_posemo, posemo_after_first='V1', posemo_all_with='V2',posemo_all_without ='V3',posemo_before_first='V4')} 
               else if (nrow(df_",i,")==6){ df_",i,"_posemo <- rename(df_",i,"_posemo, posemo_after_first='V1', posemo_after_second='V2',posemo_all_with='V3',posemo_all_without ='V4',posemo_before_first='V5',posemo_before_second='V6')}
               else if (nrow(df_",i,")==8){ df_",i,"_posemo <- rename(df_",i,"_posemo, posemo_after_first='V1', posemo_after_second='V2',posemo_after_third='V3',posemo_all_with='V4',posemo_all_without ='V5',posemo_before_first='V6',posemo_before_second='V7',posemo_before_third='V8')}
               else { df_",i,"_posemo <- rename(df_",i,"_posemo, posemo_after_first='V1', posemo_after_second='V2',posemo_after_third='V3',posemo_after_fourth='V4',posemo_all_with='V5',posemo_all_without ='V6',posemo_before_first='V7',posemo_before_second='V8',posemo_before_third='V9',posemo_before_fourth='V10')}
               ")
  a<- gsub("[\r\n]", "", a)      
  print(a)
  eval(parse(text = a))
  
}

for (i in 25:83) {
  a <-  paste0("if(nrow(df_",i,")==2){ df_",i,"_posemo <- rename(df_",i,"_posemo, posemo_all_with='V1', posemo_all_without='V2')} 
               else if (nrow(df_",i,")==4){ df_",i,"_posemo <- rename(df_",i,"_posemo, posemo_after_first='V1', posemo_all_with='V2',posemo_all_without ='V3',posemo_before_first='V4')} 
               else if (nrow(df_",i,")==6){ df_",i,"_posemo <- rename(df_",i,"_posemo, posemo_after_first='V1', posemo_after_second='V2',posemo_all_with='V3',posemo_all_without ='V4',posemo_before_first='V5',posemo_before_second='V6')}
               else if (nrow(df_",i,")==8){ df_",i,"_posemo <- rename(df_",i,"_posemo, posemo_after_first='V1', posemo_after_second='V2',posemo_after_third='V3',posemo_all_with='V4',posemo_all_without ='V5',posemo_before_first='V6',posemo_before_second='V7',posemo_before_third='V8')}
               else { df_",i,"_posemo <- rename(df_",i,"_posemo, posemo_after_first='V1', posemo_after_second='V2',posemo_after_third='V3',posemo_after_fourth='V4',posemo_all_with='V5',posemo_all_without ='V6',posemo_before_first='V7',posemo_before_second='V8',posemo_before_third='V9',posemo_before_fourth='V10')}
               ")
  a<- gsub("[\r\n]", "", a)      
  print(a)
  eval(parse(text = a))
  
}

####################################################################
##Renaming negemo

for (i in 1:6) {
  a <-  paste0("if(nrow(df_",i,")==2){ df_",i,"_negemo <- rename(df_",i,"_negemo, negemo_all_with='V1', negemo_all_without='V2')} 
               else if (nrow(df_",i,")==4){ df_",i,"_negemo <- rename(df_",i,"_negemo, negemo_after_first='V1', negemo_all_with='V2',negemo_all_without ='V3',negemo_before_first='V4')} 
               else if (nrow(df_",i,")==6){ df_",i,"_negemo <- rename(df_",i,"_negemo, negemo_after_first='V1', negemo_after_second='V2',negemo_all_with='V3',negemo_all_without ='V4',negemo_before_first='V5',negemo_before_second='V6')}
               else if (nrow(df_",i,")==8){ df_",i,"_negemo <- rename(df_",i,"_negemo, negemo_after_first='V1', negemo_after_second='V2',negemo_after_third='V3',negemo_all_with='V4',negemo_all_without ='V5',negemo_before_first='V6',negemo_before_second='V7',negemo_before_third='V8')}
               else { df_",i,"_negemo <- rename(df_",i,"_negemo, negemo_after_first='V1', negemo_after_second='V2',negemo_after_third='V3',negemo_after_fourth='V4',negemo_all_with='V5',negemo_all_without ='V6',negemo_before_first='V7',negemo_before_second='V8',negemo_before_third='V9',negemo_before_fourth='V10')}
               ")
  a<- gsub("[\r\n]", "", a)      
  print(a)
  eval(parse(text = a))
  
}

for (i in 13:23) {
  a <-  paste0("if(nrow(df_",i,")==2){ df_",i,"_negemo <- rename(df_",i,"_negemo, negemo_all_with='V1', negemo_all_without='V2')} 
               else if (nrow(df_",i,")==4){ df_",i,"_negemo <- rename(df_",i,"_negemo, negemo_after_first='V1', negemo_all_with='V2',negemo_all_without ='V3',negemo_before_first='V4')} 
               else if (nrow(df_",i,")==6){ df_",i,"_negemo <- rename(df_",i,"_negemo, negemo_after_first='V1', negemo_after_second='V2',negemo_all_with='V3',negemo_all_without ='V4',negemo_before_first='V5',negemo_before_second='V6')}
               else if (nrow(df_",i,")==8){ df_",i,"_negemo <- rename(df_",i,"_negemo, negemo_after_first='V1', negemo_after_second='V2',negemo_after_third='V3',negemo_all_with='V4',negemo_all_without ='V5',negemo_before_first='V6',negemo_before_second='V7',negemo_before_third='V8')}
               else { df_",i,"_negemo <- rename(df_",i,"_negemo, negemo_after_first='V1', negemo_after_second='V2',negemo_after_third='V3',negemo_after_fourth='V4',negemo_all_with='V5',negemo_all_without ='V6',negemo_before_first='V7',negemo_before_second='V8',negemo_before_third='V9',negemo_before_fourth='V10')}
               ")
  a<- gsub("[\r\n]", "", a)      
  print(a)
  eval(parse(text = a))
  
}

for (i in 25:83) {
  a <-  paste0("if(nrow(df_",i,")==2){ df_",i,"_negemo <- rename(df_",i,"_negemo, negemo_all_with='V1', negemo_all_without='V2')} 
               else if (nrow(df_",i,")==4){ df_",i,"_negemo <- rename(df_",i,"_negemo, negemo_after_first='V1', negemo_all_with='V2',negemo_all_without ='V3',negemo_before_first='V4')} 
               else if (nrow(df_",i,")==6){ df_",i,"_negemo <- rename(df_",i,"_negemo, negemo_after_first='V1', negemo_after_second='V2',negemo_all_with='V3',negemo_all_without ='V4',negemo_before_first='V5',negemo_before_second='V6')}
               else if (nrow(df_",i,")==8){ df_",i,"_negemo <- rename(df_",i,"_negemo, negemo_after_first='V1', negemo_after_second='V2',negemo_after_third='V3',negemo_all_with='V4',negemo_all_without ='V5',negemo_before_first='V6',negemo_before_second='V7',negemo_before_third='V8')}
               else { df_",i,"_negemo <- rename(df_",i,"_negemo, negemo_after_first='V1', negemo_after_second='V2',negemo_after_third='V3',negemo_after_fourth='V4',negemo_all_with='V5',negemo_all_without ='V6',negemo_before_first='V7',negemo_before_second='V8',negemo_before_third='V9',negemo_before_fourth='V10')}
               ")
  a<- gsub("[\r\n]", "", a)      
  print(a)
  eval(parse(text = a))
  
}

####################################################################
##Renaming cogproc

for (i in 1:6) {
  a <-  paste0("if(nrow(df_",i,")==2){ df_",i,"_cogproc <- rename(df_",i,"_cogproc, cogproc_all_with='V1', cogproc_all_without='V2')} 
               else if (nrow(df_",i,")==4){ df_",i,"_cogproc <- rename(df_",i,"_cogproc, cogproc_after_first='V1', cogproc_all_with='V2',cogproc_all_without ='V3',cogproc_before_first='V4')} 
               else if (nrow(df_",i,")==6){ df_",i,"_cogproc <- rename(df_",i,"_cogproc, cogproc_after_first='V1', cogproc_after_second='V2',cogproc_all_with='V3',cogproc_all_without ='V4',cogproc_before_first='V5',cogproc_before_second='V6')}
               else if (nrow(df_",i,")==8){ df_",i,"_cogproc <- rename(df_",i,"_cogproc, cogproc_after_first='V1', cogproc_after_second='V2',cogproc_after_third='V3',cogproc_all_with='V4',cogproc_all_without ='V5',cogproc_before_first='V6',cogproc_before_second='V7',cogproc_before_third='V8')}
               else { df_",i,"_cogproc <- rename(df_",i,"_cogproc, cogproc_after_first='V1', cogproc_after_second='V2',cogproc_after_third='V3',cogproc_after_fourth='V4',cogproc_all_with='V5',cogproc_all_without ='V6',cogproc_before_first='V7',cogproc_before_second='V8',cogproc_before_third='V9',cogproc_before_fourth='V10')}
               ")
  a<- gsub("[\r\n]", "", a)      
  print(a)
  eval(parse(text = a))
  
}

for (i in 13:23) {
  a <-  paste0("if(nrow(df_",i,")==2){ df_",i,"_cogproc <- rename(df_",i,"_cogproc, cogproc_all_with='V1', cogproc_all_without='V2')} 
               else if (nrow(df_",i,")==4){ df_",i,"_cogproc <- rename(df_",i,"_cogproc, cogproc_after_first='V1', cogproc_all_with='V2',cogproc_all_without ='V3',cogproc_before_first='V4')} 
               else if (nrow(df_",i,")==6){ df_",i,"_cogproc <- rename(df_",i,"_cogproc, cogproc_after_first='V1', cogproc_after_second='V2',cogproc_all_with='V3',cogproc_all_without ='V4',cogproc_before_first='V5',cogproc_before_second='V6')}
               else if (nrow(df_",i,")==8){ df_",i,"_cogproc <- rename(df_",i,"_cogproc, cogproc_after_first='V1', cogproc_after_second='V2',cogproc_after_third='V3',cogproc_all_with='V4',cogproc_all_without ='V5',cogproc_before_first='V6',cogproc_before_second='V7',cogproc_before_third='V8')}
               else { df_",i,"_cogproc <- rename(df_",i,"_cogproc, cogproc_after_first='V1', cogproc_after_second='V2',cogproc_after_third='V3',cogproc_after_fourth='V4',cogproc_all_with='V5',cogproc_all_without ='V6',cogproc_before_first='V7',cogproc_before_second='V8',cogproc_before_third='V9',cogproc_before_fourth='V10')}
               ")
  a<- gsub("[\r\n]", "", a)      
  print(a)
  eval(parse(text = a))
  
}

for (i in 25:83) {
  a <-  paste0("if(nrow(df_",i,")==2){ df_",i,"_cogproc <- rename(df_",i,"_cogproc, cogproc_all_with='V1', cogproc_all_without='V2')} 
               else if (nrow(df_",i,")==4){ df_",i,"_cogproc <- rename(df_",i,"_cogproc, cogproc_after_first='V1', cogproc_all_with='V2',cogproc_all_without ='V3',cogproc_before_first='V4')} 
               else if (nrow(df_",i,")==6){ df_",i,"_cogproc <- rename(df_",i,"_cogproc, cogproc_after_first='V1', cogproc_after_second='V2',cogproc_all_with='V3',cogproc_all_without ='V4',cogproc_before_first='V5',cogproc_before_second='V6')}
               else if (nrow(df_",i,")==8){ df_",i,"_cogproc <- rename(df_",i,"_cogproc, cogproc_after_first='V1', cogproc_after_second='V2',cogproc_after_third='V3',cogproc_all_with='V4',cogproc_all_without ='V5',cogproc_before_first='V6',cogproc_before_second='V7',cogproc_before_third='V8')}
               else { df_",i,"_cogproc <- rename(df_",i,"_cogproc, cogproc_after_first='V1', cogproc_after_second='V2',cogproc_after_third='V3',cogproc_after_fourth='V4',cogproc_all_with='V5',cogproc_all_without ='V6',cogproc_before_first='V7',cogproc_before_second='V8',cogproc_before_third='V9',cogproc_before_fourth='V10')}
               ")
  a<- gsub("[\r\n]", "", a)      
  print(a)
  eval(parse(text = a))
  
}

####################################################################
##Adding new column recording participant numbers
for (i in 1:6) {
  a <-  paste0("df_",i,"_posemo$p_number=",i)
  print(a)
  eval(parse(text = a))
  
}

for (i in 13:23) {
  a <-  paste0("df_",i,"_posemo$p_number=",i)
  print(a)
  eval(parse(text = a))
  
}

for (i in 25:83) {
  a <-  paste0("df_",i,"_posemo$p_number=",i)
  print(a)
  eval(parse(text = a))
 
}

####################################################################
##Combing columns


for (i in 1:6) {
  a <-  paste0("df_",i,"<- cbind(df_",i,"_posemo,df_",i,"_negemo,df_",i,"_cogproc)")
  print(a)
  eval(parse(text = a))
  
}
for (i in 13:23) {
  a <-  paste0("df_",i,"<- cbind(df_",i,"_posemo,df_",i,"_negemo,df_",i,"_cogproc)")
  print(a)
  eval(parse(text = a))
  
}
for (i in 25:83) {
  a <-  paste0("df_",i,"<- cbind(df_",i,"_posemo,df_",i,"_negemo,df_",i,"_cogproc)")
  print(a)
  eval(parse(text = a))
  
}

####################################################################
##Combing rows
detach(package:dplyr)
library(plyr)
df <- rbind.fill(df_1[2,],df_2[2,])

for (i in 3:6) {
  a <-  paste0("df<- rbind.fill(df,df_",i,"[2,])")
  print(a)
  eval(parse(text = a))
  
}

for (i in 13:23) {
  a <-  paste0("df<- rbind.fill(df,df_",i,"[2,])")
  print(a)
  eval(parse(text = a))
  
}

for (i in 25:83) {
  a <-  paste0("df<- rbind.fill(df,df_",i,"[2,])")
  print(a)
  eval(parse(text = a))
  
}

detach(package:plyr)
####################################################################
##Exporting data

library(xlsx)
write.xlsx(df, "LIWCForEach.xlsx")

####################################################################
##Merging experiment.xlsx with LIWCForEach.xlsx
experiment <- read_excel('~/info350S2/experiment.xlsx')
df<- cbind(experiment[2:77,],df)

write.xlsx(df, "experimentResult.xlsx")



