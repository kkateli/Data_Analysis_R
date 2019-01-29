library(readxl)
library(dplyr)
library(knitr)
library(ggplot2)

##Extracting texts from each interval and calculating cogproc by LIWC

df <- read_excel('~/info350S2/experimentResult.xlsx')
##5_min_interval
for(i in 1:76){
  
  a <- paste0("write.table(df[",i,",14], file = '/Users/kateli/info350S2/txt_for_each_interval/5_min_interval_",i,".txt', sep = '\t',row.names = FALSE,col.names = FALSE)")
  print(a)
  eval(parse(text = a)) 
 
}
##10_min_interval
for(i in 1:76){
  
  a <- paste0("write.table(df[",i,",15], file = '/Users/kateli/info350S2/txt_for_each_interval/10_min_interval_",i,".txt', sep = '\t',row.names = FALSE,col.names = FALSE)")
  print(a)
  eval(parse(text = a)) 
  
}

##15_min_interval
for(i in 1:76){
  
  a <- paste0("write.table(df[",i,",16], file = '/Users/kateli/info350S2/txt_for_each_interval/15_min_interval_",i,".txt', sep = '\t',row.names = FALSE,col.names = FALSE)")
  print(a)
  eval(parse(text = a)) 
  
}

##20_min_interval
for(i in 1:76){
  
  a <- paste0("write.table(df[",i,",17], file = '/Users/kateli/info350S2/txt_for_each_interval/20_min_interval_",i,".txt', sep = '\t',row.names = FALSE,col.names = FALSE)")
  print(a)
  eval(parse(text = a)) 
  
}

##25_min_interval
for(i in 1:76){
  
  a <- paste0("write.table(df[",i,",18], file = '/Users/kateli/info350S2/txt_for_each_interval/25_min_interval_",i,".txt', sep = '\t',row.names = FALSE,col.names = FALSE)")
  print(a)
  eval(parse(text = a)) 
  
}




