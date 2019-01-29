library(readxl)
library(dplyr)
library(reshape2)


##Reading each participant's csv file and combind their data 
####################################################################
##Reading data
df_1 <- read.csv('~/Downloads/350s2/Participants/1/Report - 1.csv')
df_1 <- as.data.frame(t(df_1))


df_2 <- read.csv('~/Downloads/350s2/Participants/2/Report - 2.csv')
df_2 <- as.data.frame(t(df_2))

df_3 <- read.csv('~/Downloads/350s2/Participants/3/Report - 3.csv')
df_3 <- as.data.frame(t(df_3))
# df <- rbind(df,df_3[2,])

df_4 <- read.csv('~/Downloads/350s2/Participants/4/Report - 4.csv')
df_4 <- as.data.frame(t(df_4))

df_5 <- read.csv('~/Downloads/350s2/Participants/5/Report - 5.csv')
df_5 <- as.data.frame(t(df_5))

df_6 <- read.csv('~/Downloads/350s2/Participants/6/Report - 6.csv')
df_6 <- as.data.frame(t(df_6))

# df_7 <- read.csv('~/Downloads/350s2/Participants/7/Report - 7.csv')
# df_7 <- as.data.frame(t(df_7))
# 
# df_8 <- read.csv('~/Downloads/350s2/Participants/8/Report - 8.csv')
# df_8 <- as.data.frame(t(df_8))

# df_11 <- read.csv('~/Downloads/350s2/Participants/11/Report - 11.csv')
# df_11<- as.data.frame(t(df_11))

df_13 <- read.csv('~/Downloads/350s2/Participants/13/Report - 13.csv')
df_13 <- as.data.frame(t(df_13))

df_14 <- read.csv('~/Downloads/350s2/Participants/14/Report - 14.csv')
df_14 <- as.data.frame(t(df_14))

df_15 <- read.csv('~/Downloads/350s2/Participants/15/Report - 15.csv')
df_15 <- as.data.frame(t(df_15))

df_16 <- read.csv('~/Downloads/350s2/Participants/16/Report - 16.csv')
df_16 <- as.data.frame(t(df_16))

df_17 <- read.csv('~/Downloads/350s2/Participants/17/Report - 17.csv')
df_17 <- as.data.frame(t(df_17))

df_18 <- read.csv('~/Downloads/350s2/Participants/18/Report - 18.csv')
df_18 <- as.data.frame(t(df_18))

df_19 <- read.csv('~/Downloads/350s2/Participants/19/Report - 19.csv')
df_19 <- as.data.frame(t(df_19))

df_20 <- read.csv('~/Downloads/350s2/Participants/20/Report - 20.csv')
df_20 <- as.data.frame(t(df_20))

df_21 <- read.csv('~/Downloads/350s2/Participants/21/Report - 21.csv')
df_21 <- as.data.frame(t(df_21))

df_22 <- read.csv('~/Downloads/350s2/Participants/22/Report - 22.csv')
df_22 <- as.data.frame(t(df_22))

df_23 <- read.csv('~/Downloads/350s2/Participants/23/Report - 23.csv')
df_23 <- as.data.frame(t(df_23))

df_25 <- read.csv('~/Downloads/350s2/Participants/25/Report - 25.csv')
df_25 <- as.data.frame(t(df_25))

df_26 <- read.csv('~/Downloads/350s2/Participants/26/Report - 26.csv')
df_26 <- as.data.frame(t(df_26))

df_27 <- read.csv('~/Downloads/350s2/Participants/27/Report - 27.csv')
df_27 <- as.data.frame(t(df_27))

df_28 <- read.csv('~/Downloads/350s2/Participants/28/Report - 28.csv')
df_28 <- as.data.frame(t(df_28))

df_29 <- read.csv('~/Downloads/350s2/Participants/29/Report - 29.csv')
df_29 <- as.data.frame(t(df_29))

df_30 <- read.csv('~/Downloads/350s2/Participants/30/Report - 30.csv')
df_30 <- as.data.frame(t(df_30))

df_31 <- read.csv('~/Downloads/350s2/Participants/31/Report - 31.csv')
df_31 <- as.data.frame(t(df_31))

df_32 <- read.csv('~/Downloads/350s2/Participants/32/Report - 32.csv')
df_32 <- as.data.frame(t(df_32))

df_33 <- read.csv('~/Downloads/350s2/Participants/33/Report - 33.csv')
df_33 <- as.data.frame(t(df_33))

df_34 <- read.csv('~/Downloads/350s2/Participants/34/Report - 34.csv')
df_34 <- as.data.frame(t(df_34))

df_35 <- read.csv('~/Downloads/350s2/Participants/35/Fixed reports/Report - 35.csv')
df_35 <- as.data.frame(t(df_35))

df_36 <- read.csv('~/Downloads/350s2/Participants/36/Fixed reports/Report - 36.csv')
df_36 <- as.data.frame(t(df_36))

df_37 <- read.csv('~/Downloads/350s2/Participants/37/Fixed reports/Report - 37.csv')
df_37 <- as.data.frame(t(df_37))

df_38 <- read.csv('~/Downloads/350s2/Participants/38/Fixed reports/Report - 38.csv')
df_38 <- as.data.frame(t(df_38))

df_39 <- read.csv('~/Downloads/350s2/Participants/39/Fixed reports/Report - 39.csv')
df_39 <- as.data.frame(t(df_39))

df_40 <- read.csv('~/Downloads/350s2/Participants/40/Fixed reports/Report - 40.csv')
df_40 <- as.data.frame(t(df_40))

df_41 <- read.csv('~/Downloads/350s2/Participants/41/Fixed reports/Report - 41.csv')
df_41 <- as.data.frame(t(df_41))

df_42 <- read.csv('~/Downloads/350s2/Participants/42/Fixed reports/Report - 42.csv')
df_42 <- as.data.frame(t(df_42))

df_43 <- read.csv('~/Downloads/350s2/Participants/43/Fixed reports/Report - 43.csv')
df_43 <- as.data.frame(t(df_43))

df_44 <- read.csv('~/Downloads/350s2/Participants/44/Fixed reports/Report - 44.csv')
df_44 <- as.data.frame(t(df_44))

df_45 <- read.csv('~/Downloads/350s2/Participants/45/Fixed reports/Report - 45.csv')
df_45 <- as.data.frame(t(df_45))

df_46 <- read.csv('~/Downloads/350s2/Participants/46/Fixed reports/Report - 46.csv')
df_46 <- as.data.frame(t(df_46))

df_47 <- read.csv('~/Downloads/350s2/Participants/47/Fixed reports/Report - 47.csv')
df_47 <- as.data.frame(t(df_47))

df_48 <- read.csv('~/Downloads/350s2/Participants/48/Fixed reports/Report - 48.csv')
df_48 <- as.data.frame(t(df_48))

df_49 <- read.csv('~/Downloads/350s2/Participants/49/Fixed reports/Report - 49.csv')
df_49 <- as.data.frame(t(df_49))

df_50 <- read.csv('~/Downloads/350s2/Participants/50/Fixed reports/Report - 50.csv')
df_50 <- as.data.frame(t(df_50))

df_51 <- read.csv('~/Downloads/350s2/Participants/51/Fixed reports/Report - 51.csv')
df_51 <- as.data.frame(t(df_51))

df_52 <- read.csv('~/Downloads/350s2/Participants/52/Fixed reports/Report - 52.csv')
df_52 <- as.data.frame(t(df_52))

df_53 <- read.csv('~/Downloads/350s2/Participants/53/Fixed reports/Report - 53.csv')
df_53 <- as.data.frame(t(df_53))

df_54 <- read.csv('~/Downloads/350s2/Participants/54/Fixed reports/Report - 54.csv')
df_54 <- as.data.frame(t(df_54))

df_55 <- read.csv('~/Downloads/350s2/Participants/55/Fixed reports/Report - 55.csv')
df_55 <- as.data.frame(t(df_55))

df_56 <- read.csv('~/Downloads/350s2/Participants/56/Fixed reports/Report - 56.csv')
df_56 <- as.data.frame(t(df_56))

df_57 <- read.csv('~/Downloads/350s2/Participants/57/Report - 57.csv')
df_57 <- as.data.frame(t(df_57))

df_58 <- read.csv('~/Downloads/350s2/Participants/58/Report - 58.csv')
df_58 <- as.data.frame(t(df_58))

df_59 <- read.csv('~/Downloads/350s2/Participants/59/Report - 59.csv')
df_59 <- as.data.frame(t(df_59))

df_60 <- read.csv('~/Downloads/350s2/Participants/60/Report - 60.csv')
df_60 <- as.data.frame(t(df_60))

df_61 <- read.csv('~/Downloads/350s2/Participants/61/Report - 61.csv')
df_61 <- as.data.frame(t(df_61))

df_62 <- read.csv('~/Downloads/350s2/Participants/62/Report - 62.csv')
df_62 <- as.data.frame(t(df_62))

df_63 <- read.csv('~/Downloads/350s2/Participants/63/Report - 63.csv')
df_63 <- as.data.frame(t(df_63))

df_64 <- read.csv('~/Downloads/350s2/Participants/64/Report - 64.csv')
df_64 <- as.data.frame(t(df_64))

df_65 <- read.csv('~/Downloads/350s2/Participants/65/Report - 65.csv')
df_65 <- as.data.frame(t(df_65))

df_66 <- read.csv('~/Downloads/350s2/Participants/66/Report - 66.csv')
df_66 <- as.data.frame(t(df_66))

df_67 <- read.csv('~/Downloads/350s2/Participants/67/Report - 67.csv')
df_67 <- as.data.frame(t(df_67))

df_68 <- read.csv('~/Downloads/350s2/Participants/68/Report - 68.csv')
df_68 <- as.data.frame(t(df_68))

df_69 <- read.csv('~/Downloads/350s2/Participants/69/Report - 69.csv')
df_69 <- as.data.frame(t(df_69))

df_70 <- read.csv('~/Downloads/350s2/Participants/70/Report - 70.csv')
df_70 <- as.data.frame(t(df_70))

df_71 <- read.csv('~/Downloads/350s2/Participants/71/Report - 71.csv')
df_71 <- as.data.frame(t(df_71))

df_72 <- read.csv('~/Downloads/350s2/Participants/72/Report - 72.csv')
df_72 <- as.data.frame(t(df_72))

df_73 <- read.csv('~/Downloads/350s2/Participants/73/Report - 73.csv')
df_73 <- as.data.frame(t(df_73))

df_74 <- read.csv('~/Downloads/350s2/Participants/74/Report - 74.csv')
df_74 <- as.data.frame(t(df_74))

df_75 <- read.csv('~/Downloads/350s2/Participants/75/Report - 75.csv')
df_75 <- as.data.frame(t(df_75))

df_76 <- read.csv('~/Downloads/350s2/Participants/76/Report - 76.csv')
df_76 <- as.data.frame(t(df_76))

df_77 <- read.csv('~/Downloads/350s2/Participants/77/Report - 77.csv')
df_77 <- as.data.frame(t(df_77))

df_78 <- read.csv('~/Downloads/350s2/Participants/78/Report - 78.csv')
df_78 <- as.data.frame(t(df_78))

df_79 <- read.csv('~/Downloads/350s2/Participants/79/Report - 79.csv')
df_79 <- as.data.frame(t(df_79))

df_80 <- read.csv('~/Downloads/350s2/Participants/80/Report - 80.csv')
df_80 <- as.data.frame(t(df_80))

df_81 <- read.csv('~/Downloads/350s2/Participants/81/Report - 81.csv')
df_81 <- as.data.frame(t(df_81))

df_82 <- read.csv('~/Downloads/350s2/Participants/82/Report - 82.csv')
df_82 <- as.data.frame(t(df_82))

df_83 <- read.csv('~/Downloads/350s2/Participants/83/Report - 83.csv')
df_83 <- as.data.frame(t(df_83))

####################################################################
##Selecting the attributes we need

for (i in 1:6) {
  a <- paste0("df_",i," <- df_",i," %>% select(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16)")
  print(a)
  eval(parse(text = a))
}

for (i in 13:23) {
  a <- paste0("df_",i," <- df_",i," %>% select(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16)")
  print(a)
  eval(parse(text = a))
}

for (i in 25:83) {
  a <- paste0("df_",i," <- df_",i," %>% select(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16)")
  print(a)
  eval(parse(text = a))
}

####################################################################
##Combining data

df <- rbind(df_1,df_2[2,])


for (i in 3:6) {
  a <- paste0("df <- rbind(df,df_",i,"[2,])")
  print(a)
  eval(parse(text = a))
}

for (i in 13:23) {
  a <- paste0("df <- rbind(df,df_",i,"[2,])")
  print(a)
  eval(parse(text = a))
}

for (i in 25:83) {
  a <- paste0("df <- rbind(df,df_",i,"[2,])")
  print(a)
  eval(parse(text = a))
}

####################################################################
##Exporting data

library(xlsx)
write.xlsx(df, "experiment.xlsx")