library(readxl)
library(tidyverse)
library(ggplot2)
library(plotly)

#import Table cp-1 sheet from the excel file
statedf <- read_excel("C:/Users/nebar/Desktop/Dataderby/National+education+cost+data.xlsx",sheet = "TABLE CP-3", skip = 2)

#extract the data frame for public two years in district fees in 2023 dollars and rename the first column name
statedf1 <- statedf[1:51,c(1:23)]|>
  rename("States" = `In 2023 Dollars...1`)
statedf1

#extract the data frame for public two years in district fees in current dollars and rename the first column name
statedf2 <- statedf[54:104,c(1:23)]|>
  rename("States" = `In 2023 Dollars...1`)
statedf2

#extract the data frame for public four year in-state fees in 2023 dollars and rename the first column name
statedf3 <- statedf[1:51,c(24:46)]|>
  rename("States" = `In 2023 Dollars...24`)
statedf3

#extract the data frame for public four year in-state fees in current dollars and rename the first column name
statedf4 <- statedf[54:104,c(24:46)]|>
   rename("States" = `In 2023 Dollars...24`)
statedf4

#change all the columns except the first one to numeric
statedf1u <- cbind(statedf1$States,as.data.frame(apply(statedf1[2:23],2, as.numeric)))|>
  rename("States" = `statedf1$States`)

statedf2u <- cbind(statedf2$States,as.data.frame(apply(statedf2[2:23],2,as.numeric)))|>
  rename("States" = `statedf2$States`)

statedf3u <- cbind(statedf3$States,as.data.frame(apply(statedf3[2:23],2,as.numeric)))|>
  rename("States" = `statedf3$States`)

statedf4u <- cbind(statedf4$States,as.data.frame(apply(statedf4[2:23],2,as.numeric)))|>
  rename("States" = `statedf4$States`)

#find statistical summary for all the tables
summary(statedf1u)
summary(statedf2u)
summary(statedf3u)
summary(statedf4u)

#since Alaska and district Colombia don't have data for district fees needs to be removed
statedf1u1 <- statedf1u[-c(2,9),]
statedf2u1 <- statedf2u[-c(2,9),]



df1 <- gather(statedf1u1,key = "Year",value = "fees",2:21)
df2 <- gather(statedf2u1,key = "Year", value = "fees", 2:21)
df3 <- gather(statedf3u,key = "Year", value = "fees", 2:21)
df4 <- gather(statedf4u,key = "Year", value = "fees", 2:21)



ggplotly(ggplot(df1, aes(x = Year, y=fees)) + geom_point(aes(color = States))+stat_smooth())
ggplotly(ggplot(df2, aes(x = Year, y=fees)) + geom_point(aes(color = States)))
ggplotly(ggplot(df1, aes(x = factor(States), y = fees))+ 
  geom_bar(stat = "Summary", fun = "mean",fill = "steelblue") + theme(axis.text.x = element_text(angle = 90))) 
lotly(ggplot(df3, aes(x = factor(States), y = fees))+ 
  geom_bar(stat = "Summary", fun = "mean",fill = "grey") + theme(axis.text.x = element_text(angle = 90))) 

