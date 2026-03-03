library(readxl)
library(tidyverse)
#import Table cp-1 sheet from the excel file
Iosdf <- read_excel("C:/Users/nebar/Desktop/Dataderby/National+education+cost+data.xlsx",sheet = "TABLE CP-4", skip = 2)
Iosdf

#extract the data frame for in-state tuition and fees in 2023 dollars and rename the first column name
Iosdf1 <- Iosdf[1:50,c(1:21)]|>
  rename("Universities" = `In 2023 Dollars...1`,
         "2007-08" = `2007-08...3`,
         "2008-09" = `2008-09...4`,
         "2009-10" = `2009-10...5`,
         "2010-11" = `2010-11...6`,
         "2011-12" = `2011-12...7`,
         "2012-13" = `2012-13...8`,
         "2013-14" = `2013-14...9`,
         "2014-15" = `2014-15...10`,
         "2015-16" = `2015-16...11`,
         "2016-17" = `2016-17...12`,
         "2017-18" = `2017-18...13`,
         "2018-19" = `2018-19...14`,
         "2019-20" = `2019-20...15`,
         "2020-21" = `2020-21...16`,
         "2021-22" = `2021-22...17`,
         "2022-23" = `2022-23...18`,
         "2023-24" = `2023-24...19`,
         "1-Year" = `1-Year...20`,
         "5-Year" = `5-Year...21`)
Iosdf1

#extract the data frame for in-state tuition and fees in 2023 dollars and rename the first column name
Iosdf2 <- Iosdf[53:102,c(1:21)]|>
  rename("Universities" = `In 2023 Dollars...1`,
         "2007-08" = `2007-08...3`,
         "2008-09" = `2008-09...4`,
         "2009-10" = `2009-10...5`,
         "2010-11" = `2010-11...6`,
         "2011-12" = `2011-12...7`,
         "2012-13" = `2012-13...8`,
         "2013-14" = `2013-14...9`,
         "2014-15" = `2014-15...10`,
         "2015-16" = `2015-16...11`,
         "2016-17" = `2016-17...12`,
         "2017-18" = `2017-18...13`,
         "2018-19" = `2018-19...14`,
         "2019-20" = `2019-20...15`,
         "2020-21" = `2020-21...16`,
         "2021-22" = `2021-22...17`,
         "2022-23" = `2022-23...18`,
         "2023-24" = `2023-24...19`,
         "1-Year" = `1-Year...20`,
         "5-Year" = `5-Year...21`)
Iosdf2

#extract the data frame for out-of-state tuition and fees in current dollars and rename two column names
Iosdf3 <- Iosdf[1:50,c(22:40)]|> 
  cbind(Iosdf1$Universities,Iosdf1$STATE)|>
  rename("Universities" = `Iosdf1$Universities`, "STATE" = `Iosdf1$STATE`,
         "2007-08" = `2007-08...22`,
         "2008-09" = `2008-09...23`,
         "2009-10" = `2009-10...24`,
         "2010-11" = `2010-11...25`,
         "2011-12" = `2011-12...26`,
         "2012-13" = `2012-13...27`,
         "2013-14" = `2013-14...28`,
         "2014-15" = `2014-15...29`,
         "2015-16" = `2015-16...30`,
         "2016-17" = `2016-17...31`,
         "2017-18" = `2017-18...32`,
         "2018-19" = `2018-19...33`,
         "2019-20" = `2019-20...34`,
         "2020-21" = `2020-21...35`,
         "2021-22" = `2021-22...36`,
         "2022-23" = `2022-23...37`,
         "2023-24" = `2023-24...38`,
         "1-Year" = `1-Year...39`,
         "5-Year" = `5-Year...40`)
Iosdf3

#extract the data frame for out-of-state tuition and fees in current dollars and rename two column names
Iosdf4 <- Iosdf[53:102,c(22:40)]|> 
  cbind(Iosdf1$Universities,Iosdf1$STATE)|>
  rename("Universities" = `Iosdf1$Universities`, "STATE" = `Iosdf1$STATE`,
         "2007-08" = `2007-08...22`,
         "2008-09" = `2008-09...23`,
         "2009-10" = `2009-10...24`,
         "2010-11" = `2010-11...25`,
         "2011-12" = `2011-12...26`,
         "2012-13" = `2012-13...27`,
         "2013-14" = `2013-14...28`,
         "2014-15" = `2014-15...29`,
         "2015-16" = `2015-16...30`,
         "2016-17" = `2016-17...31`,
         "2017-18" = `2017-18...32`,
         "2018-19" = `2018-19...33`,
         "2019-20" = `2019-20...34`,
         "2020-21" = `2020-21...35`,
         "2021-22" = `2021-22...36`,
         "2022-23" = `2022-23...37`,
         "2023-24" = `2023-24...38`,
         "1-Year" = `1-Year...39`,
         "5-Year" = `5-Year...40`)
Iosdf4

#preprocessing
Iosdf1u <- Iosdf1|>
  mutate_at(c(3:21), as.numeric)
Iosdf1u

Iosdf2u <- Iosdf2|>
  mutate_at(c(3:21), as.numeric)
Iosdf2u

Iosdf3u <- Iosdf3|>
  mutate_at(c(1:19),as.numeric)|>
  relocate(Universities, STATE)
Iosdf3u

Iosdf4u <- Iosdf4|>
  mutate_at(c(1:19),as.numeric)|>
  relocate(Universities, STATE)
Iosdf4u

#find the statistical summary for all the tables
summary(Iosdf1u)
summary(Iosdf2u)
summary(Iosdf3u)
summary(Iosdf4u)


Iosdf1u1 <- gather(Iosdf1u,key = "Year",value = "fees",3:19)
Iosdf2u1 <- gather(Iosdf2u,key = "Year",value = "fees",3:19)
Iosdf3u1 <- gather(Iosdf3u,key = "Year",value = "fees",3:19)
Iosdf4u1 <- gather(Iosdf4u,key = "Year",value = "fees",3:19)

library(ggplot2)
library(plotly)
ggplotly(ggplot(Iosdf1u1, aes(x = Year, y=fees)) + geom_point(aes(color = STATE)) + labs(title = "In-state tutition and fees in 2023 dollars") + theme(axis.text.x = element_text(angle = 90)))

ggplotly(ggplot(Iosdf2u1, aes(x = Year, y=fees)) + geom_point(aes(color = STATE)) + labs(title = "Out-of-state tutition and fees in 2023 dollars") + theme(axis.text.x = element_text(angle = 90)))

ggplotly(ggplot(Iosdf3u1, aes(x = Year, y=fees)) + geom_point(aes(color = STATE)) + labs(title = "In-state tutition and fees in current dollars") + theme(axis.text.x = element_text(angle = 90)))

ggplotly(ggplot(Iosdf4u1, aes(x = Year, y=fees)) + geom_point(aes(color = STATE)) + labs(title = "Out-of-state tutition and fees in current dollars") + theme(axis.text.x = element_text(angle = 90)))

Iosdf1u1|>
  group_by(Year)|>
  summarize(avetu = mean(fees),setu = sd(fees)/sqrt(length(fees)),tstar = qt(1-0.05/2,length(fees)-1))|>
  ggplot(aes(x=Year, y=avetu))+geom_bar(aes(x= Year, y= avetu), stat ='identity',fill ='steelblue')+geom_errorbar(aes(ymin=avetu-tstar*setu, ymax=avetu+tstar*setu))

Iosdf2u1|>
  group_by(Year)|>
  summarize(avetu = mean(fees),setu = sd(fees)/sqrt(length(fees)),tstar = qt(1-0.05/2,length(fees)-1))|>
  ggplot(aes(x=Year, y=avetu))+geom_bar(aes(x= Year, y= avetu), stat ='identity',fill ='steelblue')+geom_errorbar(aes(ymin=avetu-tstar*setu, ymax=avetu+tstar*setu))


Iosdf3u1|>
  group_by(Year)|>
  summarize(avetu = mean(fees),setu = sd(fees)/sqrt(length(fees)),tstar = qt(1-0.05/2,length(fees)-1))|>
  ggplot(aes(x=Year, y=avetu))+geom_bar(aes(x= Year, y= avetu), stat ='identity',fill ='steelblue')+geom_errorbar(aes(ymin=avetu-tstar*setu, ymax=avetu+tstar*setu))

Iosdf4u1|>
  group_by(Year)|>
  summarize(avetu = mean(fees),setu = sd(fees)/sqrt(length(fees)),tstar = qt(1-0.05/2,length(fees)-1))|>
  ggplot(aes(x=Year, y=avetu))+geom_bar(aes(x= Year, y= avetu), stat ='identity',fill ='steelblue')+geom_errorbar(aes(ymin=avetu-tstar*setu, ymax=avetu+tstar*setu))



ggplot(Iosdf1u1,aes(x = Year, y= fees,color = STATE))+geom_text(label = Iosdf1u1$STATE)

ggplot(Iosdf2u1,aes(x = Year, y= fees,color = STATE))+geom_text(label = Iosdf2u1$STATE)
ggplot(Iosdf3u1,aes(x = Year, y= fees,color = STATE))+geom_text(label = Iosdf3u1$STATE)
ggplot(Iosdf4u1,aes(x = Year, y= fees,color = STATE))+geom_text(label = Iosdf4u1$STATE)
