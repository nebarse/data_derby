library(readxl)
library(tidyverse)
#import Table cp-1 sheet from the excel file
Iosdf <- read_excel("C:/Users/nebar/Desktop/Dataderby/National+education+cost+data.xlsx",sheet = "TABLE CP-4", skip = 2)
Iosdf

#extract the data frame for in-state tuition and fees in 2023 dollars and rename the column name
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


#extract the data frame for out-of-state tuition and fees in current dollars and rename column names
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


#preprocessing
Iosdf1u <- Iosdf1|>
  mutate_at(c(3:21), as.numeric)
Iosdf1u


Iosdf3u <- Iosdf3|>
  mutate_at(c(1:19),as.numeric)|>
  relocate(Universities, STATE)
Iosdf3u

Iosdf1u1 <- gather(Iosdf1u,key = "Year", value = "fees", 3:19)
Iosdf1u1 <- Iosdf1u1[,-c(3,4)]
Iosdf3u1 <- gather(Iosdf3u,key = "Year", value = "fees", 3:19)
Iosdf3u1 <- Iosdf3u1[,-c(3,4)]

Iosdf_f1 <- Iosdf1u1$fees
Iosdf_f1
Iosdf_f2 <- Iosdf3u1$fees
Iosdf_f2
library(ggplot2)


#compare the average between the tables
state_df <- data.frame(group = rep(c("In-state","Out-state"),each = 850), fees = c(Iosdf_f1,Iosdf_f2))
state_df

state_sum <- group_by(state_df,group)|>
  summarise(
    mean = mean(fees,na.rm = TRUE),
    sd = sd(fees,na.rm = TRUE)
  )
state_sum


#H0: mu(in) = mu(out);mu(in) != mu(out)

state_t <- t.test(fees ~ group,data = state_df, alternative = "two.sided",paired = TRUE)
state_t
# since t = -64.753 and p-value < 0.05, reject H0
#Therefore, there is evidence to support a significant difference between the average of in_state and out-state
model_st <- aov(fees ~ group, data = state_df)
summary(model_st)

ggplotly(ggplot(Iosdf1u1, aes(x = factor(STATE), y = fees))+ 
  geom_bar(stat = "Summary", fun = "mean",fill = "steelblue") + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Average in-state fees"))

ggplot(state_df,aes(x = group,y= fees)) + geom_boxplot() + labs(title = "comparison between in-state and out-state")

ggplotly(ggplot(Iosdf3u1, aes(x = factor(STATE), y = fees))+ 
  geom_bar(stat = "Summary", fun = "mean",fill = "grey") + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Average out-state fees"))


