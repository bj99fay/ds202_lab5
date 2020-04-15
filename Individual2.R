install.packages('maps')
install.packages('mapproj')
library(ggplot2)
library(dplyr)
library(tidyr)
install.packages("RColorBrewer")
library(RColorBrewer)
library(maps)

library(mapproj)
setwd('/Users/chase/Desktop/RProjects/ds202/ds202_lab5')


accident <- read.csv("https://raw.githubusercontent.com/xdaiISU/ds202materials/master/hwlabs/fars2017/accident.csv", stringsAsFactors = FALSE)
person <- read.csv("https://raw.githubusercontent.com/xdaiISU/ds202materials/master/hwlabs/fars2017/person.csv", stringsAsFactors = FALSE)

day.dist <- accident %>%
  mutate(DAY_WEEK_NAME=recode(as.factor(DAY_WEEK), "1" = "SUNDAY", "2" = "MONDAY", "3" = "TUESDAY", "4" = "WEDNESDAY", 
                              "5" = "THURSDAY", "6" = "FRIDAY", "7" = "SATURDAY", "9" = "UNKNOWN")) %>%
  group_by(DAY_WEEK_NAME)
day.dist %>% ggplot(aes(x=DAY_WEEK_NAME)) + geom_bar() + labs(x = "DAy of Week", title = "Distribution of Accidents by Day of Week")

time.dist <- accident %>%
  filter(HOUR < 24) %>% group_by(HOUR) %>%
  time.dist %>% ggplot(aes(x=HOUR)) + geom_bar() + labs(x = "Time of Day by Hour (24hr clock)", title = "Distribution of Accidents by Time of Day")


drunk <- accident %>%
  mutate(DRUNK = ifelse(DRUNK_DR > 0, "Yes", "No")) %>% group_by(DRUNK)

drunk %>% filter(DRUNK == "Yes") %>% count() %>% rename("Drunk driving accidents" = "n")

drunk %>% ggplot(aes(x=DRUNK)) + geom_bar() + labs(x = "Was a Drunk Driver Invloved?", title = "Accidents Invloving >= 1 Drunk Driver")

person <- person %>%
  filter(PER_TYP == 1)
person


colnames(person)
colnames(accident)
accPer <- accident %>% inner_join(person, by = c('ST_CASE', 'STATE', 'COUNTY'))

accPer %>% group_by(DAY_WEEK) %>% tally() %>% ggplot(aes(x = factor(DAY_WEEK), y = n)) + geom_bar(stat = 'identity') + 
  ggtitle("Number of Accidents per Day of the Week") + xlab('Day of Week') + 
  ylab("Number of Accidents") + 
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5", "6", "7"), labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

accPer %>% group_by(HOUR.x) %>% tally() %>% filter(HOUR.x != 99) %>% ggplot(aes(x = HOUR.x, y = n)) + geom_line() + ggtitle("Total Accidents per Hour") +
  xlab("Hour of Day") + ylab("Number of Accidents")

accPer %>% group_by(SEX) %>% tally() %>% filter(n > 900) %>% ggplot(aes(x = factor(SEX), y = n)) + geom_bar(stat = 'identity') +
  ggtitle("Total Accidents per Sex") + xlab('Sex') + ylab("Number of Accidents") +
  scale_x_discrete(breaks = c('1','2','9'), labels = c('Male', "Female", "Unknown"))

colnames(accPer)


countiesGLC <- readxl::read_xlsx('./Data/FRPP GLC - United StatesFeb132020.xlsx')

accPer <- accPer %>% filter(LONGITUD < 500, LONGITUD > -125)

county <- map_data("county")

countiesGLC

accPer

county


accPer

ggplot(county, mapping = aes(x = long, y = lat, group = order)) + geom_polygon() + coord_map()

countytally <- accPer %>% group_by(COUNTY) %>% tally()
countytally
table(countytally$COUNTY)

colnames(countytally)[1] = 'group'

county
finalSet <- county %>% left_join(countytally, by = 'group')
finalSet



ggplot(finalSet, mapping = aes(x = long, y = lat, group = group, fill = n)) + geom_polygon()

talbe
sum(table(accPer$STATE))
sum(table(accPer$COUNTY))




accPer1 <- accPer %>% left_join(countytally, by = 'COUNTY')

fullSet <- accPer1 %>% inner_join(county, by = 'COUNTY')


table(accPer$COUNTY)

ggplot(fullSet, mapping = aes(x = long, y = lat, fill = n)) + geom_polygon()


ggplot(accPer, mapping = aes(x = MONTH.x, y = n, fill = MONTH.x)) + 
  geom_bar(stat = 'identity') +
  facet_wrap("STATE")+
  scale_fill_gradient(low = 'blue', high = 'red')



?geom_smooth

monthTally <- accPer %>% group_by(MONTH.x) %>% tally()

accPer <- accPer %>% right_join(monthTally, by = "MONTH.x")
accPer












