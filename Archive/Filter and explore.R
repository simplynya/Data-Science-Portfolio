
#FINAL PROJECT
#Thank you for viewing

#I started off by reviewing all modules for a refresher

#Loading packages
install.packages("dplyr")
library(dplyr)
library(ggplot2)
install.packages("gridExtra")
library("gridExtra")

#EVALUATION 4
#Visualization and Table
#Here each number is counted to find out how many times they were played

#Cash Ball
#1. The number played the most for CB as called Cash ball
countCB<- CFL_lottery%>% group_by(CB) %>% summarize(count = n())%>%
arrange(desc(count))

#2.Bar graph of the count
ggplot(data=CFL_lottery) +
  geom_bar(mapping = aes(x = CB, fill=Date))

#3. Line graph of the count
ggplot(countCB) + geom_line(aes(x = CB, y = count))


#Pick 1
#1.
countPick1<- CFL_lottery%>% group_by(Pick1) %>% summarize(count = n())%>%
arrange(desc(count))

#2.
ggplot(data=CFL_lottery) +
  geom_bar(mapping = aes(x = Pick1, fill=Date))

#3.
ggplot(countPick1) + geom_line(aes(x = Pick1, y = count))


#Pick 2
#1.
countPick2<- CFL_lottery%>% group_by(Pick2) %>% summarize(count = n())%>%
arrange(desc(count))

#2.
ggplot(data=CFL_lottery) +
  geom_bar(mapping = aes(x = Pick2, fill=Date))

#3.
ggplot(countPick2) + geom_line(aes(x = Pick2, y = count))


#Pick 3
#1.
countPick3<- CFL_lottery%>% group_by(Pick3) %>% summarize(count = n())%>%
arrange(desc(count))

#2.
ggplot(data=CFL_lottery) +
  geom_bar(mapping = aes(x = Pick3, fill=Date))

#3.
ggplot(countPick3) + geom_line(aes(x = Pick3, y = count))

#Pick 4
#1.
countPick4<- CFL_lottery%>% group_by(Pick4) %>% summarize(count = n())%>%
arrange(desc(count))

#2.
ggplot(data=CFL_lottery) +
  geom_bar(mapping = aes(x = Pick4, fill=Date))

#3.
ggplot(countPick4) + geom_line(aes(x = Pick4, y = count))

#Not sure if I will need this. Arranging the weekdays together
Weekday<- arrange(CFL_lottery, Weekday)

#Pick 5
#1.
countPick5<- CFL_lottery%>% group_by(Pick5) %>% summarize(count = n())%>%
arrange(desc(count))

#2.
ggplot(data=CFL_lottery) +
  geom_bar(mapping = aes(x = Pick5, fill=Date))

#3.
ggplot(countPick5) + geom_line(aes(x = Pick5, y = count))



#EV 5, 6
#Here I made a new data frame for just the weekend, this answers one of the
#Evaluation questions


CFL_lotteryWeekend<- filter(CFL_lottery, Weekday %in% c("Saturday", "Sunday"))

#What is the most popular number in all the picks for the weekend
weekendMAxCB<-names(which.max(table(CFL_lotteryWeekend$CB)))  

weekendMaxPk1<-names(which.max(table(CFL_lotteryWeekend$Pick1))) 

weekendMaxPk2<- names(which.max(table(CFL_lotteryWeekend$Pick2))) 

weekendMaxPk3<- names(which.max(table(CFL_lotteryWeekend$Pick3))) 

weekendMaxPk4<- names(which.max(table(CFL_lotteryWeekend$Pick4))) 

weekendMaxPk5<- names(which.max(table(CFL_lotteryWeekend$Pick5))) 

#Making a saved data frame
weekendMaxplay<- data.frame(weekendMAxCB,weekendMaxPk1, weekendMaxPk2, weekendMaxPk3, weekendMaxPk4, weekendMaxPk5)



#EV 1, 2
  
#This looks at multiple duplicates in numbers playing together the most.  
CFL_lottery$duplicatePlay <- 1
duplicatePlay6<- aggregate(duplicatePlay ~ CB + Pick1 + Pick2 + Pick3 + Pick4 + Pick5,CFL_lottery, FUN = sum)

CFL_lottery$duplicatePlay_5 <- 1
duplicatePlay5<-aggregate(duplicatePlay_5 ~ Pick1 + Pick2 + Pick3 + Pick4 + Pick5,CFL_lottery, FUN = sum)  
  
CFL_lottery$duplicatePlay_4 <- 1
duplicatePlay4<-aggregate(duplicatePlay_4 ~ Pick1 + Pick2 + Pick3 + Pick4,CFL_lottery, FUN = sum)%>%
arrange(desc(duplicatePlay_4))
  
CFL_lottery$duplicatePlay_3 <- 1
duplicatePlay3<-aggregate(duplicatePlay_3 ~ Pick1 + Pick2 + Pick3,CFL_lottery, FUN = sum)%>%
  arrange(desc(duplicatePlay_3))

CFL_lottery$duplicatePlay_2 <- 1
duplicatePlay2<-aggregate(duplicatePlay_2 ~ Pick1 + Pick2,CFL_lottery, FUN = sum)%>%
  arrange(desc(duplicatePlay_2))

CFL_lottery$duplicatePlay_1 <- 1
duplicatePlay1<-aggregate(duplicatePlay_1 ~ Pick1 + CB,CFL_lottery, FUN = sum)%>%
  arrange(desc(duplicatePlay_1))



#This looks at the days that had the most wins vs loses
#Keep in mind the game started off playing on Mondays and Thursdays
ggplot(data=CFL_lottery) +
geom_bar(mapping = aes(x = Weekday, fill=Winners))+
ggtitle("Days of most wins vs lose") +
xlab("Full Week") +
ylab("Number of plays")


#Following graphs look at the pattern of plays for pick1, pick2, pick3, pick4,
#pick5 and CB since it started.


#The plays over time were grouped together, it is better viewed individually or by year
#It is clearly hard to see so the next will break them down by year, day and picks
Pick1 <- ggplot(CFL_lottery) + geom_line(aes(x = Date, y = Pick1))
Pick2 <- ggplot(CFL_lottery) + geom_line(aes(x = Date, y = Pick2))
Pick3 <- ggplot(CFL_lottery) + geom_line(aes(x = Date, y = Pick3))
Pick4 <- ggplot(CFL_lottery) + geom_line(aes(x = Date, y = Pick4))
Pick5 <- ggplot(CFL_lottery) + geom_line(aes(x = Date, y = Pick5))
CB <- ggplot(CFL_lottery) + geom_line(aes(x = Date, y = CB))
grid.arrange(Pick1, Pick2, Pick3, Pick4, Pick5, CB, ncol = 1)


#We are just looking at 2021 plays for Pick1 and Pick2

#Making a column for year
CFL_lottery$Year <- format(as.Date(CFL_lottery$Date, format="%d/%m/%Y"),"%Y")
CFL_lottery2021<- filter(CFL_lottery, Year == 2021)

#Looking at a graph of how 2021, pick1 and pick2 pattern
Pick1_2021 <- ggplot(CFL_lottery2021) + geom_line(aes(x = Date, y = Pick1))
Pick2_2021 <- ggplot(CFL_lottery2021) + geom_line(aes(x = Date, y = Pick2))
grid.arrange(Pick1_2021, Pick2_2021, ncol = 1)
#It is still hard to see so I will break it down a bit more


#Here we are looking at just Saturdays for pick1 and pick2
yr2021_Saturday<- filter(CFL_lottery2021, Weekday == "Saturday")

Pick1_2021_Saturday <- ggplot(yr2021_Saturday) + geom_line(aes(x = Date , y = Pick1))
Pick2_2021_Saturday <- ggplot(yr2021_Saturday) + geom_line(aes(x = Date, y = Pick2))
grid.arrange(Pick1_2021_Saturday, Pick2_2021_Saturday, ncol = 1)

#Now looking the other picks 3, 4 and 5 next to each other

Pick3_2021_Saturday <- ggplot(yr2021_Saturday) + geom_line(aes(x = Date , y = Pick3))
Pick4_2021_Saturday <- ggplot(yr2021_Saturday) + geom_line(aes(x = Date, y = Pick4))
Pick5_2021_Saturday <- ggplot(yr2021_Saturday) + geom_line(aes(x = Date, y = Pick5))
grid.arrange(Pick3_2021_Saturday, Pick4_2021_Saturday,Pick5_2021_Saturday, ncol = 1)

#I see a pattern but to get a better sense I will overlay, pick1, pick2, pick3.
#overlay Pick 1, pick 2, pick 3 line plot 
ggplot(yr2021_Saturday, aes(Date)) + 
geom_line(aes(y = Pick1, col="blue")) +
geom_line(aes(y = Pick2, col="green")) +
geom_line(aes(y = Pick3, col="red")) +
  scale_color_manual(name = c("Number Picks"),
                     labels = c("Pick 1", "Pick 2", "Pick 3"),
                     values = c("red", "blue", "green")) +
  labs(title = "Pattern in plays", x = "Saturdays in 2021", y = "Pick 1, 2, 3")


#Adding more details to it
ggplot(yr2021_Saturday, aes(Date)) + 
  geom_line(aes(y = Pick1, col="blue")) +
  geom_line(aes(y = Pick2, col="green")) +
  geom_line(aes(y = Pick3, col="red")) +
  scale_linetype_manual(values=c("twodash", "dotted", "solid"))+
  scale_color_manual(name = c("Number Picks"),
                     labels = c("Pick 1", "Pick 2", "Pick 3"),
                     values = c("red", "green", "blue")) +
  scale_size_manual(values=c(1,1, 1.5))+
  labs(title = "Pattern in plays", x = "Saturdays in 2021", y = "Pick 1, 2, 3")+
  theme(legend.position="top")











#NOT SURE IF I WILL NEED BELOW

#Not sure if I will need this. Arranging the weekdays together
#Weekday<- arrange(CFL_lottery, Weekday)


#I was not able to extract much useful data from this
#CFL_lottery2<- select(CFL_lottery, Pick1, Pick2, Pick3, Pick4, Pick5, CB)

#heatmap1 <- as.matrix(CFL_lottery2)
#heatmap(heatmap1)


#Side by side graph
#ggplot(data=CFL_lottery) +
  #geom_bar(mapping = aes(x = Weekday, fill= Winners), position = "dodge")

#Grouping the weekdays as a test
#Weekday<- arrange(CFL_lottery, Weekday)

#Looking at Saturday and Sunday
#Weekend<- filter(CFL_lottery, Weekday %in% c("Saturday", "Sunday"))

# I learned nothing about the weekend dataset by doing this
#weekendgraph <- ggplot(Weekend, aes(x = CB, y = Weekday))
#weekendgraph + geom_point()

#weekendgraph1 <- ggplot(Weekend, aes(x = Pick1, y = Weekday))
#weekendgraph + geom_point()




Weekend1<- select()

