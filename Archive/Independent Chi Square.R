#Evaluation question 1 and 2 may not be what I thought I can answer, using some of these
#test but what I hope the Chi square will show me is, on what days what group of numbers might play. 


#Looking at Independent Chi Square 

install.packages("gmodels")
library("gmodels")
library("readxl")
install.packages("dplyr")
library(dplyr)
library(ggplot2)
install.packages("gridExtra")
library("gridExtra")

#Import Data
#For reference see below
#CFL_lottery = read_excel('CFL_lottery')



#Data wrangling 


#Made a new data set
CFL_lotteryCount1<-CFL_lottery%>%select(Weekday, Date, Pick1, Pick2, Pick3, Pick4, Pick5,CB, Winners)


#Make a column based on frequency for all picks and renamed it.
#Counts a pick1 by frequency
count1<-CFL_lotteryCount1%>%group_by(Pick1)%>%mutate(count=n())%>%
  arrange(desc(count))
  names(count1)[names(count1)=="count"] <- "Pick1count" #renaming the new count column

#Pick 2 count and renaming the column  
count2<-count1%>%group_by(Pick2)%>%mutate(count=n())%>%
  arrange(desc(count))
  names(count2)[names(count2)=="count"] <- "Pick2count" #renaming the new count column

#Pick 3 count and renaming the column  
count3<-count2%>%group_by(Pick3)%>%mutate(count=n())%>%
  arrange(desc(count))
  names(count3)[names(count3)=="count"] <- "Pick3count" #renaming the new count column

#Pick 4 count and renaming the column
count4<-count3%>%group_by(Pick4)%>%mutate(count=n())%>%
  arrange(desc(count))
  names(count4)[names(count4)=="count"] <- "Pick4count" #renaming the new count column

#Pick 5 count and renaming the column
count5<-count4%>%group_by(Pick5)%>%mutate(count=n())%>%
  arrange(desc(count))
  names(count5)[names(count5)=="count"] <- "Pick5count" #renaming the new count column

#CB count and renaming the column
count6<-count5%>%group_by(CB)%>%mutate(count=n())%>%
  arrange(desc(count))
  names(count6)[names(count6)=="count"] <- "CBcount" #renaming the new count column



  

#Made the column that ranks all picks based on frequency.
ranking1 <- count6 %>% 
  mutate(Pick1_rank = case_when(Pick1count >= 100 ~ "1", 
                           Pick1count >= 70 & Pick1count <= 99 ~ "2", 
                           Pick1count >= 40 & Pick1count <= 69 ~ "3", 
                           Pick1count >= 20 & Pick1count <= 39 ~ "4",
                           Pick1count <=19 ~ "5"))


ranking2 <-  ranking1%>% 
  mutate(Pick2_rank = case_when(Pick2count >= 100 ~ "1", 
                                Pick2count >= 70 & Pick2count <= 99 ~ "2", 
                                Pick2count >= 40 & Pick2count <= 69 ~ "3", 
                                Pick2count >= 20 & Pick2count <= 39 ~ "4",
                                Pick2count <=19 ~ "5"))

ranking3 <- ranking2 %>% 
  mutate(Pick3_rank = case_when(Pick3count >= 100 ~ "1", 
                                Pick3count >= 70 & Pick3count <= 99 ~ "2", 
                                Pick3count >= 40 & Pick3count <= 69 ~ "3", 
                                Pick3count >= 20 & Pick3count <= 39 ~ "4",
                                Pick3count <=19 ~ "5"))

ranking4 <- ranking3 %>% 
  mutate(Pick4_rank = case_when(Pick4count >= 100 ~ "1", 
                                Pick4count >= 70 & Pick4count <= 99 ~ "2", 
                                Pick4count >= 40 & Pick4count <= 69 ~ "3", 
                                Pick4count >= 20 & Pick4count <= 39 ~ "4",
                                Pick4count <=19 ~ "5"))

ranking5 <- ranking4 %>% 
  mutate(Pick5_rank = case_when(Pick5count >= 100 ~ "1", 
                                Pick5count >= 70 & Pick5count <= 99 ~ "2", 
                                Pick5count >= 40 & Pick5count <= 69 ~ "3", 
                                Pick5count >= 20 & Pick5count <= 39 ~ "4",
                                Pick5count <=19 ~ "5"))

ranking6 <- ranking5 %>% 
  mutate(CB_rank = case_when(CBcount >= 100 ~ "1", 
                                CBcount >= 70 & CBcount <= 99 ~ "2", 
                                CBcount >= 40 & CBcount <= 69 ~ "3", 
                                CBcount >= 20 & CBcount <= 39 ~ "4",
                                CBcount <=19 ~ "5"))




#Move the columns near the original picked numbers
move1<- ranking6 %>% relocate(Pick1count, .after=Pick1)

move2<-move1 %>% relocate(Pick1_rank, .after=Pick1count)


move3<- move2 %>% relocate(Pick2count, .after=Pick2) 

move4<-move3 %>% relocate(Pick2_rank, .after=Pick2count)


move5<- move4 %>% relocate(Pick3count, .after=Pick3) 

move6<-move5 %>% relocate(Pick3_rank, .after=Pick3count)


move7<- move6 %>% relocate(Pick4count, .after=Pick4) 

move8<-move7 %>% relocate(Pick4_rank, .after=Pick4count)


move9<- move8 %>% relocate(Pick5count, .after=Pick5) 

move10<-move9 %>% relocate(Pick5_rank, .after=Pick5count)


move11<- move10 %>% relocate(CBcount, .after=CB) 

CFL_lottery2<-move11 %>% relocate(CB_rank, .after=CB)




#Independent Chi Square for all ranked numbers
CrossTable(CFL_lottery2$Weekday, CFL_lottery2$Pick1_rank, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")




CrossTable(CFL_lottery2$Weekday, CFL_lottery2$Pick2_rank, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")




CrossTable(CFL_lottery2$Weekday, CFL_lottery2$Pick3_rank, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")




CrossTable(CFL_lottery2$Weekday, CFL_lottery2$Pick4_rank, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")





CrossTable(CFL_lottery2$Weekday, CFL_lottery2$Pick5_rank, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")





CrossTable(CFL_lottery2$Weekday, CFL_lottery2$CB_rank, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")







##MAY NOT USE

#Dropped the column
#subset(CFL_lotteryCount2, select = -c(Pick1_Rank))

#Part 1
#Make a new column with count for each number 
#CFL_lotteryCount<-CFL_lottery%>%group_by(Pick1)%>%mutate(count=n())

#names(CFL_lotteryCount)[names(CFL_lotteryCount)=="count"] <- "Pick1count"

#Part 2
#Ranking the numbers by frequancy

#CFL_lotteryCount1 <- CFL_lottery %>% mutate(
  #"Pick1_rank" = dense_rank(desc(Pick1)),
  #"Pick2_rank" = dense_rank(desc(Pick2)))


#Part 3
#This makes a rank for each number
#CFL_lotteryCount1 %>% slice_min(Pick1_rank, n = 10) %>% as.data.frame()

#CFL_lotteryCount1 %>% slice_min(Pick2_rank, n = 10) %>% as.data.frame()


#CFL_lotteryCount$total_rank <- CFL_lotteryCount %>% data.table::frank(-Pick1, -Pick2, -Pick3, -Pick4,-Pick5, ties.method = "dense")
#CFL_lotteryCount%>% slice_min(total_rank, n = 10) %>% as.data.frame()


#CFL_lotteryCount$Pick1Ranking <- NA
#CFL_lotteryCount$Pick1Ranking[CFL_lotteryCount$Pick1count => 100] <- 1
#CFL_lotteryCount$Pick1Ranking[CFL_lotteryCount$Pick1count => 70] <- 2
#CFL_lotteryCount$Pick1Ranking[CFL_lotteryCount$Pick1count <= 69] <- 3


#CFL_lottery2021<- filter(CFL_lottery, Year == 2021)

#CrossTable(CFL_lottery$Weekday, CFL_lottery$Pick1, fisher=TRUE, chisq = TRUE,
           #expected = TRUE, sresid=TRUE, format="SPSS")
