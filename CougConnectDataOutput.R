#CougConnect Data Output
#This is the continunation of CougConnect Data Cleaning
#However, this is a little different, in that it has a different excel file from what we saved from DataCleaning
#This is due to manually manuipulation of data
#Several different variables were added
#For the 2021 data, we added three variables
#1-impyn - Yes/No did I have to go in and manually add people due to multiple people being mentioned (aka children, parents, etc)
#2-imptotal - Number of people/contacts I had to impute
#3-impstart - What contact variable did I start the imputing for (ex contact5 or contact7)
#As seen in the variables, for 2021, I did go in and smooth out any multiples, which was easy enough with the low number of people others had contact with
#We also, if just children/friends/students were metnioned, assumed 2
#Better an underestimation then overestimation
#We also cut off any contacts past 22, as there were no inputs
##FOR 2020
#For all those who just put plural of something (Students/collegues/parents/laborers) we assumed it was 2
#For those who put a class of students w/o telling details, we assumed 20 students, which is the average size of a class at WSU
#For those that put more crowd size (people on bus/airplane, strangers on my 30 minute walk, child's soccer team) we tried to keep 
#a fair estimate for that (Bus - 10, strangers -5, soccer team -14) 
#After each contact variable (contactX_v2) we added another variable next to it called contactX_v2add
#This is the number of extra people if contact is plural (ex contactX_v2 = "Students", then contaxtX_v2add =1)
#Once number of contacts were input, variables were changed to their bin (Strangers, Laborers, etc), keeping the plural form
#If the contact was a single input (spouse, child, colleague,etc) their contactX_v2add =0
#this is to differentiate from those that had NA as a contact-contact add was also put at NA
#this will be easier to get totals 
#I also deleted all columns after contact15, as there were no inputs past that

setwd("~/Lofgren/COVID-19/CougConnect")
library(readr)
CC<- read_csv("CougConnect_May192021manual.csv")
View(CC)

library(dplyr)
library(tidyr)
library(tidyverse)
library(gmodels)
library(tidyREDCap)
library(expss)
library(ggplot2)
library(naniar)

#Trying to add a total variable for year 2021 (aka all the contacts they had for contact1/2/3/etc)
CC$Total2021 <- ifelse (is.na(CC$contact1), 0, ifelse (is.na(CC$contact2), 1, ifelse (is.na(CC$contact3),2, ifelse (is.na(CC$contact4),3, ifelse (is.na(CC$contact5),4,
                 ifelse(is.na(CC$contact6),5, ifelse (is.na(CC$contact7),6, ifelse (is.na(CC$contact8),7, ifelse (is.na(CC$contact9),8, ifelse (is.na(CC$contact10),9,
                   ifelse (is.na(CC$contact11),10, ifelse (is.na(CC$contact12), 11, ifelse (is.na(CC$contact13),12, ifelse(is.na(CC$contact14),13, ifelse(is.na(CC$contact15),14,
                    ifelse(is.na(CC$contact16),15, ifelse (is.na(CC$contact17),16, ifelse(is.na(CC$contact18),17, ifelse(is.na(CC$contact19),18, ifelse (is.na(CC$contact20),19,
                     ifelse(is.na(CC$contact21),20,ifelse(is.na(CC$contact22),21, 22))))))))))))))))))))))

table(CC$Total2021, useNA = "always")
CrossTable(CC$Total2021)

#First I'm plotting my Total21 variable
#Two different plots for the same thing
#More of a exercise of graphing in R for me
#ggplot way
ggplot(CC, aes(x=Total2021)) + geom_histogram(binwidth=1)
ggplot(CC, aes(x=Total2021)) + geom_histogram(binwidth=1, fill="purple") + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title = "Total Number of Contacts per Respondent on a Day in Jan-March 2021", x="Number of Contacts")

#Hist way
hist(CC$Total2021)
hist(CC$Total2021, right=F, breaks=23, main="Total Number of Contacts per Respondent on a Day in Jan-March 2021", xlab="Number of Contacts", xlim=c(0,25),ylab="Frequency", col = "violet", labels = TRUE)

#Basic Numbers
mean(CC$Total2021, na.rm=TRUE)
#mean is 3.21 people/person
median(CC$Total2021, na.rm = TRUE)
#median is 3
sd(CC$Total2021, na.rm = TRUE)
#sd of 2.67
summary(CC$Total2021, na.rm = TRUE)
# Min. 1st Qu.  Median    Mean   3rd Qu.   Max. 
#0.000   1.000   3.000   3.218   4.000  22.000 
#range 0-22
#1qr=1, 3QR-4
sum(CC$Total2021)

#Now doing some things for all the different contacts
#Trying some things to get a better idea of how many students/etc from the data
#Subsetting contacts
df1<-select(CC, contact1, contact2, contact3,contact4,contact5,contact6, contact7,contact8,contact9,contact10,contact11,contact12,contact13,contact14,contact15,contact16,contact17,contact18,contact19,contact20,contact21,contact22)
DF = data.frame(table(unlist(df1)))
DF$percent = prop.table(DF$Freq)*100
DF
sum(DF$Freq)
margin.table(DF,1)
hist(DF$Var1,DF$Freq)
a <-ggplot(DF) + geom_point(mapping = aes(x=Var1, y=Freq))
a + theme(axis.text.x = element_text(angle = 90)) +  labs(title = "Description and Frequency of Contacts from all Respondents", x="Type of Contact", y="Frequency")


#now doing this with the location that people put in, will do my catchement one next
#Again, this is for 2021 DATA
df2<-select(CC, locationcontact1, locationcontact2, locationcontact3,locationcontact4,locationcontact5,locationcontact6, locationcontact7,locationcontact8,locationcontact9,locationcontact10,locationcontact11,locationcontact12,locationcontact13,locationcontact14,locationcontact15,locationcontact16,locationcontact17,locationcontact18,locationcontact19,locationcontact20,locationcontact21,locationcontact22)
DF2 = data.frame(table(unlist(df2)))
DF2$percent = prop.table(DF2$Freq)*100
DF2
sum(DF2$Freq)
#858
#                                 Var1 Freq    percent
#1                        Bremerton WA    1  0.1165501
#2                            Camas WA    3  0.3496503
#3                                 Car    8  0.9324009
#4                           Colfax WA    1  0.1165501
#5                          Drive Thru    2  0.2331002
#6                                 Gym   13  1.5151515
#7                Health Care Facility   21  2.4475524
#8                                Home  359 41.8414918
#9                        Home and Car   12  1.3986014
#10               home and coffee shop    1  0.1165501
#11                     Home and Store    1  0.1165501
#12                Home and WSU Campus    3  0.3496503
#13         home, car, friend's garage    2  0.2331002
#14 Home, grocery store, car, outdoors    1  0.1165501
#15                              Idaho    1  0.1165501
#16                   Medical Facility    1  0.1165501
#17                          Moscow ID   28  3.2634033
#18                             Office    2  0.2331002
#19                             Oregon    3  0.3496503
#20                              Other   12  1.3986014
#21                           Outdoors   40  4.6620047
#22                       Pet Facility    4  0.4662005
#23                           Pharmacy    1  0.1165501
#24                            Pullman   18  2.0979021
#25                     Pullman-Colfax    1  0.1165501
#26                         Pullman WA   13  1.5151515
#27                         Restaurant   29  3.3799534
#28                        Richland WA    8  0.9324009
#29                              Salon    2  0.2331002
#30                             School    9  1.0489510
#31                            Seattle    4  0.4662005
#32                                SEH   20  2.3310023
#33                    Spokane Area WA    6  0.6993007
#34                              Store   61  7.1095571
#35                       Vancouver WA    2  0.2331002
#36                          Wenatchee    1  0.1165501
#37                         WSU Campus   74  8.6247086
#38                        WSU Pullman   87 10.1398601
#39   WSU Pullman/Health Care Facility    3  0.3496503


#Plotting this
b <-ggplot(DF2) + geom_point(size=4, color = "blue", mapping = aes(x=reorder(Var1, -Freq), y=Freq)) + xlim("Home" , "WSU Pullman", "WSU Campus", "Store", "Outdoors", "Restaurant", "Health Care Facility", "Pullman")
b + theme(axis.text.x = element_text(angle = 90))+ labs(title = "Description and Frequency of the Top 8 Locations Respondents Wrote Down in 2021", x="Type of Location", y="Frequency") + theme(axis.text.x = element_text(face="bold", size = 8))


#Going to location category
#1=My home 2=Work 3=Leisure 4=Travel 5=Shopping 6=Other

CC$Contact1Location <- ifelse(CC$contextcontact1___0 ==1, 1,
                              ifelse(CC$contextcontact1___1 == 1 , 2,
                                     ifelse(CC$contextcontact1___2 == 1, 3,
                                            ifelse(CC$contextcontact1___3 == 1, 4,
                                                   ifelse(CC$contextcontact1___4 == 1, 5,
                                                          ifelse(CC$contextcontact1___5 == 1, 6, NA))))))
CC$Contact2Location <- ifelse(CC$contextcontact2___0 ==1, 1,
                              ifelse(CC$contextcontact2___1 == 1 , 2,
                                     ifelse(CC$contextcontact2___2 == 1, 3,
                                            ifelse(CC$contextcontact2___3 == 1, 4,
                                                   ifelse(CC$contextcontact2___4 == 1, 5,
                                                          ifelse(CC$contextcontact2___5 == 1, 6, NA))))))
CC$Contact3Location <- ifelse(CC$contextcontact3___0 ==1, 1,
                              ifelse(CC$contextcontact3___1 == 1 , 2,
                                     ifelse(CC$contextcontact3___2 == 1, 3,
                                            ifelse(CC$contextcontact3___3 == 1, 4,
                                                   ifelse(CC$contextcontact3___4 == 1, 5,
                                                          ifelse(CC$contextcontact3___5 == 1,6, NA))))))
CC$Contact4Location <- ifelse(CC$contextcontact4___0 ==1, 1,
                              ifelse(CC$contextcontact4___1 == 1 , 2,
                                     ifelse(CC$contextcontact4___2 == 1, 3,
                                            ifelse(CC$contextcontact4___3 == 1, 4,
                                                   ifelse(CC$contextcontact4___4 == 1, 5,
                                                          ifelse(CC$contextcontact4___5 == 1,6, NA))))))
CC$Contact5Location <- ifelse(CC$contextcontact5___0 ==1, 1,
                              ifelse(CC$contextcontact5___1 == 1 , 2,
                                     ifelse(CC$contextcontact5___2 == 1, 3,
                                            ifelse(CC$contextcontact5___3 == 1, 4,
                                                   ifelse(CC$contextcontact5___4 == 1, 5,
                                                          ifelse(CC$contextcontact5___5 == 1,6, NA))))))
CC$Contact6Location <- ifelse(CC$contextcontact6___0 ==1, 1,
                              ifelse(CC$contextcontact6___1 == 1 , 2,
                                     ifelse(CC$contextcontact6___2 == 1, 3,
                                            ifelse(CC$contextcontact6___3 == 1, 4,
                                                   ifelse(CC$contextcontact6___4 == 1, 5,
                                                          ifelse(CC$contextcontact6___5 == 1,6, NA))))))
CC$Contact7Location <- ifelse(CC$contextcontact7___0 ==1, 1,
                              ifelse(CC$contextcontact7___1 == 1 , 2,
                                     ifelse(CC$contextcontact7___2 == 1, 3,
                                            ifelse(CC$contextcontact7___3 == 1, 4,
                                                   ifelse(CC$contextcontact7___4 == 1, 5,
                                                          ifelse(CC$contextcontact7___5 == 1,6, NA))))))
CC$Contact8Location <- ifelse(CC$contextcontact8___0 ==1, 1,
                              ifelse(CC$contextcontact8___1 == 1 , 2,
                                     ifelse(CC$contextcontact8___2 == 1, 3,
                                            ifelse(CC$contextcontact8___3 == 1, 4,
                                                   ifelse(CC$contextcontact8___4 == 1, 5,
                                                          ifelse(CC$contextcontact8___5 == 1,6, NA))))))
CC$Contact9Location <- ifelse(CC$contextcontact9___0 ==1, 1,
                              ifelse(CC$contextcontact9___1 == 1 , 2,
                                     ifelse(CC$contextcontact9___2 == 1, 3,
                                            ifelse(CC$contextcontact9___3 == 1, 4,
                                                   ifelse(CC$contextcontact9___4 == 1, 5,
                                                          ifelse(CC$contextcontact9___5 == 1,6, NA))))))
CC$Contact10Location <- ifelse(CC$contextcontact10___0 ==1, 1,
                               ifelse(CC$contextcontact10___1 == 1 , 2,
                                      ifelse(CC$contextcontact10___2 == 1, 3,
                                             ifelse(CC$contextcontact10___3 == 1, 4,
                                                    ifelse(CC$contextcontact10___4 == 1, 5,
                                                           ifelse(CC$contextcontact10___5 == 1,6, NA))))))
CC$Contact11Location <- ifelse(CC$contextcontact11___0 ==1, 1,
                               ifelse(CC$contextcontact11___1 == 1 , 2,
                                      ifelse(CC$contextcontact11___2 == 1, 3,
                                             ifelse(CC$contextcontact11___3 == 1, 4,
                                                    ifelse(CC$contextcontact11___4 == 1, 5,
                                                           ifelse(CC$contextcontact11___5 == 1,6, NA))))))
CC$Contact12Location <- ifelse(CC$contextcontact12___0 ==1, 1,
                               ifelse(CC$contextcontact12___1 == 1 , 2,
                                      ifelse(CC$contextcontact12___2 == 1, 3,
                                             ifelse(CC$contextcontact12___3 == 1, 4,
                                                    ifelse(CC$contextcontact12___4 == 1, 5,
                                                           ifelse(CC$contextcontact12___5 == 1,6, NA))))))
CC$Contact13Location <- ifelse(CC$contextcontact13___0 ==1, 1,
                               ifelse(CC$contextcontact13___1 == 1 , 2,
                                      ifelse(CC$contextcontact13___2 == 1, 3,
                                             ifelse(CC$contextcontact13___3 == 1, 4,
                                                    ifelse(CC$contextcontact13___4 == 1, 5,
                                                           ifelse(CC$contextcontact13___5 == 1,6, NA))))))
CC$Contact14Location <- ifelse(CC$contextcontact14___0 ==1, 1,
                               ifelse(CC$contextcontact14___1 == 1 , 2,
                                      ifelse(CC$contextcontact14___2 == 1, 3,
                                             ifelse(CC$contextcontact14___3 == 1, 4,
                                                    ifelse(CC$contextcontact14___4 == 1, 5,
                                                           ifelse(CC$contextcontact14___5 == 1,6, NA))))))
CC$Contact15Location <- ifelse(CC$contextcontact15___0 ==1, 1,
                               ifelse(CC$contextcontact15___1 == 1 , 2,
                                      ifelse(CC$contextcontact15___2 == 1, 3,
                                             ifelse(CC$contextcontact15___3 == 1, 4,
                                                    ifelse(CC$contextcontact15___4 == 1, 5,
                                                           ifelse(CC$contextcontact15___5 == 1,6, NA))))))
CC$Contact16Location <- ifelse(CC$contextcontact16___0 ==1, 1,
                               ifelse(CC$contextcontact16___1 == 1 , 2,
                                      ifelse(CC$contextcontact16___2 == 1, 3,
                                             ifelse(CC$contextcontact16___3 == 1, 4,
                                                    ifelse(CC$contextcontact16___4 == 1, 5,
                                                           ifelse(CC$contextcontact16___5 == 1,6, NA))))))
CC$Contact17Location <- ifelse(CC$contextcontact17___0 ==1, 1,
                               ifelse(CC$contextcontact17___1 == 1 , 2,
                                      ifelse(CC$contextcontact17___2 == 1, 3,
                                             ifelse(CC$contextcontact17___3 == 1, 4,
                                                    ifelse(CC$contextcontact17___4 == 1, 5,
                                                           ifelse(CC$contextcontact17___5 == 1,6, NA))))))
CC$Contact18Location <- ifelse(CC$contextcontact18___0 ==1, 1,
                               ifelse(CC$contextcontact18___1 == 1 , 2,
                                      ifelse(CC$contextcontact18___2 == 1, 3,
                                             ifelse(CC$contextcontact18___3 == 1, 4,
                                                    ifelse(CC$contextcontact18___4 == 1, 5,
                                                           ifelse(CC$contextcontact18___5 == 1,6, NA))))))
CC$Contact19Location <- ifelse(CC$contextcontact19___0 ==1, 1,
                               ifelse(CC$contextcontact19___1 == 1 , 2,
                                      ifelse(CC$contextcontact19___2 == 1, 3,
                                             ifelse(CC$contextcontact19___3 == 1, 4,
                                                    ifelse(CC$contextcontact19___4 == 1, 5,
                                                           ifelse(CC$contextcontact19___5 == 1,6, NA))))))
CC$Contact20Location <- ifelse(CC$contextcontact20___0 ==1, 1,
                               ifelse(CC$contextcontact20___1 == 1 , 2,
                                      ifelse(CC$contextcontact20___2 == 1, 3,
                                             ifelse(CC$contextcontact20___3 == 1, 4,
                                                    ifelse(CC$contextcontact20___4 == 1, 5,
                                                           ifelse(CC$contextcontact20___5 == 1,6, NA))))))
CC$Contact21Location <- ifelse(CC$contextcontact21___0 ==1, 1,
                               ifelse(CC$contextcontact21___1 == 1 , 2,
                                      ifelse(CC$contextcontact21___2 == 1, 3,
                                             ifelse(CC$contextcontact21___3 == 1, 4,
                                                    ifelse(CC$contextcontact21___4 == 1, 5,
                                                           ifelse(CC$contextcontact21___5 == 1,6, NA))))))
CC$Contact22Location <- ifelse(CC$contextcontact22___0 ==1, 1,
                               ifelse(CC$contextcontact22___1 == 1 , 2,
                                      ifelse(CC$contextcontact22___2 == 1, 3,
                                             ifelse(CC$contextcontact22___3 == 1, 4,
                                                    ifelse(CC$contextcontact22___4 == 1, 5,
                                                           ifelse(CC$contextcontact22___5 == 1,6, NA))))))
table(CC$Contact20Location, useNA = "always")


table(CC$locationcontact1,CC$Contact1Location, useNA = 'always')
#I have 15 people who put down nothing for location, but did mark for context contact
#Going to put at least the category (aka NA for 1 will get Home, NA for 2 will get WSU campus, etc)
CC$locationcontact1 <- ifelse(is.na(CC$locationcontact1) & CC$Contact1Location == "1", "Home" , CC$locationcontact1)
CC$locationcontact1 <- ifelse(is.na(CC$locationcontact1) & CC$Contact1Location == "2", "WSU Campus" , CC$locationcontact1)
CC$locationcontact1 <- ifelse(is.na(CC$locationcontact1) & CC$Contact1Location == "6", "Other" , CC$locationcontact1)

#Now with location people chose, not what they wrote down
df3<-select(CC, Contact1Location, Contact2Location, Contact3Location, Contact4Location, Contact5Location, Contact6Location, Contact7Location, Contact8Location, Contact9Location, Contact10Location,Contact11Location,Contact12Location, Contact13Location, Contact14Location, Contact15Location, Contact16Location, Contact17Location, Contact18Location, Contact19Location, Contact20Location, Contact21Location, Contact22Location)
DF3 = data.frame(table(unlist(df3)))
DF3$percent = prop.table(DF3$Freq)*100

DF3
sum(DF3$Freq)
c <-ggplot(DF3) + geom_point(size=4,color = "Black", mapping = aes(x=reorder(Var1,-Freq), y=Freq))
c + scale_x_discrete(labels =c('1'= 'My Home', '2' = "Work", '3' = "Leisure", '4' = "Travel",'5'= 'Shopping', '6' = "Other" )) + labs(title = "Description and Frequency of the Locations Respondents Chose in 2021", x="Location Choice", y="Frequency") + theme(axis.text.x = element_text(face="bold", size = 9))

#loooking at the two different variables, we have 167 people listed as "WORK" when put in a location, but with input your own it's 164. 
#not a bad comparison!


#Creating age categories that are 0-17, 18-25,26-64, 65+
#Only doing this for 2021 as 2020 is a little more...all over the place.
#a.k.a Multiple choices picked 
CC$Contact1AgeCat <- ifelse(CC$agecontact1___0 ==1, 1,
                            ifelse(CC$agecontact1___1 == 1 , 2,
                                   ifelse(CC$agecontact1___2 == 1, 3,
                                          ifelse(CC$agecontact1___3 == 1, 4, NA))))
CC$Contact2AgeCat <- ifelse(CC$agecontact2___0 ==1, 1,
                            ifelse(CC$agecontact2___1 == 1 , 2,
                                   ifelse(CC$agecontact2___2 == 1, 3,
                                          ifelse(CC$agecontact2___3 == 1, 4, NA))))
CC$Contact3AgeCat <- ifelse(CC$agecontact3___0 ==1, 1,
                            ifelse(CC$agecontact3___1 == 1 , 2,
                                   ifelse(CC$agecontact3___2 == 1, 3,
                                          ifelse(CC$agecontact3___3 == 1, 4, NA))))
CC$Contact4AgeCat <- ifelse(CC$agecontact4___0 ==1, 1,
                            ifelse(CC$agecontact4___1 == 1 , 2,
                                   ifelse(CC$agecontact4___2 == 1, 3,
                                          ifelse(CC$agecontact4___3 == 1, 4, NA))))
CC$Contact5AgeCat <- ifelse(CC$agecontact5___0 ==1, 1,
                            ifelse(CC$agecontact5___1 == 1 , 2,
                                   ifelse(CC$agecontact5___2 == 1, 3,
                                          ifelse(CC$agecontact5___3 == 1, 4, NA))))
CC$Contact6AgeCat <- ifelse(CC$agecontact6___0 ==1, 1,
                            ifelse(CC$agecontact6___1 == 1 , 2,
                                   ifelse(CC$agecontact6___2 == 1, 3,
                                          ifelse(CC$agecontact6___3 == 1, 4, NA))))
CC$Contact7AgeCat <- ifelse(CC$agecontact7___0 ==1, 1,
                            ifelse(CC$agecontact7___1 == 1 , 2,
                                   ifelse(CC$agecontact7___2 == 1, 3,
                                          ifelse(CC$agecontact7___3 == 1, 4, NA))))
CC$Contact8AgeCat <- ifelse(CC$agecontact8___0 ==1, 1,
                            ifelse(CC$agecontact8___1 == 1 , 2,
                                   ifelse(CC$agecontact8___2 == 1, 3,
                                          ifelse(CC$agecontact8___3 == 1, 4, NA))))
CC$Contact9AgeCat <- ifelse(CC$agecontact9___0 ==1, 1,
                            ifelse(CC$agecontact9___1 == 1 , 2,
                                   ifelse(CC$agecontact9___2 == 1, 3,
                                          ifelse(CC$agecontact9___3 == 1, 4, NA))))
CC$Contact10AgeCat <- ifelse(CC$agecontact10___0 ==1, 1,
                             ifelse(CC$agecontact10___1 == 1 , 2,
                                    ifelse(CC$agecontact10___2 == 1, 3,
                                           ifelse(CC$agecontact10___3 == 1, 4, NA))))
CC$Contact11AgeCat <- ifelse(CC$agecontact11___0 ==1, 1,
                             ifelse(CC$agecontact11___1 == 1 , 2,
                                    ifelse(CC$agecontact11___2 == 1, 3,
                                           ifelse(CC$agecontact11___3 == 1, 4, NA))))
CC$Contact12AgeCat <- ifelse(CC$agecontact12___0 ==1, 1,
                             ifelse(CC$agecontact12___1 == 1 , 2,
                                    ifelse(CC$agecontact12___2 == 1, 3,
                                           ifelse(CC$agecontact12___3 == 1, 4, NA))))
CC$Contact13AgeCat <- ifelse(CC$agecontact13___0 ==1, 1,
                             ifelse(CC$agecontact13___1 == 1 , 2,
                                    ifelse(CC$agecontact13___2 == 1, 3,
                                           ifelse(CC$agecontact13___3 == 1, 4, NA))))
CC$Contact14AgeCat <- ifelse(CC$agecontact14___0 ==1, 1,
                             ifelse(CC$agecontact14___1 == 1 , 2,
                                    ifelse(CC$agecontact14___2 == 1, 3,
                                           ifelse(CC$agecontact14___3 == 1, 4, NA))))
CC$Contact15AgeCat <- ifelse(CC$agecontact15___0 ==1, 1,
                             ifelse(CC$agecontact15___1 == 1 , 2,
                                    ifelse(CC$agecontact15___2 == 1, 3,
                                           ifelse(CC$agecontact15___3 == 1, 4, NA))))
CC$Contact16AgeCat <- ifelse(CC$agecontact16___0 ==1, 1,
                             ifelse(CC$agecontact16___1 == 1 , 2,
                                    ifelse(CC$agecontact16___2 == 1, 3,
                                           ifelse(CC$agecontact16___3 == 1, 4, NA))))
CC$Contact17AgeCat <- ifelse(CC$agecontact17___0 ==1, 1,
                             ifelse(CC$agecontact17___1 == 1 , 2,
                                    ifelse(CC$agecontact17___2 == 1, 3,
                                           ifelse(CC$agecontact17___3 == 1, 4, NA))))
CC$Contact18AgeCat <- ifelse(CC$agecontact18___0 ==1, 1,
                             ifelse(CC$agecontact18___1 == 1 , 2,
                                    ifelse(CC$agecontact18___2 == 1, 3,
                                           ifelse(CC$agecontact18___3 == 1, 4, NA))))
CC$Contact19AgeCat <- ifelse(CC$agecontact19___0 ==1, 1,
                             ifelse(CC$agecontact19___1 == 1 , 2,
                                    ifelse(CC$agecontact19___2 == 1, 3,
                                           ifelse(CC$agecontact19___3 == 1, 4, NA))))
CC$Contact20AgeCat <- ifelse(CC$agecontact20___0 ==1, 1,
                             ifelse(CC$agecontact20___1 == 1 , 2,
                                    ifelse(CC$agecontact20___2 == 1, 3,
                                           ifelse(CC$agecontact20___3 == 1, 4, NA))))
CC$Contact21AgeCat <- ifelse(CC$agecontact21___0 ==1, 1,
                             ifelse(CC$agecontact21___1 == 1 , 2,
                                    ifelse(CC$agecontact21___2 == 1, 3,
                                           ifelse(CC$agecontact21___3 == 1, 4, NA))))
CC$Contact22AgeCat <- ifelse(CC$agecontact22___0 ==1, 1,
                             ifelse(CC$agecontact22___1 == 1 , 2,
                                    ifelse(CC$agecontact22___2 == 1, 3,
                                           ifelse(CC$agecontact22___3 == 1, 4, NA))))

#Doing the same thing with age categories as we did with Location
df4<-select(CC, Contact1AgeCat, Contact2AgeCat, Contact3AgeCat, Contact4AgeCat, Contact5AgeCat, Contact6AgeCat, Contact7AgeCat, Contact8AgeCat, Contact9AgeCat, Contact10AgeCat,Contact11AgeCat,Contact12AgeCat, Contact13AgeCat, Contact14AgeCat, Contact15AgeCat, Contact16AgeCat, Contact17AgeCat, Contact18AgeCat, Contact19AgeCat, Contact20AgeCat, Contact21AgeCat, Contact22AgeCat)
DF4 = data.frame(table(unlist(df4)))
DF4$percent = prop.table(DF4$Freq)*100
DF4
sum(DF4$Freq)
d <-ggplot(DF4) + geom_point(mapping = aes(x=Var1, y=Freq))
d + scale_x_discrete(labels =c('1'= '0-17 Years', '2' = "18-25 Years", '3' = "26-64 Years", '4' = "65+ Years"))

#Figuring out how many people work/live in Pullman 
#On which WSU Campus do you work?
table(CC$campus_loc, useNA = 'always')
#76 NA 
# Pullman   Spokane     Vancouver    Tri-Cities    Everett     Extension    Seattle   <NA> 
#  140        18             20           8           2            6           1      76 
#Where do you live the majority of the time (if you don't live in Pullman)
table(CC$npullman)
#24 Responses
# Idaho -Moscow or just ID (9), Bremerton (1), Clarkston (1), Richland (2), Seattle (1), Spokane (4), Sumner (1), Vancouver (4), Yakima(1)

#Do you live in Pullman th majority of the time?
table(CC$pullmanyn, useNA = "always")
# Yes               No              NA
# 117 (28.9%)   143 (35.2%)    11 (37%)

#Table of Location you work and if you live in Pullman
table(CC$pullmanyn, CC$campus_loc, useNA = "always")
#0 up top is Pullman Campus,
#        0   1   2   3   4   5   6 <NA>
#0/N    48  18  20   8   2   6   1   40
#1/Y    92   0   0   0   0   0   0   25
#<NA>    0   0   0   0   0   0   0   11

#You have 48 people who work @ WSU Pullman who do not live there. And you have 25 people who live there w/o a campus location (assume Pullman)
#So you have the 117 who live in Pullman plus the 48 others who do not live , but do work there - 135/265 with at least one variable

#What percentage of people are considered/considered essential?
table(CC$essentialjobyn, useNA = 'always')
#200 No, 61 yes, 145 NA
#looking to see if there's anything between being essential and # of contacts
table(CC$Total2021, CC$essentialjobyn, useNA = "always")

#Creating total 2020 variables
#For each contact it should be a contactX20Total
#Adding each contact + the add variable
#Doing this because there is the multiple category
CC$Cont1Total20 <- ifelse (is.na(CC$contact1_v2),0, 1)
CC$Cont1Total20 <- CC$Cont1Total20 + CC$contact1_v2add
table(CC$Cont1Total20, useNA = 'always')
CC$Cont2Total20 <- ifelse (is.na(CC$contact2_v2), 0, 1)
CC$Cont2Total20 <- CC$Cont2Total20 + CC$contact2_v2add
table(CC$Cont2Total20, useNA = 'always')
CC$Cont3Total20 <- ifelse (is.na(CC$contact3_v2), 0, 1)
CC$Cont3Total20 <- CC$Cont3Total20 + CC$contact3_v2add
table(CC$Cont3Total20, useNA = 'always')
CC$Cont4Total20 <- ifelse (is.na(CC$contact4_v2), 0, 1)
CC$Cont4Total20 <- CC$Cont4Total20 + CC$contact4_v2add
table(CC$Cont4Total20, useNA = 'always')
CC$Cont5Total20 <- ifelse (is.na(CC$contact5_v2), 0, 1)
CC$Cont5Total20 <- CC$Cont5Total20 + CC$contact5_v2add
table(CC$Cont5Total20, useNA = 'always')
CC$Cont6Total20 <- ifelse (is.na(CC$contact6_v2), 0, 1)
CC$Cont6Total20 <- CC$Cont6Total20 + CC$contact6_v2add
table(CC$Cont6Total20, useNA = 'always')
CC$Cont7Total20 <- ifelse (is.na(CC$contact7_v2), 0, 1)
CC$Cont7Total20 <- CC$Cont7Total20 + CC$contact7_v2add
table(CC$Cont7Total20, useNA = 'always')
CC$Cont8Total20 <- ifelse (is.na(CC$contact8_v2), 0, 1)
CC$Cont8Total20 <- CC$Cont8Total20 + CC$contact8_v2add
table(CC$Cont8Total20, useNA = 'always')
CC$Cont9Total20 <- ifelse (is.na(CC$contact9_v2), 0, 1)
CC$Cont9Total20 <- CC$Cont9Total20 + CC$contact9_v2add
table(CC$Cont9Total20, useNA = 'always')
CC$Cont10Total20 <- ifelse (is.na(CC$contact10_v2), 0, 1)
CC$Cont10Total20 <- CC$Cont10Total20 + CC$contact10_v2add
table(CC$Cont10Total20, useNA = 'always')
CC$Cont11Total20 <- ifelse (is.na(CC$contact11_v2), 0, 1)
CC$Cont11Total20 <- CC$Cont11Total20 + CC$contact11_v2add
table(CC$Cont11Total20, useNA = 'always')
CC$Cont12Total20 <- ifelse (is.na(CC$contact12_v2), 0, 1)
CC$Cont12Total20 <- CC$Cont12Total20 + CC$contact12_v2add
table(CC$Cont12Total20, useNA = 'always')
CC$Cont13Total20 <- ifelse (is.na(CC$contact13_v2), 0, 1)
CC$Cont13Total20 <- CC$Cont13Total20 + CC$contact13_v2add
table(CC$Cont13Total20, useNA = 'always')
CC$Cont14Total20 <- ifelse (is.na(CC$contact14_v2), 0, 1)
CC$Cont14Total20 <- CC$Cont14Total20 + CC$contact14_v2add
table(CC$Cont14Total20, useNA = 'always')
CC$Cont15Total20 <- ifelse (is.na(CC$contact15_v2), 0, 1)
CC$Cont15Total20 <- CC$Cont15Total20 + CC$contact15_v2add
table(CC$Cont15Total20, useNA = 'always')

#Going to location category
#1=My home 2=Work 3=Leisure 4=Travel 5=Shopping 6=Other

CC$Contact1location2020 <- ifelse(CC$contextcontact1_v2___0 == 1, 1,
                                  ifelse(CC$contextcontact1_v2___1 == 1 , 2,
                                         ifelse(CC$contextcontact1_v2___2 == 1, 3,
                                                ifelse(CC$contextcontact1_v2___3 == 1, 4,
                                                       ifelse(CC$contextcontact1_v2___4 == 1, 5,
                                                              ifelse(CC$contextcontact1_v2___5 == 1, 6, NA))))))
CC$Contact2location20 <- ifelse(CC$contextcontact2_v2___0 ==1, 1,
                                ifelse(CC$contextcontact2_v2___1 == 1 , 2,
                                       ifelse(CC$contextcontact2_v2___2 == 1, 3,
                                              ifelse(CC$contextcontact2_v2___3 == 1, 4,
                                                     ifelse(CC$contextcontact2_v2___4 == 1, 5,
                                                            ifelse(CC$contextcontact2_v2___5 == 1, 6, NA))))))
CC$Contact3location20 <- ifelse(CC$contextcontact3_v2___0 ==1, 1,
                                ifelse(CC$contextcontact3_v2___1 == 1 , 2,
                                       ifelse(CC$contextcontact3_v2___2 == 1, 3,
                                              ifelse(CC$contextcontact3_v2___3 == 1, 4,
                                                     ifelse(CC$contextcontact3_v2___4 == 1, 5,
                                                            ifelse(CC$contextcontact3_v2___5 == 1,6, NA))))))
CC$Contact4location20 <- ifelse(CC$contextcontact4___0 ==1, 1,
                                ifelse(CC$contextcontact4_v2___1 == 1 , 2,
                                       ifelse(CC$contextcontact4_v2___2 == 1, 3,
                                              ifelse(CC$contextcontact4_v2___3 == 1, 4,
                                                     ifelse(CC$contextcontact4_v2___4 == 1, 5,
                                                            ifelse(CC$contextcontact4_v2___5 == 1,6, NA))))))
CC$Contact5location20 <- ifelse(CC$contextcontact5_v2___0 ==1, 1,
                                ifelse(CC$contextcontact5_v2___1 == 1 , 2,
                                       ifelse(CC$contextcontact5_v2___2 == 1, 3,
                                              ifelse(CC$contextcontact5_v2___3 == 1, 4,
                                                     ifelse(CC$contextcontact5_v2___4 == 1, 5,
                                                            ifelse(CC$contextcontact5_v2___5 == 1,6, NA))))))
CC$Contact6location20 <- ifelse(CC$contextcontact6_v2___0 ==1, 1,
                                ifelse(CC$contextcontact6_v2___1 == 1 , 2,
                                       ifelse(CC$contextcontact6_v2___2 == 1, 3,
                                              ifelse(CC$contextcontact6_v2___3 == 1, 4,
                                                     ifelse(CC$contextcontact6_v2___4 == 1, 5,
                                                            ifelse(CC$contextcontact6_v2___5 == 1,6, NA))))))
CC$Contact7location20 <- ifelse(CC$contextcontact7_v2___0 ==1, 1,
                                ifelse(CC$contextcontact7_v2___1 == 1 , 2,
                                       ifelse(CC$contextcontact7_v2___2 == 1, 3,
                                              ifelse(CC$contextcontact7_v2___3 == 1, 4,
                                                     ifelse(CC$contextcontact7_v2___4 == 1, 5,
                                                            ifelse(CC$contextcontact7_v2___5 == 1,6, NA))))))
CC$Contact8location20 <- ifelse(CC$contextcontact8_v2___0 ==1, 1,
                                ifelse(CC$contextcontact8_v2___1 == 1 , 2,
                                       ifelse(CC$contextcontact8_v2___2 == 1, 3,
                                              ifelse(CC$contextcontact8_v2___3 == 1, 4,
                                                     ifelse(CC$contextcontact8_v2___4 == 1, 5,
                                                            ifelse(CC$contextcontact8_v2___5 == 1,6, NA))))))
CC$Contact9location20 <- ifelse(CC$contextcontact9_v2___0 ==1, 1,
                                ifelse(CC$contextcontact9_v2___1 == 1 , 2,
                                       ifelse(CC$contextcontact9_v2___2 == 1, 3,
                                              ifelse(CC$contextcontact9_v2___3 == 1, 4,
                                                     ifelse(CC$contextcontact9_v2___4 == 1, 5,
                                                            ifelse(CC$contextcontact9_v2___5 == 1,6, NA))))))
CC$Contact10location20 <- ifelse(CC$contextcontact10_v2___0 ==1, 1,
                                 ifelse(CC$contextcontact10_v2___1 == 1 , 2,
                                        ifelse(CC$contextcontact10_v2___2 == 1, 3,
                                               ifelse(CC$contextcontact10_v2___3 == 1, 4,
                                                      ifelse(CC$contextcontact10_v2___4 == 1, 5,
                                                             ifelse(CC$contextcontact10_v2___5 == 1,6, NA))))))
CC$Contact11location20 <- ifelse(CC$contextcontact11_v2___0 ==1, 1,
                                 ifelse(CC$contextcontact11_v2___1 == 1 , 2,
                                        ifelse(CC$contextcontact11_v2___2 == 1, 3,
                                               ifelse(CC$contextcontact11_v2___3 == 1, 4,
                                                      ifelse(CC$contextcontact11_v2___4 == 1, 5,
                                                             ifelse(CC$contextcontact11_v2___5 == 1,6, NA))))))
CC$Contact12location20 <- ifelse(CC$contextcontact12_v2___0 ==1, 1,
                                 ifelse(CC$contextcontact12_v2___1 == 1 , 2,
                                        ifelse(CC$contextcontact12_v2___2 == 1, 3,
                                               ifelse(CC$contextcontact12_v2___3 == 1, 4,
                                                      ifelse(CC$contextcontact12_v2___4 == 1, 5,
                                                             ifelse(CC$contextcontact12_v2___5 == 1,6, NA))))))
CC$Contact13location20 <- ifelse(CC$contextcontact13_v2___0 ==1, 1,
                                 ifelse(CC$contextcontact13_v2___1 == 1 , 2,
                                        ifelse(CC$contextcontact13_v2___2 == 1, 3,
                                               ifelse(CC$contextcontact13_v2___3 == 1, 4,
                                                      ifelse(CC$contextcontact13_v2___4 == 1, 5,
                                                             ifelse(CC$contextcontact13_v2___5 == 1,6, NA))))))
CC$Contact14location20 <- ifelse(CC$contextcontact14_v2___0 ==1, 1,
                                 ifelse(CC$contextcontact14_v2___1 == 1 , 2,
                                        ifelse(CC$contextcontact14_v2___2 == 1, 3,
                                               ifelse(CC$contextcontact14_v2___3 == 1, 4,
                                                      ifelse(CC$contextcontact14_v2___4 == 1, 5,
                                                             ifelse(CC$contextcontact14_v2___5 == 1,6, NA))))))
CC$Contact15location20 <- ifelse(CC$contextcontact15_v2___0 ==1, 1,
                                 ifelse(CC$contextcontact15_v2___1 == 1 , 2,
                                        ifelse(CC$contextcontact15_v2___2 == 1, 3,
                                               ifelse(CC$contextcontact15_v2___3 == 1, 4,
                                                      ifelse(CC$contextcontact15_v2___4 == 1, 5,
                                                             ifelse(CC$contextcontact15_v2___5 == 1,6, NA))))))

table(CC$Contact1location2020, useNA = "always")
table(CC$Contact10location20, useNA = "always")

#Create a total variable for total number of contacts
CC$Total2020 <- CC$Cont1Total20 + CC$Cont2Total20 + CC$Cont3Total20 + CC$Cont4Total20 + CC$Cont5Total20 + CC$Cont6Total20 + CC$Cont7Total20 + CC$Cont8Total20 +
  CC$Cont9Total20 + CC$Cont10Total20 + CC$Cont11Total20 + CC$Cont12Total20 + CC$Cont13Total20 + CC$Cont14Total20 + CC$Cont15Total20
table(CC$Total2020)
sum(CC$Total2020)
#plotting
ggplot(CC, aes(x=Total2020)) + geom_histogram(binwidth=1) + xlim(0,175)
ggplot(CC, aes(x=Total2020)) + geom_histogram(binwidth=1, fill="Red") + xlim(0,160) + labs(title = "Total Number of Contacts per Respondent on a Day in Jan-March 2020", x="Number of Contacts", y="Number of Respondents")

hist(CC$Total2020)
hist(CC$Total2020, right=F, main="Total Number of Contacts per Respondent on a Day in Jan-March 2020", xlab="Number of Contacts", xlim=c(0,175),ylab="Frequency", col = "violet", labels = TRUE)

library(lessR)
Plot(Total2020, data=CC)
Plot(Total2020, data=CC, out_cut = 2, fences = TRUE, vbs_mean = TRUE)
BoxPlot(Total2020, data=CC)

#Total difference
#Sub 2021 from 2020 contacts
CC$TotalDiff <- CC$Total2020 - CC$Total2021
summary(CC$TotalDiff)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-8.00    0.50    4.00   14.41    8.50  501.00 
sd(CC$TotalDiff)
#39.9#
table(CC$TotalDiff)
CrossTable(CC$TotalDiff)
#trying to subset data so I have two graphs - one for postive, one for negatives
ggplot(CC, aes(x=TotalDiff)) + geom_histogram(binwidth=1) + xlim(-10,0)
ggplot(CC, aes(x=TotalDiff)) + geom_text(stat='count', aes(label=..count..), vjust=-1) + geom_histogram(binwidth=1, fill="Blue") + xlim(-9,0.5) + labs(title = "Negative Difference In Number of Respondents from 2020 to 2021", x="Number of Contacts", y="Number of Respondents")  + expand_limits(y = 40)
ggplot(CC, aes(x=TotalDiff)) +  geom_histogram(binwidth=1, fill="Blue") + xlim(0,100) + labs(title = "Difference In Number of Respondents from 2020 to 2021", x="Number of Contacts", y="Number of Respondents")  + expand_limits(y = 30)


#Now doing just all contacts
#Now doing some things for all the different contacts
#Trying some things to get a better idea of how many students/etc from the data
#Subsetting contacts
df1<-select(CC, contact1_v2, contact2_v2, contact3_v2,contact4_v2,contact5_v2,contact6_v2, contact7_v2,contact8_v2,contact9_v2,contact10_v2,contact11_v2,contact12_v2,contact13_v2,contact14_v2,contact15_v2)
DF = data.frame(table(unlist(df1)))
DF$percent = prop.table(DF$Freq)*100
DF
sum(DF$Freq)
margin.table(DF,1)
hist(DF$Var1,DF$Freq)
a <-ggplot(DF) + geom_point(mapping = aes(x=Var1, y=Freq))
a + theme(axis.text.x = element_text(angle = 90)) +  labs(title = "Description and Frequency of Contacts from all Respondents", x="Type of Contact", y="Frequency")
#+ geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title = "Total Number of Contacts per Respondent on a Day in Jan-March 2021", x="Number of Contacts", y="Number of Respondents")

#Going to location category
#1=My home 2=Work 3=Leisure 4=Travel 5=Shopping 6=Other

CC$Contact1location2020 <- ifelse(CC$contextcontact1_v2___0 == 1, 1,
                                  ifelse(CC$contextcontact1_v2___1 == 1 , 2,
                                         ifelse(CC$contextcontact1_v2___2 == 1, 3,
                                                ifelse(CC$contextcontact1_v2___3 == 1, 4,
                                                       ifelse(CC$contextcontact1_v2___4 == 1, 5,
                                                              ifelse(CC$contextcontact1_v2___5 == 1, 6, NA))))))
CC$Contact2location20 <- ifelse(CC$contextcontact2_v2___0 ==1, 1,
                                ifelse(CC$contextcontact2_v2___1 == 1 , 2,
                                       ifelse(CC$contextcontact2_v2___2 == 1, 3,
                                              ifelse(CC$contextcontact2_v2___3 == 1, 4,
                                                     ifelse(CC$contextcontact2_v2___4 == 1, 5,
                                                            ifelse(CC$contextcontact2_v2___5 == 1, 6, NA))))))
CC$Contact3location20 <- ifelse(CC$contextcontact3_v2___0 ==1, 1,
                                ifelse(CC$contextcontact3_v2___1 == 1 , 2,
                                       ifelse(CC$contextcontact3_v2___2 == 1, 3,
                                              ifelse(CC$contextcontact3_v2___3 == 1, 4,
                                                     ifelse(CC$contextcontact3_v2___4 == 1, 5,
                                                            ifelse(CC$contextcontact3_v2___5 == 1,6, NA))))))
CC$Contact4location20 <- ifelse(CC$contextcontact4___0 ==1, 1,
                                ifelse(CC$contextcontact4_v2___1 == 1 , 2,
                                       ifelse(CC$contextcontact4_v2___2 == 1, 3,
                                              ifelse(CC$contextcontact4_v2___3 == 1, 4,
                                                     ifelse(CC$contextcontact4_v2___4 == 1, 5,
                                                            ifelse(CC$contextcontact4_v2___5 == 1,6, NA))))))
CC$Contact5location20 <- ifelse(CC$contextcontact5_v2___0 ==1, 1,
                                ifelse(CC$contextcontact5_v2___1 == 1 , 2,
                                       ifelse(CC$contextcontact5_v2___2 == 1, 3,
                                              ifelse(CC$contextcontact5_v2___3 == 1, 4,
                                                     ifelse(CC$contextcontact5_v2___4 == 1, 5,
                                                            ifelse(CC$contextcontact5_v2___5 == 1,6, NA))))))
CC$Contact6location20 <- ifelse(CC$contextcontact6_v2___0 ==1, 1,
                                ifelse(CC$contextcontact6_v2___1 == 1 , 2,
                                       ifelse(CC$contextcontact6_v2___2 == 1, 3,
                                              ifelse(CC$contextcontact6_v2___3 == 1, 4,
                                                     ifelse(CC$contextcontact6_v2___4 == 1, 5,
                                                            ifelse(CC$contextcontact6_v2___5 == 1,6, NA))))))
CC$Contact7location20 <- ifelse(CC$contextcontact7_v2___0 ==1, 1,
                                ifelse(CC$contextcontact7_v2___1 == 1 , 2,
                                       ifelse(CC$contextcontact7_v2___2 == 1, 3,
                                              ifelse(CC$contextcontact7_v2___3 == 1, 4,
                                                     ifelse(CC$contextcontact7_v2___4 == 1, 5,
                                                            ifelse(CC$contextcontact7_v2___5 == 1,6, NA))))))
CC$Contact8location20 <- ifelse(CC$contextcontact8_v2___0 ==1, 1,
                                ifelse(CC$contextcontact8_v2___1 == 1 , 2,
                                       ifelse(CC$contextcontact8_v2___2 == 1, 3,
                                              ifelse(CC$contextcontact8_v2___3 == 1, 4,
                                                     ifelse(CC$contextcontact8_v2___4 == 1, 5,
                                                            ifelse(CC$contextcontact8_v2___5 == 1,6, NA))))))
CC$Contact9location20 <- ifelse(CC$contextcontact9_v2___0 ==1, 1,
                                ifelse(CC$contextcontact9_v2___1 == 1 , 2,
                                       ifelse(CC$contextcontact9_v2___2 == 1, 3,
                                              ifelse(CC$contextcontact9_v2___3 == 1, 4,
                                                     ifelse(CC$contextcontact9_v2___4 == 1, 5,
                                                            ifelse(CC$contextcontact9_v2___5 == 1,6, NA))))))
CC$Contact10location20 <- ifelse(CC$contextcontact10_v2___0 ==1, 1,
                                 ifelse(CC$contextcontact10_v2___1 == 1 , 2,
                                        ifelse(CC$contextcontact10_v2___2 == 1, 3,
                                               ifelse(CC$contextcontact10_v2___3 == 1, 4,
                                                      ifelse(CC$contextcontact10_v2___4 == 1, 5,
                                                             ifelse(CC$contextcontact10_v2___5 == 1,6, NA))))))
CC$Contact11location20 <- ifelse(CC$contextcontact11_v2___0 ==1, 1,
                                 ifelse(CC$contextcontact11_v2___1 == 1 , 2,
                                        ifelse(CC$contextcontact11_v2___2 == 1, 3,
                                               ifelse(CC$contextcontact11_v2___3 == 1, 4,
                                                      ifelse(CC$contextcontact11_v2___4 == 1, 5,
                                                             ifelse(CC$contextcontact11_v2___5 == 1,6, NA))))))
CC$Contact12location20 <- ifelse(CC$contextcontact12_v2___0 ==1, 1,
                                 ifelse(CC$contextcontact12_v2___1 == 1 , 2,
                                        ifelse(CC$contextcontact12_v2___2 == 1, 3,
                                               ifelse(CC$contextcontact12_v2___3 == 1, 4,
                                                      ifelse(CC$contextcontact12_v2___4 == 1, 5,
                                                             ifelse(CC$contextcontact12_v2___5 == 1,6, NA))))))
CC$Contact13location20 <- ifelse(CC$contextcontact13_v2___0 ==1, 1,
                                 ifelse(CC$contextcontact13_v2___1 == 1 , 2,
                                        ifelse(CC$contextcontact13_v2___2 == 1, 3,
                                               ifelse(CC$contextcontact13_v2___3 == 1, 4,
                                                      ifelse(CC$contextcontact13_v2___4 == 1, 5,
                                                             ifelse(CC$contextcontact13_v2___5 == 1,6, NA))))))
CC$Contact14location20 <- ifelse(CC$contextcontact14_v2___0 ==1, 1,
                                 ifelse(CC$contextcontact14_v2___1 == 1 , 2,
                                        ifelse(CC$contextcontact14_v2___2 == 1, 3,
                                               ifelse(CC$contextcontact14_v2___3 == 1, 4,
                                                      ifelse(CC$contextcontact14_v2___4 == 1, 5,
                                                             ifelse(CC$contextcontact14_v2___5 == 1,6, NA))))))
CC$Contact15location20 <- ifelse(CC$contextcontact15_v2___0 ==1, 1,
                                 ifelse(CC$contextcontact15_v2___1 == 1 , 2,
                                        ifelse(CC$contextcontact15_v2___2 == 1, 3,
                                               ifelse(CC$contextcontact15_v2___3 == 1, 4,
                                                      ifelse(CC$contextcontact15_v2___4 == 1, 5,
                                                             ifelse(CC$contextcontact15_v2___5 == 1,6, NA))))))

table(CC$Contact1location2020, useNA = "always")
table(CC$Contact10location20, useNA = "always")


#Doing my graphs this way
df3<-select(CC, Contact1location2020, Contact2location20, Contact3location20, Contact4location20, Contact5location20, Contact6location20, Contact7location20, Contact8location20, Contact9location20, Contact10location20, Contact11location20, Contact12location20, Contact13location20, Contact14location20, Contact15location20)
DF3 = data.frame(table(unlist(df3)))
DF3$percent = prop.table(DF3$Freq)*100
DF3
sum(DF3$Freq)

c <-ggplot(DF3) + geom_point(size=4,color = "red", mapping = aes(x=reorder(Var1,-Freq), y=Freq))
c + scale_x_discrete(labels =c('1'= 'My Home', '2' = "Work", '3' = "Leisure", '4' = "Travel",'5'= 'Shopping', '6' = "Other" )) + labs(title = "Description and Frequency of the Locations Respondents Chose for 2020", x="Location Choice", y="Frequency") + theme(axis.text.x = element_text(face="bold", size = 8))

#Going to do some splitting for Pullman/no pullman
#Should have 165 in PUllman (140 who work there, +25 who are NA for campus loc)
#Should have 95 who don't (55 who don't work there, + 40 who are NA for campus loc)
table(CC$pullmanyn, useNA = "always")
table(CC$campus_loc, useNA = "always")
CC$campus_loc[is.na(CC$campus_loc)] <-99
table(CC$pullmanyn, CC$campus_loc, useNA = "always")
#1=yes
CC$PullmanLive <- NA
CC$PullmanLive <- ifelse (CC$pullmanyn ==1, "Yes", "No")
table(CC$PullmanLive, useNA = 'always')
CC$PullmanWork <- NA
CC$PullmanWork <- ifelse (CC$campus_loc ==0, "Yes",
                          ifelse (CC$pullmanyn ==0 && CC$campus_loc == 99, "No", NA))
table(CC$PullmanWork, useNA = 'always')
CC$PullmanBoth <- NA
CC$PullmanBoth <- ifelse (CC$PullmanLive == "Yes" | CC$PullmanWork == "Yes", "Yes", "No")
table(CC$PullmanBoth, useNA = "always")
#WINNER WINNER CHICKEN DINNER!!
table(CC$PullmanBoth,CC$locationcontact1, useNA = "always")
view(CC)
#Record 197 for location contact 1 -  Camas WA == WSU vancouver
#There's another I input - home was in Moscow ID so I can certainly say that pullmanyn == NO
CC$campus_loc[CC$participant_id==197] <- 2
#Filtering into a Pullman and Non-Pullman data sets
Pullman <-CC %>% dplyr::filter (PullmanBoth == "Yes") 
NotPullman <-CC %>% dplyr::filter (PullmanBoth == "No") 

#Looking to see if there's any difference between Pullman and Not Pullman for Total Contacts
Pullman$TotalDiff
summary(Pullman$TotalDiff)
#Min  1st Qu.  Median   Mean   3rd Qu.  Max
#-7     1.00    4.00    18.04   10.00    501
summary(NotPullman$TotalDiff)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-8.00    1.00    3.00    9.74    8.25  136.00 

#Doing some cleaning for essential workers
table(CC$essentialjobyn, useNA = 'always')
#200 Non-essential, 61 essentail, 10 NA

#Does this break down by Pullman/Not ?
table(CC$essentialjobyn, CC$PullmanBoth, useNA = 'always')
#      No Yes <NA>
#0     76 123    1
#1     19  42    0
#<NA>   1   0    9
CrossTable(CC$essentialjobyn, CC$PullmanBoth, chisq = TRUE)
#Chi-sq p-value of .317 - so I think we're good and pretty split

#Going to look at position, and see if hopefully we can put faculty/staff into a binary category (0-Staff, 1-faculty)
library(R.utils)
CC$occupation <-capitalize(CC$occupation)
table(CC$occupation, useNA = 'always')
#12 NAs
#I have a few higher ups (VP, vets/etc - are those faculty or staff??)
#Going to create two variables - 1 binary faculty/staff
#The other will have 4 levels - faculty,staff, upper admin, and clinical
#First will be binary (easier)
CC$Faculty[CC$occupation=='Faculty'] <- "Faculty"
CC$Faculty[CC$occupation=='Academic Advisor'] <- "Staff"
CC$Faculty[CC$occupation=='Academic Coordinator'] <- "Staff"
CC$Faculty[CC$occupation=='Adjunct'] <- "Faculty"
CC$Faculty[CC$occupation=='Adjunct Faculty'] <- "Faculty"
CC$Faculty[CC$occupation=='Academic Coordinator/Advisor 1'] <- "Staff"
CC$Faculty[CC$occupation=='Adjunct Faculty, consultant, scientist'] <- "Faculty"
CC$Faculty[CC$occupation=='Adjunct instructor'] <- "Faculty"
CC$Faculty[CC$occupation=='Adjunct professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Admin Asst.'] <- "Staff"
CC$Faculty[CC$occupation=='Admin Manager'] <- "Staff"
CC$Faculty[CC$occupation=='Admin Professional'] <- "Staff"
CC$Faculty[CC$occupation=='Administration'] <- "Staff"
CC$Faculty[CC$occupation=='Administrative Assistant'] <- "Staff"
CC$Faculty[CC$occupation=='Administrator'] <- "Staff"
CC$Faculty[CC$occupation=='Advisor'] <- "Staff"
CC$Faculty[CC$occupation=='Assistant professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Assistant Professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Assistant Research Professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Assoc professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Associate Director'] <- "Staff"
CC$Faculty[CC$occupation=='Associate in Research'] <- "Staff"
CC$Faculty[CC$occupation=='Associate professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Associate Professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Associate Professor, Career Track'] <- "Faculty"
CC$Faculty[CC$occupation=='Budget & Finance Manager'] <- "Staff"
CC$Faculty[CC$occupation=='Career Advisor'] <- "Staff"
CC$Faculty[CC$occupation=='Career Track Associate Professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Civil Service'] <- "Staff"
CC$Faculty[CC$occupation=='Clinical Professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Clinical Psychologist'] <- "Staff"
CC$Faculty[CC$occupation=='College faculty and administrator'] <- "Staff"
CC$Faculty[CC$occupation=='College of Nursing faculty'] <- "Faculty"
CC$Faculty[CC$occupation=='College professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Communicator'] <- "Staff"
CC$Faculty[CC$occupation=='Coordinator'] <- "Staff"
CC$Faculty[CC$occupation=='County Director'] <- "Staff"
CC$Faculty[CC$occupation=='Dean'] <- "Faculty"
CC$Faculty[CC$occupation=='Director of Advising'] <- "Staff"
CC$Faculty[CC$occupation=='Director Public Affairs'] <- "Staff"
CC$Faculty[CC$occupation=='Director, Psychologist'] <- "Staff"
CC$Faculty[CC$occupation=='Educator'] <- "Faculty"
CC$Faculty[CC$occupation=='Employee'] <- "Staff"
CC$Faculty[CC$occupation=='Engineering Technician 2'] <- "Staff"
CC$Faculty[CC$occupation=='Extension'] <- "Staff"
CC$Faculty[CC$occupation=='Faculty member'] <- "Faculty"
CC$Faculty[CC$occupation=='Faculty, professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Faculty, senior administration'] <- "Faculty"
CC$Faculty[CC$occupation=='Faculty/Administrator'] <- "Faculty"
CC$Faculty[CC$occupation=='Faculty/pharmacist'] <- "Faculty"
CC$Faculty[CC$occupation=='Fiscal Analyst'] <- "Staff"
CC$Faculty[CC$occupation=='Fiscal specialist'] <- "Staff"
CC$Faculty[CC$occupation=='Fiscal Tech'] <- "Staff"
CC$Faculty[CC$occupation=='Instructional Classroom Support Technician'] <- "Staff"
CC$Faculty[CC$occupation=='Instructor'] <- "Faculty"
CC$Faculty[CC$occupation=='Instructor at Yakima'] <- "Faculty"
CC$Faculty[CC$occupation=='IT'] <- "Staff"
CC$Faculty[CC$occupation=='IT Manager'] <- "Staff"
CC$Faculty[CC$occupation=='Lab Director'] <- "Staff"
CC$Faculty[CC$occupation=='Lab Manager'] <- "Staff"
CC$Faculty[CC$occupation=='Laboratory manager'] <- "Staff"
CC$Faculty[CC$occupation=='Laboratory Manager'] <- "Staff"
CC$Faculty[CC$occupation=='Lecturer'] <- "Faculty"
CC$Faculty[CC$occupation=='Librarian'] <- "Staff"
CC$Faculty[CC$occupation=='Manager'] <- "Staff"
CC$Faculty[CC$occupation=='Math Faculty'] <- "Faculty"
CC$Faculty[CC$occupation=='Media maintanence technician'] <- "Staff"
CC$Faculty[CC$occupation=='Medical education'] <- "Faculty"
CC$Faculty[CC$occupation=='Mental health provider'] <- "Staff"
CC$Faculty[CC$occupation=='Nursing Clinical Assistant Professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Nursing Instructor'] <- "Faculty"
CC$Faculty[CC$occupation=='Office Assistant'] <- "Staff"
CC$Faculty[CC$occupation=='Office Asst. III / Receptionist'] <- "Staff"
CC$Faculty[CC$occupation=='Pharmacy Faculty'] <- "Faculty"
CC$Faculty[CC$occupation=='Physician administrator'] <- "Staff"
CC$Faculty[CC$occupation=='Physician Faculty'] <- "Faculty"
CC$Faculty[CC$occupation=='Physician/Faculty'] <- "Faculty"
CC$Faculty[CC$occupation=='Plant Growth Facilities Manager'] <- "Staff"
CC$Faculty[CC$occupation=='Postdoc'] <- "Staff"
CC$Faculty[CC$occupation=='Postdoctoral Research Associate'] <- "Staff"
CC$Faculty[CC$occupation=='Postdoctoral researcher'] <- "Staff"
CC$Faculty[CC$occupation=='Pre-Health advisor'] <- "Staff"
CC$Faculty[CC$occupation=='Principal Assistant'] <- "Staff"
CC$Faculty[CC$occupation=='Prof'] <- "Faculty"
CC$Faculty[CC$occupation=='Professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Professor-scientist'] <- "Faculty"
CC$Faculty[CC$occupation=='Professor and Dean'] <- "Faculty"
CC$Faculty[CC$occupation=='Professor/Physician'] <- "Faculty"
CC$Faculty[CC$occupation=='Program Coordinator'] <- "Staff"
CC$Faculty[CC$occupation=='Program manager'] <- "Staff"
CC$Faculty[CC$occupation=='Program Specialist 2'] <- "Staff"
CC$Faculty[CC$occupation=='Project manager'] <- "Staff"
CC$Faculty[CC$occupation=='Psychologist'] <- "Staff"
CC$Faculty[CC$occupation=='Research'] <- "Staff"
CC$Faculty[CC$occupation=='Research administrator'] <- "Staff"
CC$Faculty[CC$occupation=='Research Assistant Professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Research Associate'] <- "Staff"
CC$Faculty[CC$occupation=='Researcher'] <- "Staff"
CC$Faculty[CC$occupation=='Residence Life'] <- "Staff"
CC$Faculty[CC$occupation=='Scholarly Associate Professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Scholarly Asst. Professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Scholarly professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Scientist'] <- "Staff"
CC$Faculty[CC$occupation=='Staff'] <- "Staff"
CC$Faculty[CC$occupation=='Statistician'] <- "Staff"
CC$Faculty[CC$occupation=='Student Affairs Staff'] <- "Staff"
CC$Faculty[CC$occupation=='Student Services'] <- "Staff"
CC$Faculty[CC$occupation=='Supervisor'] <- "Staff"
CC$Faculty[CC$occupation=='Support Staff/Program Coordinator'] <- "Staff"
CC$Faculty[CC$occupation=='Teacher'] <- "Faculty"
CC$Faculty[CC$occupation=='Teaching Assistant Professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Teaching Associate Professor'] <- "Faculty"
CC$Faculty[CC$occupation=='Technology licensing'] <- "Staff"
CC$Faculty[CC$occupation=='Technology Licensing Associate'] <- "Staff"
CC$Faculty[CC$occupation=='University Faculty'] <- "Faculty"
CC$Faculty[CC$occupation=='Veterinarian'] <- "Staff"
CC$Faculty[CC$occupation=='Veterinarian/Clinical Instructor'] <- "Faculty"
CC$Faculty[CC$occupation=='Vice President'] <- "Staff"
CC$Faculty[CC$occupation=='Web Coordinator'] <- "Staff"
CC$Faculty[CC$occupation=='Writer/editor'] <- "Staff"
CC$Faculty[CC$occupation=='WSU Nursing Instructor & hospital RN'] <- "Faculty"
CC$Faculty[CC$occupation=='WSU staff member'] <- "Staff"
CC$Faculty[CC$occupation=='You need an \'other\' campus category for extension offices & other situations. I am affiliated with an office in Olympia. OCCupation is senior research manager.'] <- "Staff"
table(CC$occupation,CC$Faculty, useNA = 'always')
table(CC$Faculty)
# Faculty         Staff       <NA> 
#  162(59.8%0)   97(35.8%)     12 (4.4%) 

#Basic Poisson with essential, pullman, and faculty as some independent variables to understand # of total contacts in 2020
library(sandwich)
library(lmtest)
essential <- glm(Total2020 ~ essentialjobyn, data = CC, family = poisson,trace=TRUE)
summary(essential)
#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-6.164  -4.187  -3.166  -0.953  48.315  

# Coefficients:
#                Estimate       Std. Error     z value             Pr(>|z|)
#  (Intercept)     2.94444     0.01622        181.508  < 0.0000000000000002
#  essentialjobyn -0.17288     0.03590         -4.815           0.00000147

#(Dispersion parameter for poisson family taken to be 1)

#Null deviance: 9581.8  on 260  degrees of freedom
#Residual deviance: 9557.8  on 259  degrees of freedom
#(10 observations deleted due to missingness)
#AIC: 10535
#Number of Fisher Scoring iterations: 6

b.out <- NULL
se.out<- NULL
Poisson <- glm(Total2020 ~ essentialjobyn + PullmanBoth + Faculty, data=CC,family = poisson, trace=TRUE)
b.out <- rbind(b.out,Poisson$coef)
se.out <- rbind (se.out, coeftest(Poisson, vcov=sandwich)[,2])
summary(Poisson)
#Call:
#  glm(formula = Total2020 ~ essentialjobyn + PullmanBoth + Faculty, 
#      family = poisson, data = CC, trace = TRUE)

#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-7.520  -4.371  -2.902  -0.089  44.185  

#Coefficients:
#               Estimate Std. Error z value             Pr(>|z|)
#(Intercept)     2.69780    0.02950  91.466 < 0.0000000000000002
#essentialjobyn -0.14121    0.03615  -3.906            0.0000938
#PullmanBothYes  0.64408    0.03381  19.052 < 0.0000000000000002
#FacultyStaff   -0.62595    0.03350 -18.686 < 0.0000000000000002

#(Dispersion parameter for poisson family taken to be 1)

#Null deviance: 9559.0  on 257  degrees of freedom
#Residual deviance: 8910.6  on 254  degrees of freedom
#(13 observations deleted due to missingness)
#AIC: 9879.2
#Number of Fisher Scoring iterations: 6

#All significant variables, and a smaller AIC then essential. Deviance is 8910.571 after 6 iterations

b21.out <- NULL
se21.out<- NULL
Poisson21 <- glm(Total2021 ~ essentialjobyn + PullmanBoth + Faculty, data=CC,family = poisson, trace=TRUE)
b21.out <- rbind(b21.out,Poisson21$coef)
se21.out <- rbind (se21.out, coeftest(Poisson21, vcov=sandwich)[,2])
summary(Poisson21)

#glm(formula = Total2021 ~ essentialjobyn + PullmanBoth + Faculty, 
#    family = poisson, data = CC, trace = TRUE)
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.5598  -1.2421  -0.5050   0.6723   6.1934  

#Coefficients:
#               Estimate Std. Error z value             Pr(>|z|)
#(Intercept)     1.07496    0.06322  17.003 < 0.0000000000000002
#essentialjobyn  0.32943    0.07621   4.323            0.0000154
#PullmanBothYes  0.11173    0.07537   1.482                0.138
#FacultyStaff   -0.15650    0.07502  -2.086                0.037

#(Dispersion parameter for poisson family taken to be 1)
#Null deviance: 495.21  on 257  degrees of freedom
#Residual deviance: 472.49  on 254  degrees of freedom
#(13 observations deleted due to missingness)
#AIC: 1193.6
#Number of Fisher Scoring iterations: 5

#Deviance here was 472.49, with 5 iterations. The one that surprised me was that living in PUllman was NOT significant. Or at least not as much as the others

#Now working on levels of faculty
#Faculty, Staff, Upper Admin, Clinical

CC$OccType[CC$occupation=='Faculty'] <- "Faculty"
CC$OccType[CC$occupation=='Academic Advisor'] <- "Staff" #Ask Eric about aca adv- cause these might be better as staff, more personal interaction with students
CC$OccType[CC$occupation=='Academic Coordinator'] <- "Staff"
CC$OccType[CC$occupation=='Adjunct'] <- "Faculty"
CC$OccType[CC$occupation=='Adjunct Faculty'] <- "Faculty"
CC$OccType[CC$occupation=='Academic Coordinator/Advisor 1'] <- "Staff"  #ERIC
CC$OccType[CC$occupation=='Adjunct Faculty, consultant, scientist'] <- "Faculty"
CC$OccType[CC$occupation=='Adjunct instructor'] <- "Faculty"
CC$OccType[CC$occupation=='Adjunct professor'] <- "Faculty"
CC$OccType[CC$occupation=='Admin Asst.'] <- "Staff"
CC$OccType[CC$occupation=='Admin Manager'] <- "Staff"
CC$OccType[CC$occupation=='Admin Professional'] <- "Staff"
CC$OccType[CC$occupation=='Administration'] <- "Staff"
CC$OccType[CC$occupation=='Administrative Assistant'] <- "Staff"
CC$OccType[CC$occupation=='Administrator'] <- "Staff"
CC$OccType[CC$occupation=='Advisor'] <- "Upper Admin"
CC$OccType[CC$occupation=='Assistant professor'] <- "Faculty"
CC$OccType[CC$occupation=='Assistant Professor'] <- "Faculty"
CC$OccType[CC$occupation=='Assistant Research Professor'] <- "Faculty"
CC$OccType[CC$occupation=='Assoc professor'] <- "Faculty"
CC$OccType[CC$occupation=='Associate Director'] <- "Upper Admin"
CC$OccType[CC$occupation=='Associate in Research'] <- "Staff"
CC$OccType[CC$occupation=='Associate professor'] <- "Faculty"
CC$OccType[CC$occupation=='Associate Professor'] <- "Faculty"
CC$OccType[CC$occupation=='Associate Professor, Career Track'] <- "Faculty"
CC$OccType[CC$occupation=='Budget & Finance Manager'] <- "Staff"
CC$OccType[CC$occupation=='Career Advisor'] <- "Staff" #ERIC a la academic advisor
CC$OccType[CC$occupation=='Career Track Associate Professor'] <- "Faculty"
CC$OccType[CC$occupation=='Civil Service'] <- "Staff"
CC$OccType[CC$occupation=='Clinical Professor'] <- "Clinical"
CC$OccType[CC$occupation=='Clinical Psychologist'] <- "Clinical"
CC$OccType[CC$occupation=='College faculty and administrator'] <- "Upper Admin"
CC$OccType[CC$occupation=='College of Nursing faculty'] <- "Faculty"
CC$OccType[CC$occupation=='College professor'] <- "Faculty"
CC$OccType[CC$occupation=='Communicator'] <- "Staff"
CC$OccType[CC$occupation=='Coordinator'] <- "Staff"
CC$OccType[CC$occupation=='County Director'] <- "Upper Admin"
CC$OccType[CC$occupation=='Dean'] <- "Upper Admin"
CC$OccType[CC$occupation=='Director of Advising'] <- "Upper Admin"
CC$OccType[CC$occupation=='Director Public Affairs'] <- "Upper Admin"
CC$OccType[CC$occupation=='Director, Psychologist'] <- "Upper Admin"
CC$OccType[CC$occupation=='Educator'] <- "Faculty"
CC$OccType[CC$occupation=='Employee'] <- "Staff"
CC$OccType[CC$occupation=='Engineering Technician 2'] <- "Staff"
CC$OccType[CC$occupation=='Extension'] <- "Staff"
CC$OccType[CC$occupation=='Faculty member'] <- "Faculty"
CC$OccType[CC$occupation=='Faculty, professor'] <- "Faculty"
CC$OccType[CC$occupation=='Faculty, senior administration'] <- "Faculty" 
CC$OccType[CC$occupation=='Faculty/Administrator'] <- "Faculty" 
CC$OccType[CC$occupation=='Faculty/pharmacist'] <- "Clinical"
CC$OccType[CC$occupation=='Fiscal Analyst'] <- "Staff"
CC$OccType[CC$occupation=='Fiscal specialist'] <- "Staff"
CC$OccType[CC$occupation=='Fiscal Tech'] <- "Staff"
CC$OccType[CC$occupation=='Instructional Classroom Support Technician'] <- "Staff"
CC$OccType[CC$occupation=='Instructor'] <- "Faculty"
CC$OccType[CC$occupation=='Instructor at Yakima'] <- "Faculty"
CC$OccType[CC$occupation=='IT'] <- "Staff"
CC$OccType[CC$occupation=='IT Manager'] <- "Staff"
CC$OccType[CC$occupation=='Lab Director'] <- "Staff"
CC$OccType[CC$occupation=='Lab Manager'] <- "Staff"
CC$OccType[CC$occupation=='Laboratory manager'] <- "Staff"
CC$OccType[CC$occupation=='Laboratory Manager'] <- "Staff"
CC$OccType[CC$occupation=='Lecturer'] <- "Faculty"
CC$OccType[CC$occupation=='Librarian'] <- "Staff"
CC$OccType[CC$occupation=='Manager'] <- "Staff"
CC$OccType[CC$occupation=='Math Faculty'] <- "Faculty"
CC$OccType[CC$occupation=='Media maintanence technician'] <- "Staff"
CC$OccType[CC$occupation=='Medical education'] <- "Faculty" 
CC$OccType[CC$occupation=='Mental health provider'] <- "Clinical"
CC$OccType[CC$occupation=='Nursing Clinical Assistant Professor'] <- "Clinical"
CC$OccType[CC$occupation=='Nursing Instructor'] <- "Faculty"
CC$OccType[CC$occupation=='Office Assistant'] <- "Staff"
CC$OccType[CC$occupation=='Office Asst. III / Receptionist'] <- "Staff"
CC$OccType[CC$occupation=='Pharmacy Faculty'] <- "Faculty"
CC$OccType[CC$occupation=='Physician administrator'] <- "Staff"
CC$OccType[CC$occupation=='Physician Faculty'] <- "Clinical" 
CC$OccType[CC$occupation=='Physician/Faculty'] <- "Clinical"
CC$OccType[CC$occupation=='Plant Growth Facilities Manager'] <- "Staff"
CC$OccType[CC$occupation=='Postdoc'] <- "Staff"
CC$OccType[CC$occupation=='Postdoctoral Research Associate'] <- "Staff"
CC$OccType[CC$occupation=='Postdoctoral researcher'] <- "Staff"
CC$OccType[CC$occupation=='Pre-Health advisor'] <- "Staff"
CC$OccType[CC$occupation=='Principal Assistant'] <- "Staff"
CC$OccType[CC$occupation=='Prof'] <- "Faculty"
CC$OccType[CC$occupation=='Professor'] <- "Faculty"
CC$OccType[CC$occupation=='Professor-scientist'] <- "Faculty"
CC$OccType[CC$occupation=='Professor and Dean'] <- "Upper Admin"
CC$OccType[CC$occupation=='Professor/Physician'] <- "Clinical"
CC$OccType[CC$occupation=='Program Coordinator'] <- "Staff"
CC$OccType[CC$occupation=='Program manager'] <- "Staff"
CC$OccType[CC$occupation=='Program Specialist 2'] <- "Staff"
CC$OccType[CC$occupation=='Project manager'] <- "Staff"
CC$OccType[CC$occupation=='Psychologist'] <- "Clinical"
CC$OccType[CC$occupation=='Research'] <- "Staff"
CC$OccType[CC$occupation=='Research administrator'] <- "Staff"
CC$OccType[CC$occupation=='Research Assistant Professor'] <- "Faculty"
CC$OccType[CC$occupation=='Research Associate'] <- "Staff"
CC$OccType[CC$occupation=='Researcher'] <- "Staff"
CC$OccType[CC$occupation=='Residence Life'] <- "Staff"
CC$OccType[CC$occupation=='Scholarly Associate Professor'] <- "Faculty"
CC$OccType[CC$occupation=='Scholarly Asst. Professor'] <- "Faculty"
CC$OccType[CC$occupation=='Scholarly professor'] <- "Faculty"
CC$OccType[CC$occupation=='Scientist'] <- "Staff"
CC$OccType[CC$occupation=='Staff'] <- "Staff"
CC$OccType[CC$occupation=='Statistician'] <- "Staff"
CC$OccType[CC$occupation=='Student Affairs Staff'] <- "Staff"
CC$OccType[CC$occupation=='Student Services'] <- "Staff"
CC$OccType[CC$occupation=='Supervisor'] <- "Staff"
CC$OccType[CC$occupation=='Support Staff/Program Coordinator'] <- "Staff"
CC$OccType[CC$occupation=='Teacher'] <- "Faculty"
CC$OccType[CC$occupation=='Teaching Assistant Professor'] <- "Faculty"
CC$OccType[CC$occupation=='Teaching Associate Professor'] <- "Faculty"
CC$OccType[CC$occupation=='Technology licensing'] <- "Staff"
CC$OccType[CC$occupation=='Technology Licensing Associate'] <- "Staff"
CC$OccType[CC$occupation=='University Faculty'] <- "Faculty"
CC$OccType[CC$occupation=='Veterinarian'] <- "Clinical"
CC$OccType[CC$occupation=='Veterinarian/Clinical Instructor'] <- "Clinical"
CC$OccType[CC$occupation=='Vice President'] <- "Upper Admin"
CC$OccType[CC$occupation=='Web Coordinator'] <- "Staff"
CC$OccType[CC$occupation=='Writer/editor'] <- "Staff"
CC$OccType[CC$occupation=='WSU Nursing Instructor & hospital RN'] <- "Clinical"
CC$OccType[CC$occupation=='WSU staff member'] <- "Staff"
CC$OccType[CC$occupation=='You need an \'other\' campus category for extension offices & other situations. I am affiliated with an office in Olympia. OCCupation is senior research manager.'] <- "Staff"
table(CC$occupation,CC$OccType, useNA = 'always')
table(CC$OccType, useNA = 'always')
# Clinical     Faculty      Staff      Upper Admin        <NA> 
# 13 (4.8%)   152 (56%)     81 (30%)    13(4.8%)          12 (4.4%)

Occtype <- glm(Total2020 ~ OccType, data = CC, family = poisson,trace=TRUE)
summary(Occtype)
#glm(formula = Total2020 ~ OCCType, family = poisson, data = CC,trace = TRUE)

#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-6.646  -4.395  -2.951  -0.289  46.785  

#Coefficients:
#  Estimate Std. Error z value             Pr(>|z|)
#(Intercept)         2.79164    0.06868  40.647 < 0.0000000000000002
#OCCTypeFaculty      0.30329    0.07082   4.283          0.000018460
#OCCTypeStaff       -0.21912    0.07523  -2.913              0.00358
#OCCTypeUpper Admin -0.61165    0.11581  -5.281          0.000000128

#(Dispersion parameter for poisson family taken to be 1)

#Null deviance: 9562.5  on 258  degrees of freedom
#Residual deviance: 9236.3  on 255  degrees of freedom
#(12 observations deleted due to missingness)
#AIC: 10209
#Number of Fisher Scoring iterations: 6


b3.out <- NULL
se3.out<- NULL
OccPoisson <- glm(Total2020 ~ essentialjobyn + PullmanBoth + OccType, data=CC,family = poisson, trace=TRUE)
b3.out <- rbind(b3.out,OccPoisson$coef)
se3.out <- rbind (se3.out, coeftest(OccPoisson, vcov=sandwich)[,2])
summary(OccPoisson)
#Call:glm(formula = Total2020 ~ essentialjobyn + PullmanBoth + OccType, 
#    family = poisson, data = CC, trace = TRUE)

#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-7.590  -4.374  -2.653  -0.173  43.983  

#Coefficients:
#                    Estimate  Std.Error z value           Pr(>|z|)
#(Intercept)         2.56165    0.07345  34.875  < 0.0000000000000002
#essentialjobyn     -0.13401    0.03674  -3.648              0.000265
#PullmanBothYes      0.65071    0.03407  19.099  < 0.0000000000000002
#OccTypeFaculty      0.14819    0.07226   2.051              0.040297
#OccTypeStaff       -0.52747    0.07688  -6.861      0.00000000000686
#OccTypeUpperAdmin  -0.77638    0.12008  -6.466      0.00000000010092

#(Dispersion parameter for poisson family taken to be 1)

#Null deviance: 9557.3  on 256  degrees of freedom
#Residual deviance: 8834.1  on 251  degrees of freedom
#(14 observations deleted due to missingness)
#AIC: 9802.2

#Number of Fisher Scoring iterations: 6

#All significant variables, and a smaller AIC then essential. Deviance is 8834.1 after 6 iterations

b21occ.out <- NULL
se21occ.out<- NULL
Poisson21Occ <- glm(Total2021 ~ essentialjobyn + PullmanBoth + OccType, data=CC,family = poisson, trace=TRUE)
b21occ.out <- rbind(b21.out,Poisson21Occ$coef)
se21occ.out <- rbind (se21.out, coeftest(Poisson21Occ, vcov=sandwich)[,2])
summary(Poisson21Occ)
#Call:
#  glm(formula = Total2021 ~ essentialjobyn + PullmanBoth + OccType, family = poisson, data = CC, trace = TRUE)

#Deviance Residuals: 
#Min       1Q   Median       3Q      Max  
#-2.5650  -1.1983  -0.4541   0.7295   4.9653  

#Coefficients:
#                  Estimate  Std. Error  z value           Pr(>|z|)
#(Intercept)         1.53836    0.12773  12.044 < 0.0000000000000002
#essentialjobyn      0.26951    0.07839   3.438             0.000586
#PullmanBothYes      0.17794    0.07674   2.319             0.020415
#OccTypeFaculty     -0.53855    0.12780  -4.214          0.000025096
#OccTypeStaff       -0.71833    0.13792  -5.208          0.000000191
#OccTypeUpper Admin -0.51573    0.20172  -2.557             0.010568

#(Dispersion parameter for poisson family taken to be 1)

#Null deviance: 487.80  on 256  degrees of freedom
#Residual deviance: 443.55  on 251  degrees of freedom
#(14 observations deleted due to missingness)
#AIC: 1165
#Number of Fisher Scoring iterations: 5
#Still all signficant, with again, one less Fish Scoring and a smaller deviance (443.55)


#Trying to find the absolute change in contacts (ex: Total2020=2, total2021=8 totalDiff = -8)
CC$AbsTotalDiff <-abs(CC$TotalDiff)
ggplot(CC, aes(x=AbsTotalDiff)) +  geom_histogram(binwidth=1, fill="Blue") + xlim(0,100) + labs(title = "Absoluate Difference In Number of Respondents from 2020 to 2021", x="Number of Contacts", y="Number of Respondents")  + expand_limits(y = 30)
summary(CC$AbsTotalDiff)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    1.00    4.00   15.03    8.50  501.00 

#6-22-21
#Looking at COVID testing/symptoms in the group. Just for table analysis

#Have you been tested for COVID in the last 30 days
table(CC$covid30,useNA = 'always')
# Yes    No    NA   Total
# 48     213   10   271
#Of those 48, 46 wer negative, 1 prefered not to say, 1 didn't know yet

#Were you tested for COVID before these past 30 days?
table(CC$covidmonth,useNA = 'always')
#  Yes   No   NA  Total
# 138    123  10  271
table(CC$covidmonthresult,useNA = 'always')
#  Negative   Prefer not to say    Don't Know  <NA> 
#  34         1                        1        235 

#Have you ever tested positive for COVID?
table(CC$positiveever,useNA = 'always')
#Yes    No    prefer not to say    NA
#10     247          2             12

#What were people vaccinated against?
#Influenza
table(CC$vaccination___0,useNA = 'always')
#Yes         No
#206 (76%)   65 (24%)

#COVID - this was before max vax was available
table(CC$vaccination___1,useNA = 'always')
#Yes         No
#37 (14%)    234(86%)