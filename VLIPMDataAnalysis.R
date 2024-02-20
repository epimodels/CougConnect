#Data Analysis for the Paper "Impact of Shifting University Policies During
#the COVID-19 Pandemic on Self-Reported Employee Social Networks"
setwd("~/Lofgren/COVID-19/CougConnect")
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(gmodels)
library(expss)
library(ggplot2)
library(ggpubr)
library(naniar)
library(Hmisc)
library(lessR)
library(jtools)
IPM <- read_csv("IPMContactCleaned.csv")
IPM = subset(IPM, select=-c(...1)) #got rid of duplicate of participant ID column
View(IPM)
VL <- read_csv("VLManualCleaned.csv")


#Create a total variable for total number of contacts
#Trying to add a total variable for year 2021 (aka all the contacts they had for contact1/2/3/etc)
VL$Total2021 <- ifelse (is.na(VL$contact1), 0, ifelse (is.na(VL$contact2), 1, ifelse (is.na(VL$contact3),2, ifelse (is.na(VL$contact4),3, ifelse (is.na(VL$contact5),4,
                                                                                                                                                  ifelse(is.na(VL$contact6),5, ifelse (is.na(VL$contact7),6, ifelse (is.na(VL$contact8),7, ifelse (is.na(VL$contact9),8, ifelse (is.na(VL$contact10),9,
                                                                                                                                                                                                                                                                                 ifelse (is.na(VL$contact11),10, ifelse (is.na(VL$contact12), 11, ifelse (is.na(VL$contact13),12, ifelse(is.na(VL$contact14),13, ifelse(is.na(VL$contact15),14,
                                                                                                                                                                                                                                                                                                                                                                                                                        ifelse(is.na(VL$contact16),15, ifelse (is.na(VL$contact17),16, ifelse(is.na(VL$contact18),17, ifelse(is.na(VL$contact19),18, ifelse (is.na(VL$contact20),19,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ifelse(is.na(VL$contact21),20,ifelse(is.na(VL$contact22),21, 22))))))))))))))))))))))

table(VL$Total2021, useNA = "always")
CrossTable(VL$Total2021)
sum(VL$Total2021)
summary(VL$Total2021)
sd(VL$Total2021)

# Min.   1st Qu.  Median  Mean   3rd Qu.    Max.  Std.Dev
# 0.000   1.000   3.000   3.218   4.000   22.000    2.67
ggplot(VL, aes(x=Total2021)) + geom_histogram(binwidth=1)
ggplot(VL, aes(x=Total2021)) + theme(axis.text = element_text(size=14)) + theme(axis.title = element_text(size = 14))+ geom_histogram(binwidth=1, fill="purple") + labs(title = "Total Number of Contacts per Respondent on a Day with Virtual Learning", x="Number of Contacts",  y="Number of Respondents")


#doing it for IPM
IPM$TotalContacts <- rowSums(IPM[,c("Cont1Total","Cont2Total","Cont3Total","Cont4Total","Cont5Total", "Cont6Total", "Cont7Total","Cont8Total","Cont9Total", "Cont10Total")], na.rm= TRUE)
table(IPM$TotalContacts, useNA = "always")
sum(IPM$TotalContacts)
summary(IPM$TotalContacts)
sd(IPM$TotalContacts)
#Min.  1st Qu.  Median    Mean   3rd Qu.    Max.   Std,Dev
#1.00    3.00    7.00     40.62   27.75   1012.00    131.50
#plotting
ggplot(IPM, aes(x=TotalContacts)) + geom_histogram(binwidth=1)
ggplot(IPM, aes(x=TotalContacts))  + theme(axis.text = element_text(size=14)) + theme(axis.title = element_text(size = 14)) + geom_histogram(binwidth=1, fill="Green4") + xlim(0,120) + labs(title = "Total Number of Contacts per Respondent on a Day with In-Person Masking Learning", x="Number of Contacts", y="Number of Respondents")
#dropping contacts of 226, 682, 814, and 1012
#for paper, comparing Two medians of total contacts
wilcox.test(VL$Total2021, IPM$TotalContacts)

#TYPE OF CONTACTS
#Now doing just all contacts
#Now doing some things for all the different contacts
#Trying some things to get a better idea of how many students/etc from the data
#Subsetting contacts

#just looking at type of category
#NOw for Virtual Learning (VL)
contsTotalVL<-select(VL,contact1, contact2, contact3,contact4,contact5,contact6, contact7,contact8,contact9,contact10,contact11,contact12,contact13,contact14,contact15,contact16,contact17,contact18,contact19,contact20,contact21,contact22)
ContactsVL = data.frame(table(unlist(contsTotalVL)))
ContactsVL$percent = prop.table(ContactsVL$Freq)*100
ContactsVL
#Top 3 categories most common are Partner/Spouse (209), Child (166), Laborer (142), Colleague (113), Student (59)
sum(ContactsVL$Freq)

#IPM
contsTotalIPM<-select(IPM, contact1, contact2, contact3,contact4,contact5,contact6, contact7,contact8,contact9,contact10)
ContactsIPM = data.frame(table(unlist(contsTotalIPM)))
ContactsIPM$percent = prop.table(ContactsIPM$Freq)*100
ContactsIPM
sum(ContactsIPM$Freq)
#Top 5 categories are Colleagues (127), Partner/Spouse (93), Students (73), Laborers (67), Children (59)
#trying to combine PC VL and IPM Top 5 Types of Contacts MENTIONED in a graph

#chi-sq for paper between counts
# Create a matrix with observed counts
observed <- matrix(c(209, 166, 142, 113, 59, 22, 92, 173, 105, 339, 1420, 2639), nrow = 2, byrow = TRUE)
# Assign row and column names
rownames(observed) <- c("VL", "IPM")
colnames(observed) <- c("Partner/Spouse", "Child", "Laborer", "Colleague", "Student", "Stranger")
# Perform the chi-square test
chi_sq_result <- chisq.test(observed)
# Print the result
print(chi_sq_result)
#yup P <0.0001
#ACTUAL NUMBER OF CONTACTS PER CATEGORY
#Previous was how many times each category was mentioned, this is how many of those people
#So ex: Students is put down 100 times, but each time it might be 1 student or 100 students


#Due to limited numbers, we're not doing this for VL
#we never made multiples for them so their # of contacts == types of contacts
#Raw Numbers for VL: Partner/Spouse (209), Child (166), Laborer (142), 4th was Colleague (113)

#IPM
IPMContacts <-IPM %>% select(matches('Contact')) #Got all the variables with contact in them! Gotta be selective
IPMContacts <-subset(IPMContacts, select =c(contact1, contact2, contact3,contact4,contact5,contact6, contact7,contact8,contact9,contact10))
#now for the multiples
IPMMults <-subset(IPM, select =c(multiple1, multiple2, multiple3,multiple4,multiple5,multiple6,multiple7,multiple8,multiple9,multiple10))
#This is where library(reshape2) comes into play
IPMReshape<- cbind(IPMContacts %>%                     #turns all of row (row of respondent going from 1-124) and all 10 contact columns (contact_Type)
                     mutate(row = row_number()) %>%    #so the frist 124 entries are what contact all respondents put for contact 1, then contact2, contact3, etc
                     melt(id.var = 'row', value.name = "contact_type") %>%   
                     select(-variable),
                   IPMMults %>% mutate(row = row_number()) %>%   #do this again with all the mulitples so it's an equal thing
                     melt(id.var = 'row', value.name= "total_multiple") %>%                    #1-1 comparison and togetherness
                     select(-variable, -row)) %>% 
  group_by(contact_type) %>%                    
  summarise(total_multiple =  ifelse( n() == 1, 1,  sum(total_multiple, na.rm = TRUE)))
IPMReshape$percent = prop.table(IPMReshape$total_multiple)*100
show(IPMReshape)
sum(IPMReshape$total_multiple)
#highest total numbers are Strangers (2639), Students (1420), then Colleagues (339)

#For Paper-No PC
PercentNPC <- c( 24,2,19,3,16,2,13,6.5,7,28,4,52)
ContactType <-c("Partner","Partner","Children", "Children", "Laborers", "Laborers", "Colleagues","Colleagues", "Students","Students","Strangers","Strangers" )
Type <-c( "VL","IPM", "VL", "IPM","VL", "IPM", "VL", "IPM","VL", "IPM","VL", "IPM")
Both <- data.frame(PercentNPC,ContactType,Type)
Both$Type <- factor(Both$Type, levels = c("VL","IPM"))
percentplo <- ggplot(Both, aes(fill=Type, y=PercentNPC, x= ContactType)) + geom_bar(position = 'dodge', stat = 'identity') +
  labs(title = "(a)", # Description and Percent of the Top Types of Contacts", 
       x="Type of Contact", y="Percent") +
  #scale_fill_manual("Teaching Style", values = c('firebrick4', 'grey20')) +
  scale_fill_manual("Teaching Style", values = c('lightgray', 'black')) +
  #scale_fill_grey(start= 0.8, end = 0.2) +
  theme_bw() + theme(legend.position = c(0.2,0.8), axis.text = element_text(face='bold',size=8), axis.text.y = element_text(face='bold',size=8), axis.text.x = element_text(angle=45, vjust=1,hjust=1,face='bold',size=8), plot.title = element_text(face='bold',size=14))
percentplo

#looking at location for the 6 places people had to choose from
#'1'= 'My Home', '2' = "Work", '3' = "Leisure", '4' = "Travel",'5'= 'Shopping', '6' = "Other"
#Virtual Learning
ChoseVL<-select(VL, Contact1Location, Contact2Location, Contact3Location, Contact4Location, Contact5Location, Contact6Location, Contact7Location, Contact8Location, Contact9Location, Contact10Location,Contact11Location,Contact12Location, Contact13Location, Contact14Location, Contact15Location, Contact16Location, Contact17Location, Contact18Location, Contact19Location, Contact20Location, Contact21Location, Contact22Location)
ChosenLocVL = data.frame(table(unlist(ChoseVL)))
ChosenLocVL$percent = prop.table(ChosenLocVL$Freq)*100
ChosenLocVL
sum(ChosenLocVL$Freq)
#Top three places - Home (51%), Work (19.1%), Other (12.4%)-Mostly outdoors

#InPerson Masking
choseIPM<-select(IPM, Contact1location, Contact2location, Contact3location, Contact4location, Contact5location, Contact6location, Contact7location, Contact8location, Contact9location, Contact10location)
ChosenLocIPM = data.frame(table(unlist(choseIPM)))
ChosenLocIPM$percent = prop.table(ChosenLocIPM$Freq)*100
ChosenLocIPM
sum(ChosenLocIPM$Freq)
#Top three places - Work (39.9%), Home (34.8%), Other (8%)
#Chi-sq for paper time!
# Create a matrix with observed counts
observed2 <- matrix(c(446,167,55,12,86,108, 177,203,37,13,37,41), nrow = 2, byrow = TRUE)
# Assign row and column names
rownames(observed2) <- c("VL", "IPM")
colnames(observed2) <- c("MyHome", "Work","Leisure","Travel","Shopping","Other")
# Perform the chi-square test
chi_sq_result2 <- chisq.test(observed2)
# Print the result
print(chi_sq_result2)


#Graph of Location chosen of all three times
#used in Paper- No PC
Location <- c("1","2","3","4","5","6","1","2","3","4","5","6")
LocPercentNPC <-c(51,19,6,1,10,12, 35,40,7,3,7,8)
LocType <- c("VL","VL","VL","VL","VL","VL","IPM","IPM","IPM","IPM","IPM","IPM")
BothLoc<- data.frame(Location,LocPercentNPC,LocType)
BothLoc$LocType <- factor(BothLoc$LocType, levels = c("VL","IPM"))
#par(mfrow=c(1,2))
locPerplo <- 
  ggplot(BothLoc, aes(fill=LocType, y=LocPercentNPC, x= Location)) + geom_bar(position = 'dodge', stat = 'identity') +
  labs(title = "(b)", #Description and Percent of Locations Respondents Chose
       x="Location", y="Percent") +
  scale_x_discrete(labels =c('1'= 'My Home', '2' = "Work", '3' = "Leisure", '4' = "Travel",'5'= 'Shopping', '6' = "Other" ))+
  #scale_fill_manual("Teaching Style", values = c('firebrick4', 'grey20')) +
  scale_fill_manual("Teaching Style", values = c('lightgrey', 'black')) +
  #scale_fill_grey(start= 0.8, end = 0.2) + 
  theme_bw()+ theme(legend.position = c(0.8,0.8), axis.text = element_text(face='bold',size=8), axis.text.y = element_text(face='bold',size=10), axis.text.x = element_text(angle=45, vjust=1,hjust=1,face='bold',size=10), plot.title = element_text(face='bold',size=12))
locPerplo
percentplo <- 
  ggplot(Both, aes(fill=Type, y=PercentNPC, x= ContactType)) + geom_bar(position = 'dodge', stat = 'identity') +
  labs(title  = "(a)", #Description and Percent of the Top Types of Contacts", 
       x="Type of Contact", y="Percent") +
  #scale_fill_manual("Teaching Style", values = c('firebrick4', 'grey20')) +
  scale_fill_manual("Teaching Style", values = c('lightgray', 'black')) +
  #scale_fill_grey(start= 0.8, end = 0.2) +
  theme_bw() + theme(legend.position = c(0.2,0.8), axis.text = element_text(face='bold',size=8), axis.text.y = element_text(face='bold',size=10), axis.text.x = element_text(angle=45, vjust=1,hjust=1,face='bold',size=10), plot.title = element_text(face='bold',size=12))
percentplo
CrossTable(ChosenLocIPM$Freq, ChosenLocVL$Freq, chisq = TRUE)

library(gridExtra)
require(gridExtra)
plot1 <- percentplo
plot2 <- locPerplo
grid.arrange(plot1, plot2, ncol=2)

#Location of places chosen

#Figuring out how many people work/live in Pullman 
#On which WSU Campus do you work?
#0-Pullman, 1-Spokane, 2-Vancouver, 3- Tri-Cities, 4-Everett, 5 - Extension 6 -Seattle
#VL
VL$PullmanLive <- NA
VL$PullmanLive <- ifelse (VL$pullmanyn ==1, "Yes", "No")
table(VL$PullmanLive, useNA = 'always')
VL$PullmanWork <- NA
VL$PullmanWork[VL$campus_loc == 0 ] <-"Yes"
VL$PullmanWork[VL$campus_loc >=1 &  VL$campus_loc <=6] <-"No"
VL$PullmanWork[VL$npullman == "Moscow Idaho"] <-"Yes"
VL$PullmanWork[VL$npullman == "Bremerton, WA"] <-"No"
VL$PullmanWork[VL$locationcontact2 =="WSU Spokane"] <-"No"
VL$PullmanWork[VL$locationcontact2 =="WSU Extension"] <-"No"
VL$PullmanWork[VL$campus_loc == 0 && VL$pullmanyn == 99] <-"Yes"
table(VL$PullmanWork, VL$campus_loc, useNA = 'always')
VL$PullmanBoth <- NA
VL$PullmanBoth <- ifelse (VL$PullmanLive == "Yes" | VL$PullmanWork == "Yes", "Yes", "No")
VL$PullmanBoth[VL$npullman == "Moscow Idaho"] <-"Yes"
VL$PullmanBoth[VL$npullman == "Bremerton, WA"] <-"No"
VL$PullmanBoth[VL$locationcontact2 =="WSU Spokane"] <-"No"
VL$PullmanBoth[VL$locationcontact2 =="WSU Extension"] <-"No"
table(VL$PullmanBoth, useNA = "always")
#   No  Yes <NA> 
#   56  166   49 

#InPerson Masking
table(IPM$pullmanyn, useNA = "always")
#0 = No, 1 = Yes
#    0    1 <NA>   #so 55.6% say they don't live in pullman
#   69   43   12   #43% say they do
table(IPM$campus_loc, useNA = "always")
# 
# 0/Pullman    1/Spokane  2/Vancouver   3/Tri-Cities  4/Everett    5/Extension   6/Seattle <NA> 
#   59         18          15           11            1            7             2          11 
IPM$campus_loc[is.na(IPM$campus_loc)] <-99  #will need this for later
table(IPM$pullmanyn, IPM$campus_loc, useNA = "always")
#           Campus Locations  
#          0   1  2  3  4  5  6    99
# No/0     15 18 15 11  1  7  2    0
# Yes/1    43  0  0  0  0  0  0    0
# <NA>      1  0  0  0  0  0  0   11
#So 15 of those working in Pullman don't live there, and 1 is NA but works there
#11 pure NAs
#so of our 124 people, 59 or 47% live and/or work in Pullman

table(IPM$npullman) #Cleaning up the table a little bit
IPM$npullman[IPM$npullman=='Clarkston'] <- "Clarkston, WA"
IPM$npullman[IPM$npullman=='Kennewick'] <- "Kennewick, WA"
IPM$npullman[IPM$npullman=='Palouse'] <- "Palouse, WA"
IPM$npullman[IPM$npullman=='Portland, Oregon'] <- "Portland, OR"
IPM$npullman[IPM$npullman=='Richland']<- "Richland, WA"
IPM$npullman[IPM$npullman=='Spokane'] <- "Spokane, WA"
IPM$npullman[IPM$npullman=='Vancouver'] <- "Vancouver, WA"
IPM$npullman[IPM$npullman=='Vancouver WA'] <- "Vancouver, WA"
IPM$npullman[IPM$npullman=='Moscow, Idaho'] <- "Moscow, ID"
table(IPM$npullman, IPM$campus_loc, useNA = 'always')
#Lot more other places then the first pulse (Pc/VL) people
IPM$PullmanLive <- NA
IPM$PullmanLive <- ifelse (IPM$pullmanyn ==1, "Yes", "No")
table(IPM$PullmanLive, useNA = 'always')
IPM$PullmanWork <- NA
IPM$PullmanWork[IPM$campus_loc == 0 ] <-"Yes"
IPM$PullmanWork[IPM$campus_loc >=1 &  IPM$campus_loc <=6] <-"No"
IPM$PullmanWork[IPM$locationcontact4 =="WSU Pullman"] <-"Yes"
IPM$PullmanWork[IPM$locationcontact2 =="WSU Vancouver"] <-"No"
IPM$PullmanWork[IPM$campus_loc == 0 & IPM$pullmanyn == 99] <-"Yes"
table(IPM$PullmanWork, IPM$campus_loc, useNA = 'always')
IPM$PullmanBoth <- NA
IPM$PullmanBoth <- ifelse (IPM$PullmanLive == "Yes" | IPM$PullmanWork == "Yes", "Yes", "No")
IPM$PullmanBoth[IPM$locationcontact4 =="WSU Pullman"] <-"Yes"
IPM$PullmanBoth[IPM$locationcontact2 =="WSU Vancouver"] <-"No"
table(IPM$PullmanBoth, useNA = "always")
#   No  Yes <NA> 
#   55   61    8 
# table(VL$PullmanBoth, useNA = "always")
#   No  Yes <NA> 
#   56  166   49 
#chi-sq for paper between Pullman
# Create a matrix with observed counts
observed1 <- matrix(c(56, 166, 49,55,61,8), nrow = 2, byrow = TRUE)
# Assign row and column names
rownames(observed1) <- c("VL", "IPM")
colnames(observed1) <- c("No", "Yes", "NA")
# Perform the chi-square test
chi_sq_result1 <- chisq.test(observed1)
# Print the result
print(chi_sq_result1)


#Doing tibbles to determine mean of Pullman/Non contacts by period
#VL
VL %>% group_by(PullmanBoth) %>%
  summarise (N=n(),median = median(Total2021), IQR=IQR(Total2021), mean = mean(Total2021), std = sd(Total2021))
agostino.test(VL$Total2021)
#IPM
IPM %>% group_by(PullmanBoth) %>%
  summarise (N=n(),median = median(TotalContacts), IQR=IQR(TotalContacts) ,Contacts = mean(TotalContacts), std = sd(TotalContacts))
agostino.test(IPM$TotalContacts)
#Essential Worker
#VL
table(VL$essentialjobyn, useNA = 'always')
VL %>% group_by(essentialjobyn) %>%
  summarise (N=n(),median = median(Total2021), IQR=IQR(Total2021), mean = mean(Total2021), std = sd(Total2021))

#IPM
table(IPM$essentialjobyn, useNA = 'always')
IPM %>% group_by(essentialjobyn) %>%
  summarise (N=n(),median = median(TotalContacts), IQR=IQR(TotalContacts) , mean=mean(TotalContacts), std = sd(TotalContacts))
# essentialjobyn    N median   IQR  mean   std
#             0    57      6  13   20.8   38.3
#             1    56     12  31.2 67.1  189. 
#            NA    11      4   5.5  8.55  15.4

#Box plot of essential by contacts, try and do by teaching style?
VLEss <-ggplot(VL, aes(x=as.factor(essentialjobyn), y=Total2021)) + geom_boxplot(fill='palegreen', alpha=0.2) + xlab("Essential Job")
VLEss
IPMEss <-ggplot(IPM, aes(x=as.factor(essentialjobyn), y=TotalContacts)) + geom_boxplot(fill='mediumorchid1', alpha=0.2) + xlab("Essential Job") # ylim(0,175)
IPMEss #Big empty space between <250 and ~600
#figure out breaks? And how to put all together
VLessential <- data.frame(VL$essentialjobyn, VL$Total2021)
VLessential$ID <- "VL"
VLessential <- rename_(VLessential, 'essentialjobyn' = 'VL.essentialjobyn', 'TotalContacts' = 'VL.Total2021')

IPMessential <- data.frame(IPM$essentialjobyn, IPM$TotalContacts)
IPMessential$ID <- "IPM"
IPMessential <- rename_(IPMessential,'essentialjobyn' = 'IPM.essentialjobyn', 'TotalContacts' = 'IPM.TotalContacts')


EssVLIPM <-rbind(VLessential, IPMessential)
#Now to do this for all three- didn't end up in paper, but still fun
PCEss <-ggplot(EssVLIPM, aes(x=as.factor(essentialjobyn), y=TotalContacts, fill=ID)) + geom_boxplot() + labs(title = "Boxplots of number of contacts reported by essential workers or not, by Teaching Delivery", x="Essential Job")  + facet_wrap(~ID, scale="free")+
  scale_x_discrete(labels =c('1'= 'Essential', '0'= 'Non-Essential')) + stat_summary(fun.y = mean, geom="point", shape = 8, size=4, color="black")
PCEss
#Tying to do this for a violin plot
PCEssv <-ggplot(EssVLIPM, aes(x=as.factor(essentialjobyn), y=TotalContacts, fill=ID)) + geom_violin() + labs(title = "Violin plots of number of contacts reported by essential workers or not, by Teaching Delivery", x="Essential Job")  + facet_wrap(~ID, scale="free")+
  scale_x_discrete(labels =c('1'= 'Essential', '0'= 'Non-Essential')) + stat_summary(fun.y = median, geom="point", shape = 8, size=4, color="black")
PCEssv

#Now do this for Profession, first just Faculty/Staff
#VL
#VL %>% group_by(Faculty) %>%
VL %>% group_by(OccType) %>%
  summarise (N=n(), median=median(Total2021), IQR=IQR(Total2021),mean = mean(Total2021), std = sd(Total2021))
VLJob <- data.frame(VL$Faculty, VL$Total2021)
VLJob$ID <- "VL"
VLJob <- rename_(VLJob, 'Faculty' = 'VL.Faculty', 'TotalContacts' = 'VL.Total2021')

#IPM
#IPM %>% group_by(Faculty) %>%
IPM %>% group_by(OccType) %>%
  summarise (N=n(),median = median(TotalContacts), IQR=IQR(TotalContacts) , mean=mean(TotalContacts), std = sd(TotalContacts))
IPMJob <- data.frame(IPM$Faculty, IPM$TotalContacts)
IPMJob$ID <- "IPM"
IPMJob <- rename_(IPMJob, 'Faculty' = 'IPM.Faculty', 'TotalContacts' = 'IPM.TotalContacts')

#rbind together
JobVLIPM <-rbind(VLJob,IPMJob)
#Now to do this for all three
BoxJob <-ggplot(JobVLIPM, aes(x=as.factor(Faculty), y=TotalContacts, fill=ID)) + geom_boxplot() + labs(title = "Boxplots of number of contacts reported by faculty and staff, by Teaching Delivery", x="Essential Job")  + facet_wrap(~ID, scale="free") +
  stat_summary(fun.y = mean, geom="point", shape = 3, size=4, color="black")
BoxJob
#VioPlot
VioJob <-ggplot(JobVLIPM, aes(x=as.factor(Faculty), y=TotalContacts, fill=ID)) + geom_violin() + labs(title = "Violin plot of number of contacts reported by faculty and staff, by Teaching Delivery", x="Essential Job")  + facet_wrap(~ID, scale="free") +
  stat_summary(fun.y = median, geom="point", shape = 3, size=4, color="black")
VioJob

#First doing Time stuff
#Looking at Time
#Reminder: 0 is <10min, 1 is 10-30, 2-30-60, 3 >1hr

#VL -
TimeVL <-select(VL, timecontact1, timecontact2, timecontact3, timecontact4, timecontact5, timecontact6, timecontact7 , timecontact8, timecontact9, timecontact10, timecontact11, timecontact12, timecontact13, timecontact14, timecontact15, timecontact16, timecontact17, timecontact18, timecontact19, timecontact20, timecontact21,timecontact22)
VL$timecontact1[VL$timecontact1=='Less than 10 minutes'] <- "0"
TimeContVL = data.frame(table(unlist(TimeVL)))
TimeContVL$percent = prop.table(TimeContVL$Freq)*100
TimeContVL

#IPM 
TimeIPM<-select(IPM, timecontact1,timecontact2, timecontact3,timecontact4,timecontact5,timecontact6,timecontact7,timecontact8,timecontact9,timecontact10)
TimeContIPM = data.frame(table(unlist(TimeIPM)))
TimeContIPM$percent = prop.table(TimeContIPM$Freq)*100
TimeContIPM


#Doing a Close Contacts for Duration and Where

library(forcats)
library(scales)
#VL
CloseContactVL <-dplyr::select(VL, touchcontact1, touchcontact2, touchcontact3, touchcontact4, touchcontact5, touchcontact6, touchcontact7 , touchcontact8,touchcontact9, touchcontact10, touchcontact11, touchcontact12, touchcontact13, touchcontact14, touchcontact15, touchcontact16, touchcontact17, touchcontact18, touchcontact19, touchcontact20, touchcontact21,touchcontact22)
CloseConVL = data.frame(table(unlist(CloseContactVL)))
CloseConVL$percent = prop.table(CloseConVL$Freq)*100
CloseConVL #PRetty Even SPlit between Close (47%) and Non ()
# Var1 Freq  percent
# 1   No  462 53.34873
# 2  Yes  404 46.65127
VLClosecontact<- cbind(CloseContactVL %>%                     #turns all of row (row of respondent going from 1-124) and all 10 contact columns (contact_Type)
                         mutate(row = row_number()) %>%    #so the frist 124 entries are what contact all respondents put for contact 1, then contact2, contact3, etc
                         melt(id.var = 'row', value.name = "Contact") %>%   
                         select(-variable),
                       TimeVL %>% mutate(row = row_number()) %>%   #do this again with all the mulitples so it's an equal thing
                         melt(id.var = 'row', value.name= "Time") %>%                    #1-1 comparison and togetherness
                         select(-variable, -row))
table(VLClosecontact$Contact, VLClosecontact$Time, useNA = 'always')
chisq.test(VLClosecontact$Contact, VLClosecontact$Time)

Time <-c("< 10 min", "10-30 min", "30-60 min", "1+hr","< 10 min", "10-30 min", "30-60 min", "1+hr")
VTimePercent <-c(244,49,58,110,10,11,26,357)
VCloseCon <- c(rep("No",4), rep("Yes",4))
VLconandtime<-data.frame(Time,VTimePercent,VCloseCon)
v <- ggplot(VLconandtime, aes(fill=VCloseCon, y=VTimePercent, x=Time))+
  geom_bar(position="fill", stat = "identity") + scale_y_continuous(labels=percent) +
  # scale_fill_manual("CloseContacts", values = c('navyblue','khaki2'))+
  scale_fill_manual("CloseContacts", values = c('maroon','darkgrey'))+
  labs(title = "Mean Proportion of Contacts that Involved Physical Contact by Duration for VL", y="Percent")
v + aes(x=fct_inorder(Time)) + labs(x="Duration")

#IPM
CloseContactIPM <-dplyr::select(IPM, touchcontact1, touchcontact2, touchcontact3,touchcontact4,touchcontact5, touchcontact6,touchcontact7, touchcontact8, touchcontact9, touchcontact10)
CloseConIPM = data.frame(table(unlist(CloseContactIPM)))
CloseConIPM$percent = prop.table(CloseConIPM$Freq)*100
CloseConIPM
IPMClosecontact<- cbind(CloseContactIPM %>%                     #turns all of row (row of respondent going from 1-124) and all 10 contact columns (contact_Type)
                          mutate(row = row_number()) %>%    #so the frist 124 entries are what contact all respondents put for contact 1, then contact2, contact3, etc
                          melt(id.var = 'row', value.name = "Contact") %>%   
                          select(-variable),
                        TimeIPM %>% mutate(row = row_number()) %>%   #do this again with all the mulitples so it's an equal thing
                          melt(id.var = 'row', value.name= "Time") %>%                    #1-1 comparison and togetherness
                          select(-variable, -row))
#%>%       group_by(Time & Contact)
Ipm <- table(IPMClosecontact$Contact, IPMClosecontact$Time, useNA = 'always')

chisq.test(IPMClosecontact$Contact, IPMClosecontact$Time)

Time <-c("< 10 min", "10-30 min", "30-60 min", "1+hr","< 10 min", "10-30 min", "30-60 min", "1+hr")
ITimePercent <-c(99,72,74,72,5,6,12,141)
ICloseCon <- c(rep("No",4), rep("Yes",4))
IPMconandtime<-data.frame(Time,ITimePercent,ICloseCon)
i <- ggplot(IPMconandtime, aes(fill=ICloseCon, y=ITimePercent, x=Time))+
  geom_bar(position="fill", stat = "identity") +scale_y_continuous(labels=percent)+
  scale_fill_manual("CloseContacts", values = c('maroon','darkgrey'))+
  labs(title = "Mean Proportion of Contacts that Involved Physical Contact by Duration for IPM", y="Percent")
i + aes(x=fct_inorder(Time)) + labs(x="Duration")

#chi-sq for paper between Close Contacts
# Create a matrix with observed counts
observed2 <- matrix(c(462,404,318,164), nrow = 2, byrow = TRUE)
# Assign row and column names
rownames(observed2) <- c("VL", "IPM")
colnames(observed2) <- c("No", "Yes")
# Perform the chi-square test
chi_sq_result2 <- chisq.test(observed2)
# Print the result
print(chi_sq_result2)
#ONlto Location chosen
library(scales)
#VL
ConChoseVL<- cbind(CloseContactVL %>%                     #turns all of row (row of respondent going from 1-124) and all 10 contact columns (contact_Type)
                     mutate(row = row_number()) %>%    #so the frist 124 entries are what contact all respondents put for contact 1, then contact2, contact3, etc
                     melt(id.var = 'row', value.name = "Contact") %>%
                     dplyr::select(-variable),
                   ChoseVL %>% mutate(row = row_number()) %>%   #do this again with all the mulitples so it's an equal thing
                     melt(id.var = 'row', value.name = "Location") %>%     
                     dplyr::select(-variable, -row)) %>% 
  group_by(Location)
table(ConChoseVL$Contact, ConChoseVL$Location, useNA = 'always')
chisq.test(ConChoseVL$Contact, ConChoseVL$Location)
#1=My home 2=Work 3=Leisure 4=Travel 5=Shopping 6=Other
Location <-c("My Home", "Work", "Leisure", "Travel", "Shopping", "Other","My Home", "Work", "Leisure", "Travel", "Shopping", "Other")
LocPercent <-c(73,161,49,11,83,84,371,3,5,1,1,23)
CloseCon <- c(rep("No",6), rep("Yes",6))
conandloc<-data.frame(Location,LocPercent,CloseCon)
V2 <- ggplot(conandloc, aes(fill=CloseCon, y=LocPercent, x=Location))+
  geom_bar(position="fill", stat = "identity") +scale_y_continuous(labels=percent) +
  scale_fill_manual("CloseContacts", values = c('navyblue','khaki2'))+
  labs(title = "Mean Proportion of Contacts that Involved Physical Contact by Chosen Location for Virtual Learning", y="Percent")
V2 + aes(x=fct_inorder(Location)) + labs(x="Location")


#IPM
#1=My home 2=Work 3=Leisure 4=Travel 5=Shopping 6=Other
#Using ChoseIPM from earlier in the script
ConChoseIPM<- cbind(CloseContactIPM %>%                     #turns all of row (row of respondent going from 1-124) and all 10 contact columns (contact_Type)
                      mutate(row = row_number()) %>%    #so the frist 124 entries are what contact all respondents put for contact 1, then contact2, contact3, etc
                      melt(id.var = 'row', value.name = "Contact") %>%
                      dplyr::select(-variable),
                    choseIPM %>% mutate(row = row_number()) %>%   #do this again with all the mulitples so it's an equal thing
                      melt(id.var = 'row', value.name = "Location") %>%     
                      dplyr::select(-variable, -row)) %>% 
  group_by(Location)
table(ConChoseIPM$Contact, ConChoseIPM$Location, useNA = 'always')
chisq.test(ConChoseIPM$Contact, ConChoseIPM$Location)
#1=My home 2=Work 3=Leisure 4=Travel 5=Shopping 6=Other
Location <-c("My Home", "Work", "Leisure", "Travel", "Shopping", "Other","My Home", "Work", "Leisure", "Travel", "Shopping", "Other")
LocPercent <-c(26,181,31,12,35,32,143,8,3,1,0,9)
CloseCon <- c(rep("No",6), rep("Yes",6))
conandloc<-data.frame(Location,LocPercent,CloseCon)
I2 <- ggplot(conandloc, aes(fill=CloseCon, y=LocPercent, x=Location))+
  geom_bar(position="fill", stat = "identity") +scale_y_continuous(labels=percent) +
  labs(title = "Mean Proportion of Contacts that Involved Physical Contact by Chosen Location for IPM", y="Percent")
I2 + aes(x=fct_inorder(Location)) + labs(x="Location")

#Doing some Regression with things here
#NEGATIVE BINOMINAL REGRESSION- UNIVARIATE
#Basic Poisson with essential, pullman, and faculty as some independent variables to understand # of total contacts in 2020
library(sandwich)
library(lmtest)
#trying same with with negative binomial
library(MASS)
table(VL$essentialjobyn1)
VL$essentialjobyn1 <- ifelse(VL$essentialjobyn ==0, "Yes", "No")
essentialnegbiomVLtry <- glm.nb(Total2021 ~ essentialjobyn1, data = VL,trace=TRUE)
summary(essentialnegbiomVLtry)
(est <- cbind(Estimate = exp(coef(essentialnegbiomVLtry)), exp(confint(essentialnegbiomVLtry))))
#AIC 1141.9
essentialnegbiomIPM <- glm.nb(TotalContacts ~ essentialjobyn, data = IPM,trace=TRUE)
summary(essentialnegbiomIPM)
exp(coef(essentialnegbiomIPM)[2])
(est <- cbind(Estimate = coef(essentialnegbiomIPM), confint(essentialnegbiomIPM, level = 0.95)))
exp(est)
#AIC - 1002.5

#gonna stick with Neg. VL is close enough but better fit is with Neg)
facultynegbiomVL <- glm.nb(Total2021 ~ Faculty, data = VL,trace=TRUE)
summary(facultynegbiomVL)
summ(facultynegbiomVL, exp=T)
(est <- cbind(Estimate = coef(facultynegbiomVL), confint(facultynegbiomVL)))
exp(est)

facultynegbiomIPM <- glm.nb(TotalContacts ~ Faculty, data = IPM,trace=TRUE)
summary(facultynegbiomIPM)
(est <- cbind(Estimate = coef(facultynegbiomIPM), confint(facultynegbiomIPM)))
exp(est)
levels(VL$OccType)
est <- cbind(Estimate = coef(OccnegbiomPC), confint(OccnegbiomPC))
OccnegbiomVL <- glm.nb(Total2021 ~ OccType, data = VL,trace=TRUE)
summary(OccnegbiomVL)
(est <- cbind(Estimate = coef(OccnegbiomVL), confint(OccnegbiomVL)))
exp(est)
OccnegbiomIPM <- glm.nb(TotalContacts ~ OccType, data = IPM,trace=TRUE)
summary(OccnegbiomIPM)
(est <- cbind(Estimate = coef(OccnegbiomIPM), confint(OccnegbiomIPM)))
exp(est)

#FLu shot
CrossTable(IPM$vaccination___1, IPM$vaccination___0) #covid shot
table(VL$vaccination___0)
table(IPM$vaccination___0)
flunbVL <- glm.nb(Total2021 ~ vaccination___0, data = VL, trace=TRUE)
summary(flunbVL)
(est <- cbind(Estimate = coef(flunbVL), confint(flunbVL)))
exp(est)
flunbIPM <- glm.nb(TotalContacts ~ vaccination___0, data = IPM, trace=TRUE)
summary(flunbIPM)
(est <- cbind(Estimate = coef(flunbIPM), confint(flunbIPM)))
exp(est)

#COVID Shot
C19nbVL <- glm.nb(Total2021 ~ vaccination___1, data = VL, trace=TRUE)
summary(C19nbVL)
(est <- cbind(Estimate = coef(C19nbVL), confint(C19nbVL)))
exp(est)
C19nbIPM <- glm.nb(TotalContacts ~ vaccination___1, data = IPM, trace=TRUE)
summary(C19nbIPM)
(est <- cbind(Estimate = coef(C19nbIPM), confint(C19nbIPM)))
exp(est)

#campus? Or just PUllman/not
#FYI -0=Pullman, 1=SPokane, 2=Vancouver, 3= Tri-Cities, 4=Everett 5 = Extension 6= Seattle
table(VL$PullmanBoth, useNA = 'always')
#   No  Yes <NA> 
#   56  166   49 
table(IPM$PullmanBoth, useNA = 'always')
#   No  Yes <NA> 
#   55   61    8
#Better split, so we're not comparing Seattle to Pullman
PullnbVL <- glm.nb(Total2021 ~PullmanBoth, data = VL, trace=TRUE)
summary(PullnbVL)
(est <- cbind(Estimate = coef(PullnbVL), confint(PullnbVL)))
exp(est)
PullnbIPM <- glm.nb(TotalContacts ~PullmanBoth, data = IPM, trace=TRUE)
summary(PullnbIPM)
(est2 <- cbind(Estimate = coef(PullnbIPM), confint(PullnbIPM)))
exp(est2)

#typical day? Shows that is this normal for that day they asked about
TypnbVL <- glm.nb(Total2021 ~typcialyn, data = VL, trace=TRUE)
summary(TypnbVL)
(est1 <- cbind(Estimate = coef(TypnbVL), confint(TypnbVL)))
exp(est1)
TypnbIPM <- glm.nb(TotalContacts ~typcialyn, data = IPM, trace=TRUE)
summary(TypnbIPM)
(est2 <- cbind(Estimate = coef(TypnbIPM), confint(TypnbIPM)))
exp(est2)

#Doing teach classes just for IPM
table(IPM$teachyn)
TeachnbIPM <- glm.nb(TotalContacts ~teachyn, data = IPM, trace=TRUE)
summary(TeachnbIPM)
(est2 <- cbind(Estimate = coef(TeachnbIPM), confint(TeachnbIPM)))
exp(est2)

#Masking for close contacts
MaskingVL <-dplyr::select(VL, maskcontact1,maskcontact2, maskcontact3,maskcontact4,maskcontact5, maskcontact6,maskcontact7, maskcontact8, maskcontact9, maskcontact10,maskcontact11,maskcontact12,maskcontact13,maskcontact14,maskcontact15, maskcontact16, maskcontact17, maskcontact18,maskcontact19, maskcontact20, maskcontact21, maskcontact22)
MaskVL = data.frame(table(unlist(MaskingVL)))
MaskVL$percent = prop.table(MaskVL$Freq)*100
MaskVL
#                 Var1 Freq    percent
# 1 I did not they did   10  1.1428571
# 2 I did some did not    8  0.9142857
# 3 I did they did not   26  2.9714286
# 4        Neither did  467 53.3714286
# 5        We both did  364 41.6000000
VLMaskClose<- cbind(CloseContactVL %>%                     #turns all of row (row of respondent going from 1-124) and all 10 contact columns (contact_Type)
                      mutate(row = row_number()) %>%    #so the frist 124 entries are what contact all respondents put for contact 1, then contact2, contact3, etc
                      melt(id.var = 'row', value.name = "Contact") %>%   
                      dplyr::select(-variable),
                    MaskingVL %>% mutate(row = row_number()) %>%   #do this again with all the mulitples so it's an equal thing
                      melt(id.var = 'row', value.name= "Mask") %>%                    #1-1 comparison and togetherness
                      dplyr::select(-variable, -row))
#%>%       group_by(Time & Contact)
table(VLMaskClose$Contact, VLMaskClose$Mask, useNA = 'always')
chisq.test(IPMClosecontact$Contact, IPMClosecontact$Time)

MaskingIPM <-dplyr::select(IPM, maskcontact1,maskcontact2, maskcontact3,maskcontact4,maskcontact5, maskcontact6,maskcontact7, maskcontact8, maskcontact9, maskcontact10)
MaskIPM = data.frame(table(unlist(MaskingIPM)))
MaskIPM$percent = prop.table(MaskIPM$Freq)*100
MaskIPM
#                 Var1 Freq    percent
# 1 I did not some did    4  0.7936508
# 2 I did not they did   10  1.9841270
# 3 I did some did not   23  4.5634921
# 4 I did they did not   10  1.9841270
# 5        Neither did  227 45.0396825
# 6        We both did  230 45.6349206

IPMMaskClose<- cbind(CloseContactIPM %>%                     #turns all of row (row of respondent going from 1-124) and all 10 contact columns (contact_Type)
                       mutate(row = row_number()) %>%    #so the frist 124 entries are what contact all respondents put for contact 1, then contact2, contact3, etc
                       melt(id.var = 'row', value.name = "Contact") %>%   
                       dplyr::select(-variable),
                     MaskingIPM %>% mutate(row = row_number()) %>%   #do this again with all the mulitples so it's an equal thing
                       melt(id.var = 'row', value.name= "Mask") %>%                    #1-1 comparison and togetherness
                       dplyr::select(-variable, -row))
#%>%       group_by(Time & Contact)
table(IPMMaskClose$Contact, IPMMaskClose$Mask, useNA = 'always')
chisq.test(IPMMaskClose$Contact, IPMMaskClose$Mask)
#chi-sq for paper between Masking Discordant pairs
# Create a matrix with observed counts
obs1 <-matrix(c(44,364,467, 47, 227, 230), nrow = 2, byrow = TRUE)
# Assign row and column names
rownames(obs1) <- c("VL", "IPM")
colnames(obs1) <- c("Discordant", "Both", "Neither")
# Perform the chi-square test
chi_sq1 <- chisq.test(obs1)
# Print the result
print(chi_sq1)



#Close contacts and contacts
IPMcontactsClose<- cbind(CloseContactIPM %>%                     #turns all of row (row of respondent going from 1-124) and all 10 contact columns (contact_Type)
                           mutate(row = row_number()) %>%    #so the frist 124 entries are what contact all respondents put for contact 1, then contact2, contact3, etc
                           melt(id.var = 'row', value.name = "Contact") %>%   
                           dplyr::select(-variable),
                         contsTotalIPM %>% mutate(row = row_number()) %>%   #do this again with all the mulitples so it's an equal thing
                           melt(id.var = 'row', value.name= "Type") %>%                    #1-1 comparison and togetherness
                           dplyr::select(-variable, -row))
#%>%       group_by(Time & Contact)
table(IPMcontactsClose$Contact, IPMcontactsClose$Type, useNA = 'always')
table(IPMcontactsClose$Contact, useNA = 'always')
#Main close contacts Partner/Spouse (85),  Children (45),Family members (8) 
#Others Children and Friends (1), Colleagues (4), Friends (6), HCW (2), Laborers(2),Parents (6), Patients (2)
#Roomate (1), Strangers (1), Students and Colleagues (1)

VLcontactsClose<- cbind(CloseContactVL %>%                     #turns all of row (row of respondent going from 1-124) and all 10 contact columns (contact_Type)
                          mutate(row = row_number()) %>%    #so the frist 124 entries are what contact all respondents put for contact 1, then contact2, contact3, etc
                          melt(id.var = 'row', value.name = "Contact") %>%   
                          dplyr::select(-variable),
                        contsTotalVL %>% mutate(row = row_number()) %>%   #do this again with all the mulitples so it's an equal thing
                          melt(id.var = 'row', value.name= "Type") %>%                    #1-1 comparison and togetherness
                          dplyr::select(-variable, -row))
#%>%       group_by(Time & Contact)
table(VLcontactsClose$Contact, VLcontactsClose$Type, useNA = 'always')
#Main close contacts Partner/Spouse (199), Child (147), Family Member (20)
#Also Friend (7), HCW (12), Laborer(3) Other (4), Parent (7) Stranger(1) Student (1)
CloseConIPM


#doing the graph for ERic for paper
Vl_TotalConacts <- VL %>% filter(!is.na(Total2021)) %>% group_by(Total2021,)%>% summarise(freq=n()) %>% mutate(percentage = freq / sum(freq) * 100)
IPM_TotalContacts<- IPM %>% filter(!is.na(TotalContacts)) %>% group_by(TotalContacts,)%>% summarise(freq=n()) %>% mutate(percentage = freq / sum(freq) * 100)



#Trying to do a log plot-VL has zeros
VL$LogTotalContact <-VL$Total2021
VL$LogTotalContact[VL$Total2021 == 0] <- 0.001
summary(VL$LogTotalContact)
VL$LogTotalContact <- log10(VL$LogTotalContact)
#Now IPM
IPM$LogTotalContact <-IPM$TotalContacts
summary(IPM$LogTotalContact)
IPM$LogTotalContact <- log10(IPM$LogTotalContact)
Vl_TotalConacts <- VL %>% filter(!is.na(LogTotalContact)) %>% group_by(LogTotalContact,Total2021)%>% summarise(freq=n()) %>% mutate(percentage = freq / sum(freq) * 100)
IPM_TotalContacts<- IPM %>% filter(!is.na(LogTotalContact)) %>% group_by(LogTotalContact,TotalContacts)%>% summarise(freq=n()) %>% mutate(percentage = freq / sum(freq) * 100)
Vl_TotalConacts$LogTotalContact1[Vl_TotalConacts$Total2021 ==0] <- NA
Vl_TotalConacts$LogTotalContact1 <-Vl_TotalConacts$LogTotalContact
#Graphs
library(ggpubr)
par(mfrow=c(1,1))
dev.off()
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE),
       widths=c(3,1), heights=c(1,2))
Vlplot<-plot(Vl_TotalConacts$LogTotalContact,Vl_TotalConacts$freq, main = "(a)                     Virtual Learning", xlab = "Log(Number of Contacts)", ylab = "Frequency",xlim = c(0, 3.25), col = "blue", pch=16, cex= 2) 
IPMplot<-plot(IPM_TotalContacts$LogTotalContact,IPM_TotalContacts$freq, main = "(b)                     In-Person Masking", xlab = "Log(Number of Contacts)", ylab = "Frequency", xlim = c(0, 3.25), col = "maroon",  pch=16, cex= 1.5)
v
v <- ggplot(bothzero, aes(fill=Zeros, y=ZeroPercent, x=SurveyPeriod))+
  geom_bar(position="fill", stat = "identity") +scale_y_continuous(labels=percent)+
  scale_fill_manual("Proportion of Zeros", values = c('purple','black'))+
  labs(title = "(c)                     Mean Proportion of Respondents that had Zero or 1+ Contacts", y="Percent")

IPMplot<-plot(IPM_TotalContacts$TotalContacts,IPM_TotalContacts$freq, main = "(b)                     In-Person Masking", xlab = "Number of Contacts", ylab = "Frequency", col = "maroon",  pch=16, cex= 1.5)
library(plotrix)
IPMplot<-gap.plot(IPM_TotalContacts$TotalContacts,IPM_TotalContacts$freq, main = "(b)                     In-Person Masking", xlab = "Number of Contacts", ylab = "Frequency", col = "maroon",  pch=16, cex= 1.5)
gap.axis(side = 1, at = c(230, 630), gap = c(0.1, 0.2), col = "white")

library(ggplot2)
library(ggbreak)
library(patchwork)
library(ggpubr)
library(scales)
IPMplot2 <- ggplot(IPM_TotalContacts, aes(x=LogTotalContact, y=freq)) + geom_point(color="maroon", fill="maroon", size=3.5) + xlim(0, 3.25)+ scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) + scale_y_continuous(breaks = pretty_breaks(n=8))+
  labs(title = "In-Person Masking",  x="Log(Number of Contacts)", y="Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),  axis.text = element_text(face='bold',size=10), axis.text.y = element_text(face='bold',size=10), axis.line = element_line(colour = "black"))
IPMplot2
VLplot2 <- ggplot(Vl_TotalConacts, aes(x=LogTotalContact1, y=freq)) + geom_point(color="blue", fill="blue", size=3.5) + scale_x_continuous(limits=c(0, 3.25), breaks=seq(0,3.25,0.5)) + scale_y_continuous(breaks = pretty_breaks(n=9))+
  labs(title = "Virtual Learning", x="Log(Number of Contacts)", y="Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),  axis.text = element_text(face='bold',size=10), axis.text.y = element_text(face='bold',size=10), axis.line = element_line(colour = "black"))
VLplot2
#Stacked Bar Graph of 0/Not Zero
Vl_TotalConacts$Zeros[Vl_TotalConacts$Total2021 < 1] <- 0
Vl_TotalConacts$Zeros[Vl_TotalConacts$Total2021 >= 1] <- 1
Vl_zeros <- Vl_TotalConacts %>% filter(!is.na(Zeros)) %>% group_by(Zeros)%>% summarise(freq=n()) %>% mutate(percentage = freq / sum(freq) * 100)

IPM_TotalContacts$Zeros[IPM_TotalContacts$TotalContacts < 1] <- 0
IPM_TotalContacts$Zeros[IPM_TotalContacts$TotalContacts >= 1] <- 1
IPM_zeros <- IPM_TotalContacts %>% filter(!is.na(Zeros)) %>% group_by(Zeros)%>% summarise(freq=n()) %>% mutate(percentage = freq / sum(freq) * 100)


library(forcats)
library(scales)
SurveyPeriod <-c("1", "1","2", "2")
ZeroPercent <-c(7,93,0,100)
Zeros <-c("Zero", "1+", "Zero", "1+")
bothzero <-data.frame(SurveyPeriod, ZeroPercent, Zeros)
v <- ggplot(bothzero, aes(fill=Zeros, y=ZeroPercent, x=SurveyPeriod))+
  scale_x_discrete(labels =c('1'= 'VL', '2' = "IPM"))+
  geom_bar(position="fill", stat = "identity") +scale_y_continuous(labels=percent)+
  scale_fill_manual("Proportion of Zeros", values = c('purple','black'))+
  labs(title = " Mean Proportion of Respondents that had Zero or 1+ Contacts", y="Percent")
v 
ggarrange(VLplot2, IPMplot2,v, 
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)


# Create a matrix with observed values
observed <- matrix(c(48, 271-48, 28, 124-28), nrow = 2)

# Perform the chi-square test
chi_sq_result <- chisq.test(observed)

# Print the result
print(chi_sq_result)

observed1 <- matrix(c(10, 271-10, 13, 124-13), nrow = 2)

# Perform the chi-square test
chi_sq_result1 <- chisq.test(observed1)

# Print the result
print(chi_sq_result1)
