setwd("~/Lofgren/COVID-19/CougConnect")
library(readr)
#CC <- read_csv("Data 2.5.21.csv")
#Stated doing this, but realized the raw data was easier to deal with, so using other excel sheet
library(readr)
CougC<- read_csv("CougConnect_DATA_2021-02-05_1351.csv")
View(CougC)

#Cleaning data 2.5.21
#324 obs with 813 variables
#More libraries for the thing
library(dplyr)
library(tidyr)
library(gmodels)
library(tidyREDCap)
library(expss)
library(ggplot2)

table(CougC$agecontact1___0)
table(CougC$agecontact1___1)
table(CougC$agecontact1___2)
table(CougC$agecontact1___3)

#Getting an idea of total age distribution
#1=0-17  2=18-25 3= =26-64  4= 65+
CougC$Agec1 <- ifelse(CougC$agecontact1___0 ==1, 1,
                      ifelse(CougC$agecontact1___1 == 1 , 2,
                             ifelse(CougC$agecontact1___2 == 1, 3,
                                    ifelse(CougC$agecontact1___3 == 1, 4, NA))))
table(CougC$Agec1, useNA="always")
CrossTable(CougC$Agec1)
#1(17) 2(16) 3(154) 4(25) NA (125)'

#Now looking at contacts
table(CougC$contact1, useNA = "always")
CougC$contact1[CougC$contact1=='child'] <- "Child"
CougC$contact1[CougC$contact1=='child '] <- "Child"
CougC$contact1[CougC$contact1=='child 1'] <- "Child"
CougC$contact1[CougC$contact1=='colleague'] <- "Colleague"
CougC$contact1[CougC$contact1=='co-worker'] <- "Colleague"
CougC$contact1[CougC$contact1=='coworker'] <- "Colleague"
CougC$contact1[CougC$contact1=='colleague '] <- "Colleague"
CougC$contact1[CougC$contact1=='Colleague 1'] <- "Colleague"
CougC$contact1[CougC$contact1=='contractor'] <- "Contractor"
CougC$contact1[CougC$contact1=='"casheir" [sic]' ] <- "Cashier"
CougC$contact1[CougC$contact1=='Cashier at Pizza restaurant'] <- "Cashier"
CougC$contact1[CougC$contact1=="Cashier/McDondal's"] <- "Cashier"
CougC$contact1[CougC$contact1=="Add an option for \"I didn\'t make contact with anyone\""] <- "None" 
CougC$contact1[CougC$contact1=='no contact'] <- "None"
CougC$contact1[CougC$contact1=='no contact with anyone'] <- "None"
CougC$contact1[CougC$contact1=='Not Available'] <- "None"
CougC$contact1[CougC$contact1=='Daughter'] <- "Child"
CougC$contact1[CougC$contact1=='husband'] <- "Spouse"
CougC$contact1[CougC$contact1=='Husband'] <- "Spouse"
CougC$contact1[CougC$contact1=='friend'] <- "Friend"
CougC$contact1[CougC$contact1=='Parner'] <- "Partner"
CougC$contact1[CougC$contact1=='partner'] <- "Partner"
CougC$contact1[CougC$contact1=='Significant other'] <- "Partner"
CougC$contact1[CougC$contact1=='Son'] <- "Child"
CougC$contact1[CougC$contact1=='spouse'] <- "Spouse"
CougC$contact1[CougC$contact1=='spouse (female)'] <- "Spouse"
CougC$contact1[CougC$contact1=='Wife'] <- "Spouse"
CougC$contact1[CougC$contact1=='wife'] <- "Spouse"
CougC$contact1[CougC$contact1=='Partner/huband'] <- "Spouse"
CougC$contact1[CougC$contact1=='store clerk'] <- "Cashier"
CougC$contact1[CougC$contact1=='student'] <- "Student"
CougC$contact1[CougC$contact1=='Undergrad lab assistant'] <- "Student"
CougC$contact1[CougC$contact1=='Graduate Student'] <- "Student"
CougC$contact1[CougC$contact1=='5-walkers'] <- "5 walkers"
CougC$contact1[CougC$contact1=='facility people'] <- "Custodian"
CougC$contact1[CougC$contact1=='Grandson'] <- "Family Member"
CougC$contact1[CougC$contact1=='Niece whom I live with'] <- "Family Member"
CougC$contact1[CougC$contact1=='parent'] <- "Family Member"
CougC$contact1[CougC$contact1=='Paul Buckley'] <- "Other"
CougC$contact1[CougC$contact1=='Suzanne Anderson'] <- "Other"
CougC$contact1[CougC$contact1=='Walmart Grocery Pick-Up Staff'] <- "Retail Employee"
CougC$contact1[CougC$contact1=='Cashier'] <- "Retail Employee"
CougC$contact1[CougC$contact1=='Barista'] <- "Retail Employee"
#talked with Eric. putting things in wider categories, and being more vague
CougC$contact1[CougC$contact1=='Contractor'] <- "Laborer"
CougC$contact1[CougC$contact1=='Custodian'] <- "Colleague"
CougC$contact1[CougC$contact1=='family member, partner'] <- "Partner"
CougC$contact1[CougC$contact1=='Medical worker'] <- "Health Care Worker"
CougC$contact1[CougC$contact1=='Retail Employee'] <- "Laborer"
CougC$contact1[CougC$contact1=='Scientist'] <- "Colleague"
is.na(CougC$contact1[CougC$contact1=='None'])
CougC$contact1[CougC$contact1=='Instructor'] <- "Teacher"
table(CougC$contact1, useNA = "always")

#Mask contact
table(CougC$maskcontact1, useNA = "always")
CougC = apply_labels(CougC, maskcontact1 = c("We both did" = 1, "I did they did not" = 2, "I did some did not" =3, "I did not they did" = 4,"I did not some did" = 5, "Neither did" =6))
CougC = apply_labels(CougC, maskcontact2 = c("We both did" = 1, "I did they did not" = 2, "I did some did not" =3, "I did not they did" = 4,"I did not some did" = 5, "Neither did" =6))
CougC = apply_labels(CougC, maskcontact3 = c("We both did" = 1, "I did they did not" = 2, "I did some did not" =3, "I did not they did" = 4,"I did not some did" = 5, "Neither did" =6))
CougC = apply_labels(CougC, maskcontact4 = c("We both did" = 1, "I did they did not" = 2, "I did some did not" =3, "I did not they did" = 4,"I did not some did" = 5, "Neither did" =6))
CougC = apply_labels(CougC, maskcontact5 = c("We both did" = 1, "I did they did not" = 2, "I did some did not" =3, "I did not they did" = 4,"I did not some did" = 5, "Neither did" =6))
CougC = apply_labels(CougC, maskcontact6 = c("We both did" = 1, "I did they did not" = 2, "I did some did not" =3, "I did not they did" = 4,"I did not some did" = 5, "Neither did" =6))
CougC = apply_labels(CougC, maskcontact7 = c("We both did" = 1, "I did they did not" = 2, "I did some did not" =3, "I did not they did" = 4,"I did not some did" = 5, "Neither did" =6))
CougC = apply_labels(CougC, maskcontact8 = c("We both did" = 1, "I did they did not" = 2, "I did some did not" =3, "I did not they did" = 4,"I did not some did" = 5, "Neither did" =6))
CougC = apply_labels(CougC, maskcontact9 = c("We both did" = 1, "I did they did not" = 2, "I did some did not" =3, "I did not they did" = 4,"I did not some did" = 5, "Neither did" =6))
Masks <-CougC$maskcontact1
hist(Masks, xlim = c(1,6), breaks = "Sturges", labels = TRUE)


#Did you touch? Y=1, N = 0
CougC = apply_labels(CougC, touchcontact1 = c("No" =0,"Yes" =1))
CougC = apply_labels(CougC, touchcontact2 = c("No" =0,"Yes" =1))
CougC = apply_labels(CougC, touchcontact3 = c("No" =0,"Yes" =1))
CougC = apply_labels(CougC, touchcontact4 = c("No" =0,"Yes" =1))
CougC = apply_labels(CougC, touchcontact5 = c("No" =0,"Yes" =1))
CougC = apply_labels(CougC, touchcontact6 = c("No" =0,"Yes" =1))
CougC = apply_labels(CougC, touchcontact7 = c("No" =0,"Yes" =1))
CougC = apply_labels(CougC, touchcontact8 = c("No" =0,"Yes" =1))
CougC = apply_labels(CougC, touchcontact9 = c("No" =0,"Yes" =1))
table(CougC$touchcontact1)
barplot(table(CougC$touchcontact1, text(CougC$touchcontact1, 0 ,)))
ggplot(CougC, aes(x=touchcontact1)) + geom_bar()

#contextcontact 0=My home, 1 = work, 2=leisure, 3=travel, 4= shopping, 5=other


#trying MRCV package
library(MRCV)
context.irtable.one < -item.response.table(CougC)

context <- data.frame(Freq=colSums(CougC[14:19]),
                      Pct.of.Resp=(colSums(CougC[14:19])/sum(CougC[14:19]))*100,
                      Pct.of.Cases=(colSums(CougC[14:19])/nrow(CougC[14:19]))*100)                 
context
context1 <-data.frame(Freq=colSums(CougC[14:19]))
context1

ggplot(context1) + geom_bar()
                      
#this worked-somewhat
#gives me total numbers, but not quite X with home and work, X with home, work, leisure, etc.

table(CougC$locationcontact1)
#lots of different things. trying to consolidate 
#WSU Pullman  #Pullman WA     #Spokane Area WA    #Moscow ID    #Colfax WA
#Home         #Camas WA       #Vancouver WA       #Office       #Outside
#Richland WA    #Seattle WA   #Wenatchee      #Store    #Restuarant     #Car/Travel
CougC$locationcontact1[CougC$locationcontact1=='Albertsons Grocery, Vancouver'] <- "Vancouver WA"
CougC$locationcontact1[CougC$locationcontact1=="all over the house"] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='At home'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='bedroom'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=="All over the house"] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='at my home'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='Bustad Hall'] <- "WSU Pullman"
CougC$locationcontact1[CougC$locationcontact1=='camas, WA'] <- "Camas WA"
CougC$locationcontact1[CougC$locationcontact1=='Campus building'] <- "WSU Campus"
CougC$locationcontact1[CougC$locationcontact1=='Daily Grind'] <- "Pullman WA"
CougC$locationcontact1[CougC$locationcontact1=='Food Science Building - Lab'] <- "WSU Pullman"
CougC$locationcontact1[CougC$locationcontact1=='Grocery Store'] <- "Store"
CougC$locationcontact1[CougC$locationcontact1=='Hallway in the CUE'] <- "WSU Pullman"
CougC$locationcontact1[CougC$locationcontact1=='Health provider site'] <- "Medical Facility"
CougC$locationcontact1[CougC$locationcontact1=='home'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='home, pulman'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='Home.  In Vancouver WA.'] <- "Vancouver WA"
CougC$locationcontact1[CougC$locationcontact1=='Home. We live together'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='Home/house'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='Home/live together'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='home/live with my husband'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='Home/Spokane'] <- "Spokane Area WA"
CougC$locationcontact1[CougC$locationcontact1=='homr'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='house'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='House'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='In house'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='In my home'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='in my house'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='In my house'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='In my house (in Spokane)'] <- "Spokane Area WA"
CougC$locationcontact1[CougC$locationcontact1=='in my office / main office'] <- "Office"
CougC$locationcontact1[CougC$locationcontact1=='in our home'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='In our house'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='Home (we are a household)'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='home, pullman'] <- "Pullman WA"
CougC$locationcontact1[CougC$locationcontact1=='Home. We live together.'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='Inside Home'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='Kitchen'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='kitchen, dinning room'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='Liberty Lake'] <- "Spokane Area WA"
CougC$locationcontact1[CougC$locationcontact1=='Liberty Lake, WA'] <- "Spokane Area WA"
CougC$locationcontact1[CougC$locationcontact1=='Mail room at work office.'] <- "Office"
CougC$locationcontact1[CougC$locationcontact1=='Main Street, Colfax WA'] <- "Colfax WA"
CougC$locationcontact1[CougC$locationcontact1=="McDonald's Drive Thru"] <- "Drive Thru"
CougC$locationcontact1[CougC$locationcontact1=='Moscow'] <- "Moscow ID"
CougC$locationcontact1[CougC$locationcontact1=='Moscow, ID'] <- "Moscow ID"
CougC$locationcontact1[CougC$locationcontact1=='Most rooms in my home'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='my home'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='My home'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='My Home'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='My home in Spokane'] <- "Spokane Area WA"
CougC$locationcontact1[CougC$locationcontact1=='my house'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='My house'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='my Pullman residence'] <- "Pullman WA"
CougC$locationcontact1[CougC$locationcontact1=='office'] <- "Office"
CougC$locationcontact1[CougC$locationcontact1=='office door way Webster Hall 10ft separation'] <- "WSU Pullman"
CougC$locationcontact1[CougC$locationcontact1=='our home'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='Our home'] <-"Home"
CougC$locationcontact1[CougC$locationcontact1=='Our home (Battle Ground, WA)'] <- "Vancouver WA"
CougC$locationcontact1[CougC$locationcontact1=='our home; married'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='our house'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='Our house'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='our shared residence'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='Outdoors walking'] <- "Outdoors"
CougC$locationcontact1[CougC$locationcontact1=='outside on Pullman campus'] <- "WSU Pullman"
CougC$locationcontact1[CougC$locationcontact1=="Papa Murphy's Pizza, Pullman"] <- "Pullman WA"
CougC$locationcontact1[CougC$locationcontact1=='Pizza restaurant'] <- "Restaurant"
CougC$locationcontact1[CougC$locationcontact1=='pullman'] <- "Pullman WA"
CougC$locationcontact1[CougC$locationcontact1=='pullman campus'] <- "WSU Pullman"
CougC$locationcontact1[CougC$locationcontact1=='Pullman home'] <- "Pullman WA"
CougC$locationcontact1[CougC$locationcontact1=='Pullman, WA'] <- "Pullman WA"
CougC$locationcontact1[CougC$locationcontact1=='Residential streets in Moscow, Idaho'] <- "Moscow ID"
CougC$locationcontact1[CougC$locationcontact1=='Richland'] <- "Richland WA"
CougC$locationcontact1[CougC$locationcontact1=='Richland, WA'] <- "Richland WA"
CougC$locationcontact1[CougC$locationcontact1=='rooms in the house'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='Safeway'] <- "Store"
CougC$locationcontact1[CougC$locationcontact1=='shared dwelling'] <- "Home"
CougC$locationcontact1[CougC$locationcontact1=='shore and other at waiting area on campus'] <- "WSU Campus"
CougC$locationcontact1[CougC$locationcontact1=='Pullman'] <- "Pullman WA"
CougC$locationcontact1[CougC$locationcontact1=='Spokane'] <- "Spokane Area WA"
CougC$locationcontact1[CougC$locationcontact1=='store'] <- "Store"
CougC$locationcontact1[CougC$locationcontact1=='Vancouver'] <- "Vancouver WA"
CougC$locationcontact1[CougC$locationcontact1=='Various residential streets in Moscow, ID'] <- "Moscow ID"
CougC$locationcontact1[CougC$locationcontact1=='Walking in neighborhood'] <- "Outdoors"
CougC$locationcontact1[CougC$locationcontact1=='Walmart outdoor parking lot. Me in my car'] <- "Outdoors"
CougC$locationcontact1[CougC$locationcontact1=='WEbster Hall'] <- "WSU Pullman"
CougC$locationcontact1[CougC$locationcontact1=='1133 Ironsides Ave, Bremerton, WA'] <- "Bremerton WA"
CougC$locationcontact1[CougC$locationcontact1=='Outdoor'] <- "Outdoors"
Contact <-CougC$locationcontact1
#quick graph of counts
ggplot(CougC, aes(x=locationcontact1)) + geom_bar(stat="count")
#all over the place and messy af. Hopefully able to clean up more soon
table(CougC$locationcontact1, useNA = "always")

#TIME SPENT
table(CougC$timecontact1)
#adding labels
CougC = apply_labels(CougC, timecontact1 = c("Less than 10 minutes" =0,"10-30 minutes" =1, "30-60 minutes" = 2, ">1 hour" =3))
CougC = apply_labels(CougC, timecontact2 = c("Less than 10 minutes" =0,"10-30 minutes" =1, "30-60 minutes" = 2, ">1 hour" =3))
CougC = apply_labels(CougC, timecontact3 = c("Less than 10 minutes" =0,"10-30 minutes" =1, "30-60 minutes" = 2, ">1 hour" =3))
CougC = apply_labels(CougC, timecontact4 = c("Less than 10 minutes" =0,"10-30 minutes" =1, "30-60 minutes" = 2, ">1 hour" =3))
CougC = apply_labels(CougC, timecontact5 = c("Less than 10 minutes" =0,"10-30 minutes" =1, "30-60 minutes" = 2, ">1 hour" =3))
CougC = apply_labels(CougC, timecontact6 = c("Less than 10 minutes" =0,"10-30 minutes" =1, "30-60 minutes" = 2, ">1 hour" =3))
CougC = apply_labels(CougC, timecontact7 = c("Less than 10 minutes" =0,"10-30 minutes" =1, "30-60 minutes" = 2, ">1 hour" =3))
CougC = apply_labels(CougC, timecontact8 = c("Less than 10 minutes" =0,"10-30 minutes" =1, "30-60 minutes" = 2, ">1 hour" =3))
CougC = apply_labels(CougC, timecontact9 = c("Less than 10 minutes" =0,"10-30 minutes" =1, "30-60 minutes" = 2, ">1 hour" =3))
CougC = apply_labels(CougC, timecontact10 = c("Less than 10 minutes" =0,"10-30 minutes" =1, "30-60 minutes" = 2, ">1 hour" =3))
CrossTable(CougC$timecontact1)
#Less than 10 (23/11%) 10-30 (11/5%) 30-60 (10/5%) >1hr (79%)

#looking if time contact changed by contact
CrossTable(CougC$timecontact1, CougC$timecontact2, chisq = TRUE)
#again can't see to get the NAs to show up, which is frustrating. 
#But I am seeing a little bit of a difference between the two contacts

#SECOND PEOPLE!!
#age cat
#1=0-17  2=18-25 3= =26-64  4= 65+
CougC$Agec2 <- ifelse(CougC$agecontact2___0 ==1, 1,
                      ifelse(CougC$agecontact2___1 == 1 , 2,
                             ifelse(CougC$agecontact2___2 == 1, 3,
                                    ifelse(CougC$agecontact3___3 == 1, 4, NA))))
table(CougC$Agec2, useNA="always")
CrossTable(CougC$Agec1)
CrossTable(CougC$Agec2)
#N=134 for AgeC2 vs N-212 for Agec1
#1(55) 2(25) 3(54) 4(0) NA (203)'
#comparing ages between the two contacts
crossage <-table(CougC$Agec1,CougC$Agec2, useNA = "always")
crossage #C1=rows, c2=columns
#53 people put an older person first, then a young person (prob SO then child)
prop.table(crossage)

#Second person context clean up
table(CougC$contact2, useNA = "always")
CougC$contact2[CougC$contact2=='Brother'] <- "Family Member"
CougC$contact2[CougC$contact2=='child'] <- "Child"
CougC$contact2[CougC$contact2=='cashier'] <- "Cashier"
CougC$contact2[CougC$contact2=='child '] <- "Child"
CougC$contact2[CougC$contact2=='child 1'] <- "Child"
CougC$contact2[CougC$contact2=='Child 1'] <- "Child"
CougC$contact2[CougC$contact2=='child 2'] <- "Child"
CougC$contact2[CougC$contact2=='children'] <- "Children"
CougC$contact2[CougC$contact2=='co-worker'] <- "Colleague"
CougC$contact2[CougC$contact2=='colleague'] <- "Colleague"
CougC$contact2[CougC$contact2=='Colleague 1'] <- "Colleague"
CougC$contact2[CougC$contact2=='Colleague 2'] <- "Colleague"
CougC$contact2[CougC$contact2=='Coworker'] <- "Colleague"
CougC$contact2[CougC$contact2=='coworker'] <- "Colleague"
CougC$contact2[CougC$contact2=='Coworker A'] <- "Colleague"
CougC$contact2[CougC$contact2=='daughter'] <- "Child"
CougC$contact2[CougC$contact2=='Daughter'] <- "Child"
CougC$contact2[CougC$contact2=='Eldest son'] <- "Child"
CougC$contact2[CougC$contact2=='drive through worker'] <- "Cashier"
CougC$contact2[CougC$contact2=='Driver up employee'] <- "Cashier"
CougC$contact2[CougC$contact2=='Family doctor'] <- "Health Care Worker"
CougC$contact2[CougC$contact2=='friend'] <- "Friend"
CougC$contact2[CougC$contact2=='Great niece whom I live with'] <- "Family Member"
CougC$contact2[CougC$contact2=='Grocery Store Fishmonger'] <- "Retail Worker"
CougC$contact2[CougC$contact2=='Health care provider'] <- "Health Care Worker"
CougC$contact2[CougC$contact2=='Husband'] <- "Spouse"
CougC$contact2[CougC$contact2=='Mother'] <- "Family Member"
CougC$contact2[CougC$contact2=='Ms. AJ Johnson'] <- "Other"
CougC$contact2[CougC$contact2=='My child'] <- "Child"
CougC$contact2[CougC$contact2=='My daughter'] <- "Child"
CougC$contact2[CougC$contact2=='Niece'] <- "Family Member"
CougC$contact2[CougC$contact2=='Nurse'] <- "Health Care Worker"
CougC$contact2[CougC$contact2=='parent'] <- "Family Member"
CougC$contact2[CougC$contact2=='Parent'] <- "Family Member"
CougC$contact2[CougC$contact2=='partner'] <- "Partner"
CougC$contact2[CougC$contact2=='retail worker'] <- "Retail Worker"
CougC$contact2[CougC$contact2=='Roost Barista'] <- "Barista"
CougC$contact2[CougC$contact2=='son'] <- "Child"
CougC$contact2[CougC$contact2=='Son'] <- "Child"
CougC$contact2[CougC$contact2=='stepchild'] <- "Child"
CougC$contact2[CougC$contact2=='student'] <- "Student"
CougC$contact2[CougC$contact2=='student worker'] <- "Student"
CougC$contact2[CougC$contact2=='1-Friend at her place of employment (not shopping)'] <- "Friend"
CougC$contact2[CougC$contact2=='work'] <- "Colleague"
CougC$contact2[CougC$contact2=='Drive up employee'] <- "Worker"
CougC$contact2[CougC$contact2=='Work'] <- "Colleague"
CougC$contact2[CougC$contact2=='Barista'] <- "Laborer"
CougC$contact2[CougC$contact2=='Cashier'] <- "Laborer"
CougC$contact2[CougC$contact2=='Daycare Provider'] <- "Laborer"
CougC$contact2[CougC$contact2=='Employee of doggie day care'] <- "Laborer"
CougC$contact2[CougC$contact2=='House cleaner'] <- "Laborer"
CougC$contact2[CougC$contact2=='Receptionist'] <- "Laborer"
CougC$contact2[CougC$contact2=='Retail Worker'] <- "Laborer"
CougC$contact2[CougC$contact2=='Server'] <- "Laborer"
CougC$contact2[CougC$contact2=='Service call'] <- "Laborer"
CougC$contact2[CougC$contact2=='Drive up employee'] <- "Laborer"
CougC$contact2[CougC$contact2=='Employee'] <- "Laborer"
CougC$contact2[CougC$contact2=="Server McDonald's"] <- "Laborer"
CougC$contact2[CougC$contact2=='Stranger/other store patron'] <- "Stranger"
table(CougC$contact2, useNA = "always")

table(CougC$contact1, useNA = "always")

contact2 <-table(CougC$contact2, useNA = "always")

barplot(contact2, main= "Second Contact", horiz = TRUE)
contacts12<- table(CougC$contact1, CougC$contact2, useNA = "always")
contacts12              
#hmmmm needs more clean up first

#masks
Masks2 <-CougC$maskcontact2
cro(CougC$maskcontact2)
hist(Masks2, xlim = c(1,6), breaks = "Sturges", labels = TRUE)

#Second location contact
#SEH-Someone Else's House/home
table(CougC$locationcontact2, useNA = 'always')
CougC$locationcontact2[CougC$locationcontact2=='A park'] <- "Outdoors"
CougC$locationcontact2[CougC$locationcontact2=="Abelson Hall"] <- "WSU Pullman"
CougC$locationcontact2[CougC$locationcontact2=='Alley between homes -- 20 ft apart'] <- "Outdoors"
CougC$locationcontact2[CougC$locationcontact2=='athletic club'] <- "Gym"
CougC$locationcontact2[CougC$locationcontact2=="Both our houses"] <- "Homes"
CougC$locationcontact2[CougC$locationcontact2=='Bustad'] <- "WSU Pullman"
CougC$locationcontact2[CougC$locationcontact2=='Bustad Hall'] <- "WSU Pullman"
CougC$locationcontact2[CougC$locationcontact2=='camas, WA'] <- "Camas WA"
CougC$locationcontact2[CougC$locationcontact2=='CAPS - Pullman'] <- "WSU Pullman"
CougC$locationcontact2[CougC$locationcontact2=='Daycare Location'] <- "School"
CougC$locationcontact2[CougC$locationcontact2=="dinner"] <- "Restaurant"
CougC$locationcontact2[CougC$locationcontact2=='Doctor\'s office'] <- "Medical Facility"
CougC$locationcontact2[CougC$locationcontact2=='Doctors Office'] <- "Medical Facility"
CougC$locationcontact2[CougC$locationcontact2=="Dog kennel facility"] <- "Pet Facility"
CougC$locationcontact2[CougC$locationcontact2=='Doggie day care. Picking up my dog.'] <- "Pet Facility"
CougC$locationcontact2[CougC$locationcontact2=='earth'] <- "Other"
CougC$locationcontact2[CougC$locationcontact2=="Fast Food Restaurant, DQ"] <- "Restaurant"
CougC$locationcontact2[CougC$locationcontact2=='FSHN'] <- "WSU Pullman"
CougC$locationcontact2[CougC$locationcontact2=='Greenhouse'] <- "Other"
CougC$locationcontact2[CougC$locationcontact2=="hallway"] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=='her home Spokane Valley, WA'] <- "Spokane Area WA"
CougC$locationcontact2[CougC$locationcontact2=='His home'] <- "SEH"
CougC$locationcontact2[CougC$locationcontact2=='home'] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=="Home, Vancouver"] <- "Vancouver WA"
CougC$locationcontact2[CougC$locationcontact2=='house'] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=='In house'] <- " Home"
CougC$locationcontact2[CougC$locationcontact2=="in m y house"] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=='In my home'] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=='in my house'] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=='In my house'] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=="In our house"] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=='Inside Home'] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=='Iwajimaya Grocery store'] <- "Store"
CougC$locationcontact2[CougC$locationcontact2=="kitchen, dinning room"] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=='lab'] <- "WSU Campus"
CougC$locationcontact2[CougC$locationcontact2=='Lab'] <- "WSU Campus"
CougC$locationcontact2[CougC$locationcontact2=='large department office'] <- "Office"
CougC$locationcontact2[CougC$locationcontact2=="Living area, homeoffice, and maybe kitchen"] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=='LIving Room'] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=='Macdonalds'] <- "Restaurant"
CougC$locationcontact2[CougC$locationcontact2=="McDonald\'s Drive Thru"] <- "Restaurant"
CougC$locationcontact2[CougC$locationcontact2=='medical office'] <- "Medical Facility"
CougC$locationcontact2[CougC$locationcontact2=='Moscow'] <- "Moscow ID"
CougC$locationcontact2[CougC$locationcontact2=='Moscow, Idaho'] <- "Moscow ID"
CougC$locationcontact2[CougC$locationcontact2=="Moscow CoOp (not shopping)"] <- "Moscow ID"
CougC$locationcontact2[CougC$locationcontact2=='Moscow, ID'] <- "Moscow ID"
CougC$locationcontact2[CougC$locationcontact2=='My Home'] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=="Mostly at home"] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=='My home in Spokane'] <- "Spokane Area WA"
CougC$locationcontact2[CougC$locationcontact2=='my house'] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=='My house'] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=="My office"] <- "Office"
CougC$locationcontact2[CougC$locationcontact2=='our home'] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=='Our home'] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=="Outdoor dining"] <- "Outdoors"
CougC$locationcontact2[CougC$locationcontact2=='Outdoors on campus'] <- "WSU Campus"
CougC$locationcontact2[CougC$locationcontact2=='Paccar Building'] <- "WSU Pullman"
CougC$locationcontact2[CougC$locationcontact2=='Parent\s house'] <- "SEH"
CougC$locationcontact2[CougC$locationcontact2=="Pullman Aquatic Center"] <- "Pullman"
CougC$locationcontact2[CougC$locationcontact2=='pullman campus'] <- "WSU Pullman"
CougC$locationcontact2[CougC$locationcontact2=='Pullman home'] <- "Pullman"
CougC$locationcontact2[CougC$locationcontact2=="restaurant drive through"] <- "Drive Thru"
CougC$locationcontact2[CougC$locationcontact2=='Richland'] <- "Richland WA"
CougC$locationcontact2[CougC$locationcontact2=='Richland, WA'] <- "Richland WA"
CougC$locationcontact2[CougC$locationcontact2=='Roost'] <- "Pullman"
CougC$locationcontact2[CougC$locationcontact2=="Roost coffee shop"] <- "Pullman"
CougC$locationcontact2[CougC$locationcontact2=='Safeway'] <- "Store"
CougC$locationcontact2[CougC$locationcontact2=='shared dwelling'] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=="Starbucks (near Pullman Walmart)"] <- "Pullman"
CougC$locationcontact2[CougC$locationcontact2=='Starbucks, Pullman, WA'] <- "Pullman"
CougC$locationcontact2[CougC$locationcontact2=='Store while shopping'] <- "Store"
CougC$locationcontact2[CougC$locationcontact2=='Studio/classroom'] <- "School"
CougC$locationcontact2[CougC$locationcontact2=="T-Mobile"] <- "Store"
CougC$locationcontact2[CougC$locationcontact2=='The one room in our apartment where we can work.'] <- "Home"
CougC$locationcontact2[CougC$locationcontact2=='Thompson Hall'] <- "WSU Pullman"
CougC$locationcontact2[CougC$locationcontact2=="Vogel Lab 129"] <- "WSU Pullman"
CougC$locationcontact2[CougC$locationcontact2=='walking in neighborhood'] <- "Outdoors"
CougC$locationcontact2[CougC$locationcontact2=='Walking in neighborhood'] <- "Outdoors"
CougC$locationcontact2[CougC$locationcontact2=='Walmart'] <- "Store"
CougC$locationcontact2[CougC$locationcontact2=='Webster Hall 12ft separation'] <- "WSU Pullman"
CougC$locationcontact2[CougC$locationcontact2=="WSU campus"] <- "WSU Campus"
CougC$locationcontact2[CougC$locationcontact2=='YMCA'] <- "Gym"
CougC$locationcontact2[CougC$locationcontact2=='GYM'] <- "Gym"
CougC$locationcontact2[CougC$locationcontact2=='My front yard'] <- "Outdoors"

table(CougC$locationcontact2, useNA = 'always')


#Free text for contact 3
CougC$Agec3 <- ifelse(CougC$agecontact3___0 ==1, 1,
                      ifelse(CougC$agecontact3___1 == 1 , 2,
                             ifelse(CougC$agecontact3___2 == 1, 3,
                                    ifelse(CougC$agecontact3___3 == 1, 4, NA))))
table(CougC$Agec3, useNA="always")
CrossTable(CougC$Agec3)  #N=102
#1(25) 2(24) 3(46) 4(7) NA (235)'

#Now looking at contacts
table(CougC$contact3, useNA = "always")
CougC$contact3[CougC$contact3=='2nd child'] <- "Child"
CougC$contact3[CougC$contact3=='Babysitter'] <- "Laborer"
CougC$contact3[CougC$contact3=='Best friend'] <- "Friend"
CougC$contact3[CougC$contact3=='cashier'] <- "Laborer"
CougC$contact3[CougC$contact3=='Cashier'] <- "Laborer"
CougC$contact3[CougC$contact3=='checkout person'] <- "Laborer"
CougC$contact3[CougC$contact3=='child 2'] <- "Child"
CougC$contact3[CougC$contact3=='child'] <- "Child"
CougC$contact3[CougC$contact3== "child\'s piano teacher"] <- "Laborer"
CougC$contact3[CougC$contact3=='Child 2'] <- "Child"
CougC$contact3[CougC$contact3=='Child friend'] <- "Friend"
CougC$contact3[CougC$contact3=='Children center person'] <- "Laborer"
CougC$contact3[CougC$contact3=='Clerk'] <- "Laborer"
CougC$contact3[CougC$contact3=='Clerk at Doggy Day Care'] <- "Laborer"
CougC$contact3[CougC$contact3=='Co-Worker'] <- "Colleague"
CougC$contact3[CougC$contact3=='Co-worker A'] <- "Colleague"
CougC$contact3[CougC$contact3=='colleague 1'] <- "Colleague"
CougC$contact3[CougC$contact3=='Colleague 1 (my post-doc)'] <- "Colleague"
CougC$contact3[CougC$contact3=='Colleague 2'] <- "Colleague"
CougC$contact3[CougC$contact3=='coworker'] <- "Colleague"
CougC$contact3[CougC$contact3=='Coworker'] <- "Colleague"
CougC$contact3[CougC$contact3=='Daughter'] <- "Child"
CougC$contact3[CougC$contact3=='daughter-in-law'] <- "Family Member"
CougC$contact3[CougC$contact3=='Daughter-in-law'] <- "Family Member"
CougC$contact3[CougC$contact3=='Deli worker'] <- "Laborer"
CougC$contact3[CougC$contact3=='Dental Surgery Assistant'] <- "Health Care Worker"
CougC$contact3[CougC$contact3=='Drive up employee'] <- "Laborer"
CougC$contact3[CougC$contact3=='friend'] <- "Friend"
CougC$contact3[CougC$contact3=='Graduate Student'] <- "Student"
CougC$contact3[CougC$contact3=='Grandson'] <- "Family Member"
CougC$contact3[CougC$contact3=='Grocery Store Checker'] <- "Laborer"
CougC$contact3[CougC$contact3=='Hair stylist'] <- "Laborer"
CougC$contact3[CougC$contact3=='health care provider'] <- "Health Care Worker"
CougC$contact3[CougC$contact3=='Husband'] <- "Spouse"
CougC$contact3[CougC$contact3=='Middle son'] <- "Child"
CougC$contact3[CougC$contact3=='My daughter-in-law'] <- "Family Member"
CougC$contact3[CougC$contact3=='My husband'] <- "Spouse"
CougC$contact3[CougC$contact3=='Niece'] <- "Family Member"
CougC$contact3[CougC$contact3=='Our nanny'] <- "Laborer"
CougC$contact3[CougC$contact3=='Phlebotomist'] <- "Health Care Worker"
CougC$contact3[CougC$contact3=='receptionist'] <- "Laborer" 
CougC$contact3[CougC$contact3=='Receptionist'] <- "Laborer"
CougC$contact3[CougC$contact3=='colleague'] <- "Colleague"
CougC$contact3[CougC$contact3=='Coworker E'] <- "Colleague"
CougC$contact3[CougC$contact3=='custodial staff 1'] <- "Laborer"
CougC$contact3[CougC$contact3=='research advisor'] <- "Colleague"
CougC$contact3[CougC$contact3=='Restauranteer'] <- "Laborer"
CougC$contact3[CougC$contact3=='Resturaunt worker'] <- "Laborer"
CougC$contact3[CougC$contact3=='Server 2'] <- "Laborer"
CougC$contact3[CougC$contact3=='son'] <- "Child"
CougC$contact3[CougC$contact3=='Son'] <- "Child"
CougC$contact3[CougC$contact3=='Son of partner'] <- "Family Member"
CougC$contact3[CougC$contact3=='spouse'] <- "Spouse"
CougC$contact3[CougC$contact3=='Starbucks Drive Through Cashier'] <- "Laborer"
CougC$contact3[CougC$contact3=='stepchild'] <- "Child"
CougC$contact3[CougC$contact3=='Store cashier'] <- "Laborer"
CougC$contact3[CougC$contact3=='Student 1'] <- "Student"
CougC$contact3[CougC$contact3=='Swimmer'] <- "Other"
CougC$contact3[CougC$contact3=='Veterinarian assistant'] <- "Colleague"

#Locations now
table(CougC$locationcontact3)
CougC$locationcontact3[CougC$locationcontact3=='At my house'] <- "Home"
CougC$locationcontact3[CougC$locationcontact3=="athletic club"] <- "Gym"
CougC$locationcontact3[CougC$locationcontact3=='BLS 240 - Our lab at WSU'] <- "WSU Pullman"
CougC$locationcontact3[CougC$locationcontact3=='Bustad'] <- "WSU Pullman"
CougC$locationcontact3[CougC$locationcontact3=='Bustad Hall'] <- "WSU Pullman"
CougC$locationcontact3[CougC$locationcontact3=='Camas, WA'] <- "Camas WA"
CougC$locationcontact3[CougC$locationcontact3=="Children center WSUCC"] <- "WSU Pullman"
CougC$locationcontact3[CougC$locationcontact3=='Cougar Health Services'] <- "WSU Pullman/Health Care Facility"
CougC$locationcontact3[CougC$locationcontact3=='Dental Surgeon\'s Office in Spokane'] <- "Spokane Area WA"
CougC$locationcontact3[CougC$locationcontact3=='drive thru'] <- "Drive Thru"
CougC$locationcontact3[CougC$locationcontact3=='Driving child friend home'] <- "Car"
CougC$locationcontact3[CougC$locationcontact3=="Fulmer Hall"] <- "WSU Pullman"
CougC$locationcontact3[CougC$locationcontact3=='Greenhouse'] <- "Other"
CougC$locationcontact3[CougC$locationcontact3=='hallway'] <- "Other"
CougC$locationcontact3[CougC$locationcontact3=='Health clinic'] <- "Health Care Facility"
CougC$locationcontact3[CougC$locationcontact3=='Her home'] <- "SEH"
CougC$locationcontact3[CougC$locationcontact3=="home"] <- "Home"
CougC$locationcontact3[CougC$locationcontact3=='In my home'] <- "Home"
CougC$locationcontact3[CougC$locationcontact3=='in my house'] <- "Home"
CougC$locationcontact3[CougC$locationcontact3=='In my house'] <- "Home"
CougC$locationcontact3[CougC$locationcontact3=='Jack in the Box'] <- "Restaurant"
CougC$locationcontact3[CougC$locationcontact3=="Lab"] <- "WSU Campus"
CougC$locationcontact3[CougC$locationcontact3=='large department office'] <- "Work"
CougC$locationcontact3[CougC$locationcontact3=='Living area/tv room'] <- "Home"
CougC$locationcontact3[CougC$locationcontact3=='Macdonalds'] <- "Restaurant"
CougC$locationcontact3[CougC$locationcontact3=='medical office'] <- "Health Care Facility"
CougC$locationcontact3[CougC$locationcontact3=="Moscow, ID"] <- "Moscow ID"
CougC$locationcontact3[CougC$locationcontact3=='Moscow'] <- "Moscow ID"
CougC$locationcontact3[CougC$locationcontact3=='Moscow CoOp'] <- "Moscow ID"
CougC$locationcontact3[CougC$locationcontact3=='Moscow, on sidewalk'] <- "Moscow ID"
CougC$locationcontact3[CougC$locationcontact3=="My home in Spokane"] <- "Spokane Area WA"
CougC$locationcontact3[CougC$locationcontact3=='my house'] <- "Home"
CougC$locationcontact3[CougC$locationcontact3=='my office'] <- "Office"
CougC$locationcontact3[CougC$locationcontact3=='office door way Webster Hall 10ft separation'] <- "WSU Pullman"
CougC$locationcontact3[CougC$locationcontact3=='Our home'] <- "Home"
CougC$locationcontact3[CougC$locationcontact3=="Outdoor dining"] <- "Outdoors"
CougC$locationcontact3[CougC$locationcontact3=='Outside of restaurant'] <- "Outdoors"
CougC$locationcontact3[CougC$locationcontact3=='PACCAR'] <- "WSU Pullman"
CougC$locationcontact3[CougC$locationcontact3=='parking lot /car/drive through covid testing site'] <- "Health Care Facility"
CougC$locationcontact3[CougC$locationcontact3=="Pediatrician\'s office"] <- "Health Care Facility"
CougC$locationcontact3[CougC$locationcontact3=='Pullman on campus residence hall'] <- "WSU Pullman"
CougC$locationcontact3[CougC$locationcontact3=='research lab'] <- "WSU Campus"
CougC$locationcontact3[CougC$locationcontact3=='restaurant take out'] <- "Restaurant"
CougC$locationcontact3[CougC$locationcontact3=="Resturaunt at lunch"] <- "Restaurant"
CougC$locationcontact3[CougC$locationcontact3=='Richland'] <- "Richland WA"
CougC$locationcontact3[CougC$locationcontact3=='Richland, WA'] <- "Richland WA"
CougC$locationcontact3[CougC$locationcontact3=='Safeway'] <- "Store"
CougC$locationcontact3[CougC$locationcontact3=='Separate lanes in a lap pool'] <- "Gym"
CougC$locationcontact3[CougC$locationcontact3=="Stadium Way Starbucks Drive through"] <- "Pullman"
CougC$locationcontact3[CougC$locationcontact3=='The Bookie'] <- "WSU Pullman"
CougC$locationcontact3[CougC$locationcontact3=='Their home'] <- "SEH"
CougC$locationcontact3[CougC$locationcontact3=='walk up window'] <- "Other"
CougC$locationcontact3[CougC$locationcontact3=='walking outside'] <- "Outdoors"
CougC$locationcontact3[CougC$locationcontact3=="Walmart"] <- "Store"
CougC$locationcontact3[CougC$locationcontact3=='WSU Research Park'] <- "Pullman"
CougC$locationcontact3[CougC$locationcontact3=='Work office/Pullman'] <- "Pullman"

#Contact for 4!
table(CougC$contact4, useNA = 'always')
CougC$contact4[CougC$contact4=='Barrista'] <- "Laborer"
CougC$contact4[CougC$contact4=='cashier'] <- "Laborer"
CougC$contact4[CougC$contact4=='Cashier'] <- "Laborer"
CougC$contact4[CougC$contact4=='checkout person'] <- "Laborer"
CougC$contact4[CougC$contact4=='Child 1\'s elementary teacher'] <- "Teacher"
CougC$contact4[CougC$contact4=='Child 3'] <- "Child"
CougC$contact4[CougC$contact4=='Childcare Provider'] <- "Laborer"
CougC$contact4[CougC$contact4=='Colleage 1'] <- "Colleague"
CougC$contact4[CougC$contact4=='clerk'] <- "Laborer"
CougC$contact4[CougC$contact4=='Clerk'] <- "Laborer"
CougC$contact4[CougC$contact4=='Clerk at Store'] <- "Laborer"
CougC$contact4[CougC$contact4=='co worker'] <- "Colleague"
CougC$contact4[CougC$contact4=='colleague'] <- "Colleague"
CougC$contact4[CougC$contact4=='colleague 1'] <- "Colleague"
CougC$contact4[CougC$contact4=='Colleague 2'] <- "Colleague"
CougC$contact4[CougC$contact4=='Colleague 3'] <- "Colleague"
CougC$contact4[CougC$contact4=='Coworker K'] <- "Colleague"
CougC$contact4[CougC$contact4=='custodial worker 2'] <- "Laborer"
CougC$contact4[CougC$contact4=='Colleague 1'] <- "Colleague"
CougC$contact4[CougC$contact4=='Dog Daycare Attendant'] <- "Laborer"
CougC$contact4[CougC$contact4=='Drive up Employee'] <- "Laborer"
CougC$contact4[CougC$contact4=='Employee A'] <- "Laborer"
CougC$contact4[CougC$contact4=='friend'] <- "Friend"
CougC$contact4[CougC$contact4=='grandaughter'] <- "Family Member"
CougC$contact4[CougC$contact4=='Greeting monitor at child\'s school'] <- "Laborer"
CougC$contact4[CougC$contact4=='Guy knocked on our door'] <- "Stranger"
CougC$contact4[CougC$contact4=='house mate'] <- "Housemate"
CougC$contact4[CougC$contact4=='mother'] <- "Family Member"
CougC$contact4[CougC$contact4=='My husband'] <- "Spouse"
CougC$contact4[CougC$contact4=='Nail designer'] <- "Laborer"
CougC$contact4[CougC$contact4=='Nurse'] <- "Health Care Worker"
CougC$contact4[CougC$contact4=='physical therapist'] <- "Health Care Worker"
CougC$contact4[CougC$contact4=='Provider at Rieverview Retirement'] <- "Health Care Worker"
CougC$contact4[CougC$contact4=='receptionist'] <- "Laborer"
CougC$contact4[CougC$contact4=='Receptionist'] <- "Laborer"
CougC$contact4[CougC$contact4=='Retail Employee'] <- "Laborer"
CougC$contact4[CougC$contact4=='Safeway grocery'] <- "Laborer"
CougC$contact4[CougC$contact4=='spouse'] <- "Spouse"
CougC$contact4[CougC$contact4=='Staff Assistant'] <- "Colleague"
CougC$contact4[CougC$contact4=='Student 2'] <- "Student"
CougC$contact4[CougC$contact4=='Teacher (Child 1)'] <- "Teacher"
CougC$contact4[CougC$contact4=='Vet assistant'] <- "Laborer"
CougC$contact4[CougC$contact4=='Walker 1'] <- "Stranger"
CougC$contact4[CougC$contact4=='Youngest son'] <- "Child"

#location 4
table(CougC$locationcontact4, useNA = 'always')
CougC$locationcontact4[CougC$locationcontact4=='Abelson Hall'] <- "WSU Pullman"
CougC$locationcontact4[CougC$locationcontact4=='Another office on campus to get mail'] <- "WSU Campus"
CougC$locationcontact4[CougC$locationcontact4=='At home'] <- "Home"
CougC$locationcontact4[CougC$locationcontact4=='athletic club'] <- "Gym"
CougC$locationcontact4[CougC$locationcontact4=='Bustad'] <- "WSU Pullman"
CougC$locationcontact4[CougC$locationcontact4=='Co-op'] <- "Store"
CougC$locationcontact4[CougC$locationcontact4=='Cougar Health Services'] <- "WSU Pullman/Health Care Facility"
CougC$locationcontact4[CougC$locationcontact4=='Daycare'] <- "School"
CougC$locationcontact4[CougC$locationcontact4=='Doctor\'s office'] <- "Health Care Facility"
CougC$locationcontact4[CougC$locationcontact4=='Eastlick Hall'] <- "WSU Pullman"
CougC$locationcontact4[CougC$locationcontact4=='friend\'s garage, outdoors'] <- "Outdoors"
CougC$locationcontact4[CougC$locationcontact4=='Front door/porch'] <- "Home"
CougC$locationcontact4[CougC$locationcontact4=='Grass field outside of school'] <- "Outdoors"
CougC$locationcontact4[CougC$locationcontact4=='grocery story'] <- "Store"
CougC$locationcontact4[CougC$locationcontact4=='Hallway outside their office'] <- "Work"
CougC$locationcontact4[CougC$locationcontact4=='home'] <- "Home"
CougC$locationcontact4[CougC$locationcontact4=='I was in my car...she took my dog out'] <- "Car"
CougC$locationcontact4[CougC$locationcontact4=='In my home'] <- "Home"
CougC$locationcontact4[CougC$locationcontact4=='in my house'] <- "Home"
CougC$locationcontact4[CougC$locationcontact4=='In my house'] <- "Home"
CougC$locationcontact4[CougC$locationcontact4=='in my personal office'] <- "Office"
CougC$locationcontact4[CougC$locationcontact4=='Lab'] <- "WSU Campus"
CougC$locationcontact4[CougC$locationcontact4=='Lighty'] <- "WSU Pullman"
CougC$locationcontact4[CougC$locationcontact4=='Moscow CoOp'] <- "Moscow ID"
CougC$locationcontact4[CougC$locationcontact4=='Moscow, my kitchen'] <- "Moscow ID"
CougC$locationcontact4[CougC$locationcontact4=='my home'] <- "Home"
CougC$locationcontact4[CougC$locationcontact4=='my office'] <- "Office"
CougC$locationcontact4[CougC$locationcontact4=='Nail Salon'] <- "Salon"
CougC$locationcontact4[CougC$locationcontact4=='on the bike path'] <- "Outdoors"
CougC$locationcontact4[CougC$locationcontact4=='outdoor (kid soccer practice)'] <- "Outdoors"
CougC$locationcontact4[CougC$locationcontact4=='Outside my house, they were on sidewalk and I was in my yard'] <- "Outdoors"
CougC$locationcontact4[CougC$locationcontact4=='Outside, maintained social distancing, they had been vaccinated'] <- "Outdoors"
CougC$locationcontact4[CougC$locationcontact4=='Pediatrician\'s office'] <- "Health Care Facility"
CougC$locationcontact4[CougC$locationcontact4=='Pullman City Hall/Recreation Center'] <- "Pullman"
CougC$locationcontact4[CougC$locationcontact4=='Pullman on campus residence hall'] <- "WSU Pullman"
CougC$locationcontact4[CougC$locationcontact4=='Pullman, WA'] <- "Pullman"
CougC$locationcontact4[CougC$locationcontact4=='retail store'] <- "Store"
CougC$locationcontact4[CougC$locationcontact4=='Richland, WA'] <- "Richland WA"
CougC$locationcontact4[CougC$locationcontact4=='Riverview Retirement Center'] <- "Spokane Area WA"
CougC$locationcontact4[CougC$locationcontact4=='Safefway grocery'] <- "Store"
CougC$locationcontact4[CougC$locationcontact4=='Safeway'] <- "Store"
CougC$locationcontact4[CougC$locationcontact4=='School Parking Lot'] <- "outdoors"
CougC$locationcontact4[CougC$locationcontact4=='shop'] <- "Store"
CougC$locationcontact4[CougC$locationcontact4=='Stairway of apartment building'] <- "Other"
CougC$locationcontact4[CougC$locationcontact4=='Starbucks'] <- "Restaurant"
CougC$locationcontact4[CougC$locationcontact4=='Summit Therapy'] <- "Health Care Facility"
CougC$locationcontact4[CougC$locationcontact4=='Tail Waggin\' Adventures'] <- "Pet Facility"
CougC$locationcontact4[CougC$locationcontact4=='tennis court'] <- "Gym"
CougC$locationcontact4[CougC$locationcontact4=='Thompson Hall'] <- "WSU Pullman"
CougC$locationcontact4[CougC$locationcontact4=='Trader Joe\'s Spokane'] <- "Spokane Area WA"
CougC$locationcontact4[CougC$locationcontact4=='Walmart'] <- "Store"
CougC$locationcontact4[CougC$locationcontact4=='work'] <- "Work"
CougC$locationcontact4[CougC$locationcontact4=='Work office/Pullman'] <- "Pullman"
CougC$locationcontact4[CougC$locationcontact4=='outdoors'] <- "Outdoors"
CougC$locationcontact4[CougC$locationcontact4=='grocery store'] <- "Store"

#5th people and locations!!

table(CougC$contact5, useNA = "always")
CougC$contact5[CougC$contact5=='1 person at elementary school'] <- "Teacher"
CougC$contact5[CougC$contact5=='2nd friend'] <- "Friend"
CougC$contact5[CougC$contact5=='Another vet tech'] <- "Laborer"
CougC$contact5[CougC$contact5=='Barista'] <- "Laborer"
CougC$contact5[CougC$contact5=='Barrista'] <- "Laborer"
CougC$contact5[CougC$contact5=='Behavioral Technicial'] <- "Laborer"
CougC$contact5[CougC$contact5=='cashier'] <- "Laborer"
CougC$contact5[CougC$contact5=='Cashier'] <- "Laborer"
CougC$contact5[CougC$contact5=='child 2'] <- "Child"
CougC$contact5[CougC$contact5=='Cleaning Person'] <- "Laborer"
CougC$contact5[CougC$contact5=='Clerk'] <- "Laborer"
CougC$contact5[CougC$contact5=='Coffee'] <- "Laborer"
CougC$contact5[CougC$contact5=='Colleague 1'] <- "Colleague"
CougC$contact5[CougC$contact5=='colleague 2'] <- "Colleague"
CougC$contact5[CougC$contact5=='Colleague 3'] <- "Colleague"
CougC$contact5[CougC$contact5=='Conveient store employee'] <- "Laborer"
CougC$contact5[CougC$contact5=='coworker'] <- "Colleague"
CougC$contact5[CougC$contact5=='Coworker'] <- "Colleague"
CougC$contact5[CougC$contact5=='Coworker N'] <- "Colleague"
CougC$contact5[CougC$contact5=='Employee 1'] <- "Laborer"
CougC$contact5[CougC$contact5=='Fedex delivery driver'] <- "Laborer"
CougC$contact5[CougC$contact5=='friend'] <- "Friend"
CougC$contact5[CougC$contact5=='front office person'] <- "Laborer"
CougC$contact5[CougC$contact5=='Graduate Student'] <- "Student"
CougC$contact5[CougC$contact5=='grandson'] <- "Family Member"
CougC$contact5[CougC$contact5=='mail carrier'] <- "Laborer"
CougC$contact5[CougC$contact5=='Mother-in-law'] <- "Family Member"
CougC$contact5[CougC$contact5=='Mother in Law'] <- "Family Member"
CougC$contact5[CougC$contact5=='Other daycare parent'] <- "Stranger"
CougC$contact5[CougC$contact5=='Receptionist'] <- "Laborer"
CougC$contact5[CougC$contact5=='Retail employee'] <- "Laborer"
CougC$contact5[CougC$contact5=='store clerk'] <- "Laborer"
CougC$contact5[CougC$contact5=='Walker 2'] <- "Stranger"
table(CougC$contact5, useNA = "always")

table(CougC$locationcontact5, useNA = "always")
CougC$locationcontact5[CougC$locationcontact5=='athletic club'] <- "Gym"
CougC$locationcontact5[CougC$locationcontact5=='Bustad'] <- "WSU Pullman"
CougC$locationcontact5[CougC$locationcontact5=='Cougar Health Services'] <- "WSU Pullman/Health Care Facility"
CougC$locationcontact5[CougC$locationcontact5=='Eastlick Hall'] <- "WSU Pullman"
CougC$locationcontact5[CougC$locationcontact5=='Daycare'] <- "School"
CougC$locationcontact5[CougC$locationcontact5=='Counseling and Psychological Services'] <- "WSU Pullman"
CougC$locationcontact5[CougC$locationcontact5=='fast food window'] <- "Drive Thru"
CougC$locationcontact5[CougC$locationcontact5=='grocery store'] <- "Store"
CougC$locationcontact5[CougC$locationcontact5=='Grocery Store/Pullman'] <- "Pullman"
CougC$locationcontact5[CougC$locationcontact5=='hall'] <- "Home"
CougC$locationcontact5[CougC$locationcontact5=='Health clinic'] <- "Health Care Facility"
CougC$locationcontact5[CougC$locationcontact5=='Her home'] <- "SEH"
CougC$locationcontact5[CougC$locationcontact5=='home'] <- "Home"
CougC$locationcontact5[CougC$locationcontact5=='Home/live together'] <- "Home"
CougC$locationcontact5[CougC$locationcontact5=='ice skating rink'] <- "Other"
CougC$locationcontact5[CougC$locationcontact5=='in car driving to/from practice'] <- "Car"
CougC$locationcontact5[CougC$locationcontact5=='Moscow, at door of elementary school'] <- "Moscow ID"
CougC$locationcontact5[CougC$locationcontact5=='My car picking up the dog'] <- "Car"
CougC$locationcontact5[CougC$locationcontact5=='outdoors workout'] <- "Outdoors"
CougC$locationcontact5[CougC$locationcontact5=='Outside > 6 feet apart'] <- "Outdoors"
CougC$locationcontact5[CougC$locationcontact5=='Outside my house, they were on sidewalk and I was in my yard'] <- "Outdoors"
CougC$locationcontact5[CougC$locationcontact5=='Outside of house'] <- "Outdoors"
CougC$locationcontact5[CougC$locationcontact5=='Pullman Building Supply'] <- "Pullman"
CougC$locationcontact5[CougC$locationcontact5=='Richland, WA'] <- "Richland WA"
CougC$locationcontact5[CougC$locationcontact5=='Roost cafe'] <- "Pullman"
CougC$locationcontact5[CougC$locationcontact5=='safeway'] <- "Store"
CougC$locationcontact5[CougC$locationcontact5=='Starbucks'] <- "Restaurant"
CougC$locationcontact5[CougC$locationcontact5=='Safeway'] <- "Store"
CougC$locationcontact5[CougC$locationcontact5=='Starbucks drive-thru window'] <- "Drive Thru"
CougC$locationcontact5[CougC$locationcontact5=='tennis courts'] <- "Gym"
CougC$locationcontact5[CougC$locationcontact5=='Their house'] <- "SEH"
CougC$locationcontact5[CougC$locationcontact5=='Thompson Hall'] <- "WSU Pullman"
CougC$locationcontact5[CougC$locationcontact5=='Trader Joe\'s Spokane'] <- "Spokane Area WA"
CougC$locationcontact5[CougC$locationcontact5=='VBR-Pullman Campus'] <- "WSU Pullman"
table(CougC$locationcontact5, useNA = "always")


#onto 6
table(CougC$contact6, useNA = "always")
CougC$contact6[CougC$contact6=='Cashier'] <- "Laborer"
CougC$contact6[CougC$contact6=='child\'s friend'] <- "Friend"
CougC$contact6[CougC$contact6=='Child 1'] <- "Child"
CougC$contact6[CougC$contact6=='Cleaning Person 2'] <- "Laborer"
CougC$contact6[CougC$contact6=='clerk'] <- "Laborer"
CougC$contact6[CougC$contact6=='co-worker'] <- "Colleague"
CougC$contact6[CougC$contact6=='Co-worker'] <- "Colleague"
CougC$contact6[CougC$contact6=='colleague 2'] <- "Colleague"
CougC$contact6[CougC$contact6=='coworker'] <- "Colleague"
CougC$contact6[CougC$contact6=='Coworker'] <- "Colleague"
CougC$contact6[CougC$contact6=='Coworker H'] <- "Colleague"
CougC$contact6[CougC$contact6=='Colleage'] <- "Colleague"
CougC$contact6[CougC$contact6=='Employee 2'] <- "Laborer"
CougC$contact6[CougC$contact6=='Father-in-law'] <- "Family Member"
CougC$contact6[CougC$contact6=='Neigbor'] <- "Neighbor"
CougC$contact6[CougC$contact6=='Non-profit worker'] <- "Laborer"
CougC$contact6[CougC$contact6=='Pedestrian met during dog walk'] <- "Stranger"
CougC$contact6[CougC$contact6=='Student 1'] <- "Student"
CougC$contact6[CougC$contact6=='TCCON lab preceptor'] <- "Colleague"
table(CougC$contact6, useNA = "always")

table(CougC$locationcontact6, useNA = "always")
CougC$locationcontact6[CougC$locationcontact6=='An office'] <- "Office"
CougC$locationcontact6[CougC$locationcontact6=='Bustad'] <- "WSU Pullman"
CougC$locationcontact6[CougC$locationcontact6=='friend\'s garage which hosts learning pod'] <- "SEH"
CougC$locationcontact6[CougC$locationcontact6=='Eastlick Hall'] <- "WSU Pullman"
CougC$locationcontact6[CougC$locationcontact6=='hall'] <- "Other"
CougC$locationcontact6[CougC$locationcontact6=='Counseling and Psychological Services'] <- "WSU Pullman"
CougC$locationcontact6[CougC$locationcontact6=='in my car'] <- "Car"
CougC$locationcontact6[CougC$locationcontact6=='lab'] <- "WSU Campus"
CougC$locationcontact6[CougC$locationcontact6=='Living room'] <- "Home"
CougC$locationcontact6[CougC$locationcontact6=='On the street'] <- "Outdoors"
CougC$locationcontact6[CougC$locationcontact6=='Outside >6 feet apart'] <- "Outdoors"
CougC$locationcontact6[CougC$locationcontact6=='Paccar Building'] <- "WSU Pullman"
CougC$locationcontact6[CougC$locationcontact6=='Roost Coffee'] <- "Pullman"
CougC$locationcontact6[CougC$locationcontact6=='Rosauer\'s Lewiston'] <- "Lewiston ID"
CougC$locationcontact6[CougC$locationcontact6=='rennis courts'] <- "Gym"
CougC$locationcontact6[CougC$locationcontact6=='Their house'] <- "SEH"
CougC$locationcontact6[CougC$locationcontact6=='Their office/Pullman'] <- "Pullman"
CougC$locationcontact6[CougC$locationcontact6=='Thompson Hall'] <- "WSU Pullman"
CougC$locationcontact6[CougC$locationcontact6=='VBR-Pullman Campus'] <- "WSU Pullman"
CougC$locationcontact6[CougC$locationcontact6=='WSU TCCON'] <- "WSU Campus"
table(CougC$locationcontact6, useNA = "always")

#SEvens!!
table(CougC$contact7, useNA = "always")
CougC$contact7[CougC$contact7=='cashier'] <- "Laborer"
CougC$contact7[CougC$contact7=='Cashier'] <- "Laborer"
CougC$contact7[CougC$contact7=='Child 2'] <- "Child"
CougC$contact7[CougC$contact7=='co-worker'] <- "Colleague"
CougC$contact7[CougC$contact7=='Colleague 3'] <- "Colleague"
CougC$contact7[CougC$contact7=='coworker'] <- "Colleague"
CougC$contact7[CougC$contact7=='Coworker'] <- "Colleague"
CougC$contact7[CougC$contact7=='grocery store checker'] <- "Laborer"
CougC$contact7[CougC$contact7=='Parent 1'] <- "Family Member"
CougC$contact7[CougC$contact7=='Receptionist'] <- "Laborer"
CougC$contact7[CougC$contact7=='spouse'] <- "Spouse"
CougC$contact7[CougC$contact7=='Student 2'] <- "Student"
table(CougC$contact7, useNA = "always")

table(CougC$locationcontact7, useNA = "always")
CougC$locationcontact7[CougC$locationcontact7=='Counseling and Psychological Services'] <- "WSU Pullman"
CougC$locationcontact7[CougC$locationcontact7=='Eastlick Hall'] <- "WSU Pullman"
CougC$locationcontact7[CougC$locationcontact7=='Emergency room'] <- "Health Care Facility"
CougC$locationcontact7[CougC$locationcontact7=='grocery store'] <- "Store"
CougC$locationcontact7[CougC$locationcontact7=='home, multiple places'] <- "Home"
CougC$locationcontact7[CougC$locationcontact7=='my office'] <- "Office"
CougC$locationcontact7[CougC$locationcontact7=='outdoors on a walk'] <- "Outdoors"
CougC$locationcontact7[CougC$locationcontact7=='Paccar Building'] <- "WSU Pullman"
CougC$locationcontact7[CougC$locationcontact7=='Outside their house as we were on a walk.'] <- "Outdoors"
CougC$locationcontact7[CougC$locationcontact7=='Petco Lewiston'] <- "Lewiston ID"
CougC$locationcontact7[CougC$locationcontact7=='walmart'] <- "Store"
CougC$locationcontact7[CougC$locationcontact7=='WSU TCCON lab'] <- "WSU Campus"
table(CougC$locationcontact7, useNA = "always")

#Eights
table(CougC$contact8, useNA = "always")
CougC$contact8[CougC$contact8=='cashier at grocery store'] <- "Laborer"
CougC$contact8[CougC$contact8=='donut shop worker'] <- "Laborer"
CougC$contact8[CougC$contact8=='Drive-through employee'] <- "Laborer"
CougC$contact8[CougC$contact8=='Nephew'] <- "Family Member"
CougC$contact8[CougC$contact8=='Parent 2'] <- "Family Member"
CougC$contact8[CougC$contact8=='Person in waiting room'] <- "Stranger"
table(CougC$contact8, useNA = "always")

table(CougC$locationcontact8, useNA = "always")
CougC$locationcontact8[CougC$locationcontact8=='A drive through in Pullman'] <- "Pullman"
CougC$locationcontact8[CougC$locationcontact8=='Doctor\'s office waiting room'] <- "Health Care Facility"
CougC$locationcontact8[CougC$locationcontact8=='donut shop'] <- "Store"
CougC$locationcontact8[CougC$locationcontact8=='In house, outside of house'] <- "Home"
CougC$locationcontact8[CougC$locationcontact8=='My house'] <- "Home"
CougC$locationcontact8[CougC$locationcontact8=='Winco in Richland, WA'] <- "Richland WA"
table(CougC$locationcontact8, useNA = "always")

#nines
table(CougC$contact9, useNA = "always")
CougC$contact9[CougC$contact9=='Food Service'] <- "Laborer"
CougC$contact9[CougC$contact9=='Food service person at Subway'] <- "Laborer"
table(CougC$contact9, useNA = "always")

table(CougC$locationcontact9, useNA = "always")
CougC$locationcontact9[CougC$locationcontact9=='Subway on Lee Blvd in Richland, WA'] <- "Richland WA"
CougC$locationcontact9[CougC$locationcontact9=='Thai Restaurant'] <- "Restaurant"
table(CougC$locationcontact9, useNA = "always")

table(CougC$another_9)
#No one afterwards!

#tens
table(CougC$contact10, useNA = "always")
#NA only

#cleaned all location and contact that are in the 337

write.csv(CougC, "CougConnect_21621.csv")
#we will then clean some more and then do the year ago cleaning
#If there is a plural (students, children, etc) we will assume two unless it's mentioned otherwise (aka students-12)
#If there is nothing clear about relationship, we put stranger (other parent, gym member, swimmer) as we assume if was friend/family would have been mentioned
 
#NA only