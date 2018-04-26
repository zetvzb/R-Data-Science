#This is a Random Forrest Model for Animal Shelter Outcomes. 
setwd("/Users/zachtallevast/Box Sync/Kaggle Shelter Animal Outcomes/")
set.seed(1)
full <- read.csv("Shelter Outcomes TRAIN.csv", header=T)
library(plyr) 
library(MLmetrics) #For Log Loss Calc
library(ggplot2)
library(ggthemes)
library(dplyr)
library(lubridate)
library(rpart)
library(randomForest)
full$DateTime <- as.character(full$DateTime)
summary(full$AnimalType)
#Cat   Dog 
#11134 15595

#This Plot creates a table with Animal Type, Outcome Type, and Number of each category. 
outcomes <- full[1:26729, ] %>%
  group_by(AnimalType, OutcomeType) %>% 
  summarise(num_animals =n())
ggplot(outcomes, aes(x=AnimalType,y=num_animals, fill=OutcomeType)) + 
  geom_bar(stat = 'identity', position = 'fill', color='black')+
  coord_flip()+
  labs(y='Proportion of Animals', x='Animal', title = 'Outcomes: Cats & Dogs')+
  theme_few()
#Notice Cats are twice as likely to be Transferred to a new owner and full are four times as likely to be returned to owner than cats. 
##########################################################################
#create Indicator for "Intact or Unknown" for SexUponOutcome 
#create Indicator for "Sex" of Animal 
full$Fixed <- ifelse(grepl('Intact', full$SexuponOutcome),1,ifelse(grepl('Unknown',full$SexuponOutcome), 'Unknown',0))
full$Sex <- ifelse(grepl('Male', full$SexuponOutcome),'Male',ifelse(grepl('Unknown',full$Sex), 'Unknown','Female'))
full$Fixed = as.factor(full$Fixed)
full$Sex = as.factor(full$Sex)
#Separate Dogs and Cats
cats = full[full$AnimalType == "Cat",]
dogs = full[full$AnimalType == "Dog",]
ggplot(dogs, aes(x=Fixed, fill = OutcomeType))+
  geom_bar(stat = "count", position = "fill", width = 0.7)+
  coord_flip()+
  ggtitle("Dogs by Intact")

ggplot(cats, aes(x=Fixed, fill = OutcomeType))+
  geom_bar(stat = "count", position = "fill", width = 0.7)+
  coord_flip()+
  ggtitle("Cats by Intact")

#This plot shows that once dogs are fixed, they are more adoptable. 
##########################################################################
#Correction to Breed 
nlevels(factor(full$Breed))[1:10]
#1380 Breeds! Cant predict with that many breeds

#First lets take out Mix breeds denoted (breed) mix.
full$Mix <- ifelse(grepl('Mix', full$Breed), 1,0)
full$Mix = as.factor(full$Multicolor)
#Split on "/" and Remove "Mix"
full$Breed = as.character(full$Breed)
full$SimpleBreed <-sapply(full$Breed, function(x) gsub(' Mix', '', strsplit(x, split = '/')[[1]][1]))
nlevels(factor(full$SimpleBreed))[1:10]
full$SimpleBreed = as.factor(full$SimpleBreed)
#Now 220 Breeds! 
#Dog Breed By Outcome 
filter(train, AnimalType == "Dog") %>%
  ggplot(aes(x=simplebreed,fill=OutcomeType))+
  geom_bar(stat="count", position = "fill", width=0.6)+
  ggtitle ("Dog Breeds by Outcome") +
  coord_flip ()+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.title = element_blank())
###########################################################################
#Now lets deal with MISSING VALUES 
#For Name of animal, if its missing lets call them "Nameless"
full$Name = as.character(full$Name)
full$Name <- ifelse(nchar(full$Name)==0, 'Nameless', full$Name)
#Make an Indicator Variable for if they have a name or not. 1= Has Name 0=Has No Name. Could be a predictor 
full$HasAName[full$Name =='Nameless'] <-0
full$HasAName[full$Name !='Nameless'] <-1
full$HasAName = as.factor(full$HasAName)
##########################################################################
#CLEAN AGE OF ANIMALS. Display age in Days across all animals
factor(full$AgeuponOutcome)[1:10]
#That is an absolute mess of 45 levels
#Split into Time Value e.g. 1 or 12 
#Split into Unit of Time e.g. "Year", "Month" 
full$AgeuponOutcome = as.character(full$AgeuponOutcome)
full$TimeValue <- sapply(full$AgeuponOutcome, function(x) strsplit(x,split = ' ') [[1]][1])
full$UnitofTime <-sapply(full$AgeuponOutcome, function(x) strsplit(x,split = ' ') [[1]][2])
#Get Rid of Plurals on the Units of time. e.g. Turn "Years" into "Year"
full$UnitofTime <-gsub('s', '', full$UnitofTime) 
full$TimeValue <- as.numeric(full$TimeValue)
#Standardize across ages 
Multiplier <- ifelse(full$UnitofTime == 'day',1, ifelse(full$UnitofTime =='week', 7, ifelse(full$UnitofTime =='month', 30, ifelse(full$UnitofTime == 'year', 365, NA))))
full$AgeinDays <- full$TimeValue * Multiplier
summary(full$AgeinDays)
full$AgeinDays = as.factor(full$AgeinDays)
sum(is.na(full$AgeinDays))
#There are 18 Missing Ages
#Do something about missing ages. We Will PREDICT THEM USING RPART
#full$AgeinDays = as.numeric(as.character(full$AgeinDays))
#age_fit <- rpart(AgeinDays ~ AnimalType + Sex+ Intact + SimpleBreed+HasAName, 
 #               data = full[!is.na(full$AgeinDays), ],
 #               method = 'anova')

#Put predicted values into missing values 
#full$AgeinDays[is.na(full$AgeinDays)] <-predict(age_fit, full[is.na(full$AgeinDays),])
#sum(is.na(full$AgeinDays))
table(full$AgeinDays)
#Plot age by outcome type 
cats = full[full$AnimalType == "Cat",]
dogs = full[full$AnimalType == "Dog",]
#Do Cats
ggplot(cats, aes(x=AgeinDays, fill=OutcomeType))+
           geom_bar(stat = "count", position = "fill", alpha = 0.9)+
           scale_fill_brewer(palette = "Set1")+
           ggtitle("Relative Frequency of outcomes by Age in Days-CAT")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size=11, color = "green"), 
        legend.position = "bottom", 
        axis.title.y = element_text(angle = 90, color = "blue"))
#Do Dogs
ggplot(dogs, aes(x=AgeinDays, fill=OutcomeType))+
  geom_bar(stat = "count", position = "fill", alpha = 0.9)+
  
  scale_fill_brewer(palette = "Set1")+
  ggtitle("Relative Frequency of outcomes by Age in Days-DOG")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size=11, color = "green"), 
        legend.position = "bottom", 
        axis.title.y = element_text(angle = 90, color = "blue"))

#Notice the oldest animal is 20 years old, but the median is 1 year. 
##########################################################################
#For Blank Sex upon Outcome, lets replace it with unknown
summary(full$SexuponOutcome)
full$SexuponOutcome =as.character(full$SexuponOutcome)
full$SexuponOutcome<-ifelse(nchar(full$SexuponOutcome)==0, 'Unknown', full$SexuponOutcome)
full$SexuponOutcome = as.factor(full$SexuponOutcome)
################################################################################
#Correction to time and date Variables
full$DateTime = as.character(full$DateTime)
full$Date <- sapply(full$DateTime, function(x) strsplit(x,split = ' ') [[1]][1])
full$Time <-sapply(full$DateTime, function(x) strsplit(x, split = ' ') [[1]][2])
full$Date <-as.Date(full$Date, "%m/%d/%y")
full$DateTime <-paste(full$Date, " ", full$Time)
full$DateTime <-as.character(full$DateTime)

full$Hour <-hour(full$DateTime) 
full$Weekday <-wday(full$DateTime) 
full$Hour <- as.factor(full$Hour)
full$Weekday <- as.factor(full$Weekday)
###################################################################################
#Correction to Time of Day
full$Hour <-as.numeric(full$Hour)
full$TimeOfDay <- ifelse(full$Hour> 5 & full$Hour < 11 , 'Morning', 
                         ifelse(full$Hour >10 & full$Hour <16, 'Midday', 
                                ifelse(full$Hour >15 & full$Hour <20, 'LateDay', 'After Work Hours')))

full$TimeOfDay <-factor(full$TimeOfDay, levels = c('Morning', 'Midday','LateDay', 'After Work Hours'))
full$TimeOfDay <- as.factor(full$TimeOfDay)
daytimes <-full[1:26729, ] %>% group_by(AnimalType, TimeOfDay, OutcomeType) %>%
  summarise(num_animals = n())
ggplot(daytimes, aes(x=TimeOfDay, y=num_animals, fill=OutcomeType, na.rm=TRUE))+
  geom_bar(stat='identity', position = 'fill', color = 'black')+
facet_wrap(~AnimalType)+
  coord_flip()+
  labs(y='Proportion of Animals', x='Animal', title = 'Outcomes by Time of Day')+
  theme_few()
##########################################################################
#Correction to Color. Take out the Green/Blue colors. Use only first color. 
nlevels(factor(full$Color))[1:10] #366 Colors
full$Color = as.character(full$Color)
full$SimpleColor <- sapply(full$Color, function(x) strsplit(x, split = '/') [[1]][1])
full$SimpleColor <- ifelse(full$SimpleColor == 'Brown Brindle', 'Brown', full$SimpleColor)
full$SimpleColor <- ifelse(full$SimpleColor == 'Brown Merle', 'Brown', full$SimpleColor)
full$SimpleColor <- ifelse(full$SimpleColor == 'Black Brindle', 'Black', full$SimpleColor)
full$SimpleColor <- ifelse(full$SimpleColor == 'Chocolate Point', 'Chocolate', full$SimpleColor)
full$SimpleColor = as.factor(full$SimpleColor)
nlevels(factor(full$SimpleColor))[1:10] #53 Colors

#Does Color play a role in the outcome? 
cats = full[full$AnimalType == "Cat",]
dogs = full[full$AnimalType == "Dog",]
#Do Dogs first. Onlt plot colors with sample size of at least 400 
dogs %>% count(SimpleColor) %>% arrange(desc(n)) %>% 
  filter(n > 400) -> dog_colors
dogs$SimpleColor = as.character(dogs$SimpleColor)
dogs$SimpleColor[!(dogs$SimpleColor %in% dog_colors$SimpleColor)] <- "Other"
ggplot(dogs, aes(x=SimpleColor, fill=OutcomeType)) + 
  geom_bar(stat = "count", position = "fill", width = 0.5)+
  coord_flip()+
  ggtitle ("Dog Colors by Outcome")+
  scale_fill_brewer(palette = "Set1")
#Now Do Cats. Only plot colors with sample size at least 400
cats %>% count(SimpleColor) %>% arrange(desc(n)) %>% 
  filter(n > 400) -> cat_colors
cats$SimpleColor = as.character(cats$SimpleColor)
cats$SimpleColor[!(cats$SimpleColor %in% cat_colors$SimpleColor)] <- "Other"
ggplot(cats, aes(x=SimpleColor, fill=OutcomeType)) + 
  geom_bar(stat = "count", position = "fill", width = 0.5)+
  coord_flip()+
  ggtitle ("Cat Colors by Outcome")+
scale_fill_brewer(palette = "Set1")
##################################################################
#Make Age Binning
full$AgeinDays = as.numeric(as.character(full$AgeinDays))
full$AgeBin[full$AgeinDays <= 365] <- 'Baby'
full$AgeBin[full$AgeinDays <2190 & full$AgeinDays >=365] <- 'Adult'
full$AgeBin[full$AgeinDays >=2190] <- 'Senior'
full$AgeBin <- as.factor(full$AgeBin)

ggplot(data = subset(full,!is.na(full$AgeBin)), aes(x=AgeBin, fill =OutcomeType)) + 
  geom_bar(stat="count", position = "fill", width = 0.8)+
  coord_flip()+
  ggtitle("Outcomes by Age-AgeBinning")+
  scale_fill_brewer(palette = "Set1")
#############################################################################
#Create Indicator if outcome data is during Holidays. October-December. October is national adopt a dog month
full$Month <- sapply(full$DateTime, function(x) strsplit(x,split = '-') [[1]][2])
full$Month <-sapply(full$Month, function(x) strsplit(x,split = ' ') [[1]][1])
full$HolidaySeason <- ifelse(full$Month <10, 'Non-Holiday', 'Holiday')
full$Month = as.factor(full$Month)
full$HolidaySeason = as.factor(full$HolidaySeason)
###########################################################################
#Create a Top ten Most Adopted DOG by ASPCA covariate
full$TopAdopted <-ifelse(full$SimpleBreed == 'Bull Dog', 'Top 10', 
ifelse(full$SimpleBreed == 'Beagle', 'Top 10',ifelse(full$SimpleBreed == 'Bull Terrier', 'Top 10',
  ifelse(full$SimpleBreed == 'Collie', 'Top 10',ifelse(full$SimpleBreed == 'Newfoundland', 'Top 10',
  ifelse(full$SimpleBreed == 'Vizsla', 'Top 10',ifelse(full$SimpleBreed == 'Irish Setter', 'Top 10',
  ifelse(grepl( 'Poodle',full$SimpleBreed), 'Top 10',ifelse(full$SimpleBreed == 'Labrador Retriever', 'Top 10',
  ifelse(full$SimpleBreed == 'Golden Retreiver', 'Top 10', ifelse(full$SimpleBreed == 'Persian', 'Top 5', 
  ifelse(full$SimpleBreed == 'American Shorthair', 'Top 5', ifelse(full$SimpleBreed == 'Maine Coon', 'Top 5', 
  ifelse(full$SimpleBreed == 'Siamese', 'Top 5', ifelse(full$SimpleBreed == 'Abyssinian', 'Top 5', 'Not Top 10')))))))))))))))
full$TopAdopted <- as.factor(full$TopAdopted)
##########################################################################
#Create a Top Ten Breeds of Animals in Shelters covariate
table(full$SimpleBreed)
full$TopTenSheltered <-ifelse(full$SimpleBreed == 'Bulldog', 'Top 10', 
ifelse(full$SimpleBreed == 'Beagle', 'Top 10',ifelse(full$SimpleBreed == 'Boxer', 'Top 10',
  ifelse(full$SimpleBreed == 'Chihuahua Shorthair', 'Top 10',ifelse(full$SimpleBreed == 'Dachshund', 'Top 10',
  ifelse(full$SimpleBreed == 'German Shepherd', 'Top 10',ifelse(full$SimpleBreed == 'Labrador Retreiver', 'Top 10',
  ifelse(full$SimpleBreed == 'Pit Bull', 'Top 10',ifelse(full$SimpleBreed == 'Bull Terrier', 'Top 10',
  ifelse(full$SimpleBreed == 'Rottweiler', 'Top 10', ifelse(full$SimpleBreed =='Siamese', 'Top 5', 
  ifelse(full$SimpleBreed == 'Persian', 'Top 5',ifelse(full$SimpleBreed == 'Maine Coon', 'Top 5', 
  ifelse(full$SimpleBreed == 'Ragdoll', 'Top 5', ifelse(full$SimpleBreed == 'Abyssinian', 'Top 5','Not Top')))))))))))))))
full$TopTenSheltered <- as.factor(full$TopTenSheltered)
#############################################################################################
#Average Life Span 
Lifespan <-read.csv("Breed Lifespans.csv", header=T)
Lifespan = unique(Lifespan)
Lifespan$AVERAGE.LIFESPAN..YEARS. = ifelse(Lifespan$Breed == 'Yorkshire Terrier', 13, Lifespan$AVERAGE.LIFESPAN..YEARS.)
Lifespan$AVERAGE.LIFESPAN..YEARS. = ifelse(Lifespan$Breed == 'Tonkinese', 15, Lifespan$AVERAGE.LIFESPAN..YEARS.)
Lifespan = unique(Lifespan)
Lifespan = Lifespan[-c(72),]
full <-merge(x=full, y=Lifespan, by.x = "SimpleBreed", by.y = "Breed", all.x=TRUE, incomparables =='Unknown')
full$AVERAGE.LIFESPAN..YEARS.[is.na(full$AVERAGE.LIFESPAN..YEARS.)] <- 10
full$AVERAGE.LIFESPAN..YEARS. <- as.factor(full$AVERAGE.LIFESPAN..YEARS.)
sum(is.na(full$AVERAGE.LIFESPAN..YEARS.))
#############################################################################################
#Hunting Dog 
Hunting <- read.csv("Hunting Dog.csv", header=T)
Hunting = unique(Hunting)
full <- merge(x=full, y = Hunting, by.x = "SimpleBreed", by.y ="Breed", all.x=TRUE, incomparables = 'Unknown')
full$Hunting.Dog[is.na(full$Hunting.Dog)] <- "No"
sum(is.na(full$Hunting.Dog))
full$Hunting.Dog <- as.factor(full$Hunting.Dog)
########################################################################################
#Black Dog 
full$Black <- ifelse(grepl('Black', full$Color), 'Black', 
                     ifelse(grepl('Brindle', full$Color), 'Black', 'Not Black'))
full$Black <- as.factor(full$Black)
###########################################################################################
#Create a Hypoallergenic Variable. Split on Cats and Dogs
full$DOGHypo = full$SimpleBreed
full$DOGHypo = ifelse(grepl('Poodle', full$DOGHypo), 1,
ifelse(grepl('Yorkshire', full$DOGHypo), 1,ifelse(grepl('Havanese', full$DOGHypo), 1,ifelse(grepl('Maltese', full$DOGHypo), 1, 
  ifelse(grepl('Shih Tzu', full$DOGHypo), 1, ifelse(grepl('Schnauzer', full$DOGHypo), 1, ifelse(grepl('Basenji', full$DOGHypo), 1,
  ifelse(grepl('Silky Terrier', full$DOGHypo), 1,ifelse(grepl('Irish Water Spaniel', full$DOGHypo), 1,ifelse(grepl('Terrier', full$DOGHypo), 1,
  ifelse(grepl('Terr', full$DOGHypo), 1,ifelse(grepl('Afghan Hound', full$DOGHypo), 1,ifelse(grepl('Water Spaniel', full$DOGHypo), 1, 
  ifelse(grepl('Spanish Water', full$DOGHypo), 1, ifelse(grepl('Water Dog', full$DOGHypo), 1, ifelse(grepl('Bichon Frise', full$DOGHypo), 1, 0))))))))))))))))

full$CATHypo = full$SimpleBreed
full$CATHypo = ifelse(grepl('Siberian', full$CATHypo), 1,ifelse(grepl('Balinese', full$CATHypo), 1, ifelse(grepl('Bengal', full$CATHypo), 1, 
ifelse(grepl('Burmese', full$CATHypo), 1, ifelse(grepl('Siberian', full$CATHypo), 1, ifelse(grepl('Cornish Rex', full$CATHypo), 1, 
  ifelse(grepl('Devon Rex', full$CATHypo), 1, ifelse(grepl('Javanese', full$CATHypo), 1,ifelse(grepl('Ocicat', full$CATHypo), 1, 
  ifelse(grepl('Russian Blue', full$CATHypo), 1, ifelse(grepl('Siamese', full$CATHypo), 1, ifelse(grepl('Sphynx', full$CATHypo), 1, 0))))))))))))
full$DOGHypo= as.factor(full$DOGHypo)
full$CATHypo = as.factor(full$CATHypo)
###########################################################################################
#Animal Size 
full$SmallAnimal <- as.numeric(full$SimpleBreed %in% 
c(grepl('Miniature', full$SimpleBreed),'Dandie Dinmount',grepl('Mini', full$SimpleBreed),
grepl('Toy', full$SimpleBreed),grepl('Chihuahua', full$SimpleBreed), 'Dachshund', 'Maltese', 'Pomeranian',
'Shih Tzu','Yorkshire Terrier', 'Havanese', 'Pug', 'Boston Terrier','Coton De Tulear', 'Cavalier Span', 'Chinese Crested',
'Bruss Griffon', 'Bedlington Terr', 'Bichon Frise', 'Affenpinscher', 'Japanese Chin', 'Finnish Spitz', 'Feist', 
'Manchester Terrier','Schipperke','Border Terrier', 'Ppdengo Pequeno', 'Patterdale Terr', 'Manchester Terrier', 
'Bichons Frise', 'West Highland Terrier', 'Papillon','Silky Terrier', 'Lowchen','Jack Russell Terrier', 
'Cavalier King Charles Spaniel', 'Beagle', 'Shetland Sheepdog', 'Welsh Terrier','Swedish Vallhund','Corgi','Norfolk Terrier',
'Sealyham Terr', 'Scottish Terrier', 'French Bulldog','Flat Coat Retriever/Papillon','Shiba Inu','Fox Terrier','West Highland',
'Papillon','Russell Terrier','Pekingese','Norwich Terrier','Cairn Terrier','Irish Terrier','Italian Greyhound',
'Manchester Terrier','Lhasa Apso','Papillon','Russell Terrier','Pekingese','Norwich Terrier','Rat Terrier','Tibetan',
'Jack Russell Terrier','Pembroke Welsh Corgi/Brittany','Pembroke Welsh Corgi','Scottish Terrier/Cairn Terrier'))
                                                                                                                                 
full$MediumAnimal <- as.numeric(full$SimpleBreed %in% c('Border Collie', 'Brittany','English Shepherd','Australian Water Spaniel', 'Australian Cattle Dog', 'Australian Shepherd',
'Basenji','English Springer Spaniel', 'Barbet', 'Basset Hound','Cardigan Welsh Corgi', 'Dalmatian', 'Chinese Sharpei', 'Boykin Span', 'Carolina Dog', 'Blue Lacy',
'Welsh Springer Spaniel', 'Treeing Tennesse Brindle', 'Treeing Cur', 'Skye Terrier', 'Bulldog','Cocker Spaniel', 
'Samoyed','Japanese Bobtail', 'Jindo', 'German Pinscher', 'Field Spaniel', 'English Springer Spaniel', 'Port Water Dog','Pbgv',
'Mexican Hairless', 'Keeshond', 'Whippet','Staffordshire',grepl('Pointer', full$Size), 'Pit Bull/Border Collie', 'Pit Bull/Blue Lacy', 'Pit Bull/Black Mouth Cur', 
'Pit Bull/Boston Terrier', 'Medium','Pit Bull/Border Terrier', 'Medium','Pit Bull/Beagle', 'Medium','Pit Bull/Carolina Dog', 'Pit Bull/Cardigan Welsh Corgi', 'Medium',
'Pit Bull/English Bulldog', 'Medium','Pit Bull/American Bull Dog', 'Persian', 'Abyssinian','Sphynx', 'Siamese','Exotic Shorthair', 'Scottish Fold', 'Cornish Rex', 'Devon Rex',
'Burmese','Tonkinese', 'Russian Blue', 'Manx', 'German Shepherd/Cardigan Welsh Corgi','Vizsla/Beagle','Wheaten Terrier','Shetland Sheepdog/Keeshold','Labrador Retriever/Beagle', 'Labrador Retriever/Blue Lacy', 'Labrador Retriever/Black Mouth Cur',
'Labrador Retriever/Boston Terrier', 'Labrador Retriever/Border Terrier','Turkish Angora','Scottish Terrier/Basset Hound',  'Catahoula/Cardigan Welsh Corgi', 'Catahoula/Border Collie', 'Catahoula/American Bulldog', 'Catahoula/Beagle', 'Catahoula/Black Mouth Cur', 'Pit Bull',
'Queensland Heeler','Jack Russell Terrier/Labrador Retriever','Cocker Spaniel','Collie Smooth/Beagle','Canaan Dog','English Bulldog','Glen Of Imaal','Nova Scotia Duck Tolling Retriever/Border Collie','Pharaoh Hound','Jack Russell Terrier/Pointer',grepl('Australian', full$Size),'Pug/Pit Bull','Pit Bull/Pug','Standard Schnauzer','Balinese', 'Bombay', 'Havana Brown','Bengal','Cymric','Snowshoe'))

full$LargeAnimal <- as.numeric(full$SimpleBreed %in% c('Coonhound','Afghan Hound', 'Airedale Terrier','Akita','American Foxhound','Bloodhound','Belgian Sheepdog','Boxer','Bull Terrier','Chow Chow','Golden Retreiver', "English Setter",
'German Shepherd','Irish Wolfhound','Labrador Retreiver','Rotweiler','Siberian Husky','Standard Poodle','Bernese Mountain Dog', 'Greyhound','Foxhound','Belgian Tervuren','Spinone Italiano','Schnauzer Giant','Plott Hound','Picardy Sheepdog','Hovawart','Ibizan Hound','Harrier','Gordon Setter','Dutch Shepherd/Anaotol Shepherd',
'Dogue De Bordeaux/American Bulldog','Doberman Pinsch','Dutch Shepherd/Boxer','Black/Tan Hound','Belgian Malinois','Beauceron',
'American Bulldog','Chesa Bay Retr','Bluetick Hound', 'Dogo Argentino/Chinese Sharpei','Dalmatian/Basset Hound','Entlebucher','English Foxhound','Dutch Shepherd','Dogo Argentino','Saluki','Saluki/Doberman Pinsch','Otterhound','Nova Scotia Duck Tolling Retriever','Wirehaired Pointing Griffon', grepl('Siberian Husky/Great Pyranees', full$Size),'Siberian Husky/Labrador Retriever', 'Siberian Husky/Catahoula','Siberian Husky/Rottweiler','Siberian Husky/Alaskan Malamute',grepl('Siberian Husky/Anatol Shepherd', full$Size),'Newfoundland/Border Collie','Flat Coat Retriever','St. Bernard Rough Coat/Border Collie','Vizsla','Whippet/Labrador Retriever','Whippet/Labrador Retriever','Weimaraner',grepl('Basset Hound', full$Size), 'British Shorthair', 'Ocicat', 'Chartreux','Collie','Greyhound','English Bulldog/Boxer', 'Norwegian Forest Cat', 'Turkish Van', 'Pixiebob Shorthair', 'Himalayan', 'American Wirehair', 'American Shorthair', 'Large','Rottweiler','Norwegian Elkhound','Rhod Ridgeback',grepl('Old English', full$Size),'Alaskan Husky','Alaskan Malamute','Plott Hound','Great Pyranees/Border Collie','Redbone Hound','Queensland Heeler/Great Dane','Golden Retriever/Chow Chow', 'Golden Retriever/Standard Poodle','Akita', grepl('Catahoula', full$Size)))

full$XLAnimal <- as.numeric(full$SimpleBreed %in% c('Black','Great Dane','Great Pyrenees','Irish Wolfhound','Saint Bernard','Mastiff','Newfoundland', 'Leonberger', 'Presa Canario','Landseer','Dogue De Bordeaux','Cane Corso', 
'Anatol Shepherd','Greater Swiss Mountain Dog', 'Bullmastiff','Boerboel', 'Kuvasz','St.Bernard','Mastiff',grepl('Great', full$Size),'Ragdoll','Maine Coon'))

full$SmallAnimal =as.factor(full$SmallAnimal)
full$MediumAnimal = as.factor(full$MediumAnimal)
full$LargeAnimal = as.factor(full$LargeAnimal)
full$XLAnimal = as.factor(full$XLAnimal)
full$Size = ifelse(full$SmallAnimal == '1', 'Small',
                   ifelse(full$MediumAnimal == '1', 'Medium', 
                        ifelse(full$LargeAnimal == '1', 'Large', 
                              ifelse(full$XLAnimal == '1', 'X-Large', 'Unknown'))))
full$Size =as.factor(full$Size)
########################################################################################
# Hair Length
full$ShortHair <- as.numeric(full$Breed %in% 
c('Sphynx','Boerboel','Kelpie',
'Belgian Malinois','Black Mouth','Blue Lacy','Border','Boston','Bullmastiff', 'Canaan', 'Cane',
'Carolina','Catahoula','Chesa', 'Chinese Crested','Doberman','Dogo Argentino',
grepl('Entlebucher', full$Hair),grepl('Dogue', full$Hair),grepl('Feist', full$Hair),grepl('Swiss Mountain Dog', full$Hair),
grepl('hound', full$Hair),grepl('Harrier', full$Breed),grepl('Pinscher', full$Hair),grepl('Manchester Terrier', full$Hair),
grepl('Mexican Hairless', full$Hair),grepl('Mastiff', full$Hair),grepl('Russell Terrier', full$Hair),grepl('Patterdale', full$Hair),
grepl('Pointer', full$Hair),grepl('Presa', full$Hair),grepl('Queensland Heeler', full$Hair),grepl('Rat Terrier', full$Hair),
grepl('Ridgeback', full$Breed),grepl('Saluki', full$Hair),grepl('Spanish Mastiff', full$Hair),grepl('Vizsla', full$Breed),
grepl('Staffordshire', full$Hair),grepl('Treeing Cur', full$Hair),grepl('Tennesse Brindle', full$Breed),grepl('Hound', full$Breed),
grepl('Bull Terrier', full$Hair),grepl('Weimaraner', full$Hair),grepl('Whippet', full$Hair),grepl('Fox Terrier', full$Hair),
grepl('Pug', full$Hair), 'Short',grepl('Coonhound', full$Hair),grepl('Wire Hair Fox Terrier', full$Breed),grepl('Rottweiler', full$Hair),
grepl('Dutch Shepherd', full$Breed),grepl('Greyhound', full$Breed),grepl('Shorthair', full$Breed),grepl('Great Dane', full$Breed),
grepl('Dachshund',full$Breed),grepl('German Shepherd', full$Breed),grepl('Labrador',full$Breed), grepl('Bloodhound',full$Breed), 
grepl('Bulldog',full$Breed),grepl('Chihuahua',full$Breed),grepl('Australian Cattle Dog',full$Breed),grepl('Dalmatian',full$Breed),
grepl('Basset Hound', full$Breed),grepl('Beauceron',full$Breed),grepl('Boxer',full$Breed),grepl('Basenji',full$Breed),
grepl('Beagle',full$Breed),grepl('American Foxhound',full$Breed), 'Siamese','Affenpinscher','Abyssinian','Oriental','Scottish','Burmese','Ocicat','Tonkinese','Devon',
 'Havana','Snowshoe','Bombay','Chartreux','Cornish','Cymric'))

full$MediumHair <-as.numeric(full$Breed %in% c('Manx','Alaskan Husky','Alaskan Malamute','Akita',
                                               'Anatol','Bedlington','Tervuren', 'Boykin','Brittany','Dandie',
                                               'Finnish','Glen Of Imaal','Hovawart', 'Irish Terrier', 'Setter', 'Jindo',
                                               'Norfolk Terrier', 'Norwegian Elkhound','Norwich Terrier','Pbgv', 'Corgi', 
                                               'Podengo','Schipperke', 
                                               grepl('Shiba Inu', full$Hair),grepl('Spinone Italiano', full$Hair),
                                               grepl('Spinone Italiano', full$Hair),grepl('Chow Chow', full$Hair),
                                               grepl('Swedish Vallhund', full$Hair),grepl('Collie', full$Hair),
                                               grepl('Welsh Terrier', full$Hair),grepl('West Highland', full$Hair),
                                               grepl('St. Bernard', full$Hair),grepl('Husky', full$Hair),
                                               grepl('Golden Retriever', full$Breed),grepl('Schnauzer', full$Hair),
                                               grepl('Spaniel', full$Hair),grepl('Airedale Terrier',full$Hair),
                                               grepl('Australian Shepherd', full$Hair),grepl('Border Collie', full$Hair),
                                              'Bengal','Poodle','Turkish Van',grepl('Husky', full$Hair),'Wirehair',
                                              grepl('Retriever', full$Breed)))
full$LongHair <- as.numeric(full$Breed %in% c('Persian','American Eskimo','Bichon Frise', 
                                              grepl('Australian Terrier', full$Hair),grepl('Bruss', full$Hair),
                                              grepl('Cairn', full$Hair),grepl('Cavalier', full$Hair),
                                              grepl('Coton', full$Hair),grepl('English Shepherd', full$Hair),
                                              grepl('Pyrenees', full$Hair),grepl('Japanese Chin', full$Hair),
                                              grepl('Keeshond', full$Hair),grepl('Kuvasz', full$Hair),
                                              grepl('Landseer', full$Hair),grepl('Leonberger', full$Hair),
                                              grepl('Lowchen', full$Hair),grepl('Newfoundland', full$Hair),
                                              grepl('Newfoundland', full$Hair),grepl('Otterhound', full$Hair),
                                              grepl('Papillon', full$Hair),grepl('Pekingese', full$Hair),
                                              grepl('Port Water Dog', full$Hair),grepl('Sealyham Terr', full$Hair),
                                              grepl('Samoyed', full$Hair),grepl('Wheaten Terrier', full$Hair),
                                              grepl('Silky Terrier', full$Hair),grepl('Scottish Terrier', full$Hair),
                                              grepl('Tibetan Terrier', full$Hair),grepl('Wirehaired Pointing Griffon', full$Hair),
                                              grepl('Skye Terrier', full$LongHair),grepl('Sheepdog', full$Breed),
                                              grepl('Shetland Sheepdog', full$Breed),grepl('Belgian Sheepdog', full$Hair),
                                              grepl('Bearded Collie', full$Hair),grepl('Pit Bull', full$Hair),'Dachshund Longhair',
                                              'Black','Maine Coon','Norwegian','Balinese','Javanese','Russian Blue','Ragdoll',
                                              'Angora','Himalayan','Bobtail','Bearded Collie','Yorkshire Terrier', 'Afghan Hound',
                                              'Havanese','Bernese Mountain Dog','Lhasa Apso','Shih Tzu','Maltese','Pomeranian'))

full$ShortHair = as.factor(full$ShortHair)
full$MediumHair = as.factor(full$MediumHair)
full$LongHair = as.factor(full$LongHair)
full$hair = ifelse(full$ShortHair == '1', 'Short',
                   ifelse(full$MediumHair == '1', 'Medium', 
                          ifelse(full$LongHair == '1', 'Long', 'Unknown')))
full$hair = as.factor(full$hair)
#########################################################################################
full$AgeinDays = as.factor(full$AgeinDays)
full$Hour = as.factor(full$Hour)
###############################################
#Build the model :) 
#RANDOM FORREST 
#Create Test and Training Set
Sample = sample(c(TRUE,FALSE), nrow(full), replace = T, prob = c(0.7, 0.3))
train <- full[Sample,]
test <- full[!Sample,]

set.seed(1)
write.csv(train, 'NewTrain.csv', row.names = F)
write.csv(test, 'NewTest.csv', row.names= F)
#############################################
#Build the model :) 
#RANDOM FORREST 
str(full)
ls(full)
full =full %>% mutate_if(is.character, as.factor)
str(full)
#full$AboutToDie = as.factor(full$AboutToDie)
#full$Hypo = as.factor(full$Hypo)

RF_Model1 <-randomForest(OutcomeType~AnimalType+SexuponOutcome+Fixed+Sex+HasAName+AgeinDays+Hour+Weekday+TimeOfDay+SimpleColor+AgeBin+Month
                         +HolidaySeason+TopAdopted+TopTenSheltered+Mix+Hunting.Dog+Black+DOGHypo+CATHypo+Size+hair,
                         data=train, ntree=10, importance = TRUE, na.action = na.exclude)
#Show Model Error 
plot(RF_Model1, ylim = c(0,1))
legend('bottomright', colnames(RF_Model1$err.rate), col = 1:6, fill=1:6)

#Whats the most important Co-Variate 
importance <- importance(RF_Model1)
varImportance <- data.frame(variables = row.names(importance), Importance = round(importance[ , 'MeanDecreaseGini'], 2))

rankImportance <-varImportance %>% 
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x=reorder(variables, Importance), 
                           y=Importance)) + 
  geom_bar(stat='identity', color = 'black')+
  geom_text(aes(x=variables, y=0.5, label = Rank), 
            hjust = 0, vjust = 0.55, size=4, color = 'lavender', 
            fontface = 'bold')+
  labs(x='Variables', title = 'Relative Variable Importance') + 
  coord_flip()+
  theme_few()
#########################################################
#Predict on the Test Set 
preds <- predict(RF_Model1, test, type = 'vote')
solution <-data.frame(test$AnimalID, preds)
write.csv(solution, 'RFModel1.csv', row.names = F)
head(test$OutcomeType)
test$Adopted =ifelse(test$OutcomeType == 'Adoption',1,0)
test$Died1 = ifelse(test$OutcomeType == 'Died',1,0)
test$Euthanasia1 = ifelse(test$OutcomeType == 'Euthanasia', 1, 0)
test$Return_to_owner1 = ifelse(test$OutcomeType == 'Return_to_owner', 1, 0)
test$Transfer1 = ifelse(test$OutcomeType == 'Transfer',1,0)
test$OutcomeType1 = as.numeric(test$OutcomeType)
actual <-test[,c(2,46,47,48,49,50)]
NEW =cbind(solution, actual)
NEW$LogLoss = 1-(log(NEW$Adoption * NEW$Adopted) + (NEW$Died*NEW$Died1) + (NEW$Euthanasia * NEW$Euthanasia1)+(NEW$Return_to_owner * NEW$Return_to_owner1)+ (NEW$Transfer * NEW$Transfer1) )
NEW$LogLoss = as.numeric(as.character(NEW$LogLoss))
sum(NEW$LogLoss, na.rm = TRUE)/7956

#42.87% Classified Incorrectly
##################################




