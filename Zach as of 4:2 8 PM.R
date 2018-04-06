#This is a Random Forrest Model for Animal Shelter Outcomes. 

setwd("/Users/zachtallevast/Box Sync/Shelter Animal Outcomes/")
train <- read.csv("Shelter Outcomes TRAIN.csv", header=T)
test <- read.csv("test (5).csv", header=T) 
#Given 
    #Breed,Color,Sex,Age 
#Predict the Outcome of Each Animal 
 
#Combine train and test to adjust variables 

names(train)[1] <- 'ID' #Rename ID column to "ID"
test$ID < as.character(test$ID) #Make ID column a character not a factor
train$DateTime <- as.character(train$DateTime)
train$Date <- sapply(train$DateTime, function(x) strsplit(x,split = ' ') [[1]][1])
train$Time <-sapply(train$DateTime, function(x) strsplit(x, split = ' ') [[1]][2])
train$Date <-as.Date(train$Date, "%m/%d/%y")
train$DateTime <-paste(train$Date, " ", train$Time)
test$DateTime <-as.character(test$DateTime)
#test$DateTime <-sapply(test$DateTime, function(x) strsplit(x,split = ':') [[1]][1])
library(plyr)
full <-rbind.fill(train,test) #Combine datasets by row

library(ggplot2)
library(ggthemes)
library(dplyr)
library(lubridate)
library(rpart)
library(randomForest)

summary(train$AnimalType)
#Cat   Dog 
#11134 15595 
#This Outcomes function creates a table with Animal Type, Outcome Type, and Number of each category. 
outcomes <- full[1:38185, ] %>%
  group_by(AnimalType, OutcomeType) %>% 
  summarise(num_animals =n())
ggplot(outcomes, aes(x=AnimalType,y=num_animals, fill=OutcomeType)) + 
  geom_bar(stat = 'identity', position = 'fill', color='black')+
  coord_flip()+
  labs(y='Proportion of Animals', x='Animal', title = 'Outcomes: Cats & full')+
  theme_few()
#Notice Cats are twice as likely to be Transferred to a new owner and full are four times as likely to be returned to owner than cats. 

###########################################################################
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
#Notice the oldest animal is 20 years old, but the median is 1 year. 
##########################################################################
#Now lets deal with MISSING VALUES 

#For Name of animal, if its missing lets call them "Nameless"
full$Name = as.character(full$Name)
full$Name <- ifelse(nchar(full$Name)==0, 'Nameless', full$Name)
#Make an Indicator Variable for if they have a name or not. 1= Has Name 0=Has No Name. Could be a predictor 
full$HasAName[full$Name =='Nameless'] <-0
full$HasAName[full$Name !='Nameless'] <-1
full$HasAName = as.factor(full$HasAName)
#For Blank Sex upon Outcome, lets replace it with most common Sex. Which is Neutered Male
#DEBATE IF WE SHOULD DO UNKNOWN. THERE IS AN UNKNOWN VECTOR. 
summary(full$SexuponOutcome)
full$SexuponOutcome =as.character(full$SexuponOutcome)
full$SexuponOutcome<-ifelse(nchar(full$SexuponOutcome)==0, 'Unknown', full$SexuponOutcome)
full$SexuponOutcome = as.factor(full$SexuponOutcome)
################################################################################
#Correction to time and date Variables 
full$Hour <- hour(full$DateTime) 
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
daytimes <-full[1:38185, ] %>% group_by(AnimalType, TimeOfDay, OutcomeType) %>%
  summarise(num_animals = n())
ggplot(daytimes, aes(x=TimeOfDay, y=num_animals, fill=OutcomeType))+
  geom_bar(stat='identity', position = 'fill', color = 'black')+
facet_wrap(~AnimalType)+
  coord_flip()+
  labs(y='Proportion of Animals', x='Animal', title = 'Outcomes by Time of Day')+
  theme_few()
########################################################################
#Correction to Breed 
nlevels(factor(full$Breed))[1:10]
#1678 Breeds! Cant predict with that many breeds

#First lets take out Mix breeds denoted (breed) mix.
full$IsMix <- ifelse(grepl('Mix', full$Breed), 1,0)
full$IsMix = as.factor(full$IsMix)
#Split on "/" and Remove "Mix"
full$Breed = as.character(full$Breed)
full$SimpleBreed <-sapply(full$Breed, function(x) gsub(' Mix', '', strsplit(x, split = '/')[[1]][1]))
nlevels(factor(full$SimpleBreed))[1:10]
full$SimpleBreed = as.factor(full$SimpleBreed)
#Now 231 Breeds! 
##########################################################################
#Correction to Color. Take out the Green/Blue colors. Use only first color. 
nlevels(factor(full$Color))[1:10] #411 Colors
full$Color = as.character(full$Color)
full$SimpleColor <- sapply(full$Color, function(x) strsplit(x, split = '/') [[1]][1])
full$SimpleColor = as.factor(full$SimpleColor)
nlevels(factor(full$SimpleColor))[1:10] #57 Colors
##########################################################################
#create Indicator for "Intact or Unknown" for SexUponOutcome 
#create Indicator for "Sex" of Animal 
full$Intact <- ifelse(grepl('Intact', full$SexuponOutcome),1,ifelse(grepl('Unknown',full$SexuponOutcome), 'Unknown',0))
full$Sex <- ifelse(grepl('Male', full$SexuponOutcome),'Male',ifelse(grepl('Unknown',full$Sex), 'Unknown','Female'))
full$Intact = as.factor(full$Intact)
full$Sex = as.factor(full$Sex)
##########################################################################
#Do something about missing ages. We Will PREDICT THEM USING RPART
full$AgeinDays = as.numeric(as.character(full$AgeinDays))
sum(is.na(full$AgeinDays))
age_fit <- rpart(AgeinDays ~ AnimalType + Sex+ Intact + SimpleBreed+HasAName, 
                 data = full[!is.na(full$AgeinDays), ],
                 method = 'anova')

#Put predicted values into missing values 
full$AgeinDays[is.na(full$AgeinDays)] <-predict(age_fit, full[is.na(full$AgeinDays),])
sum(is.na(full$AgeinDays))
##############################################################################
#Create Indicator to show animal is a puppy or kitten
full$Youth[full$AgeinDays <365] <- 'Baby'
full$Youth[full$AgeinDays >= 365] <- 'Adult'
full$Youth <- as.factor(full$Youth)
##############################################################################
#Create Indicator to show animal is a senior animal (ASPCA defines as 7 years old or older)
full$Senior[full$AgeinDays <2190] <- 'Adult'
full$Senior[full$AgeinDays >=2190] <- 'Senior'
full$Senior <-factor(full$Senior)
#############################################################################
#Create Indicator if outcome data is during Holidays. October-December. October is national adopt a dog month
full$Month <- sapply(full$DateTime, function(x) strsplit(x,split = '-') [[1]][2])
full$Month <-sapply(full$Month, function(x) strsplit(x,split = ' ') [[1]][1])
full$HolidaySeason <- ifelse(full$Month <10, 'Non-Holiday', 'Holiday')
full$Month = as.factor(full$Month)
full$HolidaySeason = as.factor(full$HolidaySeason)
###########################################################################
#Create a Top ten Most Adopted DOG by ASPCA covariate
Top10 <- c('Bull Dog', 'Beagle', 'Bull Terrier', 'Collie', 'Newfoundland', 
                                    'Vizsla', 'Irish Setter', 'Poodle', 'Labrador Retreiver', 'Golden Retreiver')

full$TopAdopted <-ifelse(full$SimpleBreed == 'Bull Dog', 'Top 10', 
              ifelse(full$SimpleBreed == 'Beagle', 'Top 10',
              ifelse(full$SimpleBreed == 'Bull Terrier', 'Top 10',
              ifelse(full$SimpleBreed == 'Collie', 'Top 10',
              ifelse(full$SimpleBreed == 'Newfoundland', 'Top 10',
              ifelse(full$SimpleBreed == 'Vizsla', 'Top 10',
              ifelse(full$SimpleBreed == 'Irish Setter', 'Top 10',
              ifelse(grepl( 'Poodle',full$SimpleBreed), 'Top 10',
              ifelse(full$SimpleBreed == 'Labrador Retriever', 'Top 10',
              ifelse(full$SimpleBreed == 'Golden Retreiver', 'Top 10', 
              ifelse(full$SimpleBreed == 'Persian', 'Top 5', 
              ifelse(full$SimpleBreed == 'American Shorthair', 'Top 5', 
              ifelse(full$SimpleBreed == 'Maine Coon', 'Top 5', 
              ifelse(full$SimpleBreed == 'Siamese', 'Top 5', 
              ifelse(full$SimpleBreed == 'Abyssinian', 'Top 5', 'Not Top 10')))))))))))))))
full$TopAdopted <- as.factor(full$TopAdopted)
##########################################################################
#Create a Top Ten Breeds of Dog in Shelters covariate
table(full$SimpleBreed)
#Top 10: Domestic Medium Hair, Domestic Shorthair, Australian Cattle Dog, Chihuahua Shorthair, German Shepherd, Labrador Retreiver, Pit bull, Siamese, Dachshund, Miniature Poodle
full$TopTenSheltered <-ifelse(full$SimpleBreed == 'Domestic Medium Hair', 'Top 10', 
                     ifelse(full$SimpleBreed == 'Domestic Shorthair', 'Top 10',
                     ifelse(full$SimpleBreed == 'Australian Cattle Dog', 'Top 10',
                     ifelse(full$SimpleBreed == 'Chihuahua Shorthair', 'Top 10',
                     ifelse(full$SimpleBreed == 'Dachshund', 'Top 10',
                     ifelse(full$SimpleBreed == 'German Shepherd', 'Top 10',
                     ifelse(full$SimpleBreed == 'Labrador Retreiver', 'Top 10',
                     ifelse(full$SimpleBreed == 'Pit Bull', 'Top 10',
                     ifelse(full$SimpleBreed == 'Siamese', 'Top 10',
                      ifelse(full$SimpleBreed == 'Miniature Poodle', 'Top 10', 'Not Top 10'))))))))))
full$TopTenSheltered <- as.factor(full$TopTenSheltered)
#Top Ten Cat 
full$TopTenCATSSheltered #NOT DONE DOG IS WRONG
#############################################################################################
#Average Life Span 
Lifespan <-read.csv("Breed Lifespans.csv", header=T)
full <-merge(x=full, y=Lifespan, by.x = "SimpleBreed", by.y = "Breed", all.x=TRUE, incomparables = 'Unknown')
#full <-full[c(1,31)]
full$AVERAGE.LIFESPAN..YEARS. <- ifelse(full$AVERAGE.LIFESPAN..YEARS.!= "NA", full$AVERAGE.LIFESPAN..YEARS., "10")
full$AVERAGE.LIFESPAN..YEARS. <- as.factor(full$AVERAGE.LIFESPAN..YEARS.)
#############################################################################################
#Hunting Dog 
Hunting <- read.csv("Hunting Dog.csv", header=T)
full <- merge(x=full, y = Hunting, by.x = "SimpleBreed", by.y ="Breed", all.x=TRUE, incomparables = 'Unknown')
full$Hunting.Dog <- as.factor(full$Hunting.Dog)
########################################################################################
#Black Dog 
full$Black <- ifelse(grepl('Black', full$Color), 'Black', 
                     ifelse(grepl('Brindle', full$Color), 'Black', 'Not Black'))
full$Black <- as.factor(full$Black)
##########################################################################################
#Long Haired vs Short Haired
###########################################################################################
#Create a Hypoallergenic Variable. Split on Cats and Dogs
full$DOGHypo = full$SimpleBreed

full$DOGHypo = ifelse(grepl('Poodle', full$DOGHypo), 1,
ifelse(grepl('Yorkshire', full$DOGHypo), 1,
ifelse(grepl('Havanese', full$DOGHypo), 1,
ifelse(grepl('Maltese', full$DOGHypo), 1, 
ifelse(grepl('Shih Tzu', full$DOGHypo), 1, 
ifelse(grepl('Schnauzer', full$DOGHypo), 1, 
ifelse(grepl('Basenji', full$DOGHypo), 1,
ifelse(grepl('Silky Terrier', full$DOGHypo), 1,
ifelse(grepl('Irish Water Spaniel', full$DOGHypo), 1,
ifelse(grepl('Terrier', full$DOGHypo), 1,
ifelse(grepl('Terr', full$DOGHypo), 1,
ifelse(grepl('Afghan Hound', full$DOGHypo), 1,
ifelse(grepl('Water Spaniel', full$DOGHypo), 1, 
ifelse(grepl('Spanish Water', full$DOGHypo), 1, 
ifelse(grepl('Water Dog', full$DOGHypo), 1, 
ifelse(grepl('Bichon Frise', full$DOGHypo), 1, 0))))))))))))))))

full$CATHypo = full$SimpleBreed
full$CATHypo = ifelse(grepl('Siberian', full$CATHypo), 1,
ifelse(grepl('Balinese', full$CATHypo), 1, 
ifelse(grepl('Bengal', full$CATHypo), 1, 
ifelse(grepl('Burmese', full$CATHypo), 1, 
ifelse(grepl('Siberian', full$CATHypo), 1, 
ifelse(grepl('Cornish Rex', full$CATHypo), 1, 
ifelse(grepl('Devon Rex', full$CATHypo), 1, 
ifelse(grepl('Javanese', full$CATHypo), 1,
ifelse(grepl('Ocicat', full$CATHypo), 1, 
ifelse(grepl('Russian Blue', full$CATHypo), 1, 
ifelse(grepl('Siamese', full$CATHypo), 1, 
ifelse(grepl('Sphynx', full$CATHypo), 1, 0))))))))))))
full$DOGHypo= as.factor(full$DOGHypo)
full$CATHypo = as.factor(full$CATHypo)
##########################################################################################
###########################################################################################
#Animal Size 
full$SmallAnimal <- as.numeric(full$SimpleBreed %in% c(grepl('Miniature', full$SimpleBreed), 
                                                          'Dandie Dinmount',
                                                        grepl('Mini', full$SimpleBreed),grepl('Toy', full$SimpleBreed),grepl('Chihuahua', full$SimpleBreed), 
                                                       'Dachshund', 'Maltese', 'Pomeranian', 'Shih Tzu','Yorkshire Terrier', 
                                                       'Havanese', 'Pug', 'Boston Terrier','Coton De Tulear', 'Cavalier Span', 'Chinese Crested', 
                                                       'Bruss Griffon', 'Bedlington Terr', 'Bichon Frise', 'Affenpinscher', 'Japanese Chin', 'Finnish Spitz', 'Feist', 'Manchester Terrier',
                                                      'Schipperke','Border Terrier', 'Ppdengo Pequeno', 'Patterdale Terr', 'Manchester Terrier', 'Bichons Frise', 'West Highland Terrier', 'Papillon', 
                                                      'Silky Terrier', 'Lowchen','Jack Russell Terrier', 'Cavalier King Charles Spaniel', 'Beagle', 'Shetland Sheepdog', 'Welsh Terrier',
                                                      'Swedish Vallhund','Corgi','Norfolk Terrier', 'Sealyham Terr', 'Scottish Terrier', 'French Bulldog','Flat Coat Retriever/Papillon','Shiba Inu','Fox Terrier','West Highland','Papillon','Russell Terrier','Pekingese',
                                                     'Norwich Terrier','Cairn Terrier','Irish Terrier','Italian Greyhound','Manchester Terrier','Lhasa Apso','Papillon','Russell Terrier','Pekingese','Norwich Terrier','Rat Terrier','Tibetan','Jack Russell Terrier','Pembroke Welsh Corgi/Brittany','Pembroke Welsh Corgi','Scottish Terrier/Cairn Terrier'))
                                                                                                                                 
full$MediumAnimal <- as.numeric(full$SimpleBreed %in% c('Border Collie', 'Brittany','English Shepherd','Australian Water Spaniel', 'Australian Cattle Dog', 'Australian Shepherd',
                                                        'Basenji','English Springer Spaniel', 'Barbet', 'Basset Hound','Cardigan Welsh Corgi', 'Dalmatian', 'Chinese Sharpei', 'Boykin Span', 'Carolina Dog', 'Blue Lacy',
                                                        'Welsh Springer Spaniel', 'Treeing Tennesse Brindle', 'Treeing Cur', 'Skye Terrier', 'Bulldog','Cocker Spaniel', 
                                                        'Samoyed','Japanese Bobtail', 'Jindo', 'German Pinscher', 'Field Spaniel', 'English Springer Spaniel', 'Port Water Dog','Pbgv',
                                                        'Mexican Hairless', 'Keeshond', 'Whippet','Staffordshire',grepl('Pointer', full$Size), 'Pit Bull/Border Collie', 'Pit Bull/Blue Lacy', 'Pit Bull/Black Mouth Cur', 
                                                  'Pit Bull/Boston Terrier', 'Medium','Pit Bull/Border Terrier', 'Medium','Pit Bull/Beagle', 'Medium','Pit Bull/Carolina Dog', 'Pit Bull/Cardigan Welsh Corgi', 'Medium',
                                                  'Pit Bull/English Bulldog', 'Medium','Pit Bull/American Bull Dog', 'Persian', 'Abyssinian','Sphynx', 'Siamese','Exotic Shorthair', 'Scottish Fold', 'Cornish Rex', 'Devon Rex', 'Burmese','Tonkinese', 'Russian Blue', 'Manx', 
                                                  'German Shepherd/Cardigan Welsh Corgi','Vizsla/Beagle','Wheaten Terrier','Shetland Sheepdog/Keeshold','Labrador Retriever/Beagle', 'Labrador Retriever/Blue Lacy', 'Labrador Retriever/Black Mouth Cur',
'Labrador Retriever/Boston Terrier', 'Labrador Retriever/Border Terrier','Turkish Angora','Scottish Terrier/Basset Hound',  'Catahoula/Cardigan Welsh Corgi', 'Catahoula/Border Collie', 'Catahoula/American Bulldog', 'Catahoula/Beagle', 'Catahoula/Black Mouth Cur', 'Pit Bull',
'Queensland Heeler','Jack Russell Terrier/Labrador Retriever','Cocker Spaniel','Collie Smooth/Beagle','Canaan Dog','English Bulldog','Glen Of Imaal','Nova Scotia Duck Tolling Retriever/Border Collie','Pharaoh Hound','Jack Russell Terrier/Pointer',grepl('Australian', full$Size),'Pug/Pit Bull','Pit Bull/Pug','Standard Schnauzer','Balinese', 'Bombay', 'Havana Brown','Bengal','Cymric','Snowshoe'))

full$LargeAnimal <- as.numeric(full$SimpleBreed %in% c('Coonhound','Afghan Hound', 'Airedale Terrier','Akita','American Foxhound','Bloodhound','Belgian Sheepdog','Boxer','Bull Terrier','Chow Chow','Golden Retreiver', "English Setter",
                                                       'German Shepherd','Irish Wolfhound','Labrador Retreiver', 'Rotweiler','Siberian Husky','Standard Poodle','Bernese Mountain Dog', 'Greyhound',
                                                       'Foxhound','Belgian Tervuren','Spinone Italiano','Schnauzer Giant','Plott Hound','Picardy Sheepdog','Hovawart','Ibizan Hound','Harrier','Gordon Setter','Dutch Shepherd/Anaotol Shepherd',
                                                       'Dogue De Bordeaux/American Bulldog','Doberman Pinsch','Dutch Shepherd/Boxer','Black/Tan Hound','Belgian Malinois','Beauceron',
                                                       'American Bulldog','Chesa Bay Retr','Bluetick Hound', 'Dogo Argentino/Chinese Sharpei','Dalmatian/Basset Hound','Entlebucher','English Foxhound','Dutch Shepherd','Dogo Argentino','Saluki','Saluki/Doberman Pinsch','Otterhound',
                                                 'Nova Scotia Duck Tolling Retriever','Wirehaired Pointing Griffon', grepl('Siberian Husky/Great Pyranees', full$Size),'Siberian Husky/Labrador Retriever',
                                                  'Siberian Husky/Catahoula','Siberian Husky/Rottweiler','Siberian Husky/Alaskan Malamute',grepl('Siberian Husky/Anatol Shepherd', full$Size),
                                                 'Newfoundland/Border Collie','Flat Coat Retriever','St. Bernard Rough Coat/Border Collie','Vizsla','Whippet/Labrador Retriever','Whippet/Labrador Retriever','Weimaraner',grepl('Basset Hound', full$Size), 'British Shorthair', 'Ocicat', 
                                                  'Chartreux','Collie','Greyhound','English Bulldog/Boxer', 'Norwegian Forest Cat', 'Turkish Van', 'Pixiebob Shorthair', 'Himalayan', 'American Wirehair', 'American Shorthair', 'Large','Rottweiler',
                                                 'Norwegian Elkhound','Rhod Ridgeback',grepl('Old English', full$Size),'Alaskan Husky','Alaskan Malamute','Plott Hound','Great Pyranees/Border Collie','Redbone Hound','Queensland Heeler/Great Dane','Golden Retriever/Chow Chow', 'Golden Retriever/Standard Poodle','Akita', grepl('Catahoula', full$Size)))

full$XLAnimal <- as.numeric(full$SimpleBreed %in% c('Black','Great Dane','Great Pyrenees','Irish Wolfhound','Saint Bernard','Mastiff','Newfoundland', 'Leonberger', 'Presa Canario','Landseer','Dogue De Bordeaux','Cane Corso', 
                                                    'Anatol Shepherd','Greater Swiss Mountain Dog', 'Bullmastiff','Boerboel', 'Kuvasz','St.Bernard','Mastiff',grepl('Great', full$Size),'Ragdoll','Maine Coon'))
table(full$SmallAnimal)
table(full$MediumAnimal)
table(full$LargeAnimal)
table(full$XLAnimal)
full$SmallAnimal =as.factor(full$SmallAnimal)
full$MediumAnimal = as.factor(full$MediumAnimal)
full$LargeAnimal = as.factor(full$LargeAnimal)
full$XLAnimal = as.factor(full$XLAnimal)
########################################################################################

# Hair Length
full$ShortHair <- as.numeric(full$SimpleBreed %in% c('Sphynx','Boerboel','Kelpie',
                                                'Belgian Malinois','Black Mouth',
                                               'Blue Lacy','Border','Boston','Bullmastiff', 'Canaan', 'Cane',
                                               'Carolina','Catahoula',
                                               'Chesa', 'Chinese Crested',
                                               'Doberman','Dogo Argentino',
                                               grepl('Entlebucher', full$Hair),grepl('Dogue', full$Hair),
                                               grepl('Feist', full$Hair),grepl('Swiss Mountain Dog', full$Hair),
                                               grepl('hound', full$Hair),grepl('Harrier', full$Breed),
                                               grepl('Pinscher', full$Hair),grepl('Manchester Terrier', full$Hair),
                                               grepl('Mexican Hairless', full$Hair),grepl('Mastiff', full$Hair),
                                               grepl('Russell Terrier', full$Hair),grepl('Patterdale', full$Hair),
                                               grepl('Pointer', full$Hair),grepl('Presa', full$Hair),
                                               grepl('Queensland Heeler', full$Hair),grepl('Rat Terrier', full$Hair),
                                               grepl('Ridgeback', full$Breed),grepl('Saluki', full$Hair),
                                               grepl('Spanish Mastiff', full$Hair),grepl('Vizsla', full$Breed),
                                               grepl('Staffordshire', full$Hair),grepl('Treeing Cur', full$Hair),
                                               grepl('Tennesse Brindle', full$Breed),grepl('Hound', full$Breed),
                                               grepl('Bull Terrier', full$Hair),grepl('Weimaraner', full$Hair),
                                               grepl('Whippet', full$Hair),grepl('Fox Terrier', full$Hair),
                                                grepl('Pug', full$Hair), 'Short',grepl('Coonhound', full$Hair),
                                               grepl('Wire Hair Fox Terrier', full$Breed),grepl('Rottweiler', full$Hair),
                                               grepl('Dutch Shepherd', full$Breed),grepl('Greyhound', full$Breed),
                                               grepl('Shorthair', full$Breed),grepl('Great Dane', full$Breed),'Dachshund',
                                               'German Shepherd','Labrador','Bloodhound','Bulldog',
                                               'Chihuahua','Australian Cattle Dog','Dalmatian','Basset Hound','Beauceron',
                                               'Boxer','Basenji','Beagle','American Foxhound', 'Siamese','Affenpinscher',
                                               'Abyssinian','Oriental','Scottish','Burmese','Ocicat','Tonkinese','Devon',
                                               'Havana','Snowshoe','Bombay','Chartreux','Cornish','Cymric'))

full$MediumHair <-as.numeric(full$SimpleBreed %in% c('Manx','Alaskan Husky','Alaskan Malamute','Akita',
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
full$LongHair <- as.numeric(full$SimpleBreed %in% c('Persian','American Eskimo','Bichon Frise', 
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
#########################################################################################
full$AgeinDays = as.factor(full$AgeinDays)
full$Hour = as.factor(full$Hour)
###############################################
#Re-Create a New Training Set and a Test Set
train <-full[1:26729, ]
test <-full[26730:nrow(full), ]
set.seed(1)
#############################################
#Build the model :) 
#RANDOM FORREST 
str(full)
ls(full)
full =full %>% mutate_if(is.character, as.factor)
#NEED TO CUT OUT 4 COLORS FROM SIMPLE COLOR 
#Took out Outcome Subtype
RF_Model1 <-randomForest(OutcomeType ~AnimalType+SexuponOutcome+AgeinDays+HasAName+Hour+Weekday+TimeOfDay+IsMix+Intact+Sex+Youth+Senior+Month+HolidaySeason+TopAdopted+TopTenSheltered+AVERAGE.LIFESPAN..YEARS.+Hunting.Dog+Black+DOGHypo+CATHypo+SmallAnimal+MediumAnimal+LargeAnimal+XLAnimal+ShortHair+MediumHair+LongHair,
            data=train, ntree=600, importance = TRUE, na.action = na.exclude)
nlevels(factor(train$Month))[1:10]
#How to decide how many trees?

#Show Model Error 
plot(RF_Model1, ylim = c(0,1))
legend('topright', colnames(RF_Model1$err.rate), col = 1:6, fill=1:6)

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
solution <-data.frame('ID' = test$ID, preds)
write.csv(solution, 'RFModel1.csv', row.names = F)

test$Adopted =ifelse(test$OutcomeType == 'Adoption',1,0)
test$Died = ifelse(test$OutcomeType == 'Died',1,0)
test$Euthanasia = ifelse(test$OutcomeType == 'Euthanasia', 1, 0)
test$Return_to_owner = ifelse(test$OutcomeType == 'Return_to_owner', 1, 0)
test$Transfer = ifelse(test$OutcomeType == 'Transfer',1,0)

actual <-test[,c(2,43,44,45,46,47)]


#LogLoss Function
#LogLoss = function(actual, predicted, eps = 1e-15) {
#  predicted = pmin(pmax(predicted, eps), 1-eps)
#  - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
#}

#LogLoss(actual = actual$ID,predicted = solution)

#Johns help
sum(rowSums(actual[,-1]*preds))

sum(rowSums(actual[,-1]*preds),na.rm=T)

LogLoss <- sum(rowSums(actual[,-1]*preds),na.rm=T)/sum(!is.na(sum(rowSums(actual[,-1]*preds)))

LogLoss


#########################################################
#BOOSTING MODEL
#library(ISLR)
#library(MASS)
#library(tree)
#tree.boost = tree(OutcomeType ~  AnimalType + AgeinDays + HasAName+
     #               Intact + Sex + TimeOfDay + Hour  + Youth + Senior + IsMix+
      #              HolidaySeason + TopAdopted + TopTenSheltered + Hunting.Dog + Black + AVERAGE.LIFESPAN..YEARS., train)
#summary(tree.boost)
#plot(tree.boost)
#text(tree.boost, pretty=0)
#set.seed(1)
#train1 = sample(1:nrow(train),200)
#train.test = High[-train]
#tree.boost = tree(OutcomeType ~  AnimalType + AgeinDays + HasAName+
#                    Intact + Sex + TimeOfDay + Hour  + Youth + Senior + IsMix+
#                    HolidaySeason + TopAdopted + TopTenSheltered + Hunting.Dog + Black + AVERAGE.LIFESPAN..YEARS., train, subset = train1)
#tree.pred = predict(tree.boost, train.test, type = "class")
#table(tree.pred, train.test)


