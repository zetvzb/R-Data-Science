#Creating a model for Scraping twitter using DOM Parsing
library(dplyr)
library(sqldf)
library(rvest) #By Hadley Wickham
library(devtools)
#install_version("binman", version = "0.1.0", repos = "https://cran.uni-muenster.de/")
#install_version("wdman", version = "0.2.2", repos = "https://cran.uni-muenster.de/")
#install_version("RSelenium", version = "1.7.1", repos = "https://cran.uni-muenster.de/")
library(xml2)
library(tidyverse)

####Must Have Selector Gadget open source software installed with Google Chrome Extension to get CSS Code####

###########################################################
setwd("C:\\Users\\tallevastz\\Documents\\Twitter Scrape\\Twitter\\")

#############################################################
library(RSelenium)#This is a webdriver and will allow you to scroll. Set up Docker on your machine and designate the browser youre using. 
#http://www.et.bs.ehu.es/cran/web/packages/RSelenium/vignettes/RSelenium-docker.html is to setup

remDr <- remoteDriver(remoteServerAddr = "192.168.99.100", port = 4445L, browser = "chrome")
remDr$open()

#Feed RSelenium an URL to scroll. 

#Lets find out what Twitter Users said about Santa Claus on the night of Christmas Eve in 2017 for users located within 100 miles of Los Angeles, CA"
remDr$navigate("https://twitter.com/search?f=tweets&q=Santa%20Claus%20near%3A%22Los%20Angeles%22%20lang%3Aen%20within%3A100mi%20since%3A2017-12-24%20until%3A2017-12-26%20exclude%3Aretweets&src=typd") #Search for Santa Claus
#Twitter Search
####Set language = English 
####Set location = Los Angeles
####Set distance from = 100 miles
####Set Dates 2017-12-24 - 2017-12-26
####Set Exclusion of Retweets 
####Set Exclusion of Links
####Set minimum favorites to 1



remDr$screenshot(display = TRUE)#Displays a screenshot of where Rselenium is located in the URL

#Login to Twitter (Need to sign in using proper credentials)
mailid <- remDr$findElement(using = 'css', "[class = 'text-input email-input js-signin-email']")
mailid$sendKeysToElement(list("TWITTER USERNAME")) #Notice you will need to enter your TWITTER USERNAME HERE
password <- remDr$findElement(using = 'css', ".LoginForm-password .text-input")
password$sendKeysToElement(list("TWITTER PASSWORD")) #Notice you will need to enter your TWITTER PASSWORD HERE
login <- remDr$findElement(using = 'css', ".js-submit")
login$clickElement()

remDr$screenshot(display = TRUE)
#Now I am logged in

remDr$getStatus() #Check Status of the Connection before it scrolls



#Scroll 500 Times, Wait for page to load 2 seconds per scroll. This is to suspend execution for 2 seconds while scrolling. 
for(i in 1:500){      
  remDr$executeScript(paste("scroll(0,",i*10000,");"), args=list("dummy"))
  Sys.sleep(2)  #Helps with Load Limit on Twitter  
}

page_source <-remDr$getPageSource()


#Grab Data
#First Grab Each Tweets Username
User <- xml2::read_html(remDr$getPageSource()[[1]]) %>% 
  rvest::html_nodes('.show-popup-with-id') %>% 
  #rvest::html_children() %>% #Unneccessary
  rvest::html_text() %>% 
  #.[1:100] %>% ##Apply this if you are wanting x number of tweets from the scroll. Say the first 100
  dplyr::data_frame(User = .)

#Grab Each TWeets Twitter Handle
Handle <- xml2::read_html(remDr$getPageSource()[[1]]) %>% 
  rvest::html_nodes('.js-nav .u-textTruncate b') %>% 
  #rvest::html_children() %>% 
  rvest::html_text() %>% 
  dplyr::data_frame(Handle = .)

#Grab the independent text for each Tweet
Tweet_Text <- xml2::read_html(remDr$getPageSource()[[1]]) %>% 
  rvest::html_nodes('.tweet-text') %>% 
  rvest::html_text() %>% 
  dplyr::data_frame(Tweet_Text = .)

#Grab the TimeStamp
Timestamp <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
  rvest::html_nodes('.js-short-timestamp') %>% 
  rvest::html_text() %>% 
  dplyr::data_frame(Timestamp = .) 

#Grab the Number of Favorites 
Favorites <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
  rvest::html_nodes('.js-actionFavorite .ProfileTweet-actionCountForPresentation') %>% 
  rvest::html_text() %>%
  dplyr::data_frame(Favorites = .) 

#Grab the Number of Retweets 
Retweets <- xml2::read_html(remDr$getPageSource()[[1]]) %>% 
  rvest::html_nodes('.js-actionRetweet .ProfileTweet-actionCountForPresentation') %>% 
  rvest::html_text() %>% 
  dplyr::data_frame(Retweets = .) 

#Grab the Number of Replies
Replies <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
  rvest::html_nodes('.js-actionReply .ProfileTweet-actionCountForPresentation') %>% 
  rvest::html_text() %>% 
  dplyr::data_frame(Replies = .) 

#Note CSS webscraping is not always perfect. ONLY SOMETIMES will you have to make adjustments to your feed. 
#Clean Results 
Handle = Handle[-1,] #the CSS grabs your personal handle as the first handle observation. 
Tweet_Text = Tweet_Text[1:nrow(Handle),]



#Combine all variables
tmp <- data.frame(User,Handle,Tweet_Text,Replies,Timestamp)

write.csv(tmp, "INSERT_NAME_HERE.csv")
remDr$close()

