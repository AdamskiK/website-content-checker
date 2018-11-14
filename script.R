#!/usr/bin/env Rscript --vanilla
source("credentials.R")
library("mailR")

### define functions
send_email <- function(content){
  send.mail(from = cred.email,
            to = c(cred.email),
            subject = "Tickets reminder - a new building has been found",
            body = content,
            smtp = list(host.name = "smtp.gmail.com", 
                        port = 465, 
                        user.name = cred.email, 
                        passwd = cred.password, 
                        ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
}

get_page_content <- function(){
  thepage <-  readLines('https://nocwiezowcow2018.evenea.pl/', encoding = "UTF-8")
  thepage <- tolower(iconv(thepage, "UTF-8", "ASCII//TRANSLIT"))
}

### define objects
# buildings <- c("intercontinental", "ilmet", 
#                "generation", "novotel", 
#                "prosta", "zlota", 
#                "Leonardo")

buildings <- c("Zlota, Continentals")
buildings <- tolower(iconv(buildings, "UTF-8", "ASCII//TRANSLIT"))

# count how many buildings has been found
building_counter = 0

### get the page content
while(TRUE){
  thepage <- get_page_content()
  def_opt_val <- grep("option value", thepage) # in case
  
  for(building in buildings){
    vec <- grep(building, thepage)
    
    
    if(length(vec) > 5){
      content <- paste(paste0("A new building has been found a while ago --> Hurry up and buy the ticket for: ", toupper(building)), 
                       " # go to: https://nocwiezowcow2018.evenea.pl/",
                       sep = "")
      send_email(content)
      buildings <- setdiff(buildings, building)
      building_counter =+ 1
    }
  }
  
  if(building_counter != 0){
    if(length(def_opt_val) > 15){
      content <- paste(paste0("Tickets are being selling right now"), 
                       " # go to: https://nocwiezowcow2018.evenea.pl/",
                       sep = "")
      send_email(content)
      
      building_counter = 0
    }
  }
  
  Sys.sleep(15)
}
