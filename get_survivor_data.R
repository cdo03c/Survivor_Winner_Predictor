#Load packages
library(rvest)
library(gender)

###Load the data from wikipedia

#Create data frame of survivor seasons
wiki = read_html("https://en.wikipedia.org/wiki/Survivor_(U.S._TV_series)")

allseasons = wiki %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table(fill=T)

#Creates a table that contains the U.S.TV ratings information
premier = wiki %>%
   html_nodes("table") %>%
   .[[4]] %>%
   html_table(fill=T)

#Adds the column with the year the season premiered to the allseasons dataframe
premierDate = premier[2:nrow(premier), 3]
if(length(premierDate) < nrow(allseasons)){
  allseasons = allseasons[-nrow(allseasons),]
}
allseasons = cbind(allseasons, premierYear = ifelse(substring(premierDate, nchar(premierDate))==']', substring(premierDate, nchar(premierDate)-7, nchar(premierDate)-4), substring(premierDate,nchar(premierDate)-3, nchar(premierDate))))

#Creates the function getLocation which takes the names of a city and state as
#strings and returns a data frame containing the location table from the 
#wikipedia page for that city.
getLocation <- function(city = 'Madison', state = 'Wisconsin'){
    city = gsub(" ", "_", city)
    state = gsub(" ", "_", state)
    loc = read_html(paste("https://en.wikipedia.org/wiki/", city,",_", state, sep = ''))
    print(loc)
    location = loc %>%
      html_nodes("table") %>%
      .[[1]] %>%
      html_table(fill=T)
    #print(location)
}

#Creates a function called getGEO which takes in a string from a Wikipedia
#location table and extracts the degree decimal latitude and and longitude
#and returns it as a numeric vector.
getGEO <- function(coordStr){
  dd = substring(coordStr,65,83)
  return(as.numeric(unlist(strsplit(dd,';'))))
}

#Creates a function to parse all the contestant information from each season's 
#wikipedia page
parseSeason <- function(seasons, season.contest, i){
  #Creates a string vector of all the contestants' last names for a particular season
  last.name <- gsub(",(.+)", "", season.contest[,1])
  #print(last.name)
  
  #Creates a string vector of all the contestants' first names for a particular season
  first.name <- gsub("^(.+?), | (.+)","", season.contest[,1])
  first.name <- substring(first.name, 1, nchar(first.name)/2)
  #print(first.name)
  
  #Creates a integer vector of all the contestants' age
  age <- as.integer(substr(gsub("([^0-9])", "", season.contest[,1]),1,2))
  #print(age)
  
  birth.year <- as.integer(as.character(seasons[i,9])) - age
  #print(birth.year)
  
  sex <- vector()
  for(g in 1:length(first.name)){
    if(is.na(birth.year[g])){
      sex <- c(sex, "unknown")
    }
    else if(length(gender(first.name[g], years = birth.year[g], countries = "United States")$gender) == 0){
      sex <- c(sex, "unknown")
    } else {
      sex <- c(sex, gender(first.name[g], years = birth.year[g], countries = "United States")$gender)
    }
  }
  #print(sex)
  
  #Creates a string vector of all the contestants' home city
  city = gsub("(.+[1-9]), |, (.+)", "", season.contest[,1])
  #print(city)
  
  #Creates a string vector of all the contestants' home state
  state <- gsub("(.+), ","", season.contest[,1])
  #print(state)
  
  geo = data.frame()
  totalArea = vector()
  totalPop = vector()
  popDens = vector()
  
  # for(i in 1:length(city)){
  #   tryCatch(getLocation(city[i],state[i]), error = print("no location"))
  #   error = function(e) {print(paste("non-numeric argument", x)); 
  #     NaN}
  #   location <- getLocation(city[i],state[i])
  #   geo <- rbind(geo,getGEO(location[grepl('Coord', location[,2]),1]))
  #   stats = location[grepl('km2)', location[,2]),]
  #   totalPop = rbind(totalPop,as.numeric(gsub(",", "", location[grepl('[:digits{0-9}:],[:digits{0-9}:]', location[,2]),2][1])))
  #   print(totalPop[i])
  #   totalArea <- rbind(totalArea,as.numeric(gsub(",", "", unlist(strsplit(stats[grepl('Total',stats[,1]),2],'sq'))[1])))
  #   popDens <- rbind(popDens,totalPop[i]/totalArea[i])
  #   print(city[i])
  #   print(state[i])
  #   print(popDens)
  #   #totalPop <- rbind(totalPop,as.numeric(gsub(",", "", unlist(strsplit(stats[grepl('Land',stats[,1]),2],'sq'))[1])) * popDens[i,])
  # }
  # #
  
  #Creates an integer vector of all the contestants' place where he or she finished in their season
  place <- as.integer(substring(season.contest$Finish,1,2))
  #print(place)
  
  #Creates an integer vector with the number of days a contestant played the game
  num.days <- as.integer(gsub("^\\s+", "", substring(season.contest$Finish, nchar(season.contest$Finish)-1, nchar(season.contest$Finish))))
  num.days = ifelse(is.na(num.days), 40, num.days)
  #print(num.days)
  
  #Creates a data frame with all the extracted contestant information from each season
  data.frame(last.name, first.name, age, birth.year, sex, city, state, place,
             num.days, season.num = as.integer(i), season.name = seasons[i, 2],
             location = seasons[i,3], winner = seasons[i,5], second = seasons[i,6], 
             third = seasons[i,7]#, 
             #lat = geo[1], lon = geo[2], totalArea, popDens, totalPop
             )
}

###Pull data about each of the contestants from all the survivor seasons

for(i in 1:nrow(allseasons)){
  print(i)
  #create a variable that has the appropriate naming convention to call the webpage for each survivor season
  season.name = gsub(" ", "_", allseasons[i, 2])
  print(season.name)
  #create a html object for each season's wikipedia page
  season = read_html(paste("https://en.wikipedia.org/wiki/", season.name))
  #creates a variable for which table to look for the contestant table on the Survivor
  #Season wikipedia page and then tests if there is an extra table by checking if
  #the name "Contestant" is a column header in the table.
  table = 2
  if(!("Contestant" %in% colnames(season %>%
     html_nodes("table") %>%
     .[[2]] %>%
     html_table(fill=T)))){
    table = 3
  }
  #create a variable called season.contest that hold the table from the wikipedia page with all the contestant's information
  contestants = season %>%
    html_nodes("table") %>%
    .[[table]] %>%
    html_table(fill=T)
  
  #Logical check to see if the parse season function is being applied to the first
  #season or a subsequent season.
  if(i == 1){
    allcontestants <- parseSeason(allseasons, contestants, i)
  } else {
    allcontestants <- rbind(allcontestants, parseSeason(allseasons, contestants, i))
  }
}

#Remove duplicate entries for seasons where someone was voted off twice.
allcontestants$name = paste(allcontestants$first.name, allcontestants$last.name)
allcontestants = allcontestants[!duplicated(allcontestants[,c(11,16)]),]

#Write out allcontestants dataframe to csv
setwd("~/Documents/Github/Survivor_Winner_Predictor")
write.csv(allcontestants, file = "contestants.csv")

###WORK LEFT TO DO###
###1. Error handle if there is no wikipedia for a location
###2. Calculate number of times played
###3. Scrape bios from webpage 
###4. TFIDF Vectorize Bio Text
