                                                    ##BikesInfo.R
install.packages(c("tidyverse","httr", "jsonlite",'leaflet'))
# Loading packages
library(tidyverse)
library(httr)
library(jsonlite)
library(leaflet)

#API request
bike_info <- GET("https://api.jcdecaux.com/vls/v1/stations", query = list(contract="Brisbane",apiKey="117011bdd7f0262eddcbf5d578148005d42c13f7"))
names(bike_info)

#parsing the API response content
parsed_bike_info <- jsonlite::fromJSON(content(bike_info, "text" , encoding = "UTF-8"))

#verifying the API content 
class(parsed_bike_info)
names(parsed_bike_info)
##View(parsed_bike_info)

#Imported real time bike data information from Brisbane
brisbane_bikes <- parsed_bike_info


#Setting  appropriate column names to the data set
names(brisbane_bikes) <- c('Station ID', 'City', 'Station Name', 'Station Address','Position','Banking Terminal', 'Bonus Station', 'Total Bike Stands', 'Available Bike Stands', 'Bikes Available','Status','Last_Updated')
View(brisbane_bikes)

# Removing numbers and charcters from the Station Name
brisbane_bikes$`Station Name`<- str_replace_all(brisbane_bikes$`Station Name`,'\\d',"")
brisbane_bikes$`Station Name`<-str_replace(brisbane_bikes$`Station Name`, "-", "")
str_trim(brisbane_bikes$`Station Name`, side = 'left')    #trimmed the extra spaces

                                                    # DATA SUMMARY
#Overall Summary of the dataset
summary(brisbane_bikes)

    #Summary of each variable -- 
#Total Number of Bike Station in Brisbane
Number_of_BikeStations <- nrow(brisbane_bikes)
Number_of_BikeStations

# Checking the Address of Stations
Address_Check <- brisbane_bikes[brisbane_bikes$`Station Address` == '',]$`Station Name`
Address_Check
length(Address_Check)

#Does it have a Banking Terminal?
sum(brisbane_bikes$`Banking Terminal`== 'TRUE')
sum(brisbane_bikes$`Banking Terminal`== 'FALSE')
has_banking_terminal <- table(brisbane_bikes$`Banking Terminal`)
has_banking_terminal

#Is it a Bonus Station ?
sum(brisbane_bikes$`Bonus Station`)
is_it_BonusStn <- table(brisbane_bikes$`Bonus Station`)
is_it_BonusStn

#Total Bike Stands
sum(brisbane_bikes$`Total Bike Stands`)

# Station with Maximum Number of bike stands in Brisbane
max_bike_stands <- max(brisbane_bikes$`Total Bike Stands`)
max_bike_stands
Station_max_bikeStand<- brisbane_bikes[brisbane_bikes$`Total Bike Stands` == max_bike_stands,]
Station_max_bikeStand

# Station with Minimum Number of bike stands in Brisbane
min_bike_stands <- min(brisbane_bikes$`Total Bike Stands`)
min_bike_stands
Station_min_bikeStand<- brisbane_bikes[brisbane_bikes$`Total Bike Stands` == min_bike_stands,]
Station_min_bikeStand

# Average Number of bike stands in Brisbane
average_bike_stands <- floor(mean(brisbane_bikes$`Total Bike Stands`))
average_bike_stands

less_than_avg_stands <- brisbane_bikes[brisbane_bikes$`Total Bike Stands` < average_bike_stands,]
less_than_avg_stands
length(less_than_avg_stands$`Station ID`)

more_than_avg_stands <-  brisbane_bikes[brisbane_bikes$`Total Bike Stands` >= average_bike_stands,]
more_than_avg_stands
length(more_than_avg_stands$`Station ID`)

#Available Bike Stands in brisbane at the moment
sum(brisbane_bikes$`Available Bike Stands`)

## Highest Number of available bike stands at the moment
max_avb_bikestands <- max(brisbane_bikes$`Available Bike Stands`)
max_avb_bikestands
subset(brisbane_bikes, (brisbane_bikes$`Available Bike Stands` == max_avb_bikestands))

## Lowest Number of available bike stands at the moment
min_avb_bikestands <- min(brisbane_bikes$`Available Bike Stands`)
min_avb_bikestands
subset(brisbane_bikes, (brisbane_bikes$`Available Bike Stands` == min_avb_bikestands))

#Stations with valid number of available bus stands at the moment
valid_bustands <- brisbane_bikes[brisbane_bikes$`Available Bike Stands` >0, ]
valid_bustands
length(valid_bustands$`Station ID`)

#Stations with vacant available bus stands at the moment
vacant_bustands <- brisbane_bikes[brisbane_bikes$`Available Bike Stands` == 0, ]
vacant_bustands
length(vacant_bustands$`Station ID`)

## Average Number of available bike stands at the moment
avg_avai_Bikestands <- floor(mean(brisbane_bikes$`Available Bike Stands`))
avg_avai_Bikestands

less_than_avg_avb_Bikestands <- subset(brisbane_bikes, (brisbane_bikes$`Available Bike Stands` < avg_avai_Bikestands))
less_than_avg_avb_Bikestands
length(less_than_avg_avb_Bikestands$`Station ID`)

more_than_avg_avb_Bikestands <- subset(brisbane_bikes, (brisbane_bikes$`Available Bike Stands` >= avg_avai_Bikestands))
more_than_avg_avb_Bikestands
length(more_than_avg_avb_Bikestands$`Station ID`)


# Bikes Available at the moment
sum(brisbane_bikes$`Bikes Available`)

## Highest Number of bikes available
max_bikes_avl <- max(brisbane_bikes$`Bikes Available`)
max_bikes_avl
Station_max_avl_bikes <- brisbane_bikes[brisbane_bikes$`Bikes Available` == max_bikes_avl, ]
Station_max_avl_bikes

## Lowest Number of bikes available
min_bikes_avl <- min(brisbane_bikes$`Bikes Available`)
min_bikes_avl
Station_max_avl_bikes <- brisbane_bikes[brisbane_bikes$`Bikes Available` == min_bikes_avl, ]
Station_max_avl_bikes
length(subset(brisbane_bikes, (brisbane_bikes$`Bikes Available` == min_bikes_avl))$`Station ID`)

## Average Number of bikes available at the moment
avg_bike_avl <- floor(mean(brisbane_bikes$`Bikes Available`))
avg_bike_avl

less_than_avg_bavl <- subset(brisbane_bikes, (brisbane_bikes$`Bikes Available` < avg_bike_avl))
less_than_avg_bavl
length(less_than_avg_bavl$`Station ID`)

more_than_avg_bavl <- subset(brisbane_bikes, (brisbane_bikes$`Bikes Available` >= avg_bike_avl))
more_than_avg_bavl
length(more_than_avg_bavl$`Station ID`)


# Stands which are active
table(brisbane_bikes$Status)
Closed_bike_Stand<- brisbane_bikes[brisbane_bikes$Status !='OPEN', ]
Closed_bike_Stand


##
#2.2 Qplot representation regarding the Top 10 Active bike Stands in Brisbane
top_set <- subset(brisbane_bikes, select = c(`Station Name`, `Banking Terminal`, `Total Bike Stands`, `Available Bike Stands`, `Bikes Available`, `Status`))
topActive <- arrange(top_set, desc(`Total Bike Stands`))
topActive <- topActive[1:10,]
View(topActive)
qplot(data = topActive, x = `Station Name`, y=`Total Bike Stands`, fill = `Status`, main = 'Top 10 Active Bike Stations in Brisbane, Australlia', ylab='Total Number of Bike Stands', xlab='Station') + geom_bar(stat = "identity", fill = "darkblue") + coord_flip()

#Plotting in MAP of brisbane using longitude and lattitiude
brisbane_stn<- brisbane_bikes$`Station Name`
brisbane_bikes_pos<-mutate(brisbane_bikes$Position,brisbane_stn)
names(brisbane_bikes_pos)<- c('latitude','longitude','Station Name')
brisbane_Street<-inner_join(brisbane_bikes_pos,topActive, by = 'Station Name')
View(brisbane_Street)

map_brisbane <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=brisbane_Street$longitude, lat=brisbane_Street$latitude, popup=brisbane_Street$`Station Name`)
map_brisbane

                                            ##The END of BikeInfo.R



