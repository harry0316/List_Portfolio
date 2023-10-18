
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("plotrix")
install.packages("dplyr")
install.packages("leaflet")


library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotrix)
library(leaflet)


setwd("~/my_portfolio_R")

#import csv.file
delivery <- read.csv("deliverytime.csv")
glimpse(delivery)


#delivery:summary + confirm each column type
summary(delivery)
str(delivery)

#find any corrupted data
unique(delivery$Type_of_vehicle)
unique(delivery$Delivery_person_Ratings)
unique(delivery$Time_taken.min.)
unique(delivery$Type_of_order)

#organize age column
#categorize driver's ages 
delivery %>%
  select(Delivery_person_Age) %>%
  arrange(Delivery_person_Age) %>%
  unique()

delivery$age_grop <- cut(delivery$Delivery_person_Age, breaks =c(0,19,24,29,34,39,44,49,54,59), labels = c("Late teens","Early twenties","Late Twenties","Early Thirties","Late Thirties","Early Forties","Late Forties","Early Fifties","Late Fifties") )

#visualize the number of driver by ages
delivery %>%
  group_by(age_grop) %>%
  summarise(num_deliver = n()) %>%
  arrange(age_grop) %>%
  ggplot(aes(x=age_grop, y = num_deliver)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = num_deliver), vjust = -0.5) +
  labs(title = 'The number of drivers by age', x ="Age Group", y = "Number of driver") 
  

#visualize the number of driver by ages and the type of Type_of_vehicle
delivery %>%
  group_by(age_grop,Type_of_vehicle) %>%
  summarise(num_deliver = n()) %>%
  arrange(age_grop,num_deliver,Type_of_vehicle) %>%
  ggplot(aes(x=age_grop, y = num_deliver, fill = Type_of_vehicle)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = num_deliver), vjust = -0.5, hjust= 0.5) +
  labs(title = 'The number of drivers by age', x ="Age Group", y = "Number of driver")+
  theme(axis.text.x = element_text(angle= 45, hjust = 1))


glimpse(delivery)
str(delivery)

#analyze the average review by age 
ave_review_by_age <- delivery %>% group_by(age_grop) %>%
  summarise(ave_view = round(mean(Delivery_person_Ratings),2))
  view(ave_review_by_age)

#visualize the review points 
ggplot(data=delivery, aes(x=Time_taken.min. ,y = Delivery_person_Ratings)) +
  geom_point() +
  labs(title = "Deliver Time vs Review score ")

#create the spot of Restaurant and address
location_map <- leaflet(delivery) %>%
  addTiles() 
location_map <- location_map %>%
  addCircleMarkers(
    lng = ~Restaurant_longitude,
    lat = ~Restaurant_latitude,
    color = "Red",
    fillOpacity = 1,
    stroke = T
    )

location_map <- location_map %>%
  addCircleMarkers(
    lng = ~Delivery_location_longitude,
    lat = ~Delivery_location_latitude,
    color = "Blue",
    fillOpacity = 1,
    stroke = T
  )

location_map



  