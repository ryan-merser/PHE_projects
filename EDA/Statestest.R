library(tidyverse)
library(dplyr)
library(readxl)
orders <- read_excel("Finalordersstates.xlsx")


#Converting date objects and adding time to delivery variable
orders$dateAndTime <- ymd_hms(orders$dateAndTime, tz = "UTC")

orders$dateAndTime <- as.Date(orders$dateAndTime)
orders$dateAndTime <- format(orders$dateAndTime, format = "%m/%d/%Y")

orders <- orders %>%
  rename(arrival_date = dateAndTime)

orders$arrival_date <- as.Date(orders$arrival_date, format = "%m/%d/%Y")
orders$shipping_date <- as.Date(orders$shipping_date, format = "%m/%d/%Y")

orders <- orders %>%
  mutate(time_to_delivery = difftime(orders$arrival_date, orders$shipping_date, 
                                     units = "days"))

orders$time_to_delivery <- as.integer(orders$time_to_delivery)

orders <- drop_na(orders)

#Creating table for state data
statesfreq <- orders %>%
  group_by(Ship_State, Ship_Country) %>%
  rename(state = Ship_State) %>%
  summarise(count = n(),
            Mean_delivery_time = mean(time_to_delivery))

statesfreq

#US Delivery frequency heatmap
library(usmap)
library(ggplot2)

plot_usmap(data = statesfreq, values = "count", regions = "states") +
  scale_fill_gradientn(colours = c("white", "purple", "blue"),
                       name = "Number of orders", label = scales::comma) +
  labs(title = "Order Frequency by state") +
  theme(legend.position = "right")

#US Mean delivery time heatmap

plot_usmap(data = statesfreq, values = "Mean_delivery_time", regions = "states") +
  scale_fill_gradientn(colours = c("green", "yellow", "red"), 
                       name = "Time in days", label = scales::comma) +
  labs(title = "Mean Delivery Time of US Orders") +
  theme(legend.position = "right")


view(countypov)




#Reading county data
library(readxl)
zipcountydata <- read_excel("citiesandzip.xlsx")
zipcountydata$fips <- as.character(zipcountydata$fips)


#Converting date objects and adding time to delivery variable
zipcountydata$dateAndTime <- ymd_hms(zipcountydata$dateAndTime, tz = "UTC")

zipcountydata$dateAndTime <- as.Date(zipcountydata$dateAndTime)
zipcountydata$dateAndTime <- format(zipcountydata$dateAndTime, format = "%m/%d/%Y")

zipcountydata <- zipcountydata %>%
  rename(arrival_date = dateAndTime)

zipcountydata$arrival_date <- as.Date(zipcountydata$arrival_date, format = "%m/%d/%Y")
zipcountydata$shipping_date <- as.Date(zipcountydata$shipping_date, format = "%m/%d/%Y")

zipcountydata <- zipcountydata %>%
  mutate(time_to_delivery = difftime(zipcountydata$arrival_date, zipcountydata$shipping_date, 
                                     units = "days"))

zipcountydata$time_to_delivery <- as.integer(zipcountydata$time_to_delivery)

zipcountydata <- drop_na(zipcountydata)

#Creating table for Cali county data
calicountyfreq <- zipcountydata %>%
  group_by(county, fips) %>%
  filter(Ship_State == "CA") %>%
  rename(state = Ship_State) %>%
  summarise(count = n(),
            Mean_delivery_time = mean(time_to_delivery))

#City average delivery time heatmap for state of California

plot_usmap(data = calicountyfreq, values = "Mean_delivery_time", include = c("CA")) +
  scale_fill_gradientn(colours = c( "yellow", "red"), 
                       name = "Time in days", label = scales::comma) +
  labs(title = "Mean Delivery Time of California Orders by county") +
  theme(legend.position = "right")


#Creating table for Texas county data
texascountyfreq <- zipcountydata %>%
  group_by(county, fips) %>%
  filter(Ship_State == "TX") %>%
  rename(state = Ship_State) %>%
  summarise(count = n(),
            Mean_delivery_time = mean(time_to_delivery))

#City average delivery time heatmap for state of Texas

plot_usmap(data = texascountyfreq, values = "Mean_delivery_time", include = c("TX")) +
  scale_fill_gradientn(colours = c( "green","yellow", "red"), 
                       name = "Time in days", label = scales::comma) +
  labs(title = "Mean Delivery Time of Texas Orders by county") +
  theme(legend.position = "right")




#clipr::write_clip(countypov)






