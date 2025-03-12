#Attaching packages
library(tidyverse)
library(dplyr)
library(ggplot2) 


# Brainstorming
#
# Rebuy rate based on ship type/ ship price
# Look at histogram of order costs/shipping costs
# Histogram of time between purchases
## Make new query where times ordered > 1
# Rebuy rate by state/ county
# Demand allocation/ channel codes
# Do people stick with the same delivery method after rebuying?
# 
# Starting costs
# Standard: 8.99, can also be free (0), or half off (4.49)
# Standard test: 12.99
# 2-Day air: 16.99
# 1-Day air: 26.99
# DHL priority??
# 
# Free vs paid shipping on rebuy rate, combined with other variables?
#
# Actually figure out how long in days each of the shipping methods take to deliver
#
# 


#Rebuy rate based on shipping type
orders <- read.csv("Masterquerytest.csv")


#Indicate orders with free shipping



#Create new firstorders table
firstorders <- orders %>%
  mutate(order_create_date = as.Date(order_create_date, "%m/%d/%Y")) %>%
  group_by(g_user_id) %>%
  filter(order_create_date == min(order_create_date)) %>%
  filter(1:n() == 1)

# Making a dataframe for each cohort 
# (could differentiate by shipping cost as well)

#Standard
standard <- firstorders %>%
  filter(ShippingMethod == "Standard")

#Table of rebuy rate for standard shipping
rebuy_standard <- standard %>%
  group_by(times_ordered) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  mutate(percent = (n/sum(n)) * 100)

rr_std <- 100 - rebuy_standard$percent[1]

#Standard test
standardtest <- firstorders %>%
  filter(ShippingMethod == "Standard Test")

#
rebuy_standardtest <- standardtest %>%
  group_by(times_ordered) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  mutate(percent = (n/sum(n)) * 100)

rr_stdtest <- 100 - rebuy_standardtest$percent[1]

#2 day air
two_day_air <- firstorders %>%
  filter(ShippingMethod == "2 Day Air")

#
rebuy_2dayair <- two_day_air %>%
  group_by(times_ordered) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  mutate(percent = (n/sum(n)) * 100)

rr_2dair <- 100 - rebuy_2dayair$percent[1]

#1 day air
one_day_air <- firstorders %>%
  filter(ShippingMethod == "1 Day Air")

#
rebuy_1dayair <- one_day_air %>%
  group_by(times_ordered) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  mutate(percent = (n/sum(n)) * 100)

rr_1dair <- 100 - rebuy_1dayair$percent[1]

#Rounding and putting into dataframe
rr_all <- c(rr_std, rr_stdtest, rr_2dair, rr_1dair)

rr_all <- round(rr_all, digits = 2)

rebuyrates <- data.frame(Shipping_Method = c('Standard', 'Standard Test', '2 Day Air',
                                             '1 Day Air'),
                         Rebuy_rates = rr_all ) 

rebuyrates

linegraph <- ggplot(data = rebuyrates, aes(x = factor(Shipping_Method, 
             level = c('Standard', 'Standard Test', '2 Day Air','1 Day Air' )), 
                                           y = Rebuy_rates, group = 1)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(10, 70, 5)) +
  labs(y = "Rebuy Rate %", x = "Shipping method") +
  ggtitle("Plot of Rebuy rate by Shipping method for first order") +
  geom_text(aes(label = paste0(Rebuy_rates, "%")), color = "red", 
            nudge_x = .25, nudge_y = .25)

linegraph

bargraph <- ggplot(data = rebuyrates, aes(x = factor(Shipping_Method, 
                                                      level = c('Standard', 'Standard Test', '2 Day Air','1 Day Air' )), 
                                           y = Rebuy_rates, group = 1)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_line() +
  scale_y_continuous(breaks = seq(10, 50, 5)) +
  labs(y = "Rebuy Rate %", x = "Shipping method") +
  ggtitle("Plot of Rebuy rate by Shipping method for first order") +
  geom_text(aes(label = paste0(Rebuy_rates, "%")), color = "red", 
             nudge_y = 1)

bargraph

#Can overlay with rebuy rate by shipping cost


