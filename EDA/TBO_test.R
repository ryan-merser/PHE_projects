#Attaching packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)

# Histogram of time between orders
## Make new query where times ordered > 1

#Read in orders csv table
orders1 <- read_excel("Finalordersstates.xlsx")
orders1$order_create_date <- as.Date(orders1$order_create_date)

orders1 <- orders1 %>%
  arrange(g_user_id, order_create_date)

neworders <- orders1 %>%
   group_by(g_user_id) %>%
   mutate(time_between_orders = as.numeric(order_create_date 
                                           - lag(order_create_date)))

neworders <- neworders %>%
  group_by(g_user_id) %>%
  mutate(customer_order_number = row_number())

#Can break out rebuy by purchase number for each customer
# 1 is first purchase
# 2 is time between 1st and 2nd purchase
# 3 is time between 2nd and 3rd purchase, etc.

#Histogram of time between orders
newhist <- ggplot(data=subset(neworders, !is.na(time_between_orders)), 
                                   aes(x=time_between_orders))  +
  geom_histogram(binwidth = 4, color = "black", fill = "White") +
  stat_bin(binwidth = 4, geom = 'text', aes(label = ..count..), color = "red", 
           position = position_stack(vjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 500, 50)) +
  scale_x_continuous(breaks = seq(0, 53, 4)) +
  labs(y = "Count", x = "Time between orders(days)") +
  ggtitle("Histogram of time between orders") 

newhist

#Histogram excluding zero
newhist2 <- ggplot(data=subset(neworders, (time_between_orders > 0)), 
                  aes(x=time_between_orders))  +
  geom_histogram(binwidth = 1, color = "black", fill = "White") +
  #stat_bin(binwidth = 1, geom = 'text', aes(label = ..count..), color = "red", 
  #         position = position_stack(vjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 200, 25)) +
  scale_x_continuous(breaks = seq(1, 54, 3)) +
  labs(y = "Count", x = "Time between orders(days)") +
  ggtitle("Histogram of time between orders- not including 0 days") 

newhist2

TBOtable <- neworders %>%
  group_by(time_between_orders) %>%
  summarise(count = n())



