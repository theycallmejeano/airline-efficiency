library(tidyverse)
library(fuzzyjoin)
library(rDEA)
library(priceR)
source("utilities.R")


# --- STEP 1: combine Airbus and Boeing Order books -----
# merge airbus data
df_airbus<-merge_airbus_data("data/backlog data/airbus raw") %>%
  mutate(Customer=ifelse(Customer=="Wizz Air Hungary", "Wizz Air", Customer))

# clean up boeing orders
boeing<- read_csv("data/boeing orders.csv") %>%
  group_by(`Customer Name`, `Order Year`) %>%
  summarise(`Total Backlog`=sum(`Unfilled Orders`))

# merge order books
merged_backlog<-df_airbus %>% full_join(boeing, by=c("Customer"="Customer Name",
                                                     "Year"="Order Year")) %>%
  group_by(Customer, Year) %>%
  summarise(Total_Backlog = sum(Backlog, na.rm = TRUE) +
              sum(`Total Backlog`, na.rm = TRUE),
            .groups = "drop") %>% 
  rename("Backlog"="Total_Backlog")

# --- STEP 2: merge order backlogs to DEA data --------
# filter for airline sample
airline_sample<-read_csv("data/full data.csv")

merged_input<- airline_sample %>% 
       left_join(merged_backlog, 
                 by=c("Airline"="Customer",
                      "Year")) %>%
  mutate(Year=as.numeric(Year),
         DMU=paste(Airline, Year, sep = "_"))
  #filter(!is.na(Backlog))

# validations
# 1. check that all airlines have all data for the entire period
merged_input %>%
  mutate(any_missing = if_any(everything(), is.na)) %>%
  filter(any_missing) %>%
  select(Airline, Year)

# Ryanair(2018, 2019, 2021,2022, 2024) and Southwest(2020, 2022,2024) missing backlog data 

# ----- using covid paper data -----
test_data<-read_csv("data/poc data.txt") %>%
  mutate(Airline=ifelse(Airline=="EasyJet","Easyjet",Airline))

# merge to order books
mock_data<- test_data %>%
  mutate(Airline=ifelse(Airline=="Delta Airlines","Delta Air Lines",
                        ifelse(Airline=="IndiGo", "Indigo", Airline))) %>%
  left_join(merged_backlog, by=c("Airline"="Customer", "Year")) %>%
  filter(Year>2018&!Airline%in%c("LATAM Airlines","Hainan Airline","Qantas airway",
                                 "Scandinavian Airlines","Cathay Pacific Airways","	KLM")) %>%
    mutate(Backlog=ifelse(Airline=="Ryanair"&Year %in%c(2018,2019,2021,2022),10, 
                          ifelse(Airline=="Ryanair"&Year>2023, 150, Backlog)),
           DMU=paste(Airline, Year, sep = "_")) %>%
  rename("Fleet size"="Fleet_Size")

# DEA tests
mock_inputs<-c("Operating_expense", "Number_of_employees","Fleet_size")
mock_outputs<-c("Total_revenue","RPK")
run_window_dea_backlog(mock_data,mock_inputs, mock_outputs,1)
