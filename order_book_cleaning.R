library(tidyverse)
library(fuzzyjoin)
source("utilities.R")


# --- STEP 1: combine Airbus and Boeing Order books -----
# merge airbus data
df_airbus<-merge_airbus_data("data/backlog data/airbus raw") %>%
  mutate(Customer=ifelse(Customer=="Wizz Air Hungary", "Wizz Air", 
                         ifelse(Customer == "Qantas Airways", "Qantas", Customer)))
  mutate(Customer=ifelse(Customer=="Wizz Air Hungary", "Wizz Air", Customer))

# clean up boeing orders
boeing<- read_csv("data/backlog data/boeing orders.csv") %>%
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
#airline_sample<-read_csv("data/full data extended.csv")
airline_sample<-read_csv("data/full data v2.csv")
merged_input<- airline_sample %>% 
       left_join(merged_backlog, 
                 by=c("Airline"="Customer",
                      "Year")) %>%
  mutate(Year=as.numeric(Year),
         DMU=paste(Airline, Year, sep = "_"))

# validations
# 1. check that all airlines have all data for the entire period
merged_input %>%
  mutate(any_missing = if_any(everything(), is.na)) %>%
  filter(any_missing) %>%
  select(Airline, Year)

# ---- metrics for report -----
airlines <- c("Air Canada","Air France","American Airlines","British Airways",
              "Delta Air Lines","Emirates","Lufthansa",
              "Turkish Airlines","United Airlines","Qantas",
              "Easyjet","Frontier Airlines","Indigo","Jetblue Airways",
              "Ryanair","Spirit Airlines","Southwest Airlines","Wizz Air",
              "Air China")

merged_backlog %>% filter(Customer %in% airlines&Year<=2023) %>% 
  group_by(Customer) %>% 
  summarise(sum(Backlog))

merged_input %>%
  group_by(Airline) %>% 
  summarise(sum(Backlog, na.rm=T))

# Select variables for median calculation
vars <- c("Fleet size", "Passenger Load factor (%)", "Number of employees", 
          "Revenue", "Expenses","RPK" ,"ASK", "Backlog")

median_df <- merged_input %>%
  group_by(Year) %>%
  summarise(across(all_of(vars), median, na.rm = TRUE)) %>%
  pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Median") %>%
  mutate(Period = factor(
    case_when(
      Year %in% 2018:2019 ~ "Pre-pandemic",
      Year %in% 2020:2021 ~ "Pandemic",
      Year >= 2022        ~ "Pandemic recovery"
    ),
    levels = c("Pre-pandemic", "Pandemic", "Pandemic recovery")
  ))

# fig 4.1
ggplot(median_df, aes(x = factor(Year), y = Median, fill=Period)) +
  geom_col(width=0.6) +
  facet_wrap(~Variable, scales = "free_y") +
  scale_fill_manual(values = c(
    "Pre-pandemic" = "#a6cee3",      # light blue
    "Pandemic" = "#fb9a99",          # light red
    "Pandemic recovery" = "#fdbf6f"  # light orange
  )) +
  labs(
    title = "Median Values per Year",
    x = "Year",
    y = "Median",
    fill = "Period"
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.text.x = element_text(hjust = 1),
    strip.text = element_text(face="bold"))
