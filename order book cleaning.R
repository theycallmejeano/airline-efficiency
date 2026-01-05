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
airline_sample<-read_csv("data/full data extended.csv")
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


# ---- metrics for report -----
airlines <- c("Air Canada","Air France","American Airlines","British Airways",
              "Delta Air Lines","Emirates","Lufthansa",
              "Turkish Airlines","United Airlines","Qantas",
              "Easyjet","Frontier Airlines","Indigo","Jetblue Airways",
              "Ryanair","Spirit Airlines","South West Airlines","Wizz Air")

merged_backlog %>% filter(Customer %in% airlines) %>% 
  group_by(Customer) %>% 
  summarise(sum(Backlog))


# Select variables for median calculation
vars <- c("Fleet size", "Passenger Load factor (%)", "Number of employees", 
          "Revenue", "Expenses", "RPK", "ASK", "Backlog")

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

# who had bigger dip in lf
merged_input_ext %>% 
  select(Airline, Year, `Passenger Load factor (%)`) %>% 
  filter(Year %in% c(2019, 2020)) %>%
  left_join(efficiency_wbacklog_ext[,c("Airline", "Year", "Operating_Model")], by=c("Airline", "Year")) %>%
  mutate(Operating_Model=ifelse(Airline=="Air Canada", "Hub_and_Spoke", Operating_Model)) %>%
  group_by(Operating_Model, Year) %>% summarise(mean(`Passenger Load factor (%)`))

merged_input_ext %>% 
  select(Airline, Year, `RPK`) %>% 
  filter(Year %in% c(2019, 2020)) %>%
  left_join(efficiency_wbacklog_ext[,c("Airline", "Year", "Operating_Model")], by=c("Airline", "Year")) %>%
  pivot_wider(id_cols=c("Airline", "Operating_Model"), names_from=Year, values_from=`RPK`) %>% 
  mutate(point_change=`2020`-`2019`,
         Operating_Model=ifelse(Airline=="Air Canada", "Hub_and_Spoke", Operating_Model)) %>%
  group_by(Operating_Model) %>% summarise(mean(point_change))

merged_input_ext %>% 
  select(Airline, Year, RPK) %>% 
  left_join(efficiency_wbacklog_ext[,c("Airline", "Year", "Operating_Model")], by=c("Airline", "Year")) %>%
  mutate(Operating_Model=ifelse(Airline=="Air Canada", "Hub_and_Spoke", Operating_Model)) %>%
  group_by(Operating_Model, Year) %>% summarise(`Mean RPK`=mean(RPK)) %>%
  arrange(Operating_Model, Year) %>%
  group_by(Operating_Model) %>%
  mutate(
    Delta_RPK = `Mean RPK` - lag(`Mean RPK`),
    Delta_Year = paste0(Year, "–", Year - 1)
  ) %>%
  filter(!is.na(Delta_RPK)) %>%
  ggplot(aes(x = Year, y = `Delta_RPK`, color=Operating_Model)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_line(size = 1.2) +
  geom_text(
    aes(label = scales::comma(round(Delta_RPK))),
    vjust = ifelse(Delta_RPK > 0, -0.6, 1.2),
    size = 4,
    show.legend = FALSE
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_manual(values = c(
    "Hub and Spoke" = "#a6cee3",
    "Point to Point" = "#fb9a99"
  )) +
  labs(
    title = "Year-on-Year Change in RPKs by Operating Model",
    x = "Year",
    y = "Changes in average RPKs",
    colour = "Operating model"
  ) +
  theme_classic(base_size = 16)
  # theme(
  #   axis.text.x = element_text(hjust = 1),
  #   strip.text = element_text(face="bold"))