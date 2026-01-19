# utilities
library(tidyverse)
library(readxl)
library(zoo)

# --- functions ----
merge_airbus_data<-function(folder_path) {
  # Function to read through the Airbus order book data from 2018 to 2024 and merge to a single dataframe 
  # 1. Loop through folder: Get list of all excel files
  filenames <- list.files(path = folder_path, pattern = "*.xlsx")
  
  # 2. Use map_df to merge the files
  all_data <- map_df(filenames, function(filename) {
    print(paste("Cleaning", filename))
    
    filepath <- file.path(folder_path, filename)
    # read data
    df <- read_excel(filepath, sheet = "Worldwide", skip = 17)
    
    raw_names <- colnames(df)
    
    # 2. "Fill" the aircraft names across the ...n columns
    clean_names <- tibble(name = raw_names) %>%
      mutate(name = ifelse(str_detect(name, "\\.\\.\\."), NA, name)) %>%
      fill(name, .direction = "down") %>%
      pull(name)
    
    # 3. Apply these filled names to the dataframe
    # This turns [A330-200F, ...76, ...77] into [A330-200F_Ord_75, A330-200F_Del_76, A330-200F_Opr_77]
    sub_headers <- as.character(df[1, ])
    colnames(df) <- paste0(clean_names, "_", sub_headers, "_", seq_along(clean_names))
    
    # extract the dataset
    df_clean<- df %>% 
      select(2, matches("F_|TOTAL")) %>% # select the Total columns and cargo fleet columns
      slice(-1) %>%  # drop the first row
      pivot_longer(cols=2:last_col()) %>%
      mutate(raw_type=str_split_i(name, "_", 2),
             value=as.numeric(value),
             type=ifelse(str_detect(name, "TOTAL"), paste("Total",raw_type), raw_type)) %>%
      rename("Customer" = 1) %>% 
      group_by(Customer, type) %>% summarise(sum=sum(value)) %>%
      pivot_wider(id_cols=Customer, names_from="type", values_from="sum") %>%
      # subtract the cargo fleet from total fleet, so that we get the passenger fleet numbers
      mutate(Customer=str_to_title(Customer),
             Deliveries=`Total Del`-coalesce(Del,0),
             Orders=`Total Ord`-coalesce(Ord,0),
             Operational=`Total Opr`-coalesce(Opr,0),
             Backlog=Orders-Deliveries,
             # get the year from the filename
             Year = as.numeric(stringr::str_extract(filename, "\\d{4}"))) %>%
      rename("Customer" = 1) %>% 
      select(Customer, Year,Deliveries, Orders, Operational, Backlog) %>%
      filter(!is.na(Customer))
    
    return(df_clean)
    })

  return(all_data)
}

# Function to run Window DEA
run_dea <- function(data, input_list, output_list, orientation="oo", rts="vrs") {

  # General function to run DEA
  # Args
    # data: dataframe with inputs and outputs
    # input_list: list of input cols
    # output_list: list of output cols
    # orientation : DEA orientation model input oriented(io) or output oriented(oo)
    # rts : returns to scale model, variable returns to scale(vrs) or constant returns to scale(crs)
    
  # Create DEA data
  input_idx  <- which(names(data) %in% input_list)
  output_idx <- which(names(data) %in% output_list)
    
  tryCatch({
    dea_data <- make_deadata(
      data,
      dmus = "DMU",  # DMU column position
      inputs = input_idx, 
      outputs = output_idx    
    )
     cat("Number of DMUs:", dea_data$n, "\n")
     cat("Number of inputs:", dea_data$m, "\n")
     cat("Number of outputs:", dea_data$s, "\n")
    # 
    # # Print actual data to check
    # cat("\nInput data (first 5 rows):\n")
    # print(head(dea_data$input, 5))
    # print(head(dea_data$output, 5))
    
    # Run DEA
    dea_result <- model_sbmeff(dea_data, orientation = orientation, rts = rts)
    
    #cat("  DEA completed with", length(efficiencies(dea_result)), "efficiency scores\n")
    # print(efficiencies(dea_result))
    
    # Store results
    results_list <- data.frame(
      DMU = data$DMU,
      Airline = data$Airline,
      Year = data$Year,
      Backlog = data$Backlog,
      Fleet=data$`Fleet size`,
      Efficiency = efficiencies(dea_result)
    )
    
   error = function(e) {
    cat("  ERROR:", e$message, "\n")
    return(NULL)}
  })
  
  bind_rows(results_list)
}

# function to add backlog metrics to input dataframe
add_backlog_metrics<- function(dataframe){
  # Step 1: Calculate persistent backlog metrics
  backlog_metrics <- dataframe %>%
    arrange(Airline, Year) %>%
    group_by(Airline) %>%
    mutate(
      # Backlog severity (relative to fleet)
      Backlog_Fleet_Ratio = (Backlog / Fleet) * 100,
      
      # Cumulative backlog exposure
      Cumulative_Backlog = cumsum(Backlog),
  
      # Backlog severity (relative to fleet)
      Backlog_Fleet_Ratio = (Backlog / Fleet) * 100,
      
      # Years under high backlog
      High_Backlog_Years = sum(Backlog > median(Backlog, na.rm = TRUE)),
      
      # Backlog growth
      Backlog_Growth = (Backlog - lag(Backlog)) / lag(Backlog) * 100,
      Backlog_Growth = ifelse(is.na(Backlog_Growth), 0, Backlog_Growth),
      
      # lagged values, lagging values as it takes ~2 years to get a new plane
      Backlog_lag2 = lag(Backlog, 2),
      Backlog_Growth_lag2 = lag(Backlog_Growth, 2),

      # rolling averages
      Backlog_rollavg2 = rollmean(Backlog, k = 2, align = "right", fill = NA),

      # rolling averages
      Backlog_rollavg2 = rollmean(Backlog, k = 2, align = "right", fill = NA),
    ) %>%
    ungroup() %>%
    mutate(
      Operating_Model = case_when(
        # TODO : remove from code
        # Low-cost carriers (typically P2P)
        Airline %in% c("Easyjet", "Ryanair", "Indigo", "Wizz Air", "Frontier Airlines", "Jetblue Airways",
                       "Southwest Airlines", "Spirit Airlines") ~ "Point_to_Point",
        
        # Major network carriers (hub-and-spoke)
        Airline %in% c("American Airlines", "United Airlines", "Delta Air Lines",
                       "Lufthansa", "British Airways", "Emirates", "Turkish Airlines",
                       "Korean Air", "Singapore Airlines", "Qantas",
                       "China Southern Airlines", "China Eastern Airlines",
                       "Air Canada", "Air China") ~ "Hub_and_Spoke",
        TRUE ~ "Other"),
      Period=case_when(Year %in% c(2018, 2019)~"Pre-pandemic",
                       Year %in% c(2020, 2021) ~ "Pandemic",
                       TRUE ~ "Pandemic recovery"))
  
  return(backlog_metrics)
}

# weighting
normalize_data <- function(data) {
  data_norm <- data %>%
    mutate(
      # Normalize by dividing by maximum
      fleet_size_norm = `Fleet size` / max(`Fleet size`),
      opex_norm = Expenses / max(Expenses),
      ask_norm = ASK / max(ASK),
      employees_norm = `Number of employees` / max(`Number of employees`),
      revenue_norm = Revenue / max(Revenue),
      load_factor_norm = `Passenger Load factor (%)` / max(`Passenger Load factor (%)`)
    )
  
  return(data_norm)
}

calculate_weights <- function(data) {
  data <- data %>%
    mutate(
      # Aggregate Stage 1 output (normalized ASK)
      Z0 = ask_norm,
      
      # Aggregate Stage 2 outputs (normalized RPK + Load Factor)
      Y0 = revenue_norm + load_factor_norm,
      
      # Total aggregate output
      total_output = Z0 + Y0,
      
      # Calculate weights
      w1 = Z0 / total_output,
      w2 = Y0 / total_output
    )
  
  return(data)
}

rank_scores <- function(data, score_col="Additive_Efficiency"){
  ranked_data <- data %>%
    group_by(Year) %>%
    mutate(
      # Rank (1 = most efficient)
      Efficiency_Rank = rank(-get(score_col), ties.method = "min")) %>%
    ungroup() %>%
    select(Airline, Year, Operating_Model, Efficiency_Rank) %>%
    pivot_wider(id_cols=c("Airline", "Operating_Model"),
                names_from=Year,
                values_from = "Efficiency_Rank")
  
  return(ranked_data)
}