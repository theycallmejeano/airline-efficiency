library(deaR)
library(dplyr)
library(ggplot2)

source("utilities.R")

# ---- STEP 1: two-stage DEA ------
stage1_input<-c("Expenses","Fleet size")
stage1_output<-c("ASK")
stage2_input<-c("ASK", "Number of employees")
stage2_output<-c("RPK", "Load factor")

# stage 1 model
stage1_model<- run_dea(merged_input, stage1_input, stage1_output, orientation="oo")
stage2_model<- run_dea(merged_input, stage2_input, stage2_output, orientation="oo")

# one-stage model
black_box<- run_dea(merged_input, c("Expenses", "Fleet size"), c("ASK", "RPK", "Load factor"), orientation="oo")

# plots
ggplot(stage1_model, aes(x = factor(Year), y = Efficiency)) +
  geom_boxplot(outlier.alpha = 0.6) +
  labs(
    x = "Year",
    y = "Stage 1 DEA Efficiency",
    title = "Stage 1 (Operational) Efficiency Scores by Year"
  ) +
  theme_classic()

ggplot(stage2_model, aes(x = factor(Year), y = Efficiency)) +
  geom_boxplot(outlier.alpha = 0.6) +
  labs(
    x = "Year",
    y = "Stage 2 DEA Efficiency",
    title = "Stage 2 (Operational) Efficiency Scores by Year"
  ) +
  theme_classic()

# merge the DEA
overall_efficiency<- stage1_model %>% 
  left_join(stage2_model[,c("DMU", "Efficiency")],
                           by="DMU", suffix=c("_stage1", "_stage2")) %>%
  mutate(Additive_Efficiency=Efficiency_stage1*Efficiency_stage2,
         Harmonic_Mean_Efficiency=2/((1 / Efficiency_stage1) +(1 / Efficiency_stage2)))

# plot
overall_efficiency %>%
  select(Year, Efficiency_stage1, Efficiency_stage2) %>%
  pivot_longer(
    cols = c(Efficiency_stage1, Efficiency_stage2),
    names_to = "Stage",
    values_to = "Efficiency") %>%
  mutate(Stage = ifelse(Stage=="Efficiency_stage1", "Stage 1", "Stage 2")) %>%
  ggplot(aes(x = factor(Year), y = Efficiency, fill = Stage)) +
  geom_boxplot() +
  labs(x = "", y = "DEA scores", title = "Stage 1 vs Stage 2 Efficiency by Year") +
  theme_classic()

# comparing efficiency vals
overall_efficiency %>%
  select(Year, Additive_Efficiency, Harmonic_Mean_Efficiency) %>%
  pivot_longer(
    cols = c(Additive_Efficiency, Harmonic_Mean_Efficiency),
    names_to = "Method",
    values_to = "Efficiency") %>%
  mutate(Stage = ifelse(Method=="Additive_Efficiency", "Multiplicative", "Harmonic Mean")) %>%
  ggplot(aes(x = factor(Year), y = Efficiency, fill = Method)) +
  geom_boxplot() +
  labs(x = "", y = "DEA scores", title = "Overall Efficiency by Year") +
  theme_classic()

# black box
overall_efficiency %>%
  select(DMU, Year, Additive_Efficiency, Harmonic_Mean_Efficiency) %>%
  left_join(black_box[,c("DMU", "Year", "Efficiency")], by=c("DMU", "Year")) %>%
  pivot_longer(
    cols = c(Efficiency, Harmonic_Mean_Efficiency),
    names_to = "Method",
    values_to = "Efficiency") %>%
  mutate(Stage = ifelse(Method=="Harmonic_Mean_Efficiency", "Harmonic Mean", "Black box")) %>%
  ggplot(aes(x = factor(Year), y = Efficiency, fill = Method)) +
  geom_boxplot() +
  labs(x = "", y = "DEA scores", title = "Overall Efficiency by Year") +
  theme_classic()

# ----- STEP 2: regression
# first add in the backlog vars
efficiency_wbacklog<- add_backlog_metrics(overall_efficiency %>%select(-DMU))

# ---- BACKLOG GROWTH ----
print(summary(lm(Harmonic_Mean_Efficiency ~ Backlog_Growth *Fleet + Year + factor(Operating_Model),
                                           data = efficiency_wbacklog))) # fleet is significant, model has small r2, model not significant

print(summary(lm(Harmonic_Mean_Efficiency ~ Backlog_Growth *factor(Operating_Model) + Fleet + Year,
                 data = efficiency_wbacklog))) # fleet significance, very small r2

print(summary(lm(Harmonic_Mean_Efficiency ~ Backlog_Growth + factor(Operating_Model) + Fleet + Year,
                 data = efficiency_wbacklog))) # fleet significance, very small r2, model not significant

print(summary(lm(Harmonic_Mean_Efficiency ~ Backlog_Growth + factor(Operating_Model) + factor(Airline) +Fleet + Year,
                 data = efficiency_wbacklog))) # individual airline significance, weak significance for fleet + P2P

print(summary(lm(Harmonic_Mean_Efficiency ~ Backlog_Growth *Fleet + factor(Operating_Model) + factor(Airline) + factor(Year),
                 data = efficiency_wbacklog))) # individual + covid airline significance, weak significance for fleet + P2P

print(summary(lm(Harmonic_Mean_Efficiency ~ Backlog_Growth *Fleet + factor(Operating_Model) + factor(Airline) + factor(Year),
                 data = efficiency_wbacklog))) # individual airline + covid period significance, weak significance for fleet + P2P 

# --- LAGGED BACKLOG ----
print(summary(lm(Harmonic_Mean_Efficiency ~ Backlog_Growth_lag2 *Fleet + factor(Operating_Model) + factor(Airline) + factor(Year),
                 data = efficiency_wbacklog))) # individual airline + covid period significance

print(summary(lm(Harmonic_Mean_Efficiency ~ Backlog_Growth_lag2 *Fleet + factor(Operating_Model) + factor(Year),
                 data = efficiency_wbacklog))) #  covid period significance, model significance

print(summary(lm(Harmonic_Mean_Efficiency ~ Backlog_Growth_lag2 + Fleet + factor(Operating_Model) + factor(Year),
                 data = efficiency_wbacklog))) #  fleet significance, model significance

print(summary(lm(Harmonic_Mean_Efficiency ~ Backlog_Growth_lag2 *Fleet + factor(Operating_Model),
                 data = efficiency_wbacklog))) # fleet is efficient, model isn't

# ---- MEAN BACKLOG ----
print(summary(lm(Harmonic_Mean_Efficiency ~ Mean_Backlog *Fleet + factor(Operating_Model) + factor(Year),
                 data = efficiency_wbacklog))) #  covid period significance, model significance

print(summary(lm(Harmonic_Mean_Efficiency ~ Mean_Backlog + Fleet + factor(Operating_Model) + factor(Year),
                 data = efficiency_wbacklog))) #  fleet + covid period significance, model significance

print(summary(lm(Harmonic_Mean_Efficiency ~ Mean_Backlog *Fleet + factor(Operating_Model),
                 data = efficiency_wbacklog)))

print(summary(lm(Harmonic_Mean_Efficiency ~ Mean_Backlog *factor(Operating_Model) + Fleet,
                 data = efficiency_wbacklog)))

# ---- MEAN BACKLOG LAGGED ----
print(summary(lm(Harmonic_Mean_Efficiency ~ Mean_Backlog_lag2 *Fleet + factor(Operating_Model) + factor(Year),
                 data = efficiency_wbacklog))) #  no

print(summary(lm(Harmonic_Mean_Efficiency ~ Mean_Backlog_lag2 + Fleet + factor(Operating_Model) + factor(Year),
                 data = efficiency_wbacklog)))

print(summary(lm(Harmonic_Mean_Efficiency ~ Mean_Backlog_lag2 *Fleet + factor(Operating_Model),
                 data = efficiency_wbacklog)))

# ---- MEAN ROLLING AVG LAGGED ----
print(summary(lm(Harmonic_Mean_Efficiency ~ Mean_Backlog_rollavg2 *Fleet + factor(Operating_Model) + factor(Year),
                 data = efficiency_wbacklog))) #  no

print(summary(lm(Harmonic_Mean_Efficiency ~ Mean_Backlog_rollavg2 + Fleet + factor(Operating_Model) + factor(Year),
                 data = efficiency_wbacklog)))

print(summary(lm(Harmonic_Mean_Efficiency ~ Mean_Backlog_rollavg2 *Fleet + factor(Operating_Model),
                 data = efficiency_wbacklog)))

# ---- ROLLING AVERAGE BACKLOG LAGGED ----
print(summary(lm(Harmonic_Mean_Efficiency ~ Backlog_rollavg2 *Fleet + factor(Operating_Model) + factor(Year),
                 data = efficiency_wbacklog))) #  no

print(summary(lm(Harmonic_Mean_Efficiency ~ Backlog_rollavg2 + Fleet + factor(Operating_Model) + factor(Year),
                 data = efficiency_wbacklog)))

print(summary(lm(Harmonic_Mean_Efficiency ~ Backlog_rollavg2 *Fleet + factor(Operating_Model),
                 data = efficiency_wbacklog)))

# ---- STAGE 2 EFFICIENCIES: backlog growth ----
print(summary(lm(Efficiency_stage2 ~ Backlog_Growth *Fleet + factor(Year) + factor(Operating_Model),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog_Growth *Fleet + factor(Operating_Model) + factor(Year) + factor(Airline),
                 data = efficiency_wbacklog))) 

print(summary(lm(Efficiency_stage2 ~ Backlog_Growth *factor(Operating_Model) + Fleet + factor(Year),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog_Growth + factor(Operating_Model) + Fleet + factor(Year),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog_Growth + factor(Operating_Model) + factor(Airline) +Fleet + factor(Year),
                 data = efficiency_wbacklog)))


# ---- STAGE 2 EFFICIENCIES: lagged backlog growth ----
print(summary(lm(Efficiency_stage2 ~ Backlog_Growth_lag2 *Fleet + factor(Year) + factor(Operating_Model),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog_Growth_lag2 *Fleet + factor(Operating_Model) + factor(Year) + factor(Airline),
                 data = efficiency_wbacklog))) 

print(summary(lm(Efficiency_stage2 ~ Backlog_Growth_lag2 *factor(Operating_Model) + Fleet + factor(Year),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog_Growth_lag2 + factor(Operating_Model) + Fleet + factor(Year),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog_Growth_lag2 + factor(Operating_Model) + factor(Airline) +Fleet + factor(Year),
                 data = efficiency_wbacklog)))


# ---- STAGE 2 EFFICIENCIES: mean backlog ----
# NOTE: significant
print(summary(lm(Efficiency_stage2 ~ Mean_Backlog *Fleet + factor(Year) + factor(Operating_Model),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Mean_Backlog *Fleet + factor(Operating_Model) + factor(Year) + factor(Airline),
                 data = efficiency_wbacklog))) 

# NOTE: significant
print(summary(lm(Efficiency_stage2 ~ Mean_Backlog *factor(Operating_Model) + Fleet + factor(Year),
                 data = efficiency_wbacklog)))
# NOTE: significant
print(summary(lm(Efficiency_stage2 ~ Mean_Backlog + factor(Operating_Model) + Fleet + factor(Year),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Mean_Backlog + factor(Operating_Model) + factor(Airline) +Fleet + factor(Year),
                 data = efficiency_wbacklog)))

# not doing stage 1, as fleet is part of the efficiency score


# ---- STAGE 2 EFFICIENCIES: lagged mean backlog ----
print(summary(lm(Efficiency_stage2 ~ Mean_Backlog_lag2 *Fleet + factor(Year) + factor(Operating_Model),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Mean_Backlog_lag2 *Fleet + factor(Operating_Model) + factor(Year) + factor(Airline),
                 data = efficiency_wbacklog))) 

print(summary(lm(Efficiency_stage2 ~ Mean_Backlog_lag2 *factor(Operating_Model) + Fleet + factor(Year),
                 data = efficiency_wbacklog)))

# NOTE: significant, looks fishy
print(summary(lm(Efficiency_stage2 ~ Mean_Backlog_lag2 + factor(Operating_Model) + Fleet + factor(Year),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Mean_Backlog_lag2 + factor(Operating_Model) + factor(Airline) +Fleet + factor(Year),
                 data = efficiency_wbacklog)))

# not doing stage 1, as fleet is part of the efficiency score
# ---- STAGE 2 EFFICIENCIES: rolling avg backlog ----
# NOTE: significant
print(summary(lm(Efficiency_stage2 ~ Backlog_rollavg2 *Fleet + factor(Year) + factor(Operating_Model),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog_rollavg2 *Fleet + factor(Operating_Model) + factor(Year) + factor(Airline),
                 data = efficiency_wbacklog))) 

print(summary(lm(Efficiency_stage2 ~ Backlog_rollavg2 *factor(Operating_Model) + Fleet + factor(Year),
                 data = efficiency_wbacklog)))

# NOTE: significant, very fishy
print(summary(lm(Efficiency_stage2 ~ Backlog_rollavg2 + factor(Operating_Model) + Fleet + factor(Year),
                 data = efficiency_wbacklog)))