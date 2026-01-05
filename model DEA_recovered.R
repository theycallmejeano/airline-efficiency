library(deaR)
library(dplyr)
library(ggplot2)

source("utilities.R")

# ---- STEP 1: two-stage DEA ------
stage1_input<-c("Expenses","Fleet size")
stage1_output<-c("ASK")
stage2_input<-c("ASK", "Number of employees")
stage2_output<-c("Revenue", "Load factor")

# TODO: bring in sandbox
# stage 1 model
stage1_model<- run_dea(merged_input, stage1_input, stage1_output, orientation="oo")
stage2_model<- run_dea(merged_input, stage2_input, stage2_output, orientation="oo")

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
  mutate(#Additive_Efficiency=(w1*Efficiency_stage1) + (w2*Efficiency_stage2),
         Harmonic_Mean_Efficiency=2/((1 / Efficiency_stage1) +(1 / Efficiency_stage2)))

# NOTE: fig 4.2 plot
overall_efficiency %>%
  select(Year, Efficiency_stage1, Efficiency_stage2) %>%
  pivot_longer(
    cols = c(Efficiency_stage1, Efficiency_stage2),
    names_to = "Stage",
    values_to = "Efficiency") %>%
  mutate(Stage = ifelse(Stage=="Efficiency_stage1", "Stage 1", "Stage 2")) %>%
  ggplot(aes(x = factor(Year), y = Efficiency, fill = Stage)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Stage 1" = "#FFB6C1",  
                               "Stage 2" = "#FFD9A0")) +
  labs(x = "", y = "DEA scores", title = "Stage 1 vs Stage 2 Efficiency by Year") +
  theme_classic(base_size=16)

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


# compare with sandbox
overall_efficiency %>% select(Year, DMU,Efficiency_stage2) %>%
  left_join(overall_efficiency_v1[,c("DMU","Efficiency_stage2")], 
            by = "DMU",
            suffix=c("_orig", "_weighted")) %>% select(-DMU) %>%
  pivot_longer(
    cols = c(Efficiency_stage2_orig, Efficiency_stage2_weighted),
    names_to = "Method",
    values_to = "Efficiency") %>%
  mutate(Stage = ifelse(Method=="Efficiency_stage2_orig", "Revenue output", "RPK output")) %>%
  ggplot(aes(x = factor(Year), y = Efficiency, fill = Stage)) +
  geom_boxplot() +
  labs(x = "", y = "DEA scores", title = "Overall Efficiency by Year") +
  theme_classic()

# sticking to revenue
# now adding in normalised values, maybe this will help with overall score
weights_df<- calculate_weights(normalize_data(merged_input)) %>% select(Airline, Year, w1, w2)

# first add in the backlog vars
efficiency_wbacklog<- add_backlog_metrics(overall_efficiency %>%select(-DMU)) %>%
  left_join(weights_df, by = c("Airline", "Year")) %>%
  mutate(Additive_Efficiency=(w1*Efficiency_stage1) + (w2*Efficiency_stage2))
# ensure baseline is pre-pandemic
efficiency_wbacklog$Period <- relevel(factor(efficiency_wbacklog$Period, ordered = FALSE), ref = "Pre-pandemic")

# NOTE: fig 4.3
efficiency_wbacklog %>%
  select(Year, Additive_Efficiency, Operating_Model) %>%
  ggplot(aes(x = factor(Year), y = Additive_Efficiency, fill=str_replace_all(Operating_Model, "_", " "))) +
  geom_boxplot() +
  labs(x = "Year", y = "DEA scores", 
       title = "Overall Efficiency by Operating Model",
       fill = "Operating model") +
  theme_classic(base_size=16)


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

# ----- STEP 2: regression
# ---- BACKLOG: no backlog significance ----
print(summary(lm(Additive_Efficiency ~ Backlog + Fleet + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog))) # backlog, weakly significant

print(summary(lm(Additive_Efficiency ~ Backlog * Fleet + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog)))

print(summary(lm(Additive_Efficiency ~ Backlog * factor(Operating_Model) + factor(Period) + Fleet,
                 data = efficiency_wbacklog)))


# ---- BACKLOG GROWTH: no backlog growth significance ----
print(summary(lm(Additive_Efficiency ~ Backlog_Growth *Fleet + factor(Period) + factor(Operating_Model),
                                           data = efficiency_wbacklog))) # fleet is significant, model has small r2, model not significant

print(summary(lm(Additive_Efficiency ~ Backlog_Growth *factor(Operating_Model) + Fleet + factor(Period),
                 data = efficiency_wbacklog))) # fleet significance, very small r2

print(summary(lm(Additive_Efficiency ~ Backlog_Growth + factor(Operating_Model) + Fleet + factor(Period),
                 data = efficiency_wbacklog)))

print(summary(lm(Additive_Efficiency ~ Backlog_Growth + factor(Operating_Model) +Fleet + factor(Period),
                 data = efficiency_wbacklog))) # individual airline significance, weak significance for fleet + P2P

print(summary(lm(Additive_Efficiency ~ Backlog_Growth *Fleet + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # individual + covid airline significance, weak significance for fleet + P2P

print(summary(lm(Additive_Efficiency ~ Backlog_Growth *Fleet + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # individual airline + covid period significance, weak significance for fleet + P2P 

# --- LAGGED BACKLOG ----
print(summary(lm(Additive_Efficiency ~ Backlog_Growth_lag2 *Fleet + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # individual airline + covid period significance

print(summary(lm(Additive_Efficiency ~ Backlog_Growth_lag2 + Fleet + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) #  ALL BUT BACKLOG GROWTH ARE SIG

print(summary(lm(Additive_Efficiency ~ Backlog_Growth_lag2 + Fleet + factor(Operating_Model),
                 data = efficiency_wbacklog)))


# --- BACKLOG FLEET RATIO ----
print(summary(lm(Additive_Efficiency ~ Backlog_Fleet_Ratio + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog)))

# NOTE: sig, but, correlation between vars?
print(summary(lm(Additive_Efficiency ~ Backlog_Fleet_Ratio + Fleet + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog)))

print(summary(lm(Additive_Efficiency ~ Backlog_Fleet_Ratio * factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog)))

print(summary(lm(Additive_Efficiency ~ Backlog_Fleet_Ratio + factor(Operating_Model)+factor(Period),
                 data = efficiency_wbacklog)))

# ---- BACKLOG ROLLING AVG ----
print(summary(lm(Additive_Efficiency ~ Backlog_rollavg2 *Fleet + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog)))

print(summary(lm(Additive_Efficiency ~ Backlog_rollavg2 + Fleet + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog)))

print(summary(lm(Additive_Efficiency ~ Backlog_rollavg2 *Fleet + factor(Operating_Model),
                 data = efficiency_wbacklog)))

# ---- STAGE 2 EFFICIENCIES: BACKLOG ----
print(summary(lm(Efficiency_stage2 ~ Backlog + Fleet + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog * Fleet + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog * factor(Operating_Model) + factor(Period) + Fleet,
                 data = efficiency_wbacklog)))

# ---- STAGE 2 EFFICIENCIES: BACKLOG growth ----
print(summary(lm(Efficiency_stage2 ~ Backlog_Growth *Fleet + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog_Growth *factor(Operating_Model) + Fleet + factor(Period),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog_Growth + factor(Operating_Model) + Fleet + factor(Period),
                 data = efficiency_wbacklog)))

# ---- STAGE 2 EFFICIENCIES: lagged backlog growth ----
print(summary(lm(Efficiency_stage2 ~ Backlog_Growth_lag2 *Fleet + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog_Growth_lag2 *Fleet + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog_Growth_lag2 *factor(Operating_Model) + Fleet + factor(Year),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog_Growth_lag2 + factor(Operating_Model) + Fleet + factor(Period),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog_Growth_lag2 + factor(Operating_Model) +Fleet + factor(Period),
                 data = efficiency_wbacklog)))


# ---- STAGE 2 EFFICIENCIES: rolling avg backlog ----
print(summary(lm(Efficiency_stage2 ~ Backlog_rollavg2 *Fleet + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog)))
# NOTE: significant
print(summary(lm(Efficiency_stage2 ~ Backlog_rollavg2 + Fleet + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog_rollavg2 *factor(Operating_Model) + Fleet + factor(Period),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog_rollavg2 + factor(Operating_Model) + Fleet + factor(Period),
                 data = efficiency_wbacklog)))


# ---- STAGE 2 EFFICIENCIES: backlog fleet ratio ----
print(summary(lm(Efficiency_stage2 ~ Backlog_Fleet_Ratio *factor(Operating_Model) + Fleet + factor(Period),
                 data = efficiency_wbacklog)))

# NOTE: significant, very fishy
print(summary(lm(Efficiency_stage2 ~ Backlog_Fleet_Ratio + factor(Operating_Model) + Fleet + factor(Period),
                 data = efficiency_wbacklog)))
