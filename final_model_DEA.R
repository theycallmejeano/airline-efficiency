# NOTE: run this first, so that deaR runs on MacOS
options(rgl.useNULL = TRUE)
options(scipen = 999)

library(rgl)
library(deaR)
library(tidyverse)
library(broom)
library(clipr)

source("utilities.R")

# ---- STEP 1: two-stage DEA ------
stage1_input<-c("Expenses","Fleet size")
stage1_output<-c("ASK")
stage2_input<-c("ASK", "Number of employees")
stage2_output<-c("Revenue", "Load factor")

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
                           by="DMU", suffix=c("_stage1", "_stage2"))

# NOTE: fig 4.2 plot
overall_efficiency<- stage1_model %>% 
  left_join(stage2_model[,c("DMU", "Efficiency")],
                           by="DMU", suffix=c("_stage1", "_stage2"))

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
  scale_fill_manual(values = c("Stage 1" = "#FFB6C1",  
                               "Stage 2" = "#FFD9A0")) +
  labs(x = "", y = "DEA scores", title = "Stage 1 vs Stage 2 Efficiency by Year") +
  theme_classic(base_size=16)
  labs(x = "", y = "DEA scores", title = "Stage 1 vs Stage 2 Efficiency by Year") +
  theme_classic()

# now adding in weights
weights_df<- calculate_weights(normalize_data(merged_input)) %>% select(Airline, Year, w1, w2)

# first add in the backlog vars
efficiency_wbacklog<- add_backlog_metrics(overall_efficiency %>%select(-DMU)) %>%
  left_join(weights_df, by = c("Airline", "Year")) %>%
  mutate(Additive_Efficiency=(w1*Efficiency_stage1) + (w2*Efficiency_stage2))
# ensure baseline is pre-pandemic
efficiency_wbacklog$Period <- relevel(factor(efficiency_wbacklog$Period, ordered = FALSE), ref = "Pre-pandemic")

# get rankings
rank_scores(efficiency_wbacklog, score_col="Additive_Efficiency")

# NOTE: fig 4.3
efficiency_wbacklog %>%
  select(Year, Additive_Efficiency, Operating_Model) %>%
  ggplot(aes(x = factor(Year), y = Additive_Efficiency, fill=str_replace_all(Operating_Model, "_", " "))) +
  geom_boxplot() +
  labs(x = "Year", y = "DEA scores", 
       title = "Overall Efficiency by Operating Model",
       fill = "Operating model") +
  theme_classic(base_size=16)

# export efficiency scores
write_csv((efficiency_wbacklog %>% 
             select(Airline, Year, Efficiency_stage1, Efficiency_stage2, Additive_Efficiency)),
          "data/efficiency_scores.csv")

# ----- STEP 2: regression
# ----- OVERALL EFFICIENCY ---- 
# ---- BACKLOG ----
# fleet
# NOTE: table 4.3
print(summary(lm(Additive_Efficiency ~ Backlog + Fleet + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog)))

print(summary(lm(Additive_Efficiency ~ Backlog * factor(Operating_Model) + factor(Period) + Fleet,
                 data = efficiency_wbacklog))) # NOTE: OM interaction significant

# without fleet
print(summary(lm(Additive_Efficiency ~ Backlog + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog))) # no

# NOTE: table 4.4
print(summary(lm(Additive_Efficiency ~ Backlog * factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # NOTE: Backlog measure significant

# ---- BACKLOG GROWTH: no backlog growth significance ----
# no fleet
print(summary(lm(Additive_Efficiency ~ Backlog_Growth *factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # no

print(summary(lm(Additive_Efficiency ~ Backlog_Growth + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # no

# fleet
print(summary(lm(Additive_Efficiency ~ Backlog_Growth *factor(Operating_Model) + factor(Period) + Fleet,
                 data = efficiency_wbacklog))) # no

print(summary(lm(Additive_Efficiency ~ Backlog_Growth + factor(Operating_Model) + factor(Period) + Fleet,
                 data = efficiency_wbacklog))) # no

# --- LAGGED BACKLOG GROWTH ----
# no fleet
print(summary(lm(Additive_Efficiency ~ Backlog_Growth_lag2 * factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # no

print(summary(lm(Additive_Efficiency ~ Backlog_Growth_lag2 + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) #  no

# fleet
print(summary(lm(Additive_Efficiency ~ Backlog_Growth_lag2 + Fleet + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # no

print(summary(lm(Additive_Efficiency ~ Backlog_Growth_lag2 * factor(Operating_Model) + factor(Period) + Fleet,
                 data = efficiency_wbacklog))) # no

# --- BACKLOG FLEET RATIO ----
print(summary(lm(Additive_Efficiency ~ Backlog_Fleet_Ratio + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # no

print(summary(lm(Additive_Efficiency ~ Backlog_Fleet_Ratio * factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # no
# ---- BACKLOG ROLLING AVG ----
# without fleet
# NOTE: table 4.5
print(summary(lm(Additive_Efficiency ~ Backlog_rollavg2 * factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # NOTE: backlog term significant

tidy(summary(lm(Additive_Efficiency ~ Backlog_rollavg2 * factor(Operating_Model) + factor(Period),
                data = efficiency_wbacklog))) %>%
  pivot_longer(cols=c("estimate", "std.error", "p.value")) %>%
  filter(name!="std.error") %>%
  write_clip()

print(summary(lm(Additive_Efficiency ~ Backlog_rollavg2 + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # no

# with fleet
print(summary(lm(Additive_Efficiency ~ Backlog_rollavg2 + Fleet + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # no

# note: table 4.5
print(summary(lm(Additive_Efficiency ~ Backlog_rollavg2 * factor(Operating_Model) + Fleet + factor(Period),
                 data = efficiency_wbacklog))) # signficant

tidy(summary(lm(Additive_Efficiency ~ Backlog_rollavg2 * factor(Operating_Model) +Fleet +factor(Period),
                data = efficiency_wbacklog))) %>%
  pivot_longer(cols=c("estimate", "std.error", "p.value")) %>%
  filter(name!="std.error") %>%
  write_clip()
 
# ---- STAGE 2 EFFICIENCIES: BACKLOG ----
# with fleet
print(summary(lm(Efficiency_stage2 ~ Backlog + Fleet + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog))) # no
# NOTE table 4.7
print(summary(lm(Efficiency_stage2 ~ Backlog * factor(Operating_Model) + factor(Period) + Fleet,
                 data = efficiency_wbacklog)))# NOTE: significant

tidy(summary(lm(Efficiency_stage2 ~ Backlog * factor(Operating_Model) +factor(Period),
                data = efficiency_wbacklog)))


# without fleet
print(summary(lm(Efficiency_stage2 ~ Backlog + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog))) # no

# NOTE: table 4.9
print(summary(lm(Efficiency_stage2 ~ Backlog * factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # NOTE: significant backlog and interaction

tidy(summary(lm(Efficiency_stage2 ~ Backlog * factor(Operating_Model) +Fleet +factor(Period),
                data = efficiency_wbacklog)))
 

# ---- STAGE 2 EFFICIENCIES: BACKLOG growth ----
# with fleet
print(summary(lm(Efficiency_stage2 ~ Backlog_Growth + Fleet + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog))) # no

print(summary(lm(Efficiency_stage2 ~ Backlog_Growth *factor(Operating_Model) + Fleet + factor(Period),
                 data = efficiency_wbacklog))) # no

# without fleet
print(summary(lm(Efficiency_stage2 ~ Backlog_Growth + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # no

print(summary(lm(Efficiency_stage2 ~ Backlog_Growth *factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # no



# ---- STAGE 2 EFFICIENCIES: rolling avg backlog ----
# with fleet
print(summary(lm(Efficiency_stage2 ~ Backlog_rollavg2 + Fleet + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog))) # not significant
# NOTE: table 4.9
print(summary(lm(Efficiency_stage2 ~ Backlog_rollavg2 *factor(Operating_Model) + Fleet + factor(Period),
                 data = efficiency_wbacklog))) # NOTE: backlog and interaction significant

tidy(summary(lm(Efficiency_stage2 ~ Backlog_rollavg2 * factor(Operating_Model) +factor(Period) + Fleet,
                data = efficiency_wbacklog)))

# without fleet
print(summary(lm(Efficiency_stage2 ~ Backlog_rollavg2 + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # no

# NOTE: table 4.9
print(summary(lm(Efficiency_stage2 ~ Backlog_rollavg2 * factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # NOTE: significant

tidy(summary(lm(Efficiency_stage2 ~ Backlog_rollavg2 * factor(Operating_Model) +factor(Period),
                data = efficiency_wbacklog)))

# ---- STAGE 2 EFFICIENCIES: backlog fleet ratio ----
print(summary(lm(Efficiency_stage2 ~ Backlog_Fleet_Ratio *factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog)))

print(summary(lm(Efficiency_stage2 ~ Backlog_Fleet_Ratio + factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog)))



# --- STAGE 1 EFFICIENCIES: no fleet, BACKLOG ------
# without fleet
print(summary(lm(Efficiency_stage1 ~ Backlog + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog))) # no

# table 4.6
print(summary(lm(Efficiency_stage1 ~ Backlog * factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # NOTE: significant, but weak model

tidy(summary(lm(Efficiency_stage1 ~ Backlog * factor(Operating_Model) + factor(Period),
                data = efficiency_wbacklog))) %>%
  pivot_longer(cols=c("estimate", "std.error", "p.value")) %>%
  filter(name!="std.error") %>%
  write_clip()

# --- STAGE 1 EFFICIENCIES: no fleet, BACKLOG GROWTH ------
# without fleet
print(summary(lm(Efficiency_stage1 ~ Backlog_Growth + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog))) # no

print(summary(lm(Efficiency_stage1 ~ Backlog_Growth * factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # no


# --- STAGE 1 EFFICIENCIES: no fleet, rolling avg backlog ------
# without fleet
print(summary(lm(Efficiency_stage1 ~ Backlog_rollavg2 + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog))) # no
# table 4.6
print(summary(lm(Efficiency_stage1 ~ Backlog_rollavg2 * factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # signifcant

tidy(summary(lm(Efficiency_stage1 ~ Backlog_rollavg2 * factor(Operating_Model) + factor(Period),
                data = efficiency_wbacklog))) %>%
  pivot_longer(cols=c("estimate", "std.error", "p.value")) %>%
  filter(name!="std.error") %>%
  write_clip()

# --- STAGE 1 EFFICIENCIES: no fleet, BACKLOG FLEET RATIO ------
# without fleet
print(summary(lm(Efficiency_stage1 ~ Backlog_Fleet_Ratio + factor(Period) + factor(Operating_Model),
                 data = efficiency_wbacklog))) # no

print(summary(lm(Efficiency_stage1 ~ Backlog_Fleet_Ratio * factor(Operating_Model) + factor(Period),
                 data = efficiency_wbacklog))) # no


# ------ Summary of findings -----
# robust significance when:
# 1. Overall efficiency: 
#      Backlog:factor(Operating_Model)Point_to_Point, with fleet
#      Backlog *and* Backlog:factor(Operating_Model)Point_to_Point, without fleet
#      Backlog_rollavg2 *and* Backlog_rollavg2:factor(Operating_Model)Point_to_Point; without fleet

# 2. Stage 2 efficiency:
#     Backlog:factor(Operating_Model)Point_to_Point, with fleet
#     Backlog *and* Backlog:factor(Operating_Model)Point_to_Point, without fleet
#     Backlog_rollavg2:factor(Operating_Model)Point_to_Point, with fleet
#     Backlog_rollavg2 with fleet
#     Backlog_rollavg2 *and* Backlog_rollavg2:factor(Operating_Model)Point_to_Point, without fleet
