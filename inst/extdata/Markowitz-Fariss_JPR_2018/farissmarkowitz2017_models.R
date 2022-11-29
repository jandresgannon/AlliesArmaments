#############################
# Fariss and Markowitz (2017) JPR
# Power, Proximity, and Democracy: Geopolitical Competition in the International System
# Replication for figures and Tables
# 03/03/2017
############################

## Note: All data and preprocessing files necessary to produce the replication data file are available from the authors.

#################################
# Reading data and data management
#################################

rm(list = ls())

setwd("/Users/thereseanders/Dropbox/Geopolitical\ Competition\ Paper\ 2/Analysis/JPRGeopoliticalCompetition1/FarissMarkowitz2017_replication")

library(foreign)
library(plm)
library(ggplot2)
library(dplyr)
library(stargazer)
library(stringr)
library(tidyr)
library(MASS)
library(gridExtra)

###################################
# Reading data, creating indicators
###################################

data1 <- read.csv("fm2017_masterdata.csv", stringsAsFactors = F)
names(data1)

# Dropping extra years and landlocked countries, creating per 1,000,000 GDP measure and tonnage ratio
data1 <- data1 %>%
  filter(year >= 1800,
         landlocked == 0) %>%
  mutate(WorldBank_gdp_2010_estimate_mil = WorldBank_gdp_2010_estimate/1000000,
         ln_WorldBank_gdp_2010_estimate_mil = log(WorldBank_gdp_2010_estimate_mil),
         tonnageimputed_gdp = tonnageimputed/WorldBank_gdp_2010_estimate_mil,
         ln_tonnageimputed_gdp = log(tonnageimputed_gdp),
         ln_tonnageimputed = log(tonnageimputed),
         ln_tonnage = log(tonnage),
         tonnage_gdp = tonnage/WorldBank_gdp_2010_estimate_mil,
         ln_tonnage_gdp = log(tonnage_gdp)) #Note: 1 is added to all observations
  
# Creating one year lags
data2 <- data1 %>%
  mutate(year = year + 1)
names(data2)[!(names(data2) %in% c("ccode", "year"))] <- paste0(names(data2)[!(names(data2) %in% c("ccode", "year"))], "_l1")

master <- left_join(data1, data2, by = c("ccode", "year"))

#frame as TSCS data
master_tscs <- pdata.frame(master, index = c("ccode", "year"), drop.index = F, row.names = TRUE)


#############################################
# Overview plots in main text
#############################################

# Competition plot
mean_comp <- mean(master$comp_polity_logdist_logdist_all, na.rm = T)

ggplot(master, aes(x = year, y = comp_polity_logdist_logdist_all)) +
  geom_point(alpha = 0.2, size = 1, color = "darkgrey") +
  theme_bw() + 
  geom_hline(aes(yintercept = mean_comp),
             size = 0.8,
             alpha = 1,
             linetype = "dashed",
             color = "darkgrey") +
  geom_smooth(method = "lm", 
              color = "black",
              se = F, 
              formula = y ~ x,
              size = 0.8,
              alpha = .8) +
  labs(y = "Geopolitical competition",
       x = "Year") +
  theme(axis.text = element_text(size = 8)) +
  coord_cartesian(xlim = c(1800,2011)) + 
  annotate("text", x = 1970, y = 0.028, label = 'bold("Line of best fit")', color = "black", parse = T) +
  annotate("text", x = 1970, y = 0.026, label = 'bold("Mean")', color = "darkgrey", parse = T)

ggsave("plot_compatibility.jpg", width = 6, height = 4, dpi = 500)

# Naval tonnage over GDP
mean_totton <- mean((master$tonnage_gdp), na.rm = T)
options(scipen=10000)

ggplot(master, aes(x = year, y = (tonnage_gdp))) +
  geom_point(alpha = 0.2, size = 1, color = "darkgrey") +
  theme_bw() +
  geom_hline(aes(yintercept = mean_totton),
             size = 0.8,
             alpha = 1,
             linetype = "dashed",
             color = "darkgrey") +
  geom_smooth(method = "lm", 
              color = "black",
              se = F, 
              formula = y ~ x,
              size = 0.8,
              alpha = .8) +
  labs(y = "Naval ship tonnage/GDP (in millions)",
       x = "Year") +
  theme(axis.text = element_text(size = 8)) +
  coord_cartesian(xlim = c(1865,2011)) + 
  annotate("text", x = 1980, y = 12, label = 'bold("Line of best fit")', color = "black", parse = T) +
  annotate("text", x = 1980, y = 11, label = 'bold("Mean")', color = "darkgrey", parse = T)
ggsave("plot_tottongdp.jpg", width = 6, height = 4, dpi = 500)


################################
#
#
# Regression tables in main text
#
#
################################

## A) Negative binomial models
# Formula
f_predread_lngdp <- preDreadnoughts ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + preDreadnoughts_l1

f_dread_lngdp <- Dreadnoughts ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + Dreadnoughts_l1 

f_air_lngdp <- aircraft_carriers ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + aircraft_carriers_l1

f_battle_lngdp <- Battleship ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + Battleship_l1

f_aircs_lngdp <- AircraftCarrier ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + AircraftCarrier_l1

f_diesel_lngdp <- DieselSub ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + DieselSub_l1

f_nuke_lngdp <- NukeAttSub ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + NukeAttSub_l1

f_bal_lngdp <- BalSub ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + BalSub_l1


# Models
nb_predread_lngdp <- glm.nb(f_predread_lngdp, data = master)

nb_dread_lngdp <- glm.nb(f_dread_lngdp, data = master)

nb_air_lngdp <- glm.nb(f_air_lngdp, data = master)

nb_battle_lngdp <- glm.nb(f_battle_lngdp, data = master)

nb_aircs_lngdp <- glm.nb(f_aircs_lngdp, data = master)

nb_diesel_lngdp <- glm.nb(f_diesel_lngdp, data = master)

nb_nuke_lngdp <- glm.nb(f_nuke_lngdp, data = master)

nb_bal_lngdp <- glm.nb(f_bal_lngdp, data = master)

# Compiling the table
stargazer(nb_predread_lngdp, 
          nb_dread_lngdp,
          nb_air_lngdp,
          nb_battle_lngdp,
          nb_aircs_lngdp,
          nb_diesel_lngdp,
          nb_nuke_lngdp,
          nb_bal_lngdp,
          dep.var.labels = c("Pre-Dreadnoughts",
                             "Dreadnoughts",
                             "Aircraft carriers",
                             "Battleships",
                             "Aircraft carriers",
                             "Diesel submarines",
                             "Nuclear attack submarines",
                             "Ballistic submarines"),
          covariate.labels = c("$\\text{Geopolitical competition}_{i,t-1}$",
                               "$\\text{ln GPD}_{i,t-1}$",
                               "$\\text{Pre-Dreadnoughts}_{i,t-1}$",
                               "$\\text{Dreadnoughts}_{i,t-1}$",
                               "$\\text{Aircraft carriers}_{i,t-1}$",
                               "$\\text{Battleships}_{i,t-1}$",
                               "$\\text{Aircraft carriers}_{i,t-1}$",
                               "$\\text{Diesel submarines}_{i,t-1}$",
                               "$\\text{Nuclear attack submarines}_{i,t-1}$",
                               "$\\text{Ballistic submarines}_{i,t-1}$"),
          add.lines = list(c("Data", rep("MT", 3), rep("CS", 5))), 
          notes = "Estimates from two sources of data: Modelski and Thompson (MT) and Crisher and Souva (CS).",
          font.size = "tiny",
          digits = 2,
          omit.stat = c("f","ser","rsq", "aic", "theta"),
          column.sep.width = "-2pt",
          out = "tab_negbin_lngdp.tex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          title = "Negative binomial count models for different measures of capital ships, using geopolitical competition, the natural log of GDP, and a lagged dependent variable as controls. In all models, except on diesel submarines, higher levels of geopolitical competition are associated with statistically significantly higher counts of capital ships, all else equal. Figures plotting the predicted effects of geopolitical competition for low and high values of GDP can be found in the online appendix.")


## B) Panel models
# Formula
f_comp <- ln_tonnage_gdp ~ log(comp_polity_logdist_logdist_all_l1)
f_comp_dem <- ln_tonnage_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1
f_comp_dem_gdp <- ln_tonnage_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + gdp_sumgdp_w_mil_l1
f_comp_dem_lngdp <- ln_tonnage_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_gdp_sumgdp_w_mil_l1

f_comp_ldv <- ln_tonnage_gdp ~ log(comp_polity_logdist_logdist_all_l1) + ln_tonnage_gdp_l1
f_comp_dem_ldv <- ln_tonnage_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_tonnage_gdp_l1
f_comp_dem_gdp_ldv <- ln_tonnage_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + gdp_sumgdp_w_mil_l1 + ln_tonnage_gdp_l1
f_comp_dem_lngdp_ldv <- ln_tonnage_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_gdp_sumgdp_w_mil_l1 + ln_tonnage_gdp_l1

# Models
fe_comp <- plm(f_comp, data = master_tscs, model="within", effect = "individual")
fe_comp_dem <- plm(f_comp_dem, data = master_tscs, model="within", effect = "individual")
fe_comp_dem_gdp <- plm(f_comp_dem_gdp, data = master_tscs, model="within", effect = "individual")
fe_comp_dem_lngdp <- plm(f_comp_dem_lngdp, data = master_tscs, model="within", effect = "individual")

fe_comp_cy <- plm(f_comp, data = master_tscs, model="within", effect = "twoways")
fe_comp_dem_cy <- plm(f_comp_dem, data = master_tscs, model="within", effect = "twoways")
fe_comp_dem_gdp_cy <- plm(f_comp_dem_gdp, data = master_tscs, model="within", effect = "twoways")
fe_comp_dem_lngdp_cy <- plm(f_comp_dem_lngdp, data = master_tscs, model="within", effect = "twoways")

ols_comp_ldv <- lm(f_comp_ldv, data = master_tscs)
ols_comp_dem_ldv <- lm(f_comp_dem_ldv, data = master_tscs)
ols_comp_dem_gdp_ldv <- lm(f_comp_dem_gdp_ldv, data = master_tscs)
ols_comp_dem_lngdp_ldv <- lm(f_comp_dem_lngdp_ldv, data = master_tscs)

## Regression Table
stargazer(fe_comp,
          fe_comp_dem,
          fe_comp_dem_gdp,
          fe_comp_dem_lngdp,
          fe_comp_cy,
          fe_comp_dem_cy,
          fe_comp_dem_gdp_cy,
          fe_comp_dem_lngdp_cy,
          ols_comp_ldv,
          ols_comp_dem_ldv, 
          ols_comp_dem_gdp_ldv,
          ols_comp_dem_lngdp_ldv,
          font.size = "scriptsize",
          digits = 2,
          omit.stat = c("f","ser","rsq"),
          column.sep.width = "0pt",
          notes = "C denote country, and C-Y country-year fixed effects.",
          dep.var.labels = "$\\ln\\text{ Naval tonnage index}_{i,t}$",
          covariate.labels = c("$\\ln\\text{ Competition}_{i,t-1}$",
                               "$\\text{Democracy}_{i,t-1}$",
                               "$\\text{GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ Naval tonnage index}_{i,t-1}$"),
          add.lines = list(c("Fixed effects", rep("C", 4), rep("C-Y", 4), rep("None", 4))),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "tab_regress.tex",
          title = "Linear regression models (1865-2011). Geopolitical competition is associated with statistically significantly higher levels of investment in power projection capabilities across all models. The estimated size of the effect varies with the model that is used in the estimation. A ten percent increase in the level of geopolitical competition that a country faces is associated with on average a 1\\% (including a lagged dependent variable), 3.8\\% (for the model with country and year fixed effects), to 12.3\\% (using just country fixed effects) increase in the naval tonnage index for a model with the Polity 2 score and the GDP ratio as controls.")

#######################################
#
#
# Tables and figures in online appendix
#
#
#######################################

########################
# Summary Table
########################

vars <- c("ccode", 
          "year", 
          "comp_polity_logdist_logdist_all",
          "tonnage",
          "tonnageimputed",
          "tonnage_gdp",
          "tonnageimputed_gdp",
          "polity2",
          "WorldBank_gdp_2010_estimate_mil",
          "ln_WorldBank_gdp_2010_estimate_mil",
          "gdp_sumgdp_w_mil",
          "ln_gdp_sumgdp_w_mil",
          "island",
          "preDreadnoughts",
          "Dreadnoughts",
          "aircraft_carriers",
          "Battleship",
          "Battleshipimputed",
          "AircraftCarrier",
          "AircraftCarrierimputed",
          "DieselSub",
          "DieselSubimputed",
          "NukeAttSub",
          "NukeAttSubimputed",
          "BalSub",
          "BalSubimputed")

master_sub <- master %>%
  dplyr::select(one_of(vars[3:length(vars)]))

stargazer(master_sub,
          summary.stat = c("min", "median", "mean", "max", "sd", "n"),
          out = "tab_summary.tex",
          covariate.labels = c("Geopolitical competition",
                               "Naval tonnage",
                               "Naval tonnage (extended)",
                               "Naval tonnage index (tonnage/GDP)",
                               "Naval tonnage index (extended)",
                               "Democracy (Polity 2)",
                               "GDP in millions",
                               "$\\ln$ GDP in millions",
                               "GDP ratio",
                               "$\\ln$ GDP ratio",
                               "Island dummy",
                               "Pre-Dreadnoughts (MT)",
                               "Dreadnoughts (MT)",
                               "Aircraft carriers (MT)",
                               "Battleships (CS)",
                               "Battleships (CS, extended)",
                               "Aircraft carriers (CS)",
                               "Aircraft carriers (CS, extended)",
                               "Diesel submarines (CS)",
                               "Diesel submarines (CS, extended)",
                               "Nuclear attack submarines (CS)",
                               "Nuclear attack submarines (CS, extended)",
                               "Ballistic submarines (CS)",
                               "Ballistic submarines (CS, extended)"),
          title = "Summary statistics for key variables.",
          notes = c("\\textit{Notes}:",
                    "1 added to naval tonnage before logging.",
                    "GDP is measured in millions of constant USD.",
                    "Data on capital ships from Modelski and Thompson (MT) or Crisher and Souva (CS).",
                    "Extended series refer to indicators, for which the sample has been increased by coding country-years for which either",
                    "COW military expenditure or personnel data is available, but whose navel tonnage is not sufficient to be included",
                    "in the data set by Crisher and Souva (2014)."),
          font.size = "scriptsize",
          digits = 3)


#######################################################
# Substantive effect plots for negative binomial models
#######################################################
master_sub_mt <- master %>%
  dplyr::select(ccode,
                year,
                comp_polity_logdist_logdist_all_l1,
                ln_WorldBank_gdp_2010_estimate_mil_l1,
                preDreadnoughts_l1,
                Dreadnoughts_l1,
                aircraft_carriers_l1) %>%
  na.omit()

master_sub_cs <- master %>%
  dplyr::select(ccode,
                year,
                comp_polity_logdist_logdist_all_l1,
                ln_WorldBank_gdp_2010_estimate_mil_l1,
                Battleship_l1,
                AircraftCarrier_l1) %>%
  na.omit()

master_sub_cs2 <- master %>%
  dplyr::select(ccode,
                year,
                comp_polity_logdist_logdist_all_l1,
                ln_WorldBank_gdp_2010_estimate_mil_l1,
                DieselSub_l1,
                NukeAttSub_l1,
                BalSub_l1) %>%
  na.omit()

# Creating scenarios
q1_lngdp <- summary(master_sub_mt$ln_WorldBank_gdp_2010_estimate_mil_l1)[2]
q3_lngdp <- summary(master_sub_mt$ln_WorldBank_gdp_2010_estimate_mil_l1)[5]
min_comp <- summary(master_sub_mt$comp_polity_logdist_logdist_all_l1)[1]
max_comp <- summary(master_sub_mt$comp_polity_logdist_logdist_all_l1)[6]


######## Computing predictions #########
# Predread
newdata_predread_lngdp <- data.frame(comp_polity_logdist_logdist_all_l1 = rep(seq(from = min_comp, to = max_comp, length.out = 100), 2),
                                     ln_WorldBank_gdp_2010_estimate_mil_l1 = c(rep(q1_lngdp, 100),rep(q3_lngdp, 100)),
                                     preDreadnoughts_l1 = mean(master_sub_mt$preDreadnoughts_l1))
newdata_predread_lngdp <- cbind(newdata_predread_lngdp, predict(nb_predread_lngdp, newdata_predread_lngdp, type = "response", se.fit = TRUE))
newdata_predread_lngdp <- newdata_predread_lngdp %>%
  mutate(low = fit - 1.96*se.fit,
         high = fit + 1.96*se.fit,
         type = "Pre-Dreadnoughts (MT)")

# Dread
newdata_dread_lngdp <- data.frame(comp_polity_logdist_logdist_all_l1 = rep(seq(from = min_comp, to = max_comp, length.out = 100), 2),
                                  ln_WorldBank_gdp_2010_estimate_mil_l1 = c(rep(q1_lngdp, 100),rep(q3_lngdp, 100)),
                                  Dreadnoughts_l1 = mean(master_sub_mt$Dreadnoughts_l1))
newdata_dread_lngdp <- cbind(newdata_dread_lngdp, predict(nb_dread_lngdp, newdata_dread_lngdp, type = "response", se.fit = TRUE))
newdata_dread_lngdp <- newdata_dread_lngdp %>%
  mutate(low = fit - 1.96*se.fit,
         high = fit + 1.96*se.fit,
         type = "Dreadnoughts (MT)")

# Aircraft Carries
newdata_air_lngdp <- data.frame(comp_polity_logdist_logdist_all_l1 = rep(seq(from = min_comp, to = max_comp, length.out = 100), 2),
                                ln_WorldBank_gdp_2010_estimate_mil_l1 = c(rep(q1_lngdp, 100),rep(q3_lngdp, 100)),
                                aircraft_carriers_l1 = mean(master_sub_mt$aircraft_carriers_l1))
newdata_air_lngdp <- cbind(newdata_air_lngdp, predict(nb_air_lngdp, newdata_air_lngdp, type = "response", se.fit = TRUE))
newdata_air_lngdp <- newdata_air_lngdp %>%
  mutate(low = fit - 1.96*se.fit,
         high = fit + 1.96*se.fit,
         type = "Aircraft carriers (MT)") 

# Battleship  
newdata_battle_lngdp <- data.frame(comp_polity_logdist_logdist_all_l1 = rep(seq(from = min_comp, to = max_comp, length.out = 100), 2),
                                   ln_WorldBank_gdp_2010_estimate_mil_l1 = c(rep(q1_lngdp, 100),rep(q3_lngdp, 100)),
                                   Battleship_l1 = mean(master_sub_cs$Battleship_l1))
newdata_battle_lngdp <- cbind(newdata_battle_lngdp, predict(nb_battle_lngdp, newdata_battle_lngdp, type = "response", se.fit = TRUE))
newdata_battle_lngdp <- newdata_battle_lngdp %>%
  mutate(low = fit - 1.96*se.fit,
         high = fit + 1.96*se.fit,
         type = "Battleships (CS)") 

# Aircraft Carriers 
newdata_aircs_lngdp <- data.frame(comp_polity_logdist_logdist_all_l1 = rep(seq(from = min_comp, to = max_comp, length.out = 100), 2),
                                  ln_WorldBank_gdp_2010_estimate_mil_l1 = c(rep(q1_lngdp, 100),rep(q3_lngdp, 100)),
                                  AircraftCarrier_l1 = mean(master_sub_cs$AircraftCarrier_l1))
newdata_aircs_lngdp <- cbind(newdata_aircs_lngdp, predict(nb_aircs_lngdp, newdata_aircs_lngdp, type = "response", se.fit = TRUE))
newdata_aircs_lngdp <- newdata_aircs_lngdp %>%
  mutate(low = fit - 1.96*se.fit,
         high = fit + 1.96*se.fit,
         type = "Aircraft carriers (CS)") 

# Diesel Sub 
newdata_diesel_lngdp <- data.frame(comp_polity_logdist_logdist_all_l1 = rep(seq(from = min_comp, to = max_comp, length.out = 100), 2),
                                   ln_WorldBank_gdp_2010_estimate_mil_l1 = c(rep(q1_lngdp, 100),rep(q3_lngdp, 100)),
                                   DieselSub_l1 = mean(master_sub_cs2$DieselSub_l1))
newdata_diesel_lngdp <- cbind(newdata_diesel_lngdp, predict(nb_diesel_lngdp, newdata_diesel_lngdp, type = "response", se.fit = TRUE))
newdata_diesel_lngdp <- newdata_diesel_lngdp %>%
  mutate(low = fit - 1.96*se.fit,
         high = fit + 1.96*se.fit,
         type = "Diesel submarines (CS)") 

# Nuke Sub 
newdata_nuke_lngdp <- data.frame(comp_polity_logdist_logdist_all_l1 = rep(seq(from = min_comp, to = max_comp, length.out = 100), 2),
                                 ln_WorldBank_gdp_2010_estimate_mil_l1 = c(rep(q1_lngdp, 100),rep(q3_lngdp, 100)),
                                 NukeAttSub_l1 = mean(master_sub_cs2$NukeAttSub_l1))
newdata_nuke_lngdp <- cbind(newdata_nuke_lngdp, predict(nb_nuke_lngdp, newdata_nuke_lngdp, type = "response", se.fit = TRUE))
newdata_nuke_lngdp <- newdata_nuke_lngdp %>%
  mutate(low = fit - 1.96*se.fit,
         high = fit + 1.96*se.fit,
         type = "Nuclear attack submarines (CS)") 

# Ballistic Submarine
newdata_bal_lngdp <- data.frame(comp_polity_logdist_logdist_all_l1 = rep(seq(from = min_comp, to = max_comp, length.out = 100), 2),
                                ln_WorldBank_gdp_2010_estimate_mil_l1 = c(rep(q1_lngdp, 100),rep(q3_lngdp, 100)),
                                BalSub_l1 = mean(master_sub_cs2$BalSub_l1))
newdata_bal_lngdp <- cbind(newdata_bal_lngdp, predict(nb_bal_lngdp, newdata_bal_lngdp, type = "response", se.fit = TRUE))
newdata_bal_lngdp <- newdata_bal_lngdp %>%
  mutate(low = fit - 1.96*se.fit,
         high = fit + 1.96*se.fit,
         type = "Ballistic submarines (CS)") 


## Binding the two data frames together
newdata_lngdp <- bind_rows(newdata_predread_lngdp, 
                           newdata_dread_lngdp, 
                           newdata_air_lngdp,
                           newdata_battle_lngdp,
                           newdata_aircs_lngdp,
                           newdata_diesel_lngdp,
                           newdata_nuke_lngdp,
                           newdata_bal_lngdp)

table(newdata_est$type)

newdata_lngdp$type <- factor(newdata_lngdp$type,
                             levels = c("Pre-Dreadnoughts (MT)",
                                        "Dreadnoughts (MT)",
                                        "Aircraft carriers (MT)",
                                        "Battleships (CS)",
                                        "Aircraft carriers (CS)",
                                        "Diesel submarines (CS)",
                                        "Nuclear attack submarines (CS)",
                                        "Ballistic submarines (CS)"))


# Plotting it
ggplot(newdata_lngdp, aes(x = comp_polity_logdist_logdist_all_l1, y = fit)) +
  geom_ribbon(aes(ymin = low, ymax = high, fill = factor(ln_WorldBank_gdp_2010_estimate_mil_l1)), alpha = .25) +
  geom_line(aes(colour = factor(ln_WorldBank_gdp_2010_estimate_mil_l1),
                lty = factor(ln_WorldBank_gdp_2010_estimate_mil_l1)), 
            size = .8) +
  scale_color_grey(name = "ln GDP",labels = c("Low", "High"), end = 0.5) +
  scale_fill_grey(name = "ln GDP", labels = c("Low", "High"), end = 0.5) +
  scale_linetype_discrete(name = "ln GDP", labels = c("Low", "High")) +
  coord_cartesian(xlim = c(0,0.04)) +
  theme_minimal() +
  labs(y = "Predicted number of capital ships",
       x = "Geopolitical competition") +
  theme(strip.background = element_rect(fill = "grey", color = "grey"),
        legend.position = "top") +
  facet_wrap(~ type, ncol = 2, scales = "free_y")


ggsave("nb_predicted_lngdp.jpg", width = 7.5, height = 9, dpi = 500)




#######################################################
# Regression tables
#######################################################

# A) Negative binomial models
## i) Using an extended series for Crisher and Souva (2014) counts of capital ships.

# Formula
f_battle_lngdp_extended <- Battleshipimputed ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + Battleshipimputed_l1

f_aircs_lngdp_extended <- AircraftCarrierimputed ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + AircraftCarrierimputed_l1

f_diesel_lngdp_extended <- DieselSubimputed ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + DieselSubimputed_l1

f_nuke_lngdp_extended <- NukeAttSubimputed ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + NukeAttSubimputed_l1

f_bal_lngdp_extended <- BalSubimputed ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + BalSubimputed_l1


# Models
nb_battle_lngdp_extended <- glm.nb(f_battle_lngdp_extended, data = master)

nb_aircs_lngdp_extended <- glm.nb(f_aircs_lngdp_extended, data = master)

nb_diesel_lngdp_extended <- glm.nb(f_diesel_lngdp_extended, data = master)

nb_nuke_lngdp_extended <- glm.nb(f_nuke_lngdp_extended, data = master)

nb_bal_lngdp_extended <- glm.nb(f_bal_lngdp_extended, data = master)

# Compiling the table
stargazer(nb_battle_lngdp_extended,
          nb_aircs_lngdp_extended,
          nb_diesel_lngdp_extended,
          nb_nuke_lngdp_extended,
          nb_bal_lngdp_extended,
          dep.var.labels = c("Battleships",
                             "Aircraft carriers",
                             "Diesel submarines",
                             "Nuclear attack submarines",
                             "Ballistic submarines"),
          covariate.labels = c("$\\text{Geopolitical competition}_{i,t-1}$",
                               "$\\ln\\text{ GPD}_{i,t-1}$",
                               "$\\text{Battleships}_{i,t-1}$",
                               "$\\text{Aircraft carriers}_{i,t-1}$",
                               "$\\text{Diesel submarines}_{i,t-1}$",
                               "$\\text{Nuclear attack submarines}_{i,t-1}$",
                               "$\\text{Ballistic submarines}_{i,t-1}$"),
          #add.lines = list(c("Data", rep("CS", 5))), 
          notes = "Data on capital ship counts is from Crisher and Souva (2014).",
          font.size = "tiny",
          digits = 2,
          omit.stat = c("f","ser","rsq", "aic", "theta"),
          column.sep.width = "-2pt",
          out = "tab_negbin_lngdp_extended.tex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          title = "Negative binomial count models for different measures of capital ships based on the data by Crisher and Souva (2014), using geopolitical competition, the natural log of GDP, and a lagged dependent variable as controls. The series of the counts of capital ships has been extended in comparison to Table I in the main text by coding country-years as zero for which either Correlates of War (COW) National Material Capabilities (version 5.0) military expenditure or personnel data is available, but whose navel tonnage is not sufficient to be included in the data set by Crisher and Souva (2014). In all models, except on aircraft carries, higher levels of geopolitical competition are associated with statistically significantly higher counts of capital ships, all else equal. The results are robust to the extension of the data by Crisher and Souva (2014), though the estimated effect sizes vary.")


## ii) Including an island dummy
# Formula
f_predread_lngdp_island <- preDreadnoughts ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + island + preDreadnoughts_l1

f_dread_lngdp_island <- Dreadnoughts ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + island + Dreadnoughts_l1 

f_air_lngdp_island <- aircraft_carriers ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + island + aircraft_carriers_l1

f_battle_lngdp_island <- Battleship ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + island + Battleship_l1

f_aircs_lngdp_island <- AircraftCarrier ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + island + AircraftCarrier_l1

f_diesel_lngdp_island <- DieselSub ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + island + DieselSub_l1

f_nuke_lngdp_island <- NukeAttSub ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + island + NukeAttSub_l1

f_bal_lngdp_island <- BalSub ~ comp_polity_logdist_logdist_all_l1 + ln_WorldBank_gdp_2010_estimate_mil_l1 + island + BalSub_l1


# Models
nb_predread_lngdp_island <- glm.nb(f_predread_lngdp_island, data = master)

nb_dread_lngdp_island <- glm.nb(f_dread_lngdp_island, data = master)

nb_air_lngdp_island <- glm.nb(f_air_lngdp_island, data = master)

nb_battle_lngdp_island <- glm.nb(f_battle_lngdp_island, data = master)

nb_aircs_lngdp_island <- glm.nb(f_aircs_lngdp_island, data = master)

nb_diesel_lngdp_island <- glm.nb(f_diesel_lngdp_island, data = master)

nb_nuke_lngdp_island <- glm.nb(f_nuke_lngdp_island, data = master)

nb_bal_lngdp_island <- glm.nb(f_bal_lngdp_island, data = master)

stargazer(nb_predread_lngdp_island, 
          nb_dread_lngdp_island,
          nb_air_lngdp_island,
          nb_battle_lngdp_island,
          nb_aircs_lngdp_island,
          nb_diesel_lngdp_island,
          nb_nuke_lngdp_island,
          nb_bal_lngdp_island,
          dep.var.labels = c("Pre-Dreadnoughts",
                             "Dreadnoughts",
                             "Aircraft carriers",
                             "Battleships",
                             "Aircraft carriers",
                             "Diesel submarines",
                             "Nuclear attack submarines",
                             "Ballistic submarines"),
          covariate.labels = c("$\\text{Geopolitical competition}_{i,t-1}$",
                               "$\\ln\\text{ GDP}_{i,t-1}$",
                               "$\\text{Island Dummy}$",
                               "$\\text{Pre-Dreadnoughts}_{i,t-1}$",
                               "$\\text{Dreadnoughts}_{i,t-1}$",
                               "$\\text{Aircraft carriers}_{i,t-1}$",
                               "$\\text{Battleships}_{i,t-1}$",
                               "$\\text{Aircraft carriers}_{i,t-1}$",
                               "$\\text{Diesel submarines}_{i,t-1}$",
                               "$\\text{Nuclear attack submarines}_{i,t-1}$",
                               "$\\text{Ballistic submarines}_{i,t-1}$"),
          add.lines = list(c("Data", rep("MT", 3), rep("CS", 5))), 
          notes = "Estimates from two sources of data: Modelski and Thompson (MT) and Crisher and Souva (CS).",
          font.size = "tiny",
          digits = 2,
          omit.stat = c("f","ser","rsq", "aic", "theta"),
          column.sep.width = "0pt",
          out = "tab_negbin_lngdp_island.tex",
          star.cutoffs = c(0.05, 0.01, 0.001),
          title = "Negative binomial count models for different types of capital ships, using the natural log of GDP and a lagged dependent variable as controls, and accounting for island nations. For most categories of ships, island nations acquire more capital ships than non-island nations. The main results on geopolitical competition are robust to the inclusion of the island nation dummy.")



# B) Panel data models
## i) Extended tonnage series

# Formula
f_comp_extended <- ln_tonnageimputed_gdp ~ log(comp_polity_logdist_logdist_all_l1)
f_comp_dem_extended <- ln_tonnageimputed_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1
f_comp_dem_gdp_extended <- ln_tonnageimputed_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + gdp_sumgdp_w_mil_l1
f_comp_dem_lngdp_extended <- ln_tonnageimputed_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_gdp_sumgdp_w_mil_l1

f_comp_ldv_extended <- ln_tonnageimputed_gdp ~ log(comp_polity_logdist_logdist_all_l1) + ln_tonnageimputed_gdp_l1
f_comp_dem_ldv_extended <- ln_tonnageimputed_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_tonnageimputed_gdp_l1
f_comp_dem_gdp_ldv_extended <- ln_tonnageimputed_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + gdp_sumgdp_w_mil_l1 + ln_tonnageimputed_gdp_l1
f_comp_dem_lngdp_ldv_extended <- ln_tonnageimputed_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_gdp_sumgdp_w_mil_l1 + ln_tonnageimputed_gdp_l1

# Models
fe_comp_extended <- plm(f_comp_extended, data = master_tscs, model="within", effect = "individual")
fe_comp_dem_extended <- plm(f_comp_dem_extended, data = master_tscs, model="within", effect = "individual")
fe_comp_dem_gdp_extended <- plm(f_comp_dem_gdp_extended, data = master_tscs, model="within", effect = "individual")
fe_comp_dem_lngdp_extended <- plm(f_comp_dem_lngdp_extended, data = master_tscs, model="within", effect = "individual")

fe_comp_cy_extended <- plm(f_comp_extended, data = master_tscs, model="within", effect = "twoways")
fe_comp_dem_cy_extended <- plm(f_comp_dem_extended, data = master_tscs, model="within", effect = "twoways")
fe_comp_dem_gdp_cy_extended <- plm(f_comp_dem_gdp_extended, data = master_tscs, model="within", effect = "twoways")
fe_comp_dem_lngdp_cy_extended <- plm(f_comp_dem_lngdp_extended, data = master_tscs, model="within", effect = "twoways")

ols_comp_ldv_extended <- lm(f_comp_ldv_extended, data = master_tscs)
ols_comp_dem_ldv_extended <- lm(f_comp_dem_ldv_extended, data = master_tscs)
ols_comp_dem_gdp_ldv_extended <- lm(f_comp_dem_gdp_ldv_extended, data = master_tscs)
ols_comp_dem_lngdp_ldv_extended <- lm(f_comp_dem_lngdp_ldv_extended, data = master_tscs)

## Regression Table
stargazer(fe_comp_extended,
          fe_comp_dem_extended,
          fe_comp_dem_gdp_extended,
          fe_comp_dem_lngdp_extended,
          fe_comp_cy_extended,
          fe_comp_dem_cy_extended,
          fe_comp_dem_gdp_cy_extended,
          fe_comp_dem_lngdp_cy_extended,
          ols_comp_ldv_extended,
          ols_comp_dem_ldv_extended, 
          ols_comp_dem_gdp_ldv_extended,
          ols_comp_dem_lngdp_ldv_extended,
          font.size = "scriptsize",
          digits = 2,
          omit.stat = c("f","ser","rsq"),
          column.sep.width = "0pt",
          notes = "C denote country, and C-Y country-year fixed effects.",
          dep.var.labels = "$\\ln\\text{ Naval tonnage index}_{i,t}$",
          covariate.labels = c("$\\ln\\text{ Geopolitical competition}_{i,t-1}$",
                               "$\\text{Democracy}_{i,t-1}$",
                               "$\\text{GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ Naval tonnage index}_{i,t-1}$"),
          add.lines = list(c("Fixed effects", rep("C", 4), rep("C-Y", 4), rep("None", 4))),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "tab_regress_extended.tex",
          title = "Linear Regression Models (1865-2011). The series of naval tonnage has been extended in comparison to Table II in the main text by coding country-years as 0 for which either Correlates of War (COW) National Material Capabilities (version 5.0) military expenditure or personnel data is available, but whose navel tonnage is not sufficient to be included in the data set by Crisher and Souva (2014). 1 is added to naval tonnage before dividing by GDP and logging. The estimated effect of geopolitical competition differs for the country fixed effects specification, as compared to the results from the more restricted sample in the main text. Geopolitical competition is associated with lower levels of naval tonnage in the models using only country fixed effects. The results for the country-year fixed effects and lagged dependent variable specifications are robust to the usage of the extended sample.")


## ii) Using alternative DVs
### a) base sample
# Formula
f_comp_dem_gdp_unlogged <- tonnage_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + gdp_sumgdp_w_mil_l1
f_comp_dem_lngdp_unlogged <- tonnage_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_gdp_sumgdp_w_mil_l1

f_comp_dem_gdp_ldv_unlogged <- tonnage_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + gdp_sumgdp_w_mil_l1 + tonnage_gdp_l1
f_comp_dem_lngdp_ldv_unlogged <- tonnage_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_gdp_sumgdp_w_mil_l1 + tonnage_gdp_l1

f_comp_dem_gdp_justtonnage <- ln_tonnage ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + gdp_sumgdp_w_mil_l1
f_comp_dem_lngdp_justtonnage <- ln_tonnage ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_gdp_sumgdp_w_mil_l1

f_comp_dem_gdp_ldv_justtonnage <- ln_tonnage ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + gdp_sumgdp_w_mil_l1 + ln_tonnage_l1
f_comp_dem_lngdp_ldv_justtonnage <- ln_tonnage ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_gdp_sumgdp_w_mil_l1 + ln_tonnage_l1

#### Running the models ####
fe_comp_dem_gdp_unlogged <- plm(f_comp_dem_gdp_unlogged, data = master_tscs, model="within", effect = "individual")
fe_comp_dem_lngdp_unlogged <- plm(f_comp_dem_lngdp_unlogged, data = master_tscs, model="within", effect = "individual")

fe_comp_dem_gdp_cy_unlogged <- plm(f_comp_dem_gdp_unlogged, data = master_tscs, model="within", effect = "twoways")
fe_comp_dem_lngdp_cy_unlogged <- plm(f_comp_dem_lngdp_unlogged, data = master_tscs, model="within", effect = "twoways")

ols_comp_dem_gdp_ldv_unlogged <- lm(f_comp_dem_gdp_ldv_unlogged, data = master_tscs)
ols_comp_dem_lngdp_ldv_unlogged <- lm(f_comp_dem_lngdp_ldv_unlogged, data = master_tscs)

fe_comp_dem_gdp_justtonnage <- plm(f_comp_dem_gdp_justtonnage, data = master_tscs, model="within", effect = "individual")
fe_comp_dem_lngdp_justtonnage <- plm(f_comp_dem_lngdp_justtonnage, data = master_tscs, model="within", effect = "individual")

fe_comp_dem_gdp_cy_justtonnage <- plm(f_comp_dem_gdp_justtonnage, data = master_tscs, model="within", effect = "twoways")
fe_comp_dem_lngdp_cy_justtonnage <- plm(f_comp_dem_lngdp_justtonnage, data = master_tscs, model="within", effect = "twoways")

ols_comp_dem_gdp_ldv_justtonnage <- lm(f_comp_dem_gdp_ldv_justtonnage, data = master_tscs)
ols_comp_dem_lngdp_ldv_justtonnage <- lm(f_comp_dem_lngdp_ldv_justtonnage, data = master_tscs)

## Regression Table
stargazer(fe_comp_dem_gdp_unlogged,
          fe_comp_dem_lngdp_unlogged,
          fe_comp_dem_gdp_cy_unlogged,
          fe_comp_dem_lngdp_cy_unlogged,
          ols_comp_dem_gdp_ldv_unlogged,
          ols_comp_dem_lngdp_ldv_unlogged,
          fe_comp_dem_gdp_justtonnage,
          fe_comp_dem_lngdp_justtonnage,
          fe_comp_dem_gdp_cy_justtonnage,
          fe_comp_dem_lngdp_cy_justtonnage,
          ols_comp_dem_gdp_ldv_justtonnage,
          ols_comp_dem_lngdp_ldv_justtonnage,
          font.size = "scriptsize",
          digits = 2,
          omit.stat = c("f","ser","rsq"),
          column.sep.width = "0pt",
          notes = c("C denote country, and C-Y country-year fixed effects.", "Naval tonnage index measured as naval tonnage per GDP in millions. 1 added to tonnage before logging."),
          dep.var.labels = c("$\\text{ Naval tonnage index}_{i,t}$", 
                             "$\\ln\\text{Tonnage}_{i,t}$"),
          covariate.labels = c("$\\ln\\text{ Geopolitical competition}_{i,t-1}$",
                               "$\\text{Democracy}_{i,t-1}$",
                               "$\\text{GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ GDP ratio}_{i,t-1}$",
                               "$\\text{ Naval tonnage lndex}_{i,t-1}$",
                               "$\\ln\\text{ Tonnage}_{i,t-1}$"),
          add.lines = list(c("Fixed effects", rep("C", 2), rep("C-Y", 2), rep("None", 2), rep("C", 2), rep("C-Y", 2), rep("None", 2))),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "tab_regress_unlogged_justtonnage.tex",
          title = "Linear Regression Models (1865-2011) using the un-logged naval tonnage index and the natural log of naval tonnage as alternative dependent variables. For most models, geopolitical competition is associated with statistically significantly higher levels of investment in power projection capabilities across all models. Some of the competition coefficients are negative for the FE model because the absolute value of tonnage is generally increasing year to year even as competition decreases. This why we prefer the transformed tonnage variable for the main analysis.")

### b) extended sample
# Formula
f_comp_dem_gdp_unlogged_extended <- tonnageimputed_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + gdp_sumgdp_w_mil_l1
f_comp_dem_lngdp_unlogged_extended <- tonnageimputed_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_gdp_sumgdp_w_mil_l1

f_comp_dem_gdp_ldv_unlogged_extended <- tonnageimputed_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + gdp_sumgdp_w_mil_l1 + tonnageimputed_gdp_l1
f_comp_dem_lngdp_ldv_unlogged_extended <- tonnageimputed_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_gdp_sumgdp_w_mil_l1 + tonnageimputed_gdp_l1

f_comp_dem_gdp_justtonnage_extended <- ln_tonnageimputed ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + gdp_sumgdp_w_mil_l1
f_comp_dem_lngdp_justtonnage_extended <- ln_tonnageimputed ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_gdp_sumgdp_w_mil_l1

f_comp_dem_gdp_ldv_justtonnage_extended <- ln_tonnageimputed ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + gdp_sumgdp_w_mil_l1 + ln_tonnageimputed_l1
f_comp_dem_lngdp_ldv_justtonnage_extended <- ln_tonnageimputed ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_gdp_sumgdp_w_mil_l1 + ln_tonnageimputed_l1

#### Running the models ####
fe_comp_dem_gdp_unlogged_extended <- plm(f_comp_dem_gdp_unlogged_extended, data = master_tscs, model="within", effect = "individual")
fe_comp_dem_lngdp_unlogged_extended <- plm(f_comp_dem_lngdp_unlogged_extended, data = master_tscs, model="within", effect = "individual")

fe_comp_dem_gdp_cy_unlogged_extended <- plm(f_comp_dem_gdp_unlogged_extended, data = master_tscs, model="within", effect = "twoways")
fe_comp_dem_lngdp_cy_unlogged_extended <- plm(f_comp_dem_lngdp_unlogged_extended, data = master_tscs, model="within", effect = "twoways")

ols_comp_dem_gdp_ldv_unlogged_extended <- lm(f_comp_dem_gdp_ldv_unlogged_extended, data = master_tscs)
ols_comp_dem_lngdp_ldv_unlogged_extended <- lm(f_comp_dem_lngdp_ldv_unlogged_extended, data = master_tscs)

fe_comp_dem_gdp_justtonnage_extended <- plm(f_comp_dem_gdp_justtonnage_extended, data = master_tscs, model="within", effect = "individual")
fe_comp_dem_lngdp_justtonnage_extended <- plm(f_comp_dem_lngdp_justtonnage_extended, data = master_tscs, model="within", effect = "individual")

fe_comp_dem_gdp_cy_justtonnage_extended <- plm(f_comp_dem_gdp_justtonnage_extended, data = master_tscs, model="within", effect = "twoways")
fe_comp_dem_lngdp_cy_justtonnage_extended <- plm(f_comp_dem_lngdp_justtonnage_extended, data = master_tscs, model="within", effect = "twoways")

ols_comp_dem_gdp_ldv_justtonnage_extended <- lm(f_comp_dem_gdp_ldv_justtonnage_extended, data = master_tscs)
ols_comp_dem_lngdp_ldv_justtonnage_extended <- lm(f_comp_dem_lngdp_ldv_justtonnage_extended, data = master_tscs)

## Regression Table
stargazer(fe_comp_dem_gdp_unlogged_extended,
          fe_comp_dem_lngdp_unlogged_extended,
          fe_comp_dem_gdp_cy_unlogged_extended,
          fe_comp_dem_lngdp_cy_unlogged_extended,
          ols_comp_dem_gdp_ldv_unlogged_extended,
          ols_comp_dem_lngdp_ldv_unlogged_extended,
          fe_comp_dem_gdp_justtonnage_extended,
          fe_comp_dem_lngdp_justtonnage_extended,
          fe_comp_dem_gdp_cy_justtonnage_extended,
          fe_comp_dem_lngdp_cy_justtonnage_extended,
          ols_comp_dem_gdp_ldv_justtonnage_extended,
          ols_comp_dem_lngdp_ldv_justtonnage_extended,
          font.size = "scriptsize",
          digits = 2,
          omit.stat = c("f","ser","rsq"),
          column.sep.width = "0pt",
          notes = c("C denote country, and C-Y country-year fixed effects.", "Naval tonnage index measured as naval tonnage per GDP in millions. 1 added to tonnage before logging."),
          dep.var.labels = c("$\\text{ Naval tonnage index}_{i,t}$", 
                             "$\\ln\\text{Tonnage}_{i,t}$"),
          covariate.labels = c("$\\ln\\text{ Geopolitical competition}_{i,t-1}$",
                               "$\\text{Democracy}_{i,t-1}$",
                               "$\\text{GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ GDP ratio}_{i,t-1}$",
                               "$\\text{ Naval tonnage lndex}_{i,t-1}$",
                               "$\\ln\\text{ Tonnage}_{i,t-1}$"),
          add.lines = list(c("Fixed effects", rep("C", 2), rep("C-Y", 2), rep("None", 2), rep("C", 2), rep("C-Y", 2), rep("None", 2))),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "tab_regress_unlogged_justtonnage_extended.tex",
          title = "Linear Regression Models (1865-2011) using the un-logged naval tonnage index and the natural log of naval tonnage as alternative dependent variables, based on the extended sample.")





## iii) Dropping the US from the data
# Subsetting data 
master_nous <- filter(master, ccode != 2)
master_nous_tscs <- pdata.frame(master_nous, index = c("ccode", "year"), drop.index = F, row.names = TRUE)

### a) base sample
# Running Models
fe_comp_nous <- plm(f_comp, data = master_nous_tscs, model="within", effect = "individual")
fe_comp_dem_nous <- plm(f_comp_dem, data = master_nous_tscs, model="within", effect = "individual")
fe_comp_dem_gdp_nous <- plm(f_comp_dem_gdp, data = master_nous_tscs, model="within", effect = "individual")
fe_comp_dem_lngdp_nous <- plm(f_comp_dem_lngdp, data = master_nous_tscs, model="within", effect = "individual")

fe_comp_cy_nous <- plm(f_comp, data = master_nous_tscs, model="within", effect = "twoways")
fe_comp_dem_cy_nous <- plm(f_comp_dem, data = master_nous_tscs, model="within", effect = "twoways")
fe_comp_dem_gdp_cy_nous <- plm(f_comp_dem_gdp, data = master_nous_tscs, model="within", effect = "twoways")
fe_comp_dem_lngdp_cy_nous <- plm(f_comp_dem_lngdp, data = master_nous_tscs, model="within", effect = "twoways")

ols_comp_ldv_nous <- lm(f_comp_ldv, data = master_nous_tscs)
ols_comp_dem_ldv_nous <- lm(f_comp_dem_ldv, data = master_nous_tscs)
ols_comp_dem_gdp_ldv_nous <- lm(f_comp_dem_gdp_ldv, data = master_nous_tscs)
ols_comp_dem_lngdp_ldv_nous <- lm(f_comp_dem_lngdp_ldv, data = master_nous_tscs)

## Regression Table
stargazer(fe_comp_nous,
          fe_comp_dem_nous,
          fe_comp_dem_gdp_nous,
          fe_comp_dem_lngdp_nous,
          fe_comp_cy_nous,
          fe_comp_dem_cy_nous,
          fe_comp_dem_gdp_cy_nous,
          fe_comp_dem_lngdp_cy_nous,
          ols_comp_ldv_nous,
          ols_comp_dem_ldv_nous, 
          ols_comp_dem_gdp_ldv_nous,
          ols_comp_dem_lngdp_ldv_nous,
          font.size = "scriptsize",
          digits = 2,
          omit.stat = c("f","ser","rsq"),
          column.sep.width = "0pt",
          #no.space = T,
          notes = "C denote country, and C-Y country-year fixed effects. The United States are excluded from the data.",
          title = "Linear Regression Models (1865-2011): Excluding the United States from the data. The results show that the effect of geopolitical competition on power projection is robust the the exclusion of the United States. The effect sizes are similar to the specification with the United States. A ten percent increase in geopolitical competition is associated with on average 1\\% (lagged dependent variable), 3.9\\% (country and year fixed effects), to 12.7\\% higher (using just country fixed effects) investment in naval capabilities, based on a model with the Polity 2 score and the natural log of relative GDP as controls.",
          dep.var.labels = "$\\ln\\text{ Naval tonnage index}_{i,t}$",
          covariate.labels = c("$\\ln\\text{ Geopolitical competition}_{i,t-1}$",
                               "$\\text{Democracy}_{i,t-1}$",
                               "$\\text{GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ Naval tonnage index}_{i,t-1}$"),
          add.lines = list(c("Fixed effects", rep("C", 4), rep("C-Y", 4), rep("None", 4))),
          out = "tab_regress_nous.tex",
          star.cutoffs = c(0.05, 0.01, 0.001))



### b) extended sample
# Running Models
fe_comp_nous_extended <- plm(f_comp_extended, data = master_nous_tscs, model="within", effect = "individual")
fe_comp_dem_nous_extended <- plm(f_comp_dem_extended, data = master_nous_tscs, model="within", effect = "individual")
fe_comp_dem_gdp_nous_extended <- plm(f_comp_dem_gdp_extended, data = master_nous_tscs, model="within", effect = "individual")
fe_comp_dem_lngdp_nous_extended <- plm(f_comp_dem_lngdp_extended, data = master_nous_tscs, model="within", effect = "individual")

fe_comp_cy_nous_extended <- plm(f_comp_extended, data = master_nous_tscs, model="within", effect = "twoways")
fe_comp_dem_cy_nous_extended <- plm(f_comp_dem_extended, data = master_nous_tscs, model="within", effect = "twoways")
fe_comp_dem_gdp_cy_nous_extended <- plm(f_comp_dem_gdp_extended, data = master_nous_tscs, model="within", effect = "twoways")
fe_comp_dem_lngdp_cy_nous_extended <- plm(f_comp_dem_lngdp_extended, data = master_nous_tscs, model="within", effect = "twoways")

ols_comp_ldv_nous_extended <- lm(f_comp_ldv_extended, data = master_nous_tscs)
ols_comp_dem_ldv_nous_extended <- lm(f_comp_dem_ldv_extended, data = master_nous_tscs)
ols_comp_dem_gdp_ldv_nous_extended <- lm(f_comp_dem_gdp_ldv_extended, data = master_nous_tscs)
ols_comp_dem_lngdp_ldv_nous_extended <- lm(f_comp_dem_lngdp_ldv_extended, data = master_nous_tscs)

## Regression Table
stargazer(fe_comp_nous_extended,
          fe_comp_dem_nous_extended,
          fe_comp_dem_gdp_nous_extended,
          fe_comp_dem_lngdp_nous_extended,
          fe_comp_cy_nous_extended,
          fe_comp_dem_cy_nous_extended,
          fe_comp_dem_gdp_cy_nous_extended,
          fe_comp_dem_lngdp_cy_nous_extended,
          ols_comp_ldv_nous_extended,
          ols_comp_dem_ldv_nous_extended, 
          ols_comp_dem_gdp_ldv_nous_extended,
          ols_comp_dem_lngdp_ldv_nous_extended,
          font.size = "scriptsize",
          digits = 2,
          omit.stat = c("f","ser","rsq"),
          column.sep.width = "0pt",
          #no.space = T,
          notes = "C denote country, and C-Y country-year fixed effects. The United States are excluded from the data.",
          title = "Linear Regression Models (1865-2011): Excluding the United States from the data, based on an extended sample.",
          dep.var.labels = "$\\ln\\text{ Naval tonnage index}_{i,t}$",
          covariate.labels = c("$\\ln\\text{ Geopolitical competition}_{i,t-1}$",
                               "$\\text{Democracy}_{i,t-1}$",
                               "$\\text{GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ Naval tonnage index}_{i,t-1}$"),
          add.lines = list(c("Fixed effects", rep("C", 4), rep("C-Y", 4), rep("None", 4))),
          out = "tab_regress_nous_extended.tex",
          star.cutoffs = c(0.05, 0.01, 0.001))


## iv) Running the models for after 1945
# Subsetting the data
master_after1945 <- master %>%
  filter(year > 1945)
master_tscs_after1945 <- pdata.frame(master_after1945, index = c("ccode", "year"), drop.index = F, row.names = TRUE)

master_before1945 <- master %>%
  filter(year <= 1945)
master_tscs_before1945 <- pdata.frame(master_before1945, index = c("ccode", "year"), drop.index = F, row.names = TRUE)

### a) base sample
# Models
fe_comp_dem_gdp_after1945 <- plm(f_comp_dem_gdp, data = master_tscs_after1945, model="within", effect = "individual")
fe_comp_dem_lngdp_after1945 <- plm(f_comp_dem_lngdp, data = master_tscs_after1945, model="within", effect = "individual")

fe_comp_dem_gdp_cy_after1945 <- plm(f_comp_dem_gdp, data = master_tscs_after1945, model="within", effect = "twoways")
fe_comp_dem_lngdp_cy_after1945 <- plm(f_comp_dem_lngdp, data = master_tscs_after1945, model="within", effect = "twoways")

ols_comp_dem_gdp_ldv_after1945 <- lm(f_comp_dem_gdp_ldv, data = master_tscs_after1945)
ols_comp_dem_lngdp_ldv_after1945 <- lm(f_comp_dem_lngdp_ldv, data = master_tscs_after1945)

fe_comp_dem_gdp_before1945 <- plm(f_comp_dem_gdp, data = master_tscs_before1945, model="within", effect = "individual")
fe_comp_dem_lngdp_before1945 <- plm(f_comp_dem_lngdp, data = master_tscs_before1945, model="within", effect = "individual")

fe_comp_dem_gdp_cy_before1945 <- plm(f_comp_dem_gdp, data = master_tscs_before1945, model="within", effect = "twoways")
fe_comp_dem_lngdp_cy_before1945 <- plm(f_comp_dem_lngdp, data = master_tscs_before1945, model="within", effect = "twoways")

ols_comp_dem_gdp_ldv_before1945 <- lm(f_comp_dem_gdp_ldv, data = master_tscs_before1945)
ols_comp_dem_lngdp_ldv_before1945 <- lm(f_comp_dem_lngdp_ldv, data = master_tscs_before1945)

## Regression Table
stargazer(fe_comp_dem_gdp_before1945,
          fe_comp_dem_lngdp_before1945,
          fe_comp_dem_gdp_cy_before1945,
          fe_comp_dem_lngdp_cy_before1945,
          ols_comp_dem_gdp_ldv_before1945,
          ols_comp_dem_lngdp_ldv_before1945,
          fe_comp_dem_gdp_after1945,
          fe_comp_dem_lngdp_after1945,
          fe_comp_dem_gdp_cy_after1945,
          fe_comp_dem_lngdp_cy_after1945,
          ols_comp_dem_gdp_ldv_after1945,
          ols_comp_dem_lngdp_ldv_after1945,
          font.size = "scriptsize",
          digits = 2,
          omit.stat = c("f","ser","rsq"),
          column.sep.width = "0pt",
          notes = "C denote country, and C-Y country-year fixed effects. Models (1) through (6): 1865-1945. Models (7) through (12) : 1946-2011.",
          dep.var.labels = "$\\ln\\text{ Naval tonnage index}_{i,t}$",
          covariate.labels = c("$\\ln\\text{ Geopolitical competition}_{i,t-1}$",
                               "$\\text{Democracy}_{i,t-1}$",
                               "$\\text{GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ Naval tonnage index}_{i,t-1}$"),
          add.lines = list(c("Fixed effects", rep("C", 2), rep("C-Y", 2), rep("None", 2), rep("C", 2), rep("C-Y", 2), rep("None", 2))),
          star.cutoffs = c(0.05, 0.01, 0.001),
          model.names = F,
          column.labels = c("1865-1945", "1946-2011"),
          column.separate = c(6, 6),
          out = "tab_regress_beforeafter1945.tex",
          title = "Linear Regression Models 1865-1945 and 1946-2011. These models show that the results hold even during a period in which the US did not serve as a hegemon in the international system prior to 1946. The effects of geopolitical competition are in the expected direction. However, in the data for the years until 1945, some coefficients do not reach statistical significance at minimum 5\\%, as compared to the full sample. One reason might be the lower number of observations in this sample excluding the years after 1945.")


### b) extended sample
# Models
fe_comp_dem_gdp_after1945_extended <- plm(f_comp_dem_gdp_extended, data = master_tscs_after1945, model="within", effect = "individual")
fe_comp_dem_lngdp_after1945_extended <- plm(f_comp_dem_lngdp_extended, data = master_tscs_after1945, model="within", effect = "individual")

fe_comp_dem_gdp_cy_after1945_extended <- plm(f_comp_dem_gdp_extended, data = master_tscs_after1945, model="within", effect = "twoways")
fe_comp_dem_lngdp_cy_after1945_extended <- plm(f_comp_dem_lngdp_extended, data = master_tscs_after1945, model="within", effect = "twoways")

ols_comp_dem_gdp_ldv_after1945_extended <- lm(f_comp_dem_gdp_ldv_extended, data = master_tscs_after1945)
ols_comp_dem_lngdp_ldv_after1945_extended <- lm(f_comp_dem_lngdp_ldv_extended, data = master_tscs_after1945)

fe_comp_dem_gdp_before1945_extended <- plm(f_comp_dem_gdp_extended, data = master_tscs_before1945, model="within", effect = "individual")
fe_comp_dem_lngdp_before1945_extended <- plm(f_comp_dem_lngdp_extended, data = master_tscs_before1945, model="within", effect = "individual")

fe_comp_dem_gdp_cy_before1945_extended <- plm(f_comp_dem_gdp_extended, data = master_tscs_before1945, model="within", effect = "twoways")
fe_comp_dem_lngdp_cy_before1945_extended <- plm(f_comp_dem_lngdp_extended, data = master_tscs_before1945, model="within", effect = "twoways")

ols_comp_dem_gdp_ldv_before1945_extended <- lm(f_comp_dem_gdp_ldv_extended, data = master_tscs_before1945)
ols_comp_dem_lngdp_ldv_before1945_extended <- lm(f_comp_dem_lngdp_ldv_extended, data = master_tscs_before1945)

## Regression Table
stargazer(fe_comp_dem_gdp_before1945_extended,
          fe_comp_dem_lngdp_before1945_extended,
          fe_comp_dem_gdp_cy_before1945_extended,
          fe_comp_dem_lngdp_cy_before1945_extended,
          ols_comp_dem_gdp_ldv_before1945_extended,
          ols_comp_dem_lngdp_ldv_before1945_extended,
          fe_comp_dem_gdp_after1945_extended,
          fe_comp_dem_lngdp_after1945_extended,
          fe_comp_dem_gdp_cy_after1945_extended,
          fe_comp_dem_lngdp_cy_after1945_extended,
          ols_comp_dem_gdp_ldv_after1945_extended,
          ols_comp_dem_lngdp_ldv_after1945_extended,
          font.size = "scriptsize",
          digits = 2,
          omit.stat = c("f","ser","rsq"),
          column.sep.width = "0pt",
          notes = "C denote country, and C-Y country-year fixed effects. Models (1) through (6): 1865-1945. Models (7) through (12) : 1946-2011.",
          dep.var.labels = "$\\ln\\text{ Naval tonnage index}_{i,t}$",
          covariate.labels = c("$\\ln\\text{ Geopolitical competition}_{i,t-1}$",
                               "$\\text{Democracy}_{i,t-1}$",
                               "$\\text{GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ Naval tonnage index}_{i,t-1}$"),
          add.lines = list(c("Fixed effects", rep("C", 2), rep("C-Y", 2), rep("None", 2), rep("C", 2), rep("C-Y", 2), rep("None", 2))),
          star.cutoffs = c(0.05, 0.01, 0.001),
          model.names = F,
          column.labels = c("1865-1945", "1946-2011"),
          column.separate = c(6, 6),
          out = "tab_regress_beforeafter1945_extended.tex",
          title = "Linear Regression Models 1865-1945 and 1946-2011, based on an extended sample.")




## v) Adding islands indicator

### a) base sample
# Formula
f_comp_ldv_island <- ln_tonnage_gdp ~ log(comp_polity_logdist_logdist_all_l1) + ln_tonnage_gdp_l1 + island 
f_comp_dem_ldv_island <- ln_tonnage_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_tonnage_gdp_l1 + island
f_comp_dem_gdp_ldv_island <- ln_tonnage_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + gdp_sumgdp_w_mil_l1 + ln_tonnage_gdp_l1 + island
f_comp_dem_lngdp_ldv_island <- ln_tonnage_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_gdp_sumgdp_w_mil_l1 + ln_tonnage_gdp_l1 + island

# Models
ols_comp_ldv_island <- lm(f_comp_ldv_island, data = master)
ols_comp_dem_ldv_island <- lm(f_comp_dem_ldv_island, data = master)
ols_comp_dem_gdp_ldv_island <- lm(f_comp_dem_gdp_ldv_island, data = master)
ols_comp_dem_lngdp_ldv_island <- lm(f_comp_dem_lngdp_ldv_island, data = master)


stargazer(ols_comp_ldv_island,
          ols_comp_dem_ldv_island,
          ols_comp_dem_gdp_ldv_island,
          ols_comp_dem_lngdp_ldv_island,
          font.size = "scriptsize",
          digits = 2,
          omit.stat = c("f","ser","rsq"),
          column.sep.width = "0pt",
          dep.var.labels = "$\\ln\\text{ Naval tonnage index}_{i,t}$",
          covariate.labels = c("$\\ln\\text{ Geopolitical competition}_{i,t-1}$",
                               "$\\text{Democracy}_{i,t-1}$",
                               "$\\text{GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ Naval tonnage index}_{i,t-1}$",
                               "Island nation dummy"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "tab_regress_island.tex",          
          title = "Linear Regression Models (1865-2011), controlling for island nations. The results are robust to the inclusion of a dummy variable indicating whether the country is an island or not. Only the lagged dependent variable specification is estimated here, because an island dummy would be omitted upon estimating the model with country and year fixed effects. Island nations do not have statistically significantly different levels of investment in power projection capabilities than non-island countries. All other effects are robust the inclusion of the island dummy.")

### b) extended sample
# Formula
f_comp_ldv_island_extended <- ln_tonnageimputed_gdp ~ log(comp_polity_logdist_logdist_all_l1) + ln_tonnageimputed_gdp_l1 + island 
f_comp_dem_ldv_island_extended <- ln_tonnageimputed_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_tonnageimputed_gdp_l1 + island
f_comp_dem_gdp_ldv_island_extended <- ln_tonnageimputed_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + gdp_sumgdp_w_mil_l1 + ln_tonnageimputed_gdp_l1 + island
f_comp_dem_lngdp_ldv_island_extended <- ln_tonnageimputed_gdp ~ log(comp_polity_logdist_logdist_all_l1) + polity2_l1 + ln_gdp_sumgdp_w_mil_l1 + ln_tonnageimputed_gdp_l1 + island

# Models
ols_comp_ldv_island_extended <- lm(f_comp_ldv_island_extended, data = master)
ols_comp_dem_ldv_island_extended <- lm(f_comp_dem_ldv_island_extended, data = master)
ols_comp_dem_gdp_ldv_island_extended <- lm(f_comp_dem_gdp_ldv_island_extended, data = master)
ols_comp_dem_lngdp_ldv_island_extended <- lm(f_comp_dem_lngdp_ldv_island_extended, data = master)


stargazer(ols_comp_ldv_island_extended,
          ols_comp_dem_ldv_island_extended,
          ols_comp_dem_gdp_ldv_island_extended,
          ols_comp_dem_lngdp_ldv_island_extended,
          font.size = "scriptsize",
          digits = 2,
          omit.stat = c("f","ser","rsq"),
          column.sep.width = "0pt",
          dep.var.labels = "$\\ln\\text{ Naval tonnage index}_{i,t}$",
          covariate.labels = c("$\\ln\\text{ Geopolitical competition}_{i,t-1}$",
                               "$\\text{Democracy}_{i,t-1}$",
                               "$\\text{GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ GDP ratio}_{i,t-1}$",
                               "$\\ln\\text{ Naval tonnage index}_{i,t-1}$",
                               "Island nation dummy"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "tab_regress_island_extended.tex",          
          title = "Linear Regression Models (1865-2011), controlling for island nations, based on an extended sample.")



#############################################
## Substantive effect (difference in means)
#############################################
master_substantiveeffects <- master %>%
  dplyr::select(ccode, year,
                ln_tonnage_gdp,
                comp_polity_logdist_logdist_all_l1,
                ln_WorldBank_gdp_2010_estimate_mil_l1) %>%
  na.omit()

sd(master$comp_polity_logdist_logdist_all_l1, na.rm = T) #0.005701262
sd(master_substantiveeffects$comp_polity_logdist_logdist_all_l1) #0.005135865

lm_simple <- lm(ln_tonnage_gdp ~ log(comp_polity_logdist_logdist_all_l1) + ln_WorldBank_gdp_2010_estimate_mil_l1, data = master_substantiveeffects)
summary(lm_simple)$coefficients[2,1] #Coef is 0.997008
summary(lm_simple)$coefficients[2,1] + 1.96*summary(lm_simple)$coefficients[2,2] #CI upper 1.054961
summary(lm_simple)$coefficients[2,1] - 1.96*summary(lm_simple)$coefficients[2,2] #CI lower 0.9390547

dat_simple <- data.frame(comp_polity_logdist_logdist_all_l1 = c(mean(master_substantiveeffects$comp_polity_logdist_logdist_all_l1),
                                                                mean(master_substantiveeffects$comp_polity_logdist_logdist_all_l1) + sd(master_substantiveeffects$comp_polity_logdist_logdist_all_l1)),
                         ln_WorldBank_gdp_2010_estimate_mil_l1 = mean(master_substantiveeffects$ln_WorldBank_gdp_2010_estimate_mil_l1[master_substantiveeffects$year == 1990]))
dat_simple<- cbind(dat_simple, predict(lm_simple, dat_simple,  se.fit = T))
change <- exp(dat_simple$fit[2])- exp(dat_simple$fit[1])
# 0.3094317 tons per 1 million GDP
meangdp <- exp(mean(master_substantiveeffects$ln_WorldBank_gdp_2010_estimate_mil_l1[master_substantiveeffects$year == 1990])) #146089.5
change*meangdp #57415.87

