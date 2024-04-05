# Load packages
library(readxl)
library(tidyverse)
library(glmmTMB)

### IMPORT DATA

df <- read_excel("./data/appendix_studydata.xlsx", sheet='Study data')
population <- read_excel("./data/appendix_studydata.xlsx", sheet='Population estimates')
  

### PREPARE DATASETS

df <- df %>%
  mutate(region = case_when(unique_id == 618 ~ "Eastern Africa", TRUE~region), # assign study to Eastern Africa 
         region = factor(region, levels = c("Southern Africa", "Eastern Africa", "Central and Western Africa"))) %>%  
  mutate(sex = factor(sex, levels=c("Female","Male","Male and female")),
         age_group = factor(age_group, levels = c("≥25 years","<25 years")),
         cat1 = fct_collapse(cat1, n = c("u","n")), cat2 = fct_collapse(cat2, n = c("u","n")),
         cat3 = fct_collapse(cat3, n = c("u","n")), cat4 = fct_collapse(cat4, n = c("u","n")),
         cat5 = fct_collapse(cat5, n = c("u","n")), cat6 = fct_collapse(cat6, n = c("u","n")),
         cat7 = fct_collapse(cat7, n = c("u","n")), cat8 = fct_collapse(cat8, n = c("u","n")),
         cat9 = fct_collapse(cat9, n = c("u","n")), cat10 = fct_collapse(cat10, n = c("u","n")),
         cat1 = fct_relevel(cat1, "y"), cat2 = fct_relevel(cat2, "y"),
         cat3 = fct_relevel(cat3, "y"), cat4 = fct_relevel(cat4, "y"),
         cat5 = fct_relevel(cat5, "y"), cat6 = fct_relevel(cat6, "y"),
         cat7 = fct_relevel(cat7, "y"), cat8 = fct_relevel(cat8, "y"),
         cat9 = fct_relevel(cat9, "y"), cat10 = fct_relevel(cat10, "y"),
         year_regression = year - 2015)

population <- population %>%
  select(region, sex, population) %>%
  mutate(region = fct_collapse(region, "Central and Western Africa" = c("Western Africa", "Middle Africa"))) %>%
  group_by(region, sex) %>%
  summarise(population = sum(population))

## 1. Overall analysis

df_vd_overall <- df %>% filter(symptom=="VD", analysis=="Overall") %>% mutate(sid = row_number())
df_ud_overall <- df %>% filter(symptom=="UD", analysis=="Overall") %>% mutate(sid = row_number())
df_gu_overall <- df %>% filter(symptom=="GU", analysis=="Overall") %>% mutate(sid = row_number())

# Observations based on NAAT testing
df_vd_overall_naat <- df %>% filter(symptom=="VD", analysis=="Overall", test_category=="NAAT", !rti=="CA") %>% mutate(sid = row_number()) # Leave out 2 CA data points
df_ud_overall_naat <- df %>% filter(symptom=="UD", analysis=="Overall", test_category=="NAAT") %>% mutate(sid = row_number())
df_gu_overall_naat <- df %>% filter(symptom=="GU", analysis=="Overall", test_category=="NAAT") %>% mutate(sid = row_number())

## 2. HIV-stratified analysis

df_vd_hiv <- df %>% filter(symptom=="VD", analysis=="HIV-stratified") %>% mutate(sid = row_number())
df_ud_hiv <- df %>% filter(symptom=="UD", analysis=="HIV-stratified") %>% mutate(sid = row_number())
df_gu_hiv <- df %>% filter(symptom=="GU", analysis=="HIV-stratified") %>% mutate(sid = row_number())

## 3. Age-stratified analysis

df_vd_age <- df %>% filter(symptom=="VD", analysis=="Age-stratified") %>% mutate(sid = row_number())
df_ud_age <- df %>% filter(symptom=="UD", analysis=="Age-stratified") %>% mutate(sid = row_number())
df_gu_age <- df %>% filter(symptom=="GU", analysis=="Age-stratified") %>% mutate(sid = row_number())


### RUN MODELS

## 1. Trend estimates 

# 1.1 Using observations adjusted for diagnostic test performance

form1_overall_adj <- cbind(adj_num,(adj_denom-adj_num)) ~ -1 + rti + rti:year_regression + (year_regression|region:rti) + (1 | sid)
form2_overall_adj <- cbind(adj_num,(adj_denom-adj_num)) ~ -1 + rti + rti:year_regression + rti:sex + (year_regression|region:rti) + (1 | sid)

mod_vd_overall_adj <- glmmTMB(form1_overall_adj, data = df_vd_overall, family = binomial(link="logit"),
                              control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

mod_ud_overall_adj <- glmmTMB(form1_overall_adj, data = df_ud_overall, family = binomial(link="logit"),
                              control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

mod_gu_overall_adj <- glmmTMB(form2_overall_adj, data = df_gu_overall, family = binomial(link="logit"),
                              control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

# 1.2 Using observations as reported (no diagnostic test adjustment)

form1_overall_unadj <- cbind(num,(denom-num)) ~ -1 + rti + rti:year_regression + (year_regression|region:rti) + (1 | sid)
form2_overall_unadj <- cbind(num,(denom-num)) ~ -1 + rti + rti:year_regression + rti:sex + (year_regression|region:rti) + (1 | sid)

mod_vd_overall_unadj <- glmmTMB(form1_overall_unadj, data = df_vd_overall, family = binomial(link="logit"),
                                control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

mod_ud_overall_unadj <- glmmTMB(form1_overall_unadj, data = df_ud_overall, family = binomial(link="logit"),
                                control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

mod_gu_overall_unadj <- glmmTMB(form2_overall_unadj, data = df_gu_overall, family = binomial(link="logit"),
                                control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

# 1.3 Using observations with NAAT testing adjusted for diagnostic test performance

mod_vd_naat_adj <- glmmTMB(form1_overall_adj, data = df_vd_overall_naat, family = binomial(link="logit"),
                              control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

mod_ud_naat_adj <- glmmTMB(form1_overall_adj, data = df_ud_overall_naat, family = binomial(link="logit"))
                          

mod_gu_naat_adj <- glmmTMB(form2_overall_adj, data = df_gu_overall_naat, family = binomial(link="logit"))

# 1.4 Using observations with NAAT testing as reported

mod_vd_naat_unadj <- glmmTMB(form1_overall_unadj, data = df_vd_overall_naat, family = binomial(link="logit"),
                           control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

mod_ud_naat_unadj <- glmmTMB(form1_overall_unadj, data = df_ud_overall_naat, family = binomial(link="logit"),
                           control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

mod_gu_naat_unadj <- glmmTMB(form2_overall_unadj, data = df_gu_overall_naat, family = binomial(link="logit"),
                           control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

## 1.5 Analysis of critical appraisal criteria, using observations adjusted for diagnostic test performance

form1_overall_crit <- cbind(adj_num,(adj_denom-adj_num)) ~ -1 + rti + rti:year_regression + rti:cat1 + rti:cat2 + rti:cat3 + 
  rti:cat4 + rti:cat5 + rti:cat6 + rti:cat7 + rti:cat8 + rti:cat9 + rti:cat10 + (1|region:rti) + (1 | sid)

form2_overall_crit <- cbind(adj_num,(adj_denom-adj_num)) ~ -1 + rti + rti:year_regression + rti:sex + rti:cat1 + rti:cat2 + rti:cat3 + 
  rti:cat4 + rti:cat5 + rti:cat6 + rti:cat7 + rti:cat8 + rti:cat9 + rti:cat10 + (1|region:rti) + (1 | sid)

mod_vd_overall_crit <- glmmTMB(form1_overall_crit, data = df_vd_overall, family = binomial(link="logit"),
                                  control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

mod_ud_overall_crit <- glmmTMB(form1_overall_crit, data = df_ud_overall, family = binomial(link="logit"),
                                  control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

mod_gu_overall_crit <- glmmTMB(form2_overall_crit, data = df_gu_overall, family = binomial(link="logit"),
                                  control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

## 2. HIV-stratified estimates

form1_hiv <- cbind(adj_num,(adj_denom-adj_num)) ~ -1 + rti + rti:year_regression + rti:hiv_status + (1|region:rti) + (1 | sid)
form2_hiv <- cbind(adj_num,(adj_denom-adj_num)) ~ -1 + rti + rti:year_regression + rti:sex + rti:hiv_status + (1|region:rti) + (1 | sid)

mod_vd_hiv <- glmmTMB(form1_hiv, data = df_vd_hiv, family = binomial(link="logit"),
                      control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

mod_ud_hiv <- glmmTMB(form1_hiv, data = df_ud_hiv, family = binomial(link="logit"),
                      control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

mod_gu_hiv <- glmmTMB(form2_hiv, data = df_gu_hiv, family = binomial(link="logit"),
                      control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

## 3. Age-stratified estimates

form1_age <- cbind(adj_num,(adj_denom-adj_num)) ~ -1 + rti + rti:year_regression + rti:age_group + (1|region:rti) + (1 | sid)
form2_age <- cbind(adj_num,(adj_denom-adj_num)) ~ -1 + rti + rti:year_regression + rti:sex + rti:age_group + (1|region:rti) + (1 | sid)

mod_vd_age <- glmmTMB(form1_age, data = df_vd_age, family = binomial(link="logit"),
                      control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

mod_ud_age <- glmmTMB(form1_age, data = df_ud_age,family = binomial(link="logit"),
                      control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))

mod_gu_age <- glmmTMB(form2_age, data = df_gu_age, family = binomial(link="logit"),
                      control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))


### POST-STRATIFY RESULTS

## Prediction datasets

df_vd_predict <- expand.grid(rti = unique(df_vd_overall$rti), 
                             region = unique(df_vd_overall$region), 
                             year = 1970:2022, 
                             sex = "Female") %>%
  mutate(year_regression = year-2015, sid = NA, symptom="Vaginal discharge")

df_ud_predict <- expand.grid(rti = unique(df_ud_overall$rti), 
                             region = unique(df_ud_overall$region), 
                             year = 1970:2022, 
                             sex = "Male") %>%
  mutate(year_regression = year-2015, sid = NA, symptom="Urethral discharge")

df_gu_predict <- expand.grid(rti = unique(df_gu_overall$rti), 
                             region = unique(df_gu_overall$region), 
                             year = 1970:2022, 
                             sex = unique(df_gu_overall$sex)) %>%
  mutate(year_regression = year-2015, sid = NA, symptom="Genital ulcer")

df_vd_predict_hiv <- expand.grid(rti = unique(df_vd_hiv$rti), 
                             region = unique(df_vd_hiv$region), 
                             year = 2015, 
                             hiv_status = unique(df_vd_hiv$hiv_status),
                             sex = "Female") %>%
  mutate(year_regression = year-2015, sid = NA, symptom="Vaginal discharge")

df_ud_predict_hiv <- expand.grid(rti = unique(df_ud_hiv$rti), 
                             region = unique(df_ud_hiv$region), 
                             year = 2015, 
                             hiv_status = unique(df_ud_hiv$hiv_status),
                             sex = "Male") %>%
  mutate(year_regression = year-2015, sid = NA, symptom="Urethral discharge")

df_gu_predict_hiv <- expand.grid(rti = unique(df_gu_hiv$rti), 
                             region = unique(df_gu_hiv$region), 
                             year = 2015, 
                             hiv_status = unique(df_gu_hiv$hiv_status),
                             sex = unique(df_gu_hiv$sex)) %>%
  mutate(year_regression = year-2015, sid = NA, symptom="Genital ulcer")

df_vd_predict_age <- expand.grid(rti = unique(df_vd_age$rti), 
                                 region = unique(df_vd_age$region), 
                                 year = 2015, 
                                 age_group = unique(df_vd_age$age_group),
                                 sex = "Female") %>%
  mutate(year_regression = year-2015, sid = NA, symptom="Vaginal discharge")

df_ud_predict_age <- expand.grid(rti = unique(df_ud_age$rti), 
                                 region = unique(df_ud_age$region), 
                                 year = 2015, 
                                 age_group = unique(df_ud_age$age_group),
                                 sex = "Male") %>%
  mutate(year_regression = year-2015, sid = NA, symptom="Urethral discharge")

df_gu_predict_age <- expand.grid(rti = unique(df_gu_age$rti), 
                                 region = unique(df_gu_age$region), 
                                 year = 2015, 
                                 age_group = unique(df_gu_age$age_group),
                                 sex = unique(df_gu_age$sex)) %>%
  mutate(year_regression = year-2015, sid = NA, symptom="Genital ulcer")

## 1. Trend estimates 

# 1.1 Using models with observations adjusted for diagnostic test performance

prediction_vd_overall_adj <- predict(mod_vd_overall_adj, newdata=df_vd_predict, type="link", se.fit = TRUE, allow.new.levels=TRUE)
prediction_ud_overall_adj <- predict(mod_ud_overall_adj, newdata=df_ud_predict, type="link", se.fit = TRUE, allow.new.levels=TRUE)
prediction_gu_overall_adj <- predict(mod_gu_overall_adj, newdata=df_gu_predict, type="link", se.fit = TRUE, allow.new.levels=TRUE)

df_predict_overall_adj <- rbind(
  df_vd_predict %>%
    mutate(predict_logit = prediction_vd_overall_adj$fit,
           se_logit = prediction_vd_overall_adj$se.fit),
  df_ud_predict %>%
    mutate(predict_logit = prediction_ud_overall_adj$fit,
           se_logit = prediction_ud_overall_adj$se.fit),
  df_gu_predict %>%
    mutate(predict_logit = prediction_gu_overall_adj$fit,
           se_logit = prediction_gu_overall_adj$se.fit)) %>%
  mutate(symptom = factor(symptom,levels=c("Vaginal discharge","Urethral discharge","Genital ulcer")),
         rti = factor(rti,levels=c("BV","CA","CS","CT","HD","HSV","HSV-1","HSV-2","LGV","MG","NG","TV","TP","None")))

dat_overall_adj <- rbind(
  df_predict_overall_adj %>%
    left_join(population, by = c("region","sex")) %>%
    group_by(symptom, region, rti, year) %>%
    summarise(est = plogis(sum(predict_logit*population)/sum(population)),
              lwr = plogis(sum(predict_logit*population)/sum(population) - 1.96*sum(se_logit*population)/sum(population)),
              upr = plogis(sum(predict_logit*population)/sum(population) + 1.96*sum(se_logit*population)/sum(population))),
  df_predict_overall_adj %>%
    left_join(population, by = c("region","sex")) %>%
    group_by(symptom, rti, year) %>%
    summarise(est = plogis(sum(predict_logit*population)/sum(population)),
              lwr = plogis(sum(predict_logit*population)/sum(population) - 1.96*sum(se_logit*population)/sum(population)),
              upr = plogis(sum(predict_logit*population)/sum(population) + 1.96*sum(se_logit*population)/sum(population))) %>%
    mutate(region = "Sub-Saharan Africa"))

write.csv(dat_overall_adj, "./estimates/trend_estimates_adjusted.csv", row.names = FALSE, na = "")
  
# 1.2 Using models with observations as reported (no diagnostic test adjustment)

prediction_vd_overall_unadj <- predict(mod_vd_overall_unadj, newdata=df_vd_predict, type="link", se.fit = TRUE, allow.new.levels=TRUE)
prediction_ud_overall_unadj <- predict(mod_ud_overall_unadj, newdata=df_ud_predict, type="link", se.fit = TRUE, allow.new.levels=TRUE)
prediction_gu_overall_unadj <- predict(mod_gu_overall_unadj, newdata=df_gu_predict, type="link", se.fit = TRUE, allow.new.levels=TRUE)

df_predict_overall_unadj <- rbind(
  df_vd_predict %>%
    mutate(predict_logit = prediction_vd_overall_unadj$fit,
           se_logit = prediction_vd_overall_unadj$se.fit),
  df_ud_predict %>%
    mutate(predict_logit = prediction_ud_overall_unadj$fit,
           se_logit = prediction_ud_overall_unadj$se.fit),
  df_gu_predict %>%
    mutate(predict_logit = prediction_gu_overall_unadj$fit,
           se_logit = prediction_gu_overall_unadj$se.fit)) %>%
  mutate(symptom = factor(symptom,levels=c("Vaginal discharge","Urethral discharge","Genital ulcer")),
         rti = factor(rti,levels=c("BV","CA","CS","CT","HD","HSV","HSV-1","HSV-2","LGV","MG","NG","TV","TP","None")))

dat_overall_unadj <- df_predict_overall_unadj %>%
    left_join(population, by = c("region","sex")) %>%
    group_by(symptom, rti, year) %>%
    summarise(est = plogis(sum(predict_logit*population)/sum(population)),
              lwr = plogis(sum(predict_logit*population)/sum(population) - 1.96*sum(se_logit*population)/sum(population)),
              upr = plogis(sum(predict_logit*population)/sum(population) + 1.96*sum(se_logit*population)/sum(population))) %>%
    mutate(region = "Sub-Saharan Africa")

write.csv(dat_overall_unadj, "./estimates/trend_estimates_unadjusted.csv", row.names = FALSE, na = "")

# 1.3 Using observations with NAAT testing adjusted for diagnostic test performance

prediction_vd_naat_adj <- predict(mod_vd_naat_adj, newdata=df_vd_predict %>% filter(!rti %in% c("BV","CA","CS","None")), type="link", se.fit = TRUE, allow.new.levels=TRUE)
prediction_ud_naat_adj <- predict(mod_ud_naat_adj, newdata=df_ud_predict %>% filter(!rti %in% c("None")), type="link", se.fit = TRUE, allow.new.levels=TRUE)
prediction_gu_naat_adj <- predict(mod_gu_naat_adj, newdata=df_gu_predict %>% filter(!rti %in% c("None")), type="link", se.fit = TRUE, allow.new.levels=TRUE)

df_predict_naat_adj <- rbind(
  df_vd_predict %>%
    filter(!rti %in% c("BV","CA","CS","None")) %>%
    mutate(predict_logit = prediction_vd_naat_adj$fit,
           se_logit = prediction_vd_naat_adj$se.fit),
  df_ud_predict %>%
    filter(!rti %in% c("None")) %>%
    mutate(predict_logit = prediction_ud_naat_adj$fit,
           se_logit = prediction_ud_naat_adj$se.fit),
  df_gu_predict %>%
    filter(!rti %in% c("None")) %>%
    mutate(predict_logit = prediction_gu_naat_adj$fit,
           se_logit = prediction_gu_naat_adj$se.fit)) %>%
  mutate(symptom = factor(symptom,levels=c("Vaginal discharge","Urethral discharge","Genital ulcer")),
         rti = factor(rti,levels=c("CT","HD","HSV","HSV-1","HSV-2","LGV","MG","NG","TV","TP")))

dat_naat_adj <- df_predict_naat_adj %>%
    left_join(population, by = c("region","sex")) %>%
    group_by(symptom, rti, year) %>%
    summarise(est = plogis(sum(predict_logit*population)/sum(population)),
              lwr = plogis(sum(predict_logit*population)/sum(population) - 1.96*sum(se_logit*population)/sum(population)),
              upr = plogis(sum(predict_logit*population)/sum(population) + 1.96*sum(se_logit*population)/sum(population))) %>%
    mutate(region = "Sub-Saharan Africa",
           type = "Adjusted")

# 1.4 Using observations with NAAT testing as reported

prediction_vd_naat_unadj <- predict(mod_vd_naat_unadj, newdata=df_vd_predict %>% filter(!rti %in% c("BV","CA","CS","None")), type="link", se.fit = TRUE, allow.new.levels=TRUE)
prediction_ud_naat_unadj <- predict(mod_ud_naat_unadj, newdata=df_ud_predict %>% filter(!rti %in% c("None")), type="link", se.fit = TRUE, allow.new.levels=TRUE)
prediction_gu_naat_unadj <- predict(mod_gu_naat_unadj, newdata=df_gu_predict %>% filter(!rti %in% c("None")), type="link", se.fit = TRUE, allow.new.levels=TRUE)

df_predict_naat_unadj <- rbind(
  df_vd_predict %>%
    filter(!rti %in% c("BV","CA","CS","None")) %>%
    mutate(predict_logit = prediction_vd_naat_unadj$fit,
           se_logit = prediction_vd_naat_unadj$se.fit),
  df_ud_predict %>%
    filter(!rti %in% c("None")) %>%
    mutate(predict_logit = prediction_ud_naat_unadj$fit,
           se_logit = prediction_ud_naat_unadj$se.fit),
  df_gu_predict %>%
    filter(!rti %in% c("None")) %>%
    mutate(predict_logit = prediction_gu_naat_unadj$fit,
           se_logit = prediction_gu_naat_unadj$se.fit)) %>%
  mutate(symptom = factor(symptom,levels=c("Vaginal discharge","Urethral discharge","Genital ulcer")),
         rti = factor(rti,levels=c("CT","HD","HSV","HSV-1","HSV-2","LGV","MG","NG","TV","TP")))

dat_naat_unadj <- df_predict_naat_unadj %>%
  left_join(population, by = c("region","sex")) %>%
  group_by(symptom, rti, year) %>%
  summarise(est = plogis(sum(predict_logit*population)/sum(population)),
            lwr = plogis(sum(predict_logit*population)/sum(population) - 1.96*sum(se_logit*population)/sum(population)),
            upr = plogis(sum(predict_logit*population)/sum(population) + 1.96*sum(se_logit*population)/sum(population))) %>%
  mutate(region = "Sub-Saharan Africa",
         type = "Unadjusted")

dat_naat <- rbind(dat_naat_adj, dat_naat_unadj)

write.csv(dat_naat, "./estimates/trend_estimates_naat.csv", row.names = FALSE, na = "")

## 2. HIV-stratified analysis

prediction_vd_hiv <- predict(mod_vd_hiv, newdata=df_vd_predict_hiv, type="link", se.fit = TRUE, allow.new.levels=TRUE)
prediction_ud_hiv <- predict(mod_ud_hiv, newdata=df_ud_predict_hiv, type="link", se.fit = TRUE, allow.new.levels=TRUE)
prediction_gu_hiv <- predict(mod_gu_hiv, newdata=df_gu_predict_hiv, type="link", se.fit = TRUE, allow.new.levels=TRUE)

df_predict_hiv <- rbind(
  df_vd_predict_hiv %>%
    mutate(predict_logit = prediction_vd_hiv$fit,
           se_logit = prediction_vd_hiv$se.fit),
  df_ud_predict_hiv %>%
    mutate(predict_logit = prediction_ud_hiv$fit,
           se_logit = prediction_ud_hiv$se.fit),
  df_gu_predict_hiv %>%
    mutate(predict_logit = prediction_gu_hiv$fit,
           se_logit = prediction_gu_hiv$se.fit)) %>%
  mutate(symptom = factor(symptom,levels=c("Vaginal discharge","Urethral discharge","Genital ulcer")),
         rti = factor(rti,levels=c("BV","CS","CT","HD","HSV","HSV-1","HSV-2","LGV","MG","NG","TV","TP","None")))

dat_hiv <- df_predict_hiv %>%
    left_join(population, by = c("region","sex")) %>%
    group_by(symptom, rti, year, hiv_status) %>%
    summarise(est = plogis(sum(predict_logit*population)/sum(population)),
              lwr = plogis(sum(predict_logit*population)/sum(population) - 1.96*sum(se_logit*population)/sum(population)),
              upr = plogis(sum(predict_logit*population)/sum(population) + 1.96*sum(se_logit*population)/sum(population))) %>%
    mutate(region = "Sub-Saharan Africa")

write.csv(dat_hiv, "./estimates/hiv_estimates.csv", row.names = FALSE, na = "")

## 3. Age-stratified analysis

prediction_vd_age <- predict(mod_vd_age, newdata=df_vd_predict_age, type="link", se.fit = TRUE, allow.new.levels=TRUE)
prediction_ud_age <- predict(mod_ud_age, newdata=df_ud_predict_age, type="link", se.fit = TRUE, allow.new.levels=TRUE)
prediction_gu_age <- predict(mod_gu_age, newdata=df_gu_predict_age, type="link", se.fit = TRUE, allow.new.levels=TRUE)

df_predict_age <- rbind(
  df_vd_predict_age %>%
    mutate(predict_logit = prediction_vd_age$fit,
           se_logit = prediction_vd_age$se.fit),
  df_ud_predict_age %>%
    mutate(predict_logit = prediction_ud_age$fit,
           se_logit = prediction_ud_age$se.fit),
  df_gu_predict_age %>%
    mutate(predict_logit = prediction_gu_age$fit,
           se_logit = prediction_gu_age$se.fit)) %>%
  mutate(symptom = factor(symptom,levels=c("Vaginal discharge","Urethral discharge","Genital ulcer")),
         rti = factor(rti,levels=c("BV","CA","CS","CT","HD","HSV","HSV-1","HSV-2","LGV","MG","NG","TV","TP","None")))

dat_age <- df_predict_age %>%
  left_join(population, by = c("region","sex")) %>%
  group_by(symptom, rti, year, age_group) %>%
  summarise(est = plogis(sum(predict_logit*population)/sum(population)),
            lwr = plogis(sum(predict_logit*population)/sum(population) - 1.96*sum(se_logit*population)/sum(population)),
            upr = plogis(sum(predict_logit*population)/sum(population) + 1.96*sum(se_logit*population)/sum(population))) %>%
  mutate(region = "Sub-Saharan Africa")

write.csv(dat_age, "./estimates/age_estimates.csv", row.names = FALSE, na = "")


### SAVE TABLES

table_prep <- function(model){
  
  coef(summary(model))$cond %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    mutate(or = sprintf(exp(Estimate),fmt = '%#.2f'),
           lwr = sprintf(exp(Estimate - 1.96*`Std. Error`),fmt = '%#.2f'),
           upr = sprintf(exp(Estimate + 1.96*`Std. Error`),fmt = '%#.2f'),           
           p = case_when(`Pr(>|z|)` < 0.001 ~ "***", `Pr(>|z|)` >= 0.001 & `Pr(>|z|)` < 0.01 ~ "**", 
                         `Pr(>|z|)` >= 0.01 & `Pr(>|z|)` < 0.05 ~ "*", TRUE~ ""),
           estimate = paste0(or," (",lwr,"-",upr,") ",p)) %>%
    select(variable,estimate) %>%
    mutate(variable = gsub("rti","",variable),
           variable = gsub("sex","",variable),
           variable = gsub("age_group","",variable),
           variable = gsub("hiv_status","",variable),
           variable = gsub("year_regression","year",variable),
           variable = gsub("cat1n","Q1 No/unsure", variable),
           variable = gsub("cat2n","Q2 No/unsure", variable),
           variable = gsub("cat3n","Q3 No/unsure", variable),
           variable = gsub("cat4n","Q4 No/unsure", variable),
           variable = gsub("cat5n","Q5 No/unsure", variable),
           variable = gsub("cat6n","Q6 No/unsure", variable),
           variable = gsub("cat7n","Q7 No/unsure", variable),
           variable = gsub("cat8n","Q8 No/unsure", variable),
           variable = gsub("cat9n","Q9 No/unsure", variable),
           variable = gsub("cat10n","Q10 No/unsure", variable))
}

table_prep(mod_vd_overall_adj) %>% write.csv("./tables/vd_overall_adj.csv", row.names=FALSE)
table_prep(mod_vd_overall_unadj) %>% write.csv("./tables/vd_overall_unadj.csv", row.names=FALSE)
table_prep(mod_vd_overall_crit) %>% write.csv("./tables/vd_overall_crit.csv", row.names=FALSE)
table_prep(mod_vd_hiv) %>% write.csv("./tables/vd_hiv.csv", row.names=FALSE)
table_prep(mod_vd_age) %>% write.csv("./tables/vd_age.csv", row.names=FALSE)

table_prep(mod_ud_overall_adj) %>% write.csv("./tables/ud_overall_adj.csv", row.names=FALSE)
table_prep(mod_ud_overall_unadj) %>% write.csv("./tables/ud_overall_unadj.csv", row.names=FALSE)
table_prep(mod_ud_overall_crit) %>% write.csv("./tables/ud_overall_crit.csv", row.names=FALSE)
table_prep(mod_ud_hiv) %>% write.csv("./tables/ud_hiv.csv", row.names=FALSE)
table_prep(mod_ud_age) %>% write.csv("./tables/ud_age.csv", row.names=FALSE)

table_prep(mod_gu_overall_adj) %>% write.csv("./tables/gu_overall_adj.csv", row.names=FALSE)
table_prep(mod_gu_overall_unadj) %>% write.csv("./tables/gu_overall_unadj.csv", row.names=FALSE)
table_prep(mod_gu_overall_crit) %>% write.csv("./tables/gu_overall_crit.csv", row.names=FALSE)
table_prep(mod_gu_hiv) %>% write.csv("./tables/gu_hiv.csv", row.names=FALSE)
table_prep(mod_gu_age) %>% write.csv("./tables/gu_age.csv", row.names=FALSE)


### EXTRACT ODDS RATIOS FOR FIGURES

or_extract <- function(model){
  
  coef(summary(model))$cond %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    filter(grepl("HIV",variable) | grepl("age_group",variable)) %>%
    mutate(variable = gsub("hiv_status","",variable),
           variable = gsub("age_group","",variable),
           variable = gsub("rti","",variable)) %>%
    separate(variable,sep = ":", into=c("rti","variable")) %>%
    mutate(or = exp(Estimate),
           lwr = exp(Estimate - 1.96*`Std. Error`),
           upr = exp(Estimate + 1.96*`Std. Error`),
           label = paste0(sprintf(or,fmt = '%#.1f')," (",sprintf(lwr,fmt = '%#.1f'),"-",sprintf(upr,fmt = '%#.1f'),")")) %>%
    select(rti, variable, or, lwr, upr, label)
  
}

# HIV-stratified analysis

dat_or_hiv <- rbind(
  or_extract(mod_vd_hiv) %>% mutate(symptom = "Vaginal discharge"),
  or_extract(mod_ud_hiv) %>% mutate(symptom = "Urethral discharge"),
  or_extract(mod_gu_hiv) %>% mutate(symptom = "Genital ulcer"))

write.csv(dat_or_hiv, "./estimates/hiv_or.csv", row.names = FALSE, na = "")

# Age-stratified analysis

dat_or_age <- rbind(
  or_extract(mod_vd_age) %>% mutate(symptom = "Vaginal discharge"),
  or_extract(mod_ud_age) %>% mutate(symptom = "Urethral discharge"),
  or_extract(mod_gu_age) %>% mutate(symptom = "Genital ulcer"))

write.csv(dat_or_age, "./estimates/age_or.csv", row.names = FALSE, na = "")

or_extract <- function(model){
  
  coef(summary(model))$cond %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    filter(grepl("cat",variable)) %>%
    mutate(variable = gsub("rti","",variable),
           variable = gsub("cat","Q",variable)) %>%
    separate(variable,sep = ":", into=c("rti","category")) %>%
    mutate(category = gsub("n","",category),
           colour = case_when(`Pr(>|z|)` < 0.05 ~ "y", TRUE ~ "n")) %>%
    mutate(or = exp(Estimate),
           lwr = exp(Estimate - 1.96*`Std. Error`),
           upr = exp(Estimate + 1.96*`Std. Error`),
           label = paste0(sprintf(or,fmt = '%#.1f')," (",sprintf(lwr,fmt = '%#.1f'),"-",sprintf(upr,fmt = '%#.1f'),")")) %>%
    select(rti, category, or, lwr, upr, label, colour)
  
}

# Risk of bias analysis

dat_or_crit <- rbind(
  or_extract(mod_vd_overall_crit) %>% mutate(symptom = "Vaginal discharge"),
  or_extract(mod_ud_overall_crit) %>% mutate(symptom = "Urethral discharge"),
  or_extract(mod_gu_overall_crit) %>% mutate(symptom = "Genital ulcer"))

write.csv(dat_or_crit, "./estimates/crit_or.csv", row.names = FALSE, na = "")

## QUANTIFY HETEROGENEITY

variance_output <- function(mod, df) {
  
  var <- insight::get_variance(mod, tolerance = 1e-100)
  # var.fixed is variance of the matrix-multiplication β∗X (parameter vector by model matrix)
  # var.random is the mean variance of random effects in the model, according to Johnson 2014 equation 10
  # var.residual is combination of var.distribution and var.dispersion, according to Nakagawa 2017
  # Distribution variance is variation that comes from expected variability in binomial distribution 
  # Overdispersion variance is the additional observation-level variation, that is more than expected from the binomial distribution
  # Dispersion variance is the same as having an observation-level random effect (therefore var.dispersion and variance for sid intercept are the same)
  
  # tolerance level set to very small to ensure random effect variance included for model with almost zero variance (gu model)
  
    # get_variance() assumes a binary outcome and doesn't account for cbind(n, y-n) formulation of binomial model
  
  # If binary event E happens with probability p, then the variance is var(E) = p * (1-p)
  # If do the same n times and get y results "true", then y ~ binomial(n, p) and var(y) = n * p * (1-p) 
  # If interested in the proportion w = y / n , then var(w) = var(y/n) = 1/n^2 * var(y) = p * (1 - p) / n = var(E) / n
  
  # Therefore, divide $var.distribution (has value 3.29, which is the theoretical variance on logistic distribution for 1 binary event) by mean sample size
  
  output <- data.frame(
    var_fixed_random = var$var.fixed + var$var.random,
    var_dispersion = var$var.dispersion,
    var_distribution = var$var.distribution/mean(df$adj_denom), # divide distribution variance by average sample size
    var_total = var$var.fixed + var$var.random + var$var.dispersion + var$var.distribution/mean(df$adj_denom)) %>%
    pivot_longer(cols=everything()) %>%
    mutate(percent = sprintf("%.2f",value/(var$var.fixed + var$var.random + var$var.dispersion + var$var.distribution/mean(df$adj_denom))*100),
           value = sprintf("%.2f", value))
  
  return(output)
  
}

variance_output(mod_vd_overall_adj, df_vd_overall)
variance_output(mod_ud_overall_adj, df_ud_overall)
variance_output(mod_gu_overall_adj, df_gu_overall)

variance_output(mod_vd_hiv, df_vd_hiv)
variance_output(mod_ud_hiv, df_ud_hiv)
variance_output(mod_gu_hiv, df_gu_hiv)

variance_output(mod_vd_age, df_vd_age)
variance_output(mod_ud_age, df_ud_age)
variance_output(mod_gu_age, df_gu_age)
