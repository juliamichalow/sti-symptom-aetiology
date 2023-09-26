# Aetiologic trends for adjusted and unadjusted NAAT-based observations in SSA

# Load packages
library(tidyverse)
library(readxl)
library(patchwork)

# Load and prepare data

df_est_all <- read.csv("./estimates/trend_estimates_adjusted.csv") %>%
  mutate(type="Adjusted")

df_est_naat <- read.csv("./estimates/trend_estimates_naat.csv") 

df_study <- read_excel("appendix_studydata.xlsx", sheet='Study data') %>%
  filter(analysis == "Overall") %>%
  mutate(symptom = factor(symptom,levels=c("VD","UD","GU"), labels=c("Vaginal discharge","Urethral discharge","Genital ulcer")),
         region = case_when(unique_id == 618 ~ "Eastern Africa", TRUE~region), # assign study to Eastern Africa
         test_plot = case_when(test_category=="NAAT" ~ "NAAT", TRUE ~ "Other tests"))

# For each symptom, create full dataframe (estimates from 1970-2022) and sub dataframe (estimates during study years)

year_range <- df_study %>%
  group_by(symptom, rti) %>%
  summarise(minyear=min(year), maxyear=max(year))

df_full_vd <- df_est_all %>%
  filter(symptom == "Vaginal discharge", region == "Sub-Saharan Africa") %>%
  mutate(rti = factor(rti,levels=c("CS","BV","CA","CT","TV","NG","MG","None")))

df_sub_vd <- df_full_vd %>%
  left_join(year_range) %>%
  filter(year >= minyear & year <= maxyear) %>%
  mutate(rti = factor(rti,levels=c("CS","BV","CA","CT","TV","NG","MG","None")))

df_full_ud <- df_est_all %>%
  filter(symptom=="Urethral discharge", region == "Sub-Saharan Africa") %>%
  mutate(rti=factor(rti,levels=c("NG","CT","MG","TV","None")))

df_sub_ud <- df_full_ud %>%
  left_join(year_range) %>%
  filter(year >= minyear & year <= maxyear) %>%
  mutate(rti=factor(rti,levels=c("NG","CT","MG","TV","None")))

df_full_gu <- df_est_all %>%
  filter(symptom=="Genital ulcer", region == "Sub-Saharan Africa") %>%
  mutate(rti=factor(rti,levels=c("HSV","HSV-2","TP","HSV-1","HD","LGV","None")))

df_sub_gu <- df_full_gu %>%
  left_join(year_range) %>%
  filter(year >= minyear & year <= maxyear)  %>%
  mutate(rti=factor(rti,levels=c("HSV","HSV-2","TP","HSV-1","HD","LGV","None")))

year_range_naat <- df_study %>%
  filter(test_plot=="NAAT", !rti=="CA") %>%
  group_by(symptom, rti) %>%
  summarise(minyear=min(year), maxyear=max(year))

df_naat_full <- df_est_naat %>%
  left_join(year_range_naat, by = c("symptom", "rti")) 

df_naat_full_vd <- df_naat_full %>%
  filter(symptom == "Vaginal discharge") %>%
  mutate(rti = factor(rti,levels=c("TV","CT","NG","MG")))

df_naat_sub_vd <- df_naat_full_vd %>%
  filter(year >= minyear & year <= maxyear)

df_naat_full_ud <- df_naat_full %>%
  filter(symptom=="Urethral discharge") %>%
  mutate(rti=factor(rti,levels=c("NG","CT","MG","TV")))

df_naat_sub_ud <- df_naat_full_ud %>%
  filter(year >= minyear & year <= maxyear)

df_naat_full_gu <- df_naat_full %>%
  filter(symptom=="Genital ulcer") %>%
  mutate(rti=factor(rti,levels=c("HSV","HSV-2","TP","HSV-1","LGV","HD")))

df_naat_sub_gu <- df_naat_full_gu %>%
  filter(year >= minyear & year <= maxyear)

# Theme

my_theme <- function(){
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        # move legend closer to figure
        legend.box.spacing = unit(0, "pt"), 
        legend.margin = margin(0,0,0,0),
        legend.key.size = unit(0.4, 'cm'),
        legend.box = 'vertical',
        # panel
        panel.border = element_rect(color = "black", fill = NA),
        #axis.line = element_line(colour="black"),
        panel.spacing = unit(0.2,"cm"),
        # text size
        axis.text = element_text(size = rel(1.0)),
        axis.title = element_text(size = rel(1.1), face="bold"),
        legend.text = element_text(size = rel(1.1)),
        legend.title = element_text(size = rel(1.1), face="bold"),
        plot.tag = element_text(size=rel(1.25), face="bold"),
        #change facet labels
        strip.text = element_text(color="black", size = rel(1.1), face="bold",vjust=1.5, hjust=0), # facet labels
        # axis ticks
        axis.ticks = element_line(size = rel(1.0)),
        # change facet label background and border
        strip.background = element_blank())
}


colour_compare <- c("#58D0BF","coral1","grey30")
colour_compare_point <- c("coral1","grey30")

vd_naat <-  ggplot() +
  # ribbons
  geom_ribbon(data=df_full_vd, aes(x=year, ymin=upr, ymax=lwr, alpha="Extrapolated", fill="All tests, adjusted")) +
  geom_ribbon(data=df_sub_vd, aes(x=year, ymin=upr, ymax=lwr, alpha="Interpolated", fill="All tests, adjusted")) +
  geom_ribbon(data=df_naat_full_vd %>% filter(type=="Unadjusted"), aes(x=year, ymin=upr, ymax=lwr, alpha="Extrapolated", fill="NAAT, unadjusted")) +
  geom_ribbon(data=df_naat_sub_vd %>% filter(type=="Unadjusted"), aes(x=year, ymin=upr, ymax=lwr, alpha="Interpolated", fill="NAAT, unadjusted")) +
  geom_ribbon(data=df_naat_full_vd %>% filter(type=="Adjusted"), aes(x=year, ymin=upr, ymax=lwr, alpha="Extrapolated", fill="NAAT, adjusted")) +
  geom_ribbon(data=df_naat_sub_vd %>% filter(type=="Adjusted"), aes(x=year, ymin=upr, ymax=lwr, alpha="Interpolated", fill="NAAT, adjusted")) +
  scale_fill_manual("", breaks=c("NAAT, unadjusted","NAAT, adjusted", "All tests, adjusted"), values = colour_compare) +
  scale_alpha_manual("", values = c("Interpolated" = 0.1,"Extrapolated" = 0.06), breaks = c("Interpolated","Extrapolated")) +
  # points
  geom_point(data = df_study %>% filter(symptom=="Vaginal discharge") %>% mutate(rti = factor(rti,levels=c("CS","BV","CA","CT","TV","NG","MG","None"))), 
             aes(x=year, y=adj_prev, colour = test_plot), alpha=0.6, size=1.1, stroke=NA) +
  scale_colour_manual("", breaks=c("NAAT","Other tests"), labels=c("NAAT, adjusted","Other tests, adjusted"), values = colour_compare_point) +
  guides(colour=guide_legend(order=1)) +
  # lines on new colour scale
  ggnewscale::new_scale_color() +
  geom_line(data=df_full_vd, aes(x=year, y=est, linetype="Extrapolated", colour="All tests, adjusted"), linewidth=0.5) +
  geom_line(data=df_sub_vd, aes(x=year, y=est, linetype="Interpolated", colour="All tests, adjusted"), linewidth=0.5) +
  geom_line(data=df_naat_full_vd %>% filter(type=="Unadjusted"), aes(x=year, y=est, linetype="Extrapolated", colour="NAAT, unadjusted"), linewidth=0.5) +
  geom_line(data=df_naat_sub_vd %>% filter(type=="Unadjusted"), aes(x=year, y=est, linetype="Interpolated", colour="NAAT, unadjusted"), linewidth=0.5) +
  geom_line(data=df_naat_full_vd %>% filter(type=="Adjusted"), aes(x=year, y=est, linetype="Extrapolated", colour="NAAT, adjusted"), linewidth=0.5) +
  geom_line(data=df_naat_sub_vd %>% filter(type=="Adjusted"), aes(x=year, y=est, linetype="Interpolated", colour="NAAT, adjusted"), linewidth=0.5) +
  scale_linetype_manual("", values = c("Interpolated" = "solid","Extrapolated" = "dotted"), breaks = c("Interpolated","Extrapolated")) +
  scale_colour_manual("", breaks=c("NAAT, unadjusted","NAAT, adjusted", "All tests, adjusted"), values = colour_compare) +
  guides(colour=guide_legend(order=1), fill="none", linetype=guide_legend(order=2), alpha=guide_legend(order=2)) +
  facet_wrap(~rti, ncol=5) +
  theme_classic(base_size = 6) +
  my_theme() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0,1),
                     expand = expansion(mult = c(0, 0.1))) + # make y axis fit all the way
  scale_x_continuous(breaks = c(1970,1980,1990,2000,2010,2020), guide = guide_axis(angle = 90)) +
  labs(x="", y="Vaginal discharge \n Diagnosed proportion", colour="", fill="", tag = "A") 

ud_naat <-  ggplot() +
  # ribbons
  geom_ribbon(data=df_full_ud, aes(x=year, ymin=upr, ymax=lwr, alpha="Extrapolated", fill="All tests, adjusted")) +
  geom_ribbon(data=df_sub_ud, aes(x=year, ymin=upr, ymax=lwr, alpha="Interpolated", fill="All tests, adjusted")) +
  geom_ribbon(data=df_naat_full_ud %>% filter(type=="Unadjusted"), aes(x=year, ymin=upr, ymax=lwr, alpha="Extrapolated", fill="NAAT, unadjusted")) +
  geom_ribbon(data=df_naat_sub_ud %>% filter(type=="Unadjusted"), aes(x=year, ymin=upr, ymax=lwr, alpha="Interpolated", fill="NAAT, unadjusted")) +
  geom_ribbon(data=df_naat_full_ud %>% filter(type=="Adjusted"), aes(x=year, ymin=upr, ymax=lwr, alpha="Extrapolated", fill="NAAT, adjusted")) +
  geom_ribbon(data=df_naat_sub_ud %>% filter(type=="Adjusted"), aes(x=year, ymin=upr, ymax=lwr, alpha="Interpolated", fill="NAAT, adjusted")) +
  scale_fill_manual("", breaks=c("NAAT, unadjusted","NAAT, adjusted", "All tests, adjusted"), values = colour_compare, guide="none") +
  scale_alpha_manual("", values = c("Interpolated" = 0.1,"Extrapolated" = 0.06), breaks = c("Interpolated","Extrapolated")) +
  # points
  geom_point(data = df_study %>% filter(symptom=="Urethral discharge") %>% mutate(rti = factor(rti,levels=c("NG","CT","MG","TV","None"))), 
             aes(x=year, y=adj_prev, colour = test_plot), alpha=0.6, size=1.1, stroke=NA) +
  scale_colour_manual("", breaks=c("NAAT","Other tests"), labels=c("NAAT, adjusted","Other tests, adjusted"), values = colour_compare_point) +
  guides(colour=guide_legend(order=1)) +
  # lines on new colour scale
  ggnewscale::new_scale_color() +
  geom_line(data=df_full_ud, aes(x=year, y=est, linetype="Extrapolated", colour="All tests, adjusted"), linewidth=0.5) +
  geom_line(data=df_sub_ud, aes(x=year, y=est, linetype="Interpolated", colour="All tests, adjusted"), linewidth=0.5) +
  geom_line(data=df_naat_full_ud %>% filter(type=="Unadjusted"), aes(x=year, y=est, linetype="Extrapolated", colour="NAAT, unadjusted"), linewidth=0.5) +
  geom_line(data=df_naat_sub_ud %>% filter(type=="Unadjusted"), aes(x=year, y=est, linetype="Interpolated", colour="NAAT, unadjusted"), linewidth=0.5) +
  geom_line(data=df_naat_full_ud %>% filter(type=="Adjusted"), aes(x=year, y=est, linetype="Extrapolated", colour="NAAT, adjusted"), linewidth=0.5) +
  geom_line(data=df_naat_sub_ud %>% filter(type=="Adjusted"), aes(x=year, y=est, linetype="Interpolated", colour="NAAT, adjusted"), linewidth=0.5) +
  scale_linetype_manual("", values = c("Interpolated" = "solid","Extrapolated" = "dotted"), breaks = c("Interpolated","Extrapolated")) +
  scale_colour_manual("", breaks=c("NAAT, unadjusted","NAAT, adjusted", "All tests, adjusted"), values = colour_compare) +
  guides(colour=guide_legend(order=1), linetype=guide_legend(order=2), alpha=guide_legend(order=2)) +
  facet_wrap(~rti, ncol=5) +
  theme_classic(base_size = 6) +
  my_theme() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0,1),
                     expand = expansion(mult = c(0, 0.1))) + # make y axis fit all the way
  scale_x_continuous(breaks = c(1970,1980,1990,2000,2010,2020), guide = guide_axis(angle = 90)) +
  labs(x="", y="Urethral discharge \n Diagnosed proportion", colour="", fill="", tag = "B") 

gu_naat <-  ggplot() +
  # ribbons
  geom_ribbon(data=df_full_gu, aes(x=year, ymin=upr, ymax=lwr, alpha="Extrapolated", fill="All tests, adjusted")) +
  geom_ribbon(data=df_sub_gu, aes(x=year, ymin=upr, ymax=lwr, alpha="Interpolated", fill="All tests, adjusted")) +
  geom_ribbon(data=df_naat_full_gu %>% filter(type=="Unadjusted"), aes(x=year, ymin=upr, ymax=lwr, alpha="Extrapolated", fill="NAAT, unadjusted")) +
  geom_ribbon(data=df_naat_sub_gu %>% filter(type=="Unadjusted"), aes(x=year, ymin=upr, ymax=lwr, alpha="Interpolated", fill="NAAT, unadjusted")) +
  geom_ribbon(data=df_naat_full_gu %>% filter(type=="Adjusted"), aes(x=year, ymin=upr, ymax=lwr, alpha="Extrapolated", fill="NAAT, adjusted")) +
  geom_ribbon(data=df_naat_sub_gu %>% filter(type=="Adjusted"), aes(x=year, ymin=upr, ymax=lwr, alpha="Interpolated", fill="NAAT, adjusted")) +
  scale_fill_manual("", breaks=c("NAAT, unadjusted","NAAT, adjusted", "All tests, adjusted"), values = colour_compare, guide="none") +
  scale_alpha_manual("", values = c("Interpolated" = 0.1,"Extrapolated" = 0.06), breaks = c("Interpolated","Extrapolated")) +
  # points
  geom_point(data = df_study %>% filter(symptom=="Genital ulcer") %>% mutate(rti = factor(rti,levels=c("HSV","HSV-2","TP","HSV-1","HD","LGV","None"))), 
             aes(x=year, y=adj_prev, colour = test_plot), alpha=0.6, size=1.1, stroke=NA) +
  scale_colour_manual("", breaks=c("NAAT","Other tests"), labels=c("NAAT, adjusted","Other tests, adjusted"), values = colour_compare_point) +
  guides(colour=guide_legend(order=1)) +
  # lines on new colour scale
  ggnewscale::new_scale_color() +
  geom_line(data=df_full_gu, aes(x=year, y=est, linetype="Extrapolated", colour="All tests, adjusted"), linewidth=0.5) +
  geom_line(data=df_sub_gu, aes(x=year, y=est, linetype="Interpolated", colour="All tests, adjusted"), linewidth=0.5) +
  geom_line(data=df_naat_full_gu %>% filter(type=="Unadjusted"), aes(x=year, y=est, linetype="Extrapolated", colour="NAAT, unadjusted"), linewidth=0.5) +
  geom_line(data=df_naat_sub_gu %>% filter(type=="Unadjusted"), aes(x=year, y=est, linetype="Interpolated", colour="NAAT, unadjusted"), linewidth=0.5) +
  geom_line(data=df_naat_full_gu %>% filter(type=="Adjusted"), aes(x=year, y=est, linetype="Extrapolated", colour="NAAT, adjusted"), linewidth=0.5) +
  geom_line(data=df_naat_sub_gu %>% filter(type=="Adjusted"), aes(x=year, y=est, linetype="Interpolated", colour="NAAT, adjusted"), linewidth=0.5) +
  scale_linetype_manual("", values = c("Interpolated" = "solid","Extrapolated" = "dotted"), breaks = c("Interpolated","Extrapolated")) +
  scale_colour_manual("", breaks=c("NAAT, unadjusted","NAAT, adjusted", "All tests, adjusted"), values = colour_compare) +
  guides(colour=guide_legend(order=1), linetype=guide_legend(order=2), alpha=guide_legend(order=2)) +
  facet_wrap(~rti, ncol=5) +
  theme_classic(base_size = 6) +
  my_theme() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0,1),
                     expand = expansion(mult = c(0, 0.1))) + # make y axis fit all the way
  scale_x_continuous(breaks = c(1970,1980,1990,2000,2010,2020), guide = guide_axis(angle = 90)) +
  labs(x="", y="Genital ulcer \n Diagnosed proportion", colour="", fill="", tag = "C") 

plot_naat <- (vd_naat / ud_naat / gu_naat) + 
  plot_layout(heights=c(2,1,2),
              guides="collect") &
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.margin = margin(-0.2,0,0,0, unit="cm"))

ggsave("./plots/figure_s5.png", plot_naat, width=15.4, height=18.5, units="cm", dpi=700)
