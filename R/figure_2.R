# Aetiologic trends in SSA

# Load packages
library(tidyverse)
library(readxl)
library(patchwork)

# Load and prepare data

df_est <- read.csv("./estimates/trend_estimates_adjusted.csv")

df_study <- read_excel("./data/appendix_studydata.xlsx", sheet='Study data') %>%
  filter(analysis == "Overall") %>%
  mutate(symptom = factor(symptom, levels=c("VD","UD","GU"), labels=c("Vaginal discharge","Urethral discharge","Genital ulcer")))

# For each symptom, create full dataframe (estimates from 1970-2022) and sub dataframe (estimates during study years)

year_range <- df_study %>%
  group_by(symptom, rti) %>%
  summarise(minyear=min(year), maxyear=max(year))

df_full_vd <- df_est %>%
  filter(symptom == "Vaginal discharge", region == "Sub-Saharan Africa") %>%
  mutate(rti = factor(rti,levels=c("CS","BV","CA","CT","TV","NG","MG","None")))

df_sub_vd <- df_full_vd %>%
  left_join(year_range) %>%
  filter(year >= minyear & year <= maxyear) %>%
  mutate(rti = factor(rti,levels=c("CS","BV","CA","CT","TV","NG","MG","None")))

df_full_ud <- df_est %>%
  filter(symptom=="Urethral discharge", region == "Sub-Saharan Africa") %>%
  mutate(rti=factor(rti,levels=c("NG","CT","MG","TV","None")))

df_sub_ud <- df_full_ud %>%
  left_join(year_range) %>%
  filter(year >= minyear & year <= maxyear)  %>%
  mutate(rti=factor(rti,levels=c("NG","CT","MG","TV","None")))

df_full_gu <- df_est %>%
  filter(symptom=="Genital ulcer", region == "Sub-Saharan Africa") %>%
  mutate(rti=factor(rti,levels=c("HSV","HSV-2","TP","LGV","HSV-1","HD","None")))

df_sub_gu <- df_full_gu %>%
  left_join(year_range) %>%
  filter(year >= minyear & year <= maxyear) %>%
  mutate(rti=factor(rti,levels=c("HSV","HSV-2","TP","LGV","HSV-1","HD","None")))

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

vd_ssa <-  ggplot() +
  geom_ribbon(data=df_full_vd, aes(x=year, ymin=upr, ymax=lwr,alpha="Extrapolated")) +
  geom_ribbon(data=df_sub_vd, aes(x=year, ymin=upr, ymax=lwr,alpha="Interpolated")) +
  geom_point(data = df_study %>% filter(symptom=="Vaginal discharge") %>% mutate(rti = factor(rti,levels=c("CS","BV","CA","CT","TV","NG","MG","None"))), 
             aes(x=year, y=adj_prev), alpha=0.6, size=1.1, stroke=NA) +
  geom_line(data=df_full_vd, aes(x=year, y=est, linetype="Extrapolated"), 
            linewidth=0.5) +
  geom_line(data=df_sub_vd, aes(x=year, y=est, linetype="Interpolated"), 
            linewidth=0.5) +
  geom_vline(xintercept=2015, linetype = "dotted", linewidth=0.2) +
  geom_point(data=df_full_vd %>% filter(region=="Sub-Saharan Africa", year == 2015), aes(x=year, y=est), 
             colour="blue", size=2, shape="diamond") +
  geom_label(data=df_full_vd %>% filter(region=="Sub-Saharan Africa", year == 2015), 
             aes(x=year,y=est + 0.14,label=paste0(sprintf(est*100,fmt = '%#.1f'),"%")), size = 6/.pt, # /.pt converts mm to pt in line with base_theme
             colour="blue", fill="white", alpha=0.8,
             label.size=NA, label.padding=unit(0.1,"lines")) +
  facet_wrap(~rti, ncol=5) +
  theme_classic(base_size = 6) +
  my_theme() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0,1),
                     expand = expansion(mult = c(0, 0.1))) + # make y axis fit all the way
  scale_x_continuous(breaks = c(1970,1980,1990,2000,2010,2020), guide = guide_axis(angle = 90)) +
  scale_alpha_manual("", values = c("Interpolated" = 0.1,"Extrapolated" = 0.06), breaks = c("Interpolated","Extrapolated")) +
  scale_linetype_manual("", values = c("Interpolated" = "solid","Extrapolated" = "dotted"), breaks = c("Interpolated","Extrapolated")) +
  labs(x="", y="Vaginal discharge \n Diagnosed proportion", colour="", fill="", tag = "A") 

ud_ssa <- ggplot() +
  geom_ribbon(data=df_full_ud, aes(x=year, ymin=upr, ymax=lwr,alpha="Extrapolated")) +
  geom_ribbon(data=df_sub_ud, aes(x=year, ymin=upr, ymax=lwr,alpha="Interpolated")) +
  geom_point(data = df_study %>% filter(symptom=="Urethral discharge") %>% mutate(rti=factor(rti,levels=c("NG","CT","MG","TV","None"))), 
             aes(x=year, y=adj_prev), alpha=0.6, size=1.1, stroke=NA) +
  geom_line(data=df_full_ud, aes(x=year, y=est, linetype="Extrapolated"), 
            linewidth=0.5) +
  geom_line(data=df_sub_ud, aes(x=year, y=est, linetype="Interpolated"), 
            linewidth=0.5) +
  geom_vline(xintercept=2015, linetype = "dotted", linewidth=0.2) +
  geom_point(data=df_full_ud %>% filter(region=="Sub-Saharan Africa", year == 2015), aes(x=year, y=est), 
             colour="blue", size=2, shape="diamond") +
  geom_label(data=df_full_ud %>% filter(region=="Sub-Saharan Africa", year == 2015), 
             aes(x=year,y=est + 0.14,label=paste0(sprintf(est*100,fmt = '%#.1f'),"%")), size = 6/.pt, # /.pt converts mm to pt in line with base_theme
             colour="blue", fill="white", alpha=0.8,
             label.size=NA, label.padding=unit(0.1,"lines")) +
  facet_wrap(~rti, ncol=5) +
  theme_classic(base_size = 6) +
  my_theme() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0,1),
                     expand = expansion(mult = c(0, 0.1))) + # make y axis fit all the way
  scale_x_continuous(breaks = c(1970,1980,1990,2000,2010,2020), guide = guide_axis(angle = 90)) +
  scale_alpha_manual("", values = c("Interpolated" = 0.1,"Extrapolated" = 0.06), breaks = c("Interpolated","Extrapolated")) +
  scale_linetype_manual("", values = c("Interpolated" = "solid","Extrapolated" = "dotted"), breaks = c("Interpolated","Extrapolated")) +
  labs(x="", y="Urethral discharge \n Diagnosed proportion", colour="", fill="", tag = "B") 

gu_ssa <- ggplot() +
  geom_ribbon(data=df_full_gu, aes(x=year, ymin=upr, ymax=lwr,alpha="Extrapolated")) +
  geom_ribbon(data=df_sub_gu, aes(x=year, ymin=upr, ymax=lwr,alpha="Interpolated")) +
  geom_point(data = df_study %>% filter(symptom=="Genital ulcer") %>% mutate(rti=factor(rti,levels=c("HSV","HSV-2","TP","HSV-1","HD","LGV","None"))), 
             aes(x=year, y=adj_prev), alpha=0.6, size=1.1, stroke=NA) +
  geom_line(data=df_full_gu, aes(x=year, y=est, linetype="Extrapolated"), 
            linewidth=0.5) +
  geom_line(data=df_sub_gu, aes(x=year, y=est, linetype="Interpolated"), 
            linewidth=0.5) +
  geom_vline(xintercept=2015, linetype = "dotted", linewidth=0.2) +
  geom_point(data=df_full_gu %>% filter(region=="Sub-Saharan Africa", year == 2015), aes(x=year, y=est), 
             colour="blue", size=2, shape="diamond") +
  geom_label(data=df_full_gu %>% filter(region=="Sub-Saharan Africa", year == 2015), 
             aes(x=year,y=est + 0.14,label=paste0(sprintf(est*100,fmt = '%#.1f'),"%")), size = 6/.pt, # /.pt converts mm to pt in line with base_theme
             colour="blue", fill="white", alpha=0.8,
             label.size=NA, label.padding=unit(0.1,"lines")) +
  facet_wrap(~rti, ncol=5) +
  theme_classic(base_size = 6) +
  my_theme() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0,1),
                     expand = expansion(mult = c(0, 0.1))) + # make y axis fit all the way
  scale_x_continuous(breaks = c(1970,1980,1990,2000,2010,2020), guide = guide_axis(angle = 90)) +
  scale_alpha_manual("", values = c("Interpolated" = 0.1,"Extrapolated" = 0.06), breaks = c("Interpolated","Extrapolated")) +
  scale_linetype_manual("", values = c("Interpolated" = "solid","Extrapolated" = "dotted"), breaks = c("Interpolated","Extrapolated")) +
  labs(x="Study year", y="Genital ulcer \n Diagnosed proportion", colour="", fill="", tag = "C") 

plot_ssa <- (vd_ssa / ud_ssa / gu_ssa) + 
  plot_layout(heights=c(2,0.88,2),
              guides="collect") &
  theme(legend.position = "bottom")

ggsave("./plots/figure_2.png", plot_ssa, width=16.2, height=19, units="cm", dpi=800)
ggsave("./plots/figure_2.tiff", plot_ssa, width=16.2, height=19, units="cm", dpi=300)
