# Aetiologic trends in SSA according to risk of bias

# Load packages
library(tidyverse)
library(readxl)
library(patchwork)

# Load and prepare data

df_est <- read.csv("./estimates/trend_estimates_rob.csv")

df_study <- read_excel("appendix_studydata.xlsx", sheet='Study data') %>%
  filter(analysis == "Overall") %>%
  mutate(symptom = factor(symptom,levels=c("VD","UD","GU"), labels=c("Vaginal discharge","Urethral discharge","Genital ulcer")),
         region = case_when(unique_id == 618 ~ "Eastern Africa", TRUE~region), # assign study to Eastern Africa
         rob = factor(rob, levels=c("Higher","Moderate","Lower")))
         
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
  filter(year >= minyear & year <= maxyear) %>%
  mutate(rti=factor(rti,levels=c("NG","CT","MG","TV","None")))

df_full_gu <- df_est %>%
  filter(symptom=="Genital ulcer", region == "Sub-Saharan Africa") %>%
  mutate(rti=factor(rti,levels=c("HSV","HSV-2","TP","HSV-1","HD","LGV","None")))

df_sub_gu <- df_full_gu %>%
  left_join(year_range) %>%
  filter(year >= minyear & year <= maxyear) %>%
  mutate(rti=factor(rti,levels=c("HSV","HSV-2","TP","HSV-1","HD","LGV","None")))


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

colour_rob <- c("red3","orange","green4")

vd_rob <-  ggplot() +
  geom_ribbon(data=df_full_vd, aes(x=year, ymin=upr, ymax=lwr,alpha="Extrapolated",fill=model)) +
  geom_ribbon(data=df_sub_vd, aes(x=year, ymin=upr, ymax=lwr,alpha="Interpolated",fill=model)) +
  geom_line(data=df_full_vd, aes(x=year, y=est, linetype="Extrapolated",colour=model), linewidth=0.5) +
  geom_line(data=df_sub_vd, aes(x=year, y=est, linetype="Interpolated",colour=model), linewidth=0.5) +
  facet_wrap(~rti, ncol=5) +
  theme_classic(base_size = 6) +
  my_theme() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0,1),
                     expand = expansion(mult = c(0, 0.1))) + # make y axis fit all the way
  scale_x_continuous(breaks = c(1970,1980,1990,2000,2010,2020), guide = guide_axis(angle = 90)) +
  scale_alpha_manual("", values = c("Interpolated" = 0.1,"Extrapolated" = 0.06), breaks = c("Interpolated","Extrapolated")) +
  scale_linetype_manual("", values = c("Interpolated" = "solid","Extrapolated" = "dotted"), breaks = c("Interpolated","Extrapolated")) +
  scale_fill_manual("", breaks = c("Lower, moderate, and higher","Lower and moderate","Lower only"), values=colour_rob, guide="none") +
  scale_colour_manual("", breaks = c("Lower, moderate, and higher","Lower and moderate","Lower only"), values=colour_rob) +
  guides(colour=guide_legend(order=1), linetype=guide_legend(order=2), alpha=guide_legend(order=2)) +
  ggnewscale::new_scale_color() +
  geom_point(data = df_study %>% filter(symptom=="Vaginal discharge") %>% mutate(rti = factor(rti,levels=c("CS","BV","CA","CT","TV","NG","MG","None"))), 
             aes(x=year, y=adj_prev,colour=rob), alpha=0.6, size=1.1, stroke=NA) +
  scale_colour_manual("", breaks = c("Higher","Moderate","Lower"), values=colour_rob) +
  guides(colour=guide_legend(order=1)) +
  labs(x="", y="Vaginal discharge \n Diagnosed proportion", colour="", fill="", tag = "A") 

ud_rob <-  ggplot() +
  geom_ribbon(data=df_full_ud, aes(x=year, ymin=upr, ymax=lwr,alpha="Extrapolated",fill=model)) +
  geom_ribbon(data=df_sub_ud, aes(x=year, ymin=upr, ymax=lwr,alpha="Interpolated",fill=model)) +
  geom_line(data=df_full_ud, aes(x=year, y=est, linetype="Extrapolated",colour=model), linewidth=0.5) +
  geom_line(data=df_sub_ud, aes(x=year, y=est, linetype="Interpolated",colour=model), linewidth=0.5) +
  facet_wrap(~rti, ncol=5) +
  theme_classic(base_size = 6) +
  my_theme() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0,1),
                     expand = expansion(mult = c(0, 0.1))) + # make y axis fit all the way
  scale_x_continuous(breaks = c(1970,1980,1990,2000,2010,2020), guide = guide_axis(angle = 90)) +
  scale_alpha_manual("", values = c("Interpolated" = 0.1,"Extrapolated" = 0.06), breaks = c("Interpolated","Extrapolated")) +
  scale_linetype_manual("", values = c("Interpolated" = "solid","Extrapolated" = "dotted"), breaks = c("Interpolated","Extrapolated")) +
  scale_fill_manual("", breaks = c("Lower, moderate, and higher","Lower and moderate","Lower only"), values=colour_rob, guide="none") +
  scale_colour_manual("", breaks = c("Lower, moderate, and higher","Lower and moderate","Lower only"), values=colour_rob) +
  guides(colour=guide_legend(order=1), linetype=guide_legend(order=2), alpha=guide_legend(order=2)) +
  ggnewscale::new_scale_color() +
  geom_point(data = df_study %>% filter(symptom=="Urethral discharge") %>% mutate(rti=factor(rti,levels=c("NG","CT","MG","TV","None"))), 
             aes(x=year, y=adj_prev,colour=rob), alpha=0.6, size=1.1, stroke=NA) +
  scale_colour_manual("", breaks = c("Higher","Moderate","Lower"), values=colour_rob) +
  guides(colour=guide_legend(order=1)) +
  labs(x="", y="Urethral discharge \n Diagnosed proportion", colour="", fill="", tag = "B") 

gu_rob <-  ggplot() +
  geom_ribbon(data=df_full_gu, aes(x=year, ymin=upr, ymax=lwr,alpha="Extrapolated",fill=model)) +
  geom_ribbon(data=df_sub_gu, aes(x=year, ymin=upr, ymax=lwr,alpha="Interpolated",fill=model)) +
  geom_line(data=df_full_gu, aes(x=year, y=est, linetype="Extrapolated",colour=model), linewidth=0.5) +
  geom_line(data=df_sub_gu, aes(x=year, y=est, linetype="Interpolated",colour=model), linewidth=0.5) +
  facet_wrap(~rti, ncol=5) +
  theme_classic(base_size = 6) +
  my_theme() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0,1),
                     expand = expansion(mult = c(0, 0.1))) + # make y axis fit all the way
  scale_x_continuous(breaks = c(1970,1980,1990,2000,2010,2020), guide = guide_axis(angle = 90)) +
  scale_alpha_manual("", values = c("Interpolated" = 0.1,"Extrapolated" = 0.06), breaks = c("Interpolated","Extrapolated")) +
  scale_linetype_manual("", values = c("Interpolated" = "solid","Extrapolated" = "dotted"), breaks = c("Interpolated","Extrapolated")) +
  scale_fill_manual("", breaks = c("Lower, moderate, and higher","Lower and moderate","Lower only"), values=colour_rob, guide="none") +
  scale_colour_manual("", breaks = c("Lower, moderate, and higher","Lower and moderate","Lower only"), values=colour_rob) +
  guides(colour=guide_legend(order=1), linetype=guide_legend(order=2), alpha=guide_legend(order=2)) +
  ggnewscale::new_scale_color() +
  geom_point(data = df_study %>% filter(symptom=="Genital ulcer") %>% mutate(rti=factor(rti,levels=c("HSV","HSV-2","TP","HSV-1","HD","LGV","None"))), 
             aes(x=year, y=adj_prev,colour=rob), alpha=0.6, size=1.1, stroke=NA) +
  scale_colour_manual("", breaks = c("Higher","Moderate","Lower"), values=colour_rob) +
  guides(colour=guide_legend(order=1)) +
  labs(x="", y="Genital ulcer \n Diagnosed proportion", colour="", fill="", tag = "C") 

plot_rob <- (vd_rob / ud_rob / gu_rob) + 
  plot_layout(heights=c(2,1,2),
              guides="collect") &
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.margin = margin(-0.2,0,0,0, unit="cm"))

ggsave("./plots/figure_s3.png", plot_rob, width=15.4, height=18.5, units="cm", dpi=700)
