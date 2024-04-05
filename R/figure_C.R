# Differences in aetiology according to JBI criteria

# Load packages
library(tidyverse)
library(readxl)
library(patchwork)

# Load and prepare data

df_or <- read.csv("./estimates/crit_or.csv") %>%
  mutate(label = case_when(is.na(or) ~ "", 
                           label == "53.1 (10.2-276.4)" ~ "53.1 (10.2-276)",
                           label == "0.0 (0.0-0.2)" ~ "0.03 (0.0-0.2)",
                           label == "0.0 (0.0-5.3)" ~ "0.04 (0.0-5.3)",
                           label == "35.2 (0.0-37420.6)" ~ "35.2 (0.0-37421)",
                           TRUE~label))

# Theme

my_theme <- function(){
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.box.spacing = unit(1, "pt"), 
        #legend.margin = margin(t=5,b=0,l=10,r=0),
        legend.key.size = unit(0.3, 'cm'),
        legend.position = "bottom",
        panel.spacing = unit(0.2,"cm"),
        # text size
        axis.text = element_text(size = rel(1.0)),
        axis.title = element_text(size = rel(1.1), face="bold"),
        legend.text = element_text(size = rel(1.1)),
        legend.title = element_text(size = rel(1.1), face="bold"),
        plot.tag = element_text(size=rel(1.25), face="bold"),
        panel.border = element_rect(color = "black", fill = NA),
        strip.text = element_text(color="black", size = rel(1.1), face="bold",vjust=1.5, hjust=0), # facet labels
        axis.ticks = element_line(size = rel(1.0)),
        strip.background = element_blank())
}

# Plot

colour_label <- "grey20"

## Include labels

plot_vd <- df_or %>%
  filter(symptom == "Vaginal discharge") %>%
  mutate(rti = factor(rti, levels=c("CS","BV","CA","CT","TV","NG","MG","None")),
         category = factor(category, levels=c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10"))) %>%
  ggplot() +
  geom_pointrange(aes(x = or, xmin = lwr, xmax = upr, y = reorder(category, desc(category))), size = 0.06, linewidth = 0.2, fatten = 1.8) +
  geom_vline(xintercept = 1, linetype = "dotted", linewidth = 0.2) +
  #geom_label(aes(x = or, y = category, label = label), size = 4/.pt, label.size = NA, label.padding = unit(0.1,"cm"), hjust=0.55, nudge_y = 0.35, colour = colour_label) +
  geom_label(aes(x = 3500, y = category, label = label), size = 6/.pt, label.size = NA, label.padding = unit(0.08,"cm"), hjust=1, colour = colour_label) +
  facet_wrap(~rti, ncol = 3) +
  theme_classic(base_size = 6) +
  my_theme() +
  theme(legend.position="none") +
  scale_x_continuous(breaks = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100")) +
  coord_trans(x = "log10", xlim = c(0.025, 2000)) +
  labs(x="Adjusted odds ratio (log scale)",y="Vaginal discharge",colour="",fill="",tag="I")

plot_ud <- df_or %>%
  filter(symptom == "Urethral discharge") %>%
  mutate(rti = factor(rti, levels=c("NG","CT","MG","TV","None")),
         category = factor(category, levels=c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10"))) %>%
  ggplot() +
  geom_pointrange(aes(x = or, xmin = lwr, xmax = upr, y = reorder(category, desc(category))), size = 0.06, linewidth = 0.2, fatten = 1.8) +
  geom_vline(xintercept = 1, linetype = "dotted", linewidth = 0.2) +
  #geom_label(aes(x = or, y = category, label = label), size = 4/.pt, label.size = NA, label.padding = unit(0.1,"cm"), hjust=0.55, nudge_y = 0.35, colour = colour_label) +
  geom_label(aes(x = 3500, y = category, label = label), size = 6/.pt, label.size = NA, label.padding = unit(0.08,"cm"), hjust=1, colour = colour_label) +
  facet_wrap(~rti, ncol = 3) +
  theme_classic(base_size = 6) +
  my_theme() +
  theme(legend.position="none") +
  scale_x_continuous(breaks = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100")) +
  coord_trans(x = "log10", xlim = c(0.025, 2000)) +
  labs(x="Adjusted odds ratio (log scale)",y="Urethral discharge",colour="",fill="",tag="II")

plot_gu <- df_or %>%
  filter(symptom == "Genital ulcer") %>%
  mutate(rti = factor(rti, levels=c("HSV","HSV-2","TP","LGV","HSV-1","HD","None")),
         category = factor(category, levels=c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10"))) %>%
  ggplot() +
  geom_pointrange(aes(x = or, xmin = lwr, xmax = upr, y = reorder(category, desc(category))), size = 0.06, linewidth = 0.2, fatten = 1.8) +
  geom_vline(xintercept = 1, linetype = "dotted", linewidth = 0.2) +
  #geom_label(aes(x = or, y = category, label = label), size = 4/.pt, label.size = NA, label.padding = unit(0.1,"cm"), hjust=0.55, nudge_y = 0.35, colour = colour_label) +
  geom_label(aes(x = 3500, y = category, label = label), size = 6/.pt, label.size = NA, label.padding = unit(0.08,"cm"), hjust=1, colour = colour_label) +
  facet_wrap(~rti, ncol = 3) +
  theme_classic(base_size = 6) +
  my_theme() +
  scale_x_continuous(breaks = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100")) +
  coord_trans(x = "log10", xlim = c(0.02, 2000)) +
  labs(x="Adjusted odds ratio (log scale)",y="Genital ulcer",colour="",fill="",tag="III")

ggsave("./plots/figure_CI.png", plot_vd, width=17.5, height=14, units="cm", dpi=700)
ggsave("./plots/figure_CII.png", plot_ud, width=17.5, height=9.3, units="cm", dpi=700)
ggsave("./plots/figure_CIII.png", plot_gu, width=17.5, height=14, units="cm", dpi=700)

