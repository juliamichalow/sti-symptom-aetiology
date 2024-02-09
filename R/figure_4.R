# Differences in aetiology by HIV status 

# Load packages
library(tidyverse)
library(readxl)
library(patchwork)

# Load and prepare data

df_est <- read.csv("./estimates/age_estimates.csv") %>%
  mutate(label = paste0(sprintf(est*100,fmt='%#.1f'),"%"))

df_or <- read.csv("./estimates/age_or.csv") %>%
  # add * to indicate truncation of x axis in plots
  mutate(label = case_when(upr > 4 ~ paste0(label,"*"), TRUE ~ label))

# Theme

my_theme <- function(){
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.box.spacing = unit(1, "pt"), 
        legend.margin = margin(t=5,b=0,l=10,r=0),
        legend.key.size = unit(0.3, 'cm'),
        legend.position = "top",
        panel.spacing = unit(0.2,"cm"),
        axis.text = element_text(size = rel(1.0)),
        axis.title = element_text(size = rel(1.1), face="bold"),
        axis.title.x = element_text(margin = margin(t = 5)),
        legend.text = element_text(size = rel(1.1)),
        legend.title = element_blank(),
        plot.tag = element_text(size=rel(1.25), face="bold"),
        strip.text = element_text(color="black", size = rel(1.1), face="bold",vjust=1.5, hjust=0), # facet labels
        axis.ticks = element_line(size = rel(1.0)),
        strip.background = element_blank())
}

# Plot

colour_age <- c("#E07A62","#56847D")
colour_label <- "grey20"

plot_vd1 <- ggplot(df_est %>% filter(symptom == "Vaginal discharge") %>%
                     mutate(rti = factor(rti, levels=c("CS","BV","CA","CT","TV","NG","MG","None")))) +
  geom_col(aes(x = est, y = reorder(rti, desc(rti)), fill = age_group), position = position_dodge2(0.7, reverse = TRUE), width = 0.7) +
  geom_linerange(aes(xmin = lwr, xmax = upr, y = reorder(rti, desc(rti)), group = age_group), 
                 position = position_dodge2(0.7, reverse = TRUE), size = 0.2) +
  geom_label(aes(x = upr + 0.004, y = rti, group = age_group, label = label), position = position_dodge2(0.7, reverse = TRUE),
             size = 6/.pt, label.size = NA, label.padding = unit(0.1,"lines"), hjust = 0, colour = colour_label) +
  theme_classic(base_size = 6) +
  my_theme() +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1), breaks=c(0,0.2,0.4,0.6,0.8,1),
                     expand = expansion(mult = c(0.02, 0.04))) +
  coord_cartesian(xlim = c(0, 1)) +   
  scale_fill_manual(values=colour_age) +
  labs(x="",y="Vaginal discharge",tag="A") 

plot_ud1 <- ggplot(df_est %>% filter(symptom == "Urethral discharge") %>%
                     mutate(rti = factor(rti, levels=c("NG","CT","MG","TV","None")))) +
  geom_col(aes(x = est, y = reorder(rti, desc(rti)), fill = age_group), position = position_dodge2(0.7, reverse = TRUE), width = 0.7) +
  geom_linerange(aes(xmin = lwr, xmax = upr, y = reorder(rti, desc(rti)), group = age_group), 
                 position = position_dodge2(0.7, reverse = TRUE), size = 0.2) +
  geom_label(aes(x = upr + 0.004, y = rti, group = age_group, label = label), position = position_dodge2(0.7, reverse = TRUE),
             size = 6/.pt, label.size = NA, label.padding = unit(0.1,"lines"), hjust = 0, colour = colour_label) +
  theme_classic(base_size = 6) +
  my_theme() +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1), breaks=c(0,0.2,0.4,0.6,0.8,1),
                     expand = expansion(mult = c(0.02, 0.04))) +
  coord_cartesian(xlim = c(0, 1)) +   
  scale_fill_manual(values=colour_age) +
  labs(x="",y="Urethral discharge",tag="B") 

plot_gu1 <- ggplot(df_est %>% filter(symptom == "Genital ulcer") %>%
                     mutate(rti = factor(rti, levels=c("HSV","HSV-2","TP","LGV","HSV-1","HD","None")))) +
  geom_col(aes(x = est, y = reorder(rti, desc(rti)), fill = age_group), position = position_dodge2(0.7, reverse = TRUE), width = 0.7) +
  geom_linerange(aes(xmin = lwr, xmax = upr, y = reorder(rti, desc(rti)), group = age_group), 
                 position = position_dodge2(0.7, reverse = TRUE), size = 0.2) +
  geom_label(aes(x = upr + 0.004, y = rti, group = age_group, label = label), position = position_dodge2(0.7, reverse = TRUE),
             size = 6/.pt, label.size = NA, label.padding = unit(0.1,"lines"), hjust = 0, colour = colour_label) +
  theme_classic(base_size = 6) +
  my_theme() +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1), breaks=c(0,0.2,0.4,0.6,0.8,1),
                     expand = expansion(mult = c(0.02, 0.04))) +
  coord_cartesian(xlim = c(0, 1)) +   
  scale_fill_manual(values=colour_age) +
  labs(x="Pooled diagnosed proportion",y="Genital ulcer",tag="C") 

plot_vd2 <- ggplot(df_or %>% filter(symptom == "Vaginal discharge") %>%
                     mutate(rti = factor(rti, levels=c("CS","BV","CA","CT","TV","NG","MG","None")))) +
  geom_pointrange(aes(x = or, xmin = lwr, xmax = upr, y = reorder(rti, desc(rti)), colour = variable), size = 0.06, linewidth = 0.2, fatten = 1.8) +
  geom_vline(xintercept = 1, linetype = "dotted", linewidth = 0.2) +
  geom_label(aes(x = or, y = rti, label = label), size = 6/.pt, label.size = NA, label.padding = unit(0.1,"cm"), hjust=0.48, nudge_y = 0.35, colour=colour_label) +
  theme_classic(base_size = 6) +
  my_theme() +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4),
                     expand = expansion(mult = c(0.02, 0.02))) +
  coord_trans(x = "log10", xlim = c(0.35,4)) +
  scale_colour_manual(values="black", labels="aOR (95% CI) for <25 years") +
  labs(x="",y="",colour="",fill="") 

plot_ud2 <- ggplot(df_or %>% filter(symptom == "Urethral discharge") %>%
                     mutate(rti = factor(rti, levels=c("NG","CT","MG","TV","None")))) +
  geom_pointrange(aes(x = or, xmin = lwr, xmax = upr, y = reorder(rti, desc(rti)), colour = variable), size = 0.06, linewidth = 0.2, fatten = 1.8) +
  geom_vline(xintercept = 1, linetype = "dotted", linewidth = 0.2) +
  geom_label(aes(x = or, y = rti, label = label), size = 6/.pt, label.size = NA, label.padding = unit(0.1,"cm"), hjust=0.48, nudge_y = 0.35, colour=colour_label) +
  theme_classic(base_size = 6) +
  my_theme() +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4),
                     expand = expansion(mult = c(0.02, 0.02))) +
  coord_trans(x = "log10", xlim = c(0.35,4)) +
  scale_colour_manual(values="black", labels="aOR (95% CI) for <25 years") +
  labs(x="",y="",colour="",fill="") 

plot_gu2 <- ggplot(df_or %>% filter(symptom == "Genital ulcer") %>%
                     mutate(rti = factor(rti, levels=c("HSV","HSV-2","TP","LGV","HSV-1","HD","None")))) +
  geom_pointrange(aes(x = or, xmin = lwr, xmax = upr, y = reorder(rti, desc(rti)), colour = variable), size = 0.06, linewidth = 0.2, fatten = 1.8) +
  geom_vline(xintercept = 1, linetype = "dotted", linewidth = 0.2) +
  geom_label(aes(x = or, y = rti, label = label), size = 6/.pt, label.size = NA, label.padding = unit(0.1,"cm"), hjust=0.48, nudge_y = 0.35, colour=colour_label) +
  theme_classic(base_size = 6) +
  my_theme() +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4),
                     expand = expansion(mult = c(0.02, 0.02))) +
  coord_trans(x = "log10", xlim = c(0.35,4)) +
  scale_colour_manual(values="black", labels="aOR (95% CI) for <25 years") +
  labs(x="Adjusted odds ratio (log scale)",y="",colour="",fill="") 

plot <- (plot_vd1 + plot_vd2 + plot_layout(widths = c(2,1))) / 
  (plot_ud1 + plot_ud2 + plot_layout(widths = c(2,1))) /
  (plot_gu1 + plot_gu2 + plot_layout(widths = c(2,1))) +
  plot_layout(heights = c(8,5,7), guides="collect") &
  theme(legend.position="bottom")

ggsave("./plots/figure_4.png", plot, width=16.2, height=19, units="cm", dpi=800)
ggsave("./plots/figure_4.tiff", plot, width=16.2, height=19, units="cm", dpi=300)

