# Number of studies per country by symptom

# Load packages
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(readxl)
library(patchwork)

# Load and prepare data

df_study <- read_excel("./data/appendix_studydata.xlsx", sheet='Study data') %>%
  mutate(symptom = factor(symptom, levels=c("VD","UD","GU"), labels=c("Vaginal discharge","Urethral discharge","Genital ulcer"))) %>%
  # assign study variable for publications from the same study
  mutate(study = case_when(
    unique_id %in% c(1247,1654) ~ "study_1",
    unique_id %in% c(1211,1305) ~ "study_2",
    unique_id %in% c(1423,1429) ~ "study_3",
    unique_id %in% c(998,1111) ~ "study_4",
    unique_id %in% c(1632,3715) ~ "study_5",
    unique_id %in% c(2449,2470) ~ "study_6",
    unique_id %in% c(1208,1248) ~ "study_7", # GHWP 
    unique_id %in% c(1042,1240,1444,1469,1619) ~ "study_8", # RCT acyclovir
    unique_id %in% c(368,473,497,508,509) ~ "study_9", # Zimbabwe etiology study
    TRUE ~ NA)) 

africa <- ne_countries(scale = "medium", type = "countries", continent = "Africa", returnclass = "sf") %>%
  filter(region_wb == "Sub-Saharan Africa") %>%
  # Rename countries to match
  mutate(admin = case_when(admin == "Ivory Coast" ~ "Côte d'Ivoire",
                           admin == "Democratic Republic of the Congo" ~ "Democratic Republic of Congo",
                           admin == "Republic of the Congo" ~ "Congo",
                           admin == "Gambia" ~ "The Gambia",
                           admin == "eSwatini" ~ "Eswatini",
                           admin == "United Republic of Tanzania" ~ "Tanzania",
                           admin == "São Tomé and Principe" ~ "Sao Tome and Principe",
                           TRUE ~ admin)) %>%
  select(admin, geometry)

df_country <- africa %>%
  crossing(data.frame(symptom = c("Vaginal discharge","Urethral discharge","Genital ulcer"))) %>%
  mutate(symptom = factor(symptom, levels = c("Vaginal discharge","Urethral discharge","Genital ulcer"))) %>%
  left_join(
    # Calculate number of unique studies per country
    df_study %>%
      group_by(symptom,study,country) %>%
      summarise(n_id = n_distinct(unique_id)) %>%
      mutate(n_study = case_when(!is.na(study) ~ 1, is.na(study) ~ n_id)) %>%
      # Separate studies across multiple countries (double count)
      mutate(country = strsplit(country, ", ")) %>%
      unnest(country) %>%
      group_by(symptom, country) %>%
      summarise(n_study = sum(n_study)) %>%
      ungroup(),
    by = c("admin" = "country", "symptom")) 
  
my_theme <- function(){
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # move legend closer to figure
        legend.margin = margin(10,10,10,30),
        legend.key.width = unit(0.6, 'cm'),
        legend.key.height = unit(0.4, "cm"),
        #legend.box = 'horizontal',
        legend.position = "bottom",
        #axis.line = element_line(colour="black"),
        panel.spacing = unit(0.2,"cm"),
        # text size
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1.1), face="bold"),
        #change facet labels
        strip.text = element_text(color="black", size = rel(1.1), face="bold",vjust=1.5), 
        # axis ticks
        axis.ticks = element_blank(),
        # change facet label background and border
        strip.background = element_blank())
}

plot_country <- ggplot(df_country) +
  geom_sf(aes(fill = n_study, geometry = geometry)) +
  facet_wrap(~symptom) +
  scale_fill_viridis_c(direction=-1, breaks = c(0,2,4,6,8,10,12,14,16,18), na.value="lightgrey") +
  theme_minimal(base_size=8) +
  my_theme() +
  labs(fill="Number of \n studies")

ggsave("./plots/figure_A.png", plot_country, width = 18.5, units="cm", dpi=700)

