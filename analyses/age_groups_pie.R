
##### Load Libraries 
library(ggplot2)
library(showtext)
library(extrafont)
library(sysfonts)
library(stringr)
library(tidyverse)


##### Load data, functions, colours, and fonts 
age_group_data <- readRDS("data/age_groups_2024.rds")
daily_data_2024 <- readRDS("data/daily_data_2024.rds")

source("R/colour_palette.R")
source("R/rolling_mean.R")
source("R/pad_with_NAs.R")

font_add("Gotham Bold", "resources/Gotham-Bold.otf")
font_add("Gotham BoldItalic", "resources/Gotham-BoldItalic.otf")
font_add("Gotham Book", "resources/Gotham-Book.otf")
font_add("Gotham Cond Bold", "resources/GothamCond-Bold.otf")
showtext_auto()



##### Pie Chart of Age Groups
age_groups <- data.frame(age_group=colnames(age_group_data[,-1]),
                             total=NA)
age_group_totals <- sapply(age_group_data[,-1], MARGIN=2, FUN=sum)
age_groups$total <- age_group_totals
overall_total <- sum(age_group_totals)
age_groups$perc <- round(age_group_totals/overall_total, 2)

age_groups$age_group <- factor(age_groups$age_group, levels=c("Under_25", "age_25_to_34",
                                                              "age_35_to_44", "age_45_to_54",
                                                              "age_55_to_64", "age_65_to_74",
                                                              "Over_75"))

pie <- ggplot(age_groups, aes(x="", y=perc)) +
  geom_bar(stat="identity", aes(fill=age_group), color="white", width=0.5) +
  theme_void() +
  labs(title="YOUNG VOTERS MAKE UP MAJORITY OF \n NEW VOTER REGISTRATIONS",
       subtitle="Percentage of 2024 Voter Registrations by \n Age Group",
       x="", y="",
       fill="Age Group") +
  theme(plot.title=element_text(family="Gotham Cond Bold", size=20, 
                                margin=margin(t=8, l=8, r=8, b=8)),
        plot.subtitle = element_text(family="Gotham Book", size=11, hjust=0,
                                     margin=margin(8,8,8,8))) +
  coord_polar(theta="y", ) +
  scale_fill_manual(values=c(country, land, park, silver, 
                               neighbour, dmnpink, dmnbeige),
                    labels=c("Under 25", "25 to 34", "35 to 44", "45 to 54", 
                             "55 to 64", "65 to 74", "Over 75")) +
  theme(panel.border=element_blank(),
        plot.background=element_rect(fill="white", color="black"),
        legend.margin = margin(l=3, r=20,8,8),
        legend.title=element_text(size=8),
        legend.text=element_text(size=8))




## add annotations
age_groups$percentage <- age_groups$perc * 100
labels<-c("Under 25 \n (25%)", "25 to 34 \n (31%)", "35 to 44 \n (18%)", "45 to 54 \n (11%)", 
         "55 to 64 \n (8%)", "65 to 74 (5%)", "Over 75 (3%)")


age_groups <- age_groups |>
  mutate(
    cumulative_value = cumsum(age_groups$perc),
    ypos = 1 - (cumulative_value - age_groups$perc / 2), 
    
    angle_start = 2*pi*c(0, cumulative_value[-7])
  )



pie <- pie + geom_text(data=age_groups, mapping=aes(label=labels,
                    x=1.5,
                    y=ypos),
                color='black',
                size=3,
                hjust=0.5,
                family="Gotham Book")

## save image 
ggsave("age_groups_pie_chart.pdf", pie, path="outputs", width=3.5, height=3.5)
