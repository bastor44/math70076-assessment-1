
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



##### rolling mean number per day by age group
age_group_data <- age_group_data |>
  mutate(
    under_25_rolling=rolling_mean(Under_25, 7), 
    age_25_to_34_rolling=rolling_mean(age_25_to_34, 7), 
    age_35_to_44_rolling=rolling_mean(age_35_to_44, 7),
    age_45_to_54_rolling=rolling_mean(age_45_to_54, 7),
    age_55_to_64_rolling=rolling_mean(age_55_to_64, 7),
    age_65_to_74_rolling=rolling_mean(age_65_to_74, 7),
    over_75_rolling=rolling_mean(Over_75, 7)
  )


age_groups_long <- age_group_data |>
  select(Date, 
         under_25_rolling, 
         age_25_to_34_rolling,
         age_35_to_44_rolling,
         age_45_to_54_rolling,
         age_55_to_64_rolling,
         age_65_to_74_rolling,
         over_75_rolling) |>
  pivot_longer(cols=-Date,
               names_to="age_group", 
               values_to="registrations")

age_groups_long$age_group <- factor(age_groups_long$age_group, levels=c("under_25_rolling", "age_25_to_34_rolling",
                                                              "age_35_to_44_rolling", "age_45_to_54_rolling",
                                                              "age_55_to_64_rolling", "age_65_to_74_rolling",
                                                              "over_75_rolling"))


subtitle_text <- str_wrap("Average voter registrations for all age groups peaked 
                  before UK general and local elections in 2024")
title_text <- str_wrap("YOUNGEST VOTERS HAVE HIGHEST PEAK IN REGISTRATIONS")


#### Plot 
age_lines <- ggplot(age_groups_long, aes(x=Date, y=registrations, color=age_group)) +
  geom_line(lwd=1.2)+
  scale_color_manual(values=c(alpha(dmnred, 0.7), alpha(dmnblue, 0.7), 
                              alpha(dmnbeige,0.7), alpha(ocre,0.7), 
                              alpha(dmngreen,0.7), alpha(dmnorange,0.7),
                              alpha(dmnpink,0.7)),
                     labels=c("Under 25", "25 to 34", "35 to 44", "45 to 54", 
                              "55 to 64", "65 to 74", "Over 75")) +
  theme_minimal() +
  
  # axes
  labs(title=title_text,
        subtitle=subtitle_text,
        y="Number of Voter Registrations",
       color="Age Group") +
  theme(plot.title=element_text(family="Gotham Cond Bold", size=20, 
                                margin=margin(t=8, l=8, r=8, b=1), hjust=0), 
        plot.subtitle = element_text(family="Gotham Book", size=11, hjust=0),
        axis.text.x=element_text(family="Gotham Book", size=8, angle=45, 
                                 vjust=1, hjust=1),
        axis.text.y=element_text(family="Gotham Book", size=8)) +
  scale_x_date(breaks=seq(as.Date("2024-01-01"), as.Date("2025-01-01"), "2 months"),
               date_labels = "%b %Y") +
  
  # background
  geom_hline(aes(yintercept=0), color='black') +
  geom_vline(aes(xintercept=as.Date("2024-07-04")), 
             linetype="dashed", 
             color='grey70', lwd=1) +
  theme(plot.background = element_rect(fill="white", color="black"), 
        panel.background = element_rect(fill="white", color="white"),
        panel.grid.major=element_line(color='grey90'),
        panel.grid.minor.x=element_line(color='grey90')) +
  geom_hline(aes(yintercept=0), color='black') +
  
  # lines for election day 
  geom_vline(aes(xintercept=as.Date("2024-07-04")), 
             linetype="dashed", 
             color='grey70', lwd=.8) +
  geom_vline(aes(xintercept=as.Date("2024-05-02")),
             linetype="dashed",
             color="grey70", lwd=0.8)


## add points to ends of lines
age_group_points <- data.frame(age_group=c("Under_25", "age_25_to_34",
                                           "age_35_to_44", "age_45_to_54", 
                                           "age_55_to_64", "age_65_to_74", 
                                           "Over_75"),
                               count_start=unlist(slice(age_group_data, 4)[2:8]),
                               count_end=unlist(slice(age_group_data, 364)[2:8]))
age_group_points$age_group <- factor(age_group_points$age_group, levels=c("Under_25", "age_25_to_34",
                                                                          "age_35_to_44", "age_45_to_54", 
                                                                          "age_55_to_64", "age_65_to_74", 
                                                                          "Over_75"))
age_group_points$age_group <- droplevels(age_group_points$age_group)

age_lines <- age_lines + geom_point(aes(x=as.Date("2024-01-04"), y=age_group_points$count_start[1]),
                       color=dmnred, shape=21, size=3, stroke=1.5, fill="white") +
  geom_point(aes(x=as.Date("2024-01-04"), y=age_group_points$count_start[2]),
             color=dmnblue, shape=21, size=3, stroke=1.5, fill="white") +
  geom_point(aes(x=as.Date("2024-01-04"), y=age_group_points$count_start[3]),
            color=dmnbeige, shape=21, size=3, stroke=1.5, fill="white") +
  geom_point(aes(x=as.Date("2024-01-04"), y=age_group_points$count_start[4]),
             color=ocre, shape=21, size=3, stroke=1.5, fill="white")+
  geom_point(aes(x=as.Date("2024-01-04"), y=age_group_points$count_start[5]),
             color=dmngreen, shape=21, size=3, stroke=1.5, fill="white") +
  geom_point(aes(x=as.Date("2024-01-04"), y=age_group_points$count_start[6]),
             color=dmnorange, shape=21, size=3, stroke=1.5, fill="white") +
  geom_point(aes(x=as.Date("2024-01-04"), y=age_group_points$count_start[7]),
             color=dmnpink, shape=21, size=3, stroke=1.5, fill="white") +
  geom_point(aes(x=as.Date("2024-12-28"), y=age_group_points$count_end[1]),
             color=dmnred, shape=21, size=3, stroke=1.5, fill="white") +
  geom_point(aes(x=as.Date("2024-12-28"), y=age_group_points$count_end[2]),
             color=dmnblue, shape=21, size=3, stroke=1.5, fill="white") +
  geom_point(aes(x=as.Date("2024-12-28"), y=age_group_points$count_end[3]),
             color=dmnbeige, shape=21, size=3, stroke=1.5, fill="white") +
  geom_point(aes(x=as.Date("2024-12-28"), y=age_group_points$count_end[4]),
             color=ocre, shape=21, size=3, stroke=1.5, fill="white")+
  geom_point(aes(x=as.Date("2024-12-28"), y=age_group_points$count_end[5]),
             color=dmngreen, shape=21, size=3, stroke=1.5, fill="white") +
  geom_point(aes(x=as.Date("2024-12-28"), y=age_group_points$count_end[6]),
             color=dmnorange, shape=21, size=3, stroke=1.5, fill="white") +
  geom_point(aes(x=as.Date("2024-12-28"), y=age_group_points$count_end[7]),
             color=dmnpink, shape=21, size=3, stroke=1.5, fill="white")


## add annotations
age_lines <- age_lines + 
  annotate("label", x=as.Date("2024-08-01"), y=30000, 
                                  label="4 July 2024 \n UK General Election", 
                                  color="white", fill="black", label.size=0, 
                                  hjust=0, family="Gotham Bold", size=3) +
  annotate("segment", x=as.Date("2024-07-04"), xend=as.Date("2024-08-01"),
           y=30000, yend=30000, color='black') +
  annotate("segment", x=as.Date("2024-04-01"), xend=as.Date("2024-05-02"),
           y=55000, yend=55000,
           color='black') +
  annotate("label", x=as.Date("2024-02-26"), y=55000,
           label="02 May 2024 \n Local Elections",
           color="white", 
           fill="black",
           label.size=0, 
           hjust=0, 
           family="Gotham Bold", size=3)


## save image 
ggsave("age_lines.jpeg", plot=age_lines, path="outputs", width=4, height=3, dpi=300)
