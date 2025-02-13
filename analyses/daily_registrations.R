
##### Load Libraries 
library(ggplot2)
library(showtext)
library(extrafont)
library(sysfonts)
library(stringr)


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



##### Graphic matching all DMN style guidelines 
daily_data_2024$rolling_mean <- rolling_mean(daily_data_2024$total, window_width=7)

subtitle_text <- str_wrap("Voter registration numbers peaked before local and general 
       elections in April and July 2024")

# plot with correct aesthetics 
p <- ggplot(daily_data_2024, aes(x=Date, y=rolling_mean)) +
  geom_line(color=dmnred, lwd=1.3) +
  labs(title="7 DAY AVERAGE VOTER REGISTRATION PEAKS BEFORE GENERAL ELECTION", 
       subtitle=subtitle_text,
       y="Number of Voter Registrations",
       color="Type") +
  theme(plot.title=element_text(family="Gotham Cond Bold", size=20), 
        plot.subtitle = element_text(family="Gotham Book", size=11, hjust=0),
        axis.text.x=element_text(family="Gotham Book", size=8, angle=45, 
                                 vjust=1, hjust=1),
        axis.text.y=element_text(family="Gotham Book", size=8)) +
  scale_x_date(breaks=seq(as.Date("2024-01-01"), as.Date("2025-01-01"), "2 months"),
               date_labels = "%b %Y")+
  geom_point(daily_data_2024[c(4, 363), ], mapping=aes(x=Date, y=rolling_mean),
             shape=21, size=3, stroke=1.5, color=dmnred, fill="white") +
  theme(plot.background = element_rect(fill="white", color="black"), 
        panel.background = element_rect(fill="white"),
        panel.grid.major=element_line(color='grey90'),
        panel.grid.minor.x=element_line(color='grey90')) +
  geom_hline(aes(yintercept=0), color='black') +
  geom_vline(aes(xintercept=as.Date("2024-07-04")), 
             linetype="dashed", 
             color='grey70', lwd=1) 


##### Add Annotations
# Election Dates
p <- p + annotate("label", x=as.Date("2024-08-01"), y=160000, 
                  label="4 July 2024 \n UK General Election", 
                  color="white", fill="black", label.size=0, 
                  hjust=0, family="Gotham Bold", size=3) +
  annotate("segment", x=as.Date("2024-07-04"), xend=as.Date("2024-08-01"),
           y=160000, yend=160000, color='black') +
  
  # local elections
  geom_vline(aes(xintercept=as.Date("2024-05-02")),
             linetype="dashed",
             color="grey70", lwd=1) +
  annotate("segment", x=as.Date("2024-04-01"), xend=as.Date("2024-05-02"),
           y=200000, yend=200000,
           color='black') +
  annotate("label", x=as.Date("2024-02-26"), y=200000,
           label="02 May 2024 \n Local Elections",
           color="white", 
           fill="black",
           label.size=0, 
           hjust=0, 
           family="Gotham Bold", size=3)


# Add Daily Registration Counts
p <- p + 
  geom_line(aes(x=Date, y=total),
            color=alpha(dmnpink, 0.5),
            lwd=1) +
  scale_y_continuous(limits=c(0, 633000))


# peak number of registrations
max_date <- daily_data_2024$Date[which.max(daily_data_2024$total)]
max_count <- daily_data_2024$total[which.max(daily_data_2024$total)]

p <- p + annotate("label", x=max_date+30, y=max_count-15000,
                  label="18 June 2024 \n Peak Number of \n Registrations",
                  color='white', fill='black', label.size=0,
                  hjust=0, family="Gotham Bold", size=3) +
  annotate("segment", x=max_date, y=max_count,
           xend=max_date+30, yend=max_count, 
           color='black')




# Add Caption
caption_text <- str_wrap("Note: Weekly rolling average of total number of voter registrations 
                in the UK from January 2024 to December 2024 are shown in red. Daily voter registration
                numbers are shown in pink. Local and general 
                election dates are indicated by grey dashed lines. SOURCE: UK Register to vote - performance dashboard")
p <- p + 
  labs(caption=caption_text) +
  theme(plot.caption = element_text(family = "Gotham Book", size=8, hjust=0))


ggsave("daily_registrations_plot.jpeg", plot=age_lines, path="outputs", width=7, height=4, dpi=300)
