library(tidyverse)
library(readxl)
library(stats)
library(scales)

#Graphs

#Shiller Index Nationally
nat <- read_xlsx(here::here("Original Graph Location","natindex.xlsx"))
nat$Year = as.Date(nat$Year, format = "%m/%d/%Y")


ggplot(nat) +
  geom_line(aes(x=Year, y= Index_Price), size = 1.5) +
  labs(title= "Shiller National Price Index",
       caption = "Source: St. Louis Federal Reserve FRED") +
  scale_y_continuous(limits = c(50,350), breaks = seq(50, 350,50)) +
  scale_x_date(date_breaks = "3 year", labels = date_format("%Y")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color= "black"),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size = 12, color = "black", family = "serif"),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 16, color = "black", family = "serif"),
        plot.title = element_text(family = "serif", size = 24, hjust = .5),
        plot.caption = element_text(family = "serif", size = 12, hjust = .01),
        panel.border = element_rect(fill=NA),
        plot.background = element_rect(color="black"),
        legend.position = "none") 

#Shiller Index for Locations
mult <- read_xlsx(here::here("Original Graph Location","multiindex.xlsx"))
mult$Year = as.Date(mult$Year, format = "%m/%d/%Y")

ggplot(mult) +
  geom_line(aes(x=Year, y= Index_Price, group= Location, color = Location), size = 1.5) +
  labs(title= "Shiller National Price Index for Select Locations",
       caption = "Source: St. Louis Federal Reserve FRED") +
  scale_y_continuous(limits = c(50,450), breaks = seq(50, 450,50)) +
  scale_x_date(date_breaks = "3 year", labels = date_format("%Y")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color= "black"),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size = 12, color = "black", family = "serif"),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 16, color = "black", family = "serif"),
        plot.title = element_text(family = "serif", size = 24, hjust = .5),
        plot.caption = element_text(family = "serif", size = 12, hjust = .01),
        panel.border = element_rect(fill=NA),
        plot.background = element_rect(color="black"),
        legend.position = "bottom",
        legend.text=element_text(size=12, family = "serif")) +
scale_color_manual(values = c("red", "blue", "#194a12", "#ebe010", "black"))

#Homeownership Rate
owner <- read_xlsx(here::here("Original Graph Location","homeowner.xlsx"))
owner$Year = as.Date(owner$Year, format = "%m/%d/%Y")


ggplot(owner) +
  geom_line(aes(x=Year, y= Homeownership_Rate), size = 1.5) +
  labs(title= "Homeownership Rate in the United States",
       caption = "Source: St. Louis Federal Reserve FRED") +
  scale_y_continuous(limits = c(62,70), breaks = seq(62, 70,1)) +
  scale_x_date(date_breaks = "3 year", labels = date_format("%Y")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color= "black"),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size = 12, color = "black", family = "serif"),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 16, color = "black", family = "serif"),
        plot.title = element_text(family = "serif", size = 24, hjust = .5),
        plot.caption = element_text(family = "serif", size = 12, hjust = .01),
        panel.border = element_rect(fill=NA),
        plot.background = element_rect(color="black"),
        legend.position = "none") 


#Shiller Index Percent Change
homepct <- read_xlsx(here::here("Original Graph Location","indexpct.xlsx"))
homepct$Year = as.Date(homepct$Year, format = "%m/%d/%Y")


ggplot(homepct) +
  geom_line(aes(x=Year, y= Percent_Change_from_Year_Prior), size = 1.5) +
  labs(title= "Shiller National Price Index Percent Change from Year Prior",
       caption = "Source: St. Louis Federal Reserve FRED") +
  scale_y_continuous(limits = c(-15,15), breaks = seq(-15, 15,3)) +
  scale_x_date(date_breaks = "3 year", labels = date_format("%Y")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color= "black"),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size = 12, color = "black", family = "serif"),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 16, color = "black", family = "serif"),
        plot.title = element_text(family = "serif", size = 22, hjust = .5),
        plot.caption = element_text(family = "serif", size = 12, hjust = .01),
        panel.border = element_rect(fill=NA),
        plot.background = element_rect(color="black"),
        legend.position = "none") 

#Housing Investment
invest <- read_xlsx(here::here("Original Graph Location","invest.xlsx")) %>%
  mutate(Investor_Market_Share = Investor_Market_Shar *100)
invest$Year = as.Date(invest$Year, format = "%m/%d/%Y")

ggplot(invest) +
  geom_line(aes(x=Year, y = Investor_Market_Share, group = Metro, color = Metro), size = 1.5) +
  labs(title= "Investor Market Share",
       caption = "Source: Redfin Data Center") +
  scale_y_continuous(limits = c(0,50), breaks = seq(0, 50,5)) +
  scale_x_date(date_breaks = "3 year", labels = date_format("%Y")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color= "black"),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size = 12, color = "black", family = "serif"),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 16, color = "black", family = "serif"),
        plot.title = element_text(family = "serif", size = 24, hjust = .5),
        plot.caption = element_text(family = "serif", size = 12, hjust = .01),
        panel.border = element_rect(fill=NA),
        plot.background = element_rect(color="black"),
        legend.position = "bottom") +
  scale_color_manual(values = c("red", "blue", "#194a12", "black"))

#Sold Above
above <- read_xlsx(here::here("Original Graph Location","soldabove.xlsx")) %>%
  mutate(Percent = Percen *100)
above$Year = as.Date(above$Year, format = "%m/%d/%Y")

ggplot(above) +
  geom_line(aes(x=Year, y = Percent, group = Type, color = Type), size = 1.5) +
  labs(title= "Percent of Homes Sold Above List Price",
       caption = "Source: Redfin Data Center") +
  scale_y_continuous(limits = c(10,60), breaks = seq(10, 60,5)) +
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color= "black"),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size = 12, color = "black", family = "serif"),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 16, color = "black", family = "serif"),
        plot.title = element_text(family = "serif", size = 24, hjust = .5),
        plot.caption = element_text(family = "serif", size = 12, hjust = .01),
        panel.border = element_rect(fill=NA),
        plot.background = element_rect(color="black"),
        legend.position = "bottom") +
  scale_color_manual(values = c("red", "blue"))

#Sold Above
days <- read_xlsx(here::here("Original Graph Location","days.xlsx"))
days$Year = as.Date(days$Year, format = "%m/%d/%Y")

ggplot(days) +
  geom_line(aes(x=Year, y = Days_on_Market, group = Type, color = Type), size = 1.5) +
  labs(title= "Average Number of Days on the Market",
       caption = "Source: Redfin Data Center") +
  scale_y_continuous(limits = c(10,140), breaks = seq(10, 140,20)) +
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color= "black"),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size = 12, color = "black", family = "serif"),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 16, color = "black", family = "serif"),
        plot.title = element_text(family = "serif", size = 24, hjust = .5),
        plot.caption = element_text(family = "serif", size = 12, hjust = .01),
        panel.border = element_rect(fill=NA),
        plot.background = element_rect(color="black"),
        legend.position = "bottom") +
  scale_color_manual(values = c("red", "blue"))

#Inventory
inventory <- read_xlsx(here::here("Original Graph Location","inventory.xlsx"))
inventory$Year = as.Date(inventory$Year, format = "%m/%d/%Y")

ggplot(inventory) +
  geom_line(aes(x=Year, y = Inventory, group = Type, color = Type), size = 1.5) +
  labs(title= "Housing Inventory",
       caption = "Source: Redfin Data Center") +
  scale_y_continuous(limits = c(0,2000000), breaks = seq(0, 2000000,200000)) +
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color= "black"),
        panel.background = element_rect(fill="white"),
        axis.text = element_text(size = 12, color = "black", family = "serif"),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 16, color = "black", family = "serif"),
        plot.title = element_text(family = "serif", size = 24, hjust = .5),
        plot.caption = element_text(family = "serif", size = 12, hjust = .01),
        panel.border = element_rect(fill=NA),
        plot.background = element_rect(color="black"),
        legend.position = "bottom") +
  scale_color_manual(values = c("red", "blue"))
