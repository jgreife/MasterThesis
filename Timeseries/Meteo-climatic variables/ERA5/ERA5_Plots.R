library(rgdal)
library(exactextractr)
library(data.table)
library(sf)
library(dplyr)
library(RNetCDF)
library(raster)
library(ggplot2)
library(reshape2)
library(plyr)
library(readr)
library(ggpubr)
library(purrr)
gc()

#PAR: ERA5 Surface pressure
## Plotting the time series MONTHLY AGGREGATED Whole Time Period

####################

setwd("D:/Thesis/ERA5")

sp_m <- read.csv("D:/Thesis/ERA5/sp.csv")

sp_m$date <- as.Date(sp_m$date, origin="0000-01-01", format="%d/%m/%Y")
sp_m$Naivasha <- as.numeric(sp_m$Naivasha)
sp_m$year <- strftime(sp_m$date, "%Y")
sp_m$month <- strftime(sp_m$date, "%m")

#filter out extreme values (not necessary in tam data)
#tam <- tam %>% filter(0 < Naivasha & Naivasha < 150)

sp_aggr <- aggregate(Naivasha ~ month + year,
                      sp_m, 
                      FUN = mean)



sp_aggr$date <- as.POSIXct(paste(sp_aggr$year, sp_aggr$month, "01", sep = "-"))
sp_aggr$date <- as.Date(sp_aggr$date) + 1



sp_plot_m_whole <- ggplot(sp_aggr, aes(x=date, y=Naivasha)) + 
  geom_point() +
  #  geom_line() +
  geom_smooth() +
  theme_classic() +
  scale_x_date(limit=c(as.Date("1995-01-01"),as.Date("2023-12-31")), 
               breaks=c(as.Date("1995-01-01"), as.Date("2000-01-01"), as.Date("2005-01-01"), as.Date("2010-01-01"), as.Date("2015-01-01"), as.Date("2020-01-01"), as.Date("2025-01-01")),
               label=c("1995", "2000", "2005", "2010", "2015", "2020", "2025")) +
  scale_y_continuous(limits = c(77700, 81400), breaks = seq(0, 1100000, 250)) +
  labs(title="ERA5, Surface pressure: Lake Naivasha.", subtitle="Timeperiod: 1995-2022, monthly aggregation (mean).", x="Year", y=bquote('Surface pressure (mean)' ~ (Pa)))

sp_plot_m_whole


ggsave(sp_plot_m_whole, filename = paste0("D:/Thesis/ERA5/Naivasha/sp/Naivasha_monthly_Whole_Period", ".png"), device = "png", width = 9.375, height = 6.25, dpi = 300)
######################################
######### PER YEAR MONTHLY AGGREGATED

sp_plot_m_yearly <- ggplot(sp_aggr, aes(x=date, y=Naivasha)) + 
  geom_point() +
  #  geom_line() +
  geom_smooth() +
  theme_classic() +
  scale_x_date(limit=c(as.Date("2022-01-01"),as.Date("2022-12-31")),
               breaks=c(as.Date("2022-01-01"), as.Date("2022-02-01"), as.Date("2022-03-01"), as.Date("2022-04-01"), as.Date("2022-05-01"), as.Date("2022-06-01"), as.Date("2022-07-01"), as.Date("2022-08-01"), as.Date("2022-09-01"), as.Date("2022-10-01"), as.Date("2022-11-01"), as.Date("2022-12-01")),
               label=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  scale_y_continuous(limits = c(78400, 78850), breaks = seq(0, 150500, 50)) +
  labs(title="ERA5, Surface pressure: Lake Naivasha.", subtitle="Year: 2022, monthly aggregation (mean).", x="Month", y=bquote('Surface pressure (mean)' ~ (Pa)))

sp_plot_m_yearly

ggsave(sp_plot_m_yearly, filename = paste0("D:/Thesis/ERA5/Naivasha/sp/Naivasha_monthly_2022", ".png"), device = "png", width = 9.375, height = 6.25, dpi = 300)


## Plotting the time series daily
#Per Parameter per Lake
#PAR: tam tp daily per year

##############################
sp_d <- read.csv("D:/Thesis/ERA5/sp.csv")

sp_d$date <- as.Date(sp_d$date, origin="0000-01-01", format="%d/%m/%Y")
sp_d$Naivasha <- as.numeric(sp_d$Naivasha)

#filter out extreme values (not necessary in tam data)
#tam <- tam %>% filter(0 < Naivasha & Naivasha < 150)  

######## whole period

sp_plot_d_whole <- ggplot(sp_d, aes(x=date, y=Naivasha)) + 
  geom_point() +
  geom_line() +
  geom_smooth() +
  theme_classic() +
  scale_x_date(limit=c(as.Date("1995-01-01"),as.Date("2023-12-31")), 
               breaks=c(as.Date("1995-01-01"), as.Date("2000-01-01"), as.Date("2005-01-01"), as.Date("2010-01-01"), as.Date("2015-01-01"), as.Date("2020-01-01"), as.Date("2025-01-01")),
               label=c("1995", "2000", "2005", "2010", "2015", "2020", "2025")) +
  scale_y_continuous(limits = c(77700, 79200), breaks = seq(0, 200000, 100)) +
  labs(title="ERA5, Surface pressure: Lake Naivasha.", subtitle="Timeperiod: 1995-2022, daily.", x="Year", y=bquote('Surface pressure (mean)' ~ (Pa)))

sp_plot_d_whole

ggsave(sp_plot_d_whole, filename = paste0("D:/Thesis/ERA5/Naivasha/sp/Naivasha_whole_period_daily", ".png"), device = "png", width = 9.375, height = 6.25, dpi = 300)
######################

####### YEARLY

sp_plot_d_yearly <- ggplot(sp_d, aes(x=date, y=Naivasha)) + 
  geom_point() +
  geom_line() +
  geom_smooth() +
  theme_classic() +
  scale_x_date(limit=c(as.Date("2022-01-01"),as.Date("2022-12-31")),
               breaks=c(as.Date("2022-01-01"), as.Date("2022-02-01"), as.Date("2022-03-01"), as.Date("2022-04-01"), as.Date("2022-05-01"), as.Date("2022-06-01"), as.Date("2022-07-01"), as.Date("2022-08-01"), as.Date("2022-09-01"), as.Date("2022-10-01"), as.Date("2022-11-01"), as.Date("2022-12-01")),
               label=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  scale_y_continuous(limits = c(78400, 79000), breaks = seq(0, 960000, 100)) +
  labs(title="ERA5, Surface pressure: Lake Naivasha.", subtitle="Year: 2022, daily.", x="Months", y=bquote('Surface pressure (mean)' ~ (Pa)))

sp_plot_d_yearly

ggsave(sp_plot_d_yearly, filename = paste0("D:/Thesis/ERA5/Naivasha/sp/Naivasha_daily_2022", ".png"), device = "png", width = 9.375, height = 6.25, dpi = 300)


