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


#PAR: tamsat total precipitation
## Plotting the time series MONTHLY AGGREGATED Whole Time Period

####################

setwd("D:/Thesis/TAMSAT")

tam_m <- read.csv("D:/Thesis/TAMSAT/TAMSAT_6_Lakes.csv")

tam_m$date <- as.Date(tam_m$date, origin="0000-01-01", format="%d/%m/%Y")
tam_m$Kariba <- as.numeric(tam_m$Kariba)
tam_m$year <- strftime(tam_m$date, "%Y")
tam_m$month <- strftime(tam_m$date, "%m")

#filter out extreme values (not necessary in tam data)
#tam <- tam %>% filter(0 < Kariba & Kariba < 150)
#######################
tam_aggr <- aggregate(Kariba ~ month + year,
                      tam_m, 
                      FUN = mean)



tam_aggr$date <- as.POSIXct(paste(tam_aggr$year, tam_aggr$month, "01", sep = "-"))
tam_aggr$date <- as.Date(tam_aggr$date) + 1



tam_plot_m_whole <- ggplot(tam_aggr, aes(x=date, y=Kariba)) + 
  geom_point() +
  #  geom_line() +
  geom_smooth() +
  theme_classic() +
  scale_x_date(limit=c(as.Date("1983-01-01"),as.Date("2023-12-31")), 
               breaks=c(as.Date("1985-01-01"), as.Date("1990-01-01"), as.Date("1995-01-01"), as.Date("2000-01-01"), as.Date("2005-01-01"), as.Date("2010-01-01"), as.Date("2015-01-01"), as.Date("2020-01-01")),
               label=c("1985", "1990", "1995", "2000", "2005", "2010", "2015", "2020")) +
  scale_y_continuous(limits = c(0, 13), breaks = seq(0, 500, 1)) +
  labs(title="TAMSAT Rainfall Estimates: Lake Kariba.", subtitle="Timeperiod: 1983-2023, monthly aggregation (mean).", x="Year", y="Rainfall Estimate (mm)")

tam_plot_m_whole

ggsave(tam_plot_m_whole, filename = paste0("D:/Thesis/TAMSAT/Kariba/Kariba_monthly_Whole_Period", ".png"), device = "png", width = 9.375, height = 6.25, dpi = 300)
######################################
######### PER YEAR MONTHLY AGGREGATED

tam_plot_m_yearly <- ggplot(tam_aggr, aes(x=date, y=Kariba)) + 
  geom_point() +
  #  geom_line() +
  geom_smooth() +
  theme_classic() +
  scale_x_date(limit=c(as.Date("2018-01-01"),as.Date("2018-12-31")),
               breaks=c(as.Date("2018-01-01"), as.Date("2018-02-01"), as.Date("2018-03-01"), as.Date("2018-04-01"), as.Date("2018-05-01"), as.Date("2018-06-01"), as.Date("2018-07-01"), as.Date("2018-08-01"), as.Date("2018-09-01"), as.Date("2018-10-01"), as.Date("2018-11-01"), as.Date("2018-12-01")),
               label=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  scale_y_continuous(limits = c(0, 8.5), breaks = seq(0, 150, 0.5)) +
  labs(title="TAMSAT Rainfall Estimates: Lake Kariba.", subtitle="Year: 2018, monthly aggregation (mean).", x="Month", y="Rainfall Estimate (mm)")

tam_plot_m_yearly

ggsave(tam_plot_m_yearly, filename = paste0("D:/Thesis/TAMSAT/Kariba/Kariba_monthly_2018", ".png"), device = "png", width = 9.375, height = 6.25, dpi = 300)


## Plotting the time series daily
#Per Parameter per Lake
#PAR: tam tp daily per year

##############################
tam_d <- read.csv("D:/Thesis/TAMSAT/TAMSAT_6_Lakes.csv")

tam_d$date <- as.Date(tam_d$date, origin="0000-01-01", format="%d/%m/%Y")
tam_d$Kariba <- as.numeric(tam_d$Kariba)

#filter out extreme values (not necessary in tam data)
#tam <- tam %>% filter(0 < Kariba & Kariba < 150)  

######## whole period

tam_plot_d_whole <- ggplot(tam_d, aes(x=date, y=Kariba)) + 
  geom_point() +
  geom_line() +
  geom_smooth() +
  theme_classic() +
  scale_x_date(limit=c(as.Date("1983-01-01"),as.Date("2023-12-31")), 
               breaks=c(as.Date("1985-01-01"), as.Date("1990-01-01"), as.Date("1995-01-01"), as.Date("2000-01-01"), as.Date("2005-01-01"), as.Date("2010-01-01"), as.Date("2015-01-01"), as.Date("2020-01-01")),
               label=c("1985", "1990", "1995", "2000", "2005", "2010", "2015", "2020")) +
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 500, 5)) +
  labs(title="TAMSAT Rainfall Estimates: Lake Kariba.", subtitle="Timeperiod: 1983-2023, daily.", x="Year", y="Rainfall Estimate (mm)")

tam_plot_d_whole

ggsave(tam_plot_d_whole, filename = paste0("D:/Thesis/TAMSAT/Kariba/Kariba_whole_period_daily", ".png"), device = "png", width = 9.375, height = 6.25, dpi = 300)
######################

####### YEARLY

tam_plot_d_yearly <- ggplot(tam_d, aes(x=date, y=Kariba)) + 
  geom_point() +
  geom_line() +
  geom_smooth() +
  theme_classic() +
  scale_x_date(limit=c(as.Date("2018-01-01"),as.Date("2018-12-31")),
               breaks=c(as.Date("2018-01-01"), as.Date("2018-02-01"), as.Date("2018-03-01"), as.Date("2018-04-01"), as.Date("2018-05-01"), as.Date("2018-06-01"), as.Date("2018-07-01"), as.Date("2018-08-01"), as.Date("2018-09-01"), as.Date("2018-10-01"), as.Date("2018-11-01"), as.Date("2018-12-01")),
               label=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  scale_y_continuous(limits = c(0, 27.5), breaks = seq(0, 500, 2.5)) +
  labs(title="TAMSAT Rainfall Estimates: Lake Kariba.", subtitle="Year: 2018, daily.", x="Months", y="Rainfall Estimate (mm)")

tam_plot_d_yearly

ggsave(tam_plot_d_yearly, filename = paste0("D:/Thesis/TAMSAT/Kariba/Kariba_daily_2018", ".png"), device = "png", width = 9.375, height = 6.25, dpi = 300)


