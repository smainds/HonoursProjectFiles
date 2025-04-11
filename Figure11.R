library(tidyverse)
library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(lubridate)
library(readr)
library(sf)
library(forecast)
library(zoo)
library(cowplot)

places <- c("Camera  308 Murraygate","Camera 310 - Seagate","Camera 317 Reform St",
            "Camera 320 Westport","Camera 323 Union Street",
            "Camera 328 South Marketgait","Camera 332 Waterfront",
            "Camera 500 Hilltown")
cameras <- c("Murraygate","Seagate",
             "Reform St",
             "Westport",
             "Union Street",
             "South Marketgait",
             "Waterfront",
             "Hilltown")
traffictype <- c("F__of_Bicycles", "F__of_People","F__of_Road_Vehicles")

March2024 <- read_excel("C:/Users/smain/Desktop/newMarch2024.xlsx")
March2024$Date <- ymd(ymd_hms(March2024$Date))
March2024 <- March2024 |> separate(Hour, into = c('interval_start', 
                                                  'interval_end'), sep = '-')

March2024$interval_start <- hm(March2024$interval_start)

March2024$datetime <- March2024$Date + March2024$interval_start

s <- data.frame(matrix(nrow = 744, ncol = 8))

for (i in 1:8){
  
  location <- filter(March2024, Source == places[i])
  ped <- location |>  
    pivot_longer(traffictype) |> filter(name == "F__of_People") |>
    dplyr::select(value, datetime)
  
  full_date_df <- data.frame(
    datetime = seq.POSIXt(min(ped$datetime), max(ped$datetime), by = "hour")
  )
  newped <- merge(full_date_df, ped, by = "datetime", all.x = TRUE)
  newped <- newped[!duplicated(newped$datetime),]
  newped$value <- na.approx(newped$value)
  
  z <- msts(newped$value, seasonal.periods = c(7*24), 
            start = 1) 
  
  mstl_out <- mstl(z)
  
  
  s[i] <- data.frame(mstl_out) |> dplyr::select(Trend)
}

plot_full_date <- data.frame(Time = seq.int(1,744))

q <- data.frame(c(plot_full_date,s))

# remove lines 66-68 if don't wish normalised curves (and update plot title accordingly)

for (i in 2:9){
  q[[i]] <- q[[i]] - mean(q[[i]])
}

for (i in 1:8){
  names(q)[i+1] <- cameras[i]
}

qnew <- q |>  
  pivot_longer(cameras)

names(qnew)[2] <- "Location"

p <- ggplot(qnew,aes(x=Time,y=value,col=Location)) + geom_line(size=1) + 
  xlab("Hour") + ylab("Trend Average Value") +
  ggtitle("Trend Component of STL Decomposition for Pedestrian Counts") + 
  theme(legend.key.size = unit(1, 'cm'), legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))
p
