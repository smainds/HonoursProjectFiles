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

ptitle <- "- STL Decomposition by "
places <- c("Camera  308 Murraygate","Camera 310 - Seagate","Camera 317 Reform St",
            "Camera 320 Westport","Camera 323 Union Street",
            "Camera 328 South Marketgait","Camera 332 Waterfront",
            "Camera 500 Hilltown")
traffictype <- c("F__of_Bicycles", "F__of_People","F__of_Road_Vehicles")

cameras <- c("Murraygate",
             "Seagate",
             "Reform St",
             "Westport",
             "Union Street",
             "South Marketgait",
             "Waterfront",
             "Hilltown")

traffictypenoF <- c("Bicycles",
                    "Pedestrians",
                    "Road Vehicles")

i <- 3       #location (1 = Murraygate, 2 = Seagate, ..., 8 = Hilltown)
j <- 3       #traffic type - (1 = bikes, 2 = pedestrians, 3 = road traffic)

March2024 <- read_excel("C:/Users/smain/Desktop/newMarch2024.xlsx")
March2024$Date <- ymd(ymd_hms(March2024$Date))
March2024 <- March2024 |> separate(Hour, into = c('interval_start', 
                                                  'interval_end'), sep = '-')

March2024$interval_start <- hm(March2024$interval_start)

March2024$datetime <- March2024$Date + March2024$interval_start

bycamera <- filter(March2024, Source == places[i])
bycameralong <- bycamera |>  
  pivot_longer(traffictype) |> filter(name == traffictype[j]) |>
  dplyr::select(value, datetime)
    
# lines 51-62 fill in any missing values in the time series

full_date_df <- data.frame(
  datetime = seq.POSIXt(min(bycameralong$datetime), max(bycameralong$datetime), by = "hour")
)
newbycameralong <- merge(full_date_df, bycameralong, by = "datetime", all.x = TRUE)
newbycameralong <- newbycameralong[!duplicated(newbycameralong$datetime),]
newbycameralong$value <- na.approx(newbycameralong$value)

funinsert <- function(x, pos, insert) {
  gsub(paste0("(.{", pos, "})(.*)$"),
       paste0("\\1", insert, " \\2"),
       x)
}
ptitlenew1 <- funinsert(x = ptitle, pos = 23, insert = traffictypenoF[j])
ptitlenew2 <- funinsert(x = ptitlenew1, pos = 0, insert = cameras[i])

# msts function - seasonal.periods could used more than one period at once (e.g. daily and weekly) but only focus on one period just now

z <- msts(newbycameralong$value, seasonal.periods = c(1*24), 
          start = 1) # change 1*24 to 7*24 if wish to study weekly seasonality instead of daily

mstl_out <- mstl(z)
mstl_out |> autoplot() + ggtitle(ptitlenew2) + xlab("Day")
