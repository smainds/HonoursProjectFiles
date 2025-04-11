library(tidyverse)
library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)

place <- "Camera 317 Reform St"       # change this to the location you wish 
                                      # to plot (make sure it is exactly the same as the name of 
                                      # the camera location in March 2024)
ptitle <- "Traffic Data over March 2024"

March2024 <- read_excel("C:/Users/smain/Desktop/newMarch2024.xlsx")
March2024$Date <- ymd(ymd_hms(March2024$Date))
March2024 <- March2024 |> separate(Hour, into = c('interval_start', 'interval_end'), sep = '-')

March2024$interval_start <- hm(March2024$interval_start)

March2024$datetime <- March2024$Date + March2024$interval_start

bycamera <- filter(March2024, Source == place)

names(bycamera)[6] <- "Bicycles"          # lines 24 - 26 rename the traffic type to a readable name
names(bycamera)[7] <- "Pedestrians"
names(bycamera)[8] <- "Road Vehicles"

bycameralong <- bycamera %>%  pivot_longer(c("Bicycles", "Pedestrians","Road Vehicles"))

funinsert <- function(x, pos, insert) {
  gsub(paste0("(.{", pos, "})(.*)$"),
       paste0("\\1", insert, " \\2"),
       x)
}

place <- "Reform Street"      # choose the location you wish to plot and type a readable name for it

ptitlenew <- funinsert(x = ptitle, pos = 0, insert = place)

wplot <- ggplot(bycameralong, aes(x = datetime, y = value, col = name)) + geom_line(linewidth=1) + 
  ggtitle(ptitlenew) + theme(axis.text.x = element_text(angle=90, 
  vjust = 0.5)) + xlab("Date") + ylab("Count") + labs(color = "Traffic Type") + 
  theme(legend.key.size = unit(2, 'cm'), legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14))
wplot
