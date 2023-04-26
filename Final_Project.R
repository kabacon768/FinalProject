library(tidyverse)
library(readr)
library(dplyr)
#Reading in the CSV file with all of the bike crashes
crashes = read_csv("BikePedCrash.csv")
#Reading in CSV file with bike lanes
bikelanes = read_csv("CHNetwork.csv")

#Created this in GQIS, only has bike lane crashes which I created used geoprocessing tools
crashes_in_bike_lanes = read_csv("Bike_Lane_Crashes.csv")
#only want chapel hill
crashes = crashes %>% filter(City == "Chapel Hill")
#Just filling this column with 0 to start
crashes = crashes %>% mutate(bike_ln = 0)
#Boolean values that will tell me if the crash happened in a bike lane
bike_lanes_boolean = crashes$CrashID %in% crashes_in_bike_lanes$CrashID
#Adding to column so now I can work with the data.
crashes$bike_ln = bike_lanes_boolean



