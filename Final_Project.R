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

nrow(filter(crashes, bike_ln == TRUE))/nrow(crashes)
nrow(filter(crashes, bike_ln == FALSE))/nrow(crashes)

#Looking at propotions, in Chapel Hill approximately 22.58% of crashes were in bike lanes. 77.42% were in non-bike lanes.

#Let's do a two-propotions Z-test to test for significance
# HO: There will be no difference in the proportions of bike lane crashes versus non-bike lane crashes. 
# HA: There will be fewer crashes in bike lanes. 


prop.test(x = c(nrow(filter(crashes, bike_ln == TRUE)), nrow(filter(crashes, bike_ln == FALSE))), n = c(155, 155),
          alternative = "less")


#95% CI = (-1.0000000 -0.4638142)
#P-value is low and significant
#X-squared = 91.045

# I may conclude that the proportion of bike crashes in the two groups is significantly different, with less in the bike lane group. 

# I'm interested in whether crashes that were not in bike lanes were worse. 

#I am going to add labels my CrashSevr variable to be a scale so I can measure severity easier. 

# Null Hypothesis: "There is no relationship between bike lanes and crash severity". 
# Alternative Hypothesis: "There is a relationship between bike lanes and crash severity"

levels(crashes$CrashSevr) <-  c("O: No Injury", "B: Suspected Minor Injury", "C: Possible Injury", "A: Suspected Serious Injury", "K: Killed")

severity_table <- table(crashes$bike_ln,crashes$CrashSevr) 

crashes$CrashSevr = factor(crashes$CrashSevr, c("O: No Injury", "B: Suspected Minor Injury", "C: Possible Injury", "A: Suspected Serious Injury", "K: Killed")
)

ggplot(crashes, aes(x = CrashSevr, fill = bike_ln)) +
  geom_bar() +
  xlab("Crash Severity") +
  ylab("Number of Crashes") +   
  scale_fill_manual("Bike Lane",values=c("blue","green"))



# The chisq.test() function runs chi-square tests on table objects. 
# Will return the chi-square statistic, degrees of freedom, and, most importantly, the p-value.

chisq.test(severity_table)
#X-squared = 3.9046, df = 4, p-value = 0.4191

#This means that there is a 41.91% chance of observing data more extreme than ours given that the null hypothesis is true. 

# Fail to reject the null hypothesis.
# There doesn't seem to be a relationship between severity of crashes and bike lanes. 

# Interesting to check how crashes differ across the months

Monthly_Crashes = crashes %>% group_by(CrashMonth) %>% summarize(count = n()) 

crashes$CrashMonth = factor(crashes$CrashMonth, c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
ggplot(crashes, aes(x = CrashMonth, fill = bike_ln)) +
  geom_bar() +
  xlab("Month") +
  ylab("Number of Crashes") +   
  scale_fill_manual("Bike Lane",values=c("blue","green"))



#Are there roads with more crashes than others? Maybe they need a bike lane?



