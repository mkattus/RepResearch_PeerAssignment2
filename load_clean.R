## Load data

download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "StormData.csv.bz2")
raw_data <- read.csv("StormData.csv.bz2")

library(lubridate)
library(dplyr)
raw_data$BGN_DATE <- mdy_hms(raw_data$BGN_DATE)         #change date format to POSIXct

subset_data <- raw_data %>%                             #subset data
        filter(year(BGN_DATE) >= 1996) %>%              #choose observations from 1996 and later
        select(EVTYPE, FATALITIES:CROPDMGEXP) %>%       #choose relevant variables for analysis
        filter(FATALITIES != 0 | INJURIES !=0 | PROPDMG != 0 | CROPDMG != 0) #choose observations that resulted in casualty or damages
                        
## Clean event type to the 48 unique types



## Clean damage costs to single, easily manipulated numbers and create casualties column

numeric_exp_data <- subset_data %>%                                     #convert exp symbols to numbers
        mutate(PROPDMGEXP = gsub("^", 0, PROPDMGEXP)) %>%
        mutate(PROPDMGEXP = gsub("K", 1000, PROPDMGEXP)) %>%
        mutate(PROPDMGEXP = gsub("M", 1000000, PROPDMGEXP)) %>%
        mutate(PROPDMGEXP = gsub("B", 1000000000, PROPDMGEXP)) %>%
        mutate(PROPDMGEXP = as.numeric(PROPDMGEXP)) %>%
        mutate(CROPDMGEXP = gsub("^", 0, CROPDMGEXP)) %>%
        mutate(CROPDMGEXP = gsub("K", 1000, CROPDMGEXP)) %>%
        mutate(CROPDMGEXP = gsub("M", 1000000, CROPDMGEXP)) %>%
        mutate(CROPDMGEXP = gsub("B", 1000000000, CROPDMGEXP)) %>%
        mutate(CROPDMGEXP = as.numeric(CROPDMGEXP))
        
unified_numeric_data <- numeric_exp_data %>%
        mutate(propertydamage = PROPDMG * PROPDMGEXP) %>%
        mutate(cropdamage = CROPDMG * CROPDMGEXP) %>%
        mutate(economicimpact = propertydamage + cropdamage) %>%
        mutate(casualties = FATALITIES + INJURIES) %>%
        select(!(FATALITIES:cropdamage))


