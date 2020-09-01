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

## Clean event type to the 48 unique types

library(stringdist)

eventtypes <- c("Astronomical Low Tide", 
                "Avalanche",
                "Blizzard",
                "Coastal Flood",
                "Cold/Wind Chill",
                "Debris Flow",
                "Dense Fog",
                "Dense Smoke",
                "Drought",
                "Dust Devil",
                "Dust Storm",
                "Excessive Heat",
                "Extreme Cold/Wind Chill",
                "Flash Flood",
                "Flood",
                "Frost/Freeze",
                "Funnel Cloud",
                "Freezing Fog",
                "Hail",
                "Heat",
                "Heavy Rain",
                "Heavy Snow",
                "High Surf",
                "High Wind",
                "Hurricane (Typhoon)",
                "Ice Storm",
                "Lake-Effect Snow",
                "Lakeshore Flood",
                "Lightning",
                "Marine Hail",
                "Marine High Wind",
                "Marine Strong Wind",
                "Marine Thunderstorm Wind",
                "Rip Current",
                "Seiche",
                "Sleet",
                "Storm Surge/Tide",
                "Strong Wind",
                "Thunderstorm Wind",
                "Tornado",
                "Tropical Depression",
                "Tropical Storm",
                "Tsunami",
                "Volcanic Ash",
                "Waterspout",
                "Wildfire",
                "Winter Storm",
                "Winter Weather")  

analysis_data <- unified_numeric_data %>%
        mutate(EVTYPE = gsub("TSTM", "Thunderstorm", EVTYPE, ignore.case = T)) %>%
        mutate(EVTYPE = eventtypes[amatch(EVTYPE, eventtypes, method = "soundex")])

#analysis

library(ggplot2)

#human health
casualties_by_eventtype <- analysis_data %>%
        group_by(EVTYPE) %>%
        summarize(totalcasualties = sum(casualties)) %>%
        arrange(desc(totalcasualties)) %>%
        filter(totalcasualties > 3000)          #include only top 5 event types

ggplot(casualties_by_eventtype, aes(EVTYPE, totalcasualties)) +
        geom_bar(stat = "identity") +
        ggtitle("Impact on Population Health", subtitle = "Total casualties for the top five most harmful event types") +
        xlab("Event Type") +
        ylab("Total Casualties")

#economic impact

econimp_by_eventtype <- analysis_data %>%
        group_by(EVTYPE) %>%
        summarize(economicimpact = sum(economicimpact)/1000000000) %>%
        arrange(desc(economicimpact)) %>%
        filter(economicimpact > 17)    #include only top 5 event types

ggplot(econimp_by_eventtype, aes(EVTYPE, economicimpact)) +
        geom_bar(stat = "identity") +
        ggtitle("Economic Impact", subtitle = "Total cost of property and crop damages for the top five most costly event types") +
        xlab("Event Type") +
        ylab("Total Cost (in billion US dollars)")
