
# DATA CLEANING

setwd("Prog for DS/Coursework PG/sourcecode/") # Setting working directory where files needed are stored

y6 <- read.csv('../datasets/2006.csv') # Imporitng 2006 data
y7 <- read.csv('../datasets/2007.csv') # Importing 2007 data

rfullset <- rbind(y6,y7) # Combining flight data of both years together, 2006 on top of 2007

# Importing datasets of airports, carriers and planes

airports = read.csv('../datasets/airports.csv') 
carriers = read.csv('../datasets/carriers.csv')
planedata = read.csv('../datasets/plane-data.csv')

# Cleaning the data

colSums(is.na(rfullset))  # Looking for null values in flight datase
rfullset_cleaned <- na.omit(rfullset) # Removing rows with null values

# Repeating process for other datasets

colSums(is.na(airports))
colSums(is.na(carriers))
colSums(is.na(planedata))

airports_cleaned <- na.omit(airports) #removing rows with na values
carriers_cleaned <- na.omit(carriers)
planedata_cleaned <- na.omit(planedata)

# Exporting cleaned datasets

write.csv(rfullset_cleaned, file = 'R fullset cleaned.csv')
write.csv(airports_cleaned, file = 'R airports cleaned.csv')
write.csv(carriers_cleaned, file = 'R carries cleaned.csv')
write.csv(planedata_cleaned, file = 'R planedata cleaned.csv')

------------------------------------------------------------------------------------

# Q1: When is the best time of day, day of the week, and time of year to fly to minimise delays?

library(ggplot2) # Library for visualizing
library(dplyr)  # Library for data wrangling

setwd("Prog for DS/Coursework PG/sourcecode/")  # Setting working directory where files needed are stored

fullset <- read.csv('R fullset cleaned.csv') # Importing dataset

Q1 <- fullset[c("Year","Month","DayofMonth","DayOfWeek","CRSDepTime","DepDelay")] # Subset of columns needed
Q1

# Adding a dummy variable, where Delayed more than 15mins = 1, otherwise = 0

Q1 <- Q1 %>% mutate('Delayed' = if_else(Q1$DepDelay > 15, 1, 0))
Q1

--------------------------------------------------------------------------------------

# The best time of the year to fly to minimize delays

# Subsetting to get percentage of journeys that were late to depart per month

yearly <- Q1[c('Year','Month','Delayed')]
yearly

sums <- yearly %>% group_by(Year,Month) %>%
  summarise(percDelayed=sum(Delayed),
            .groups='drop')

counts <- count(yearly,Year,Month)

sums$percDelayed <- round((sums$percDelayed/counts$n)*100, 1)

# Separating the percentages by Year
year06 <- subset(sums, Year == 2006)
year07 <- subset(sums, Year == 2007)


# Visualizing

plot1 <- ggplot(data=year06, aes(x=Month, y=percDelayed)) +
  geom_bar(stat="identity", fill='maroon', width=0.5) +
  
  scale_x_continuous(breaks = seq(0,12,1)) +
  
  xlab("Month") +
  ylab("Percentage of Departure Delays") + ggtitle("% of Flights Delayed for >15mins in 2006") + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))
plot1

plot2 <- ggplot(data=year07, aes(x=Month, y=percDelayed)) +
  geom_bar(stat="identity", fill='maroon', width=0.5) +
  
  scale_x_continuous(breaks = seq(0,12,1)) +
  
  xlab("Month") +
  ylab("Percentage of Departure Delays") + ggtitle("% of Flights Delayed for >15mins in 2007") + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))
plot2

# Saving plots as png files
ggsave(plot = plot1, filename = 'R - 2006 delays.png', width=5, height = 5)
ggsave(plot = plot2, filename = 'R - 2007 delays.png', width=5, height = 5)

------------------------------------------------------------------------------------

# The best time of day to travel to minimize delays

daily <- Q1[c('CRSDepTime','Delayed')]
daily$CRSDepTime <- as.character(daily$CRSDepTime)

# Extracting Hours from CRSDepTime

Hours <- data.frame(substring(daily$CRSDepTime, 0, nchar(daily$CRSDepTime)-2))

# Setting empty entries to 0 or 12am
Hours[1][Hours[1] == ""] <- "0"

daily$CRSDepTime <- Hours

daily[,1] <- daily[,1][[1]] # Unnesting/unlisting the columns 

# Getting percentages of delays

dailysums <- aggregate(daily[,2], by=list(daily[,1]), FUN=sum)
dailycounts <- count(daily, CRSDepTime)

dailysums$percDelayed <- round((dailysums$x/dailycounts$n)*100, 1)


# Visualizing

ggplot(data=dailysums, aes(x=Group.1, y=percDelayed, group=1)) +
  geom_line(color='brown')+ 
  
  scale_x_discrete(limits=c(0:24)) +
  
  xlab("Hour of Day") +
  ylab("Percentage of Departure Delays per Hour") + ggtitle("Pattern of DepDelays in a Day") + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        
        
        axis.title.x=element_text(size=10,
                                  margin = margin(t = 10, r = 0, b = 0, l = 0)
        ),
        
        axis.title.y=element_text(size=10,
                                  margin = margin(t = 0, r = 10, b = 0, l = 0)
        ),
        
        axis.text.x = element_text(size = 8
        ),
        
        axis.text.y = element_text(size = 8
        ),
        
        panel.grid.major.x = element_blank(),
        
        panel.grid.minor.x = element_blank(),
        
        panel.grid.major.y = element_blank(),
        
        panel.grid.minor.y = element_blank(),
        )

ggsave('R - Daily Delays.png', width=6, height=5)

--------------------------------------------------------------------------------------

# The best day of week to travel to minimize delays

# Subsetting and renaming the days of the week

weekly <- Q1[c('DayOfWeek','Delayed')]

weekly$DayOfWeek <- recode(weekly$DayOfWeek, 
                        "7"="Sunday",
                        "1"="Monday",
                        "2"="Tuesday",
                        "3"="Wednesday",
                        "4"="Thursday",
                        "5"="Friday",
                        "6"="Saturday")

# Obtaining percentage of journeys that were delays per day of the week

sums2 <- weekly %>% group_by(DayOfWeek) %>%
  summarise(percDelayed=sum(Delayed))

counts2 <- count(weekly,DayOfWeek)

sums2$percDelayed <- round((sums2$percDelayed/counts2$n)*100, 5)

# Visualizing

ggplot(data=sums2, aes(x=DayOfWeek, y=percDelayed, group=1)) +
  geom_line(color='red')+ geom_point()+
  
  scale_x_discrete(limits=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')) +
  
  xlab("Day of Week") +
  ylab("Percentage of Departure Delays") + ggtitle("Departure delays per day of the week") + 
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))

ggsave('R - Weekly Delays.png', width=6, height=5)

