
# Q4: Can you detect cascading failures as delays in one airport create delays in others?


library(dplyr)
library(ggplot2)

setwd("Prog for DS/Coursework PG/sourcecode/") # Setting working directory where files needed are stored
fullset <- read.csv('R fullset cleaned.csv') # Importing data

colnames(fullset)

Q4 <- fullset[, c('Year', 'Month', 'DayofMonth','TailNum',
              'ArrDelay','DepDelay','CRSDepTime', 'CRSArrTime','Origin', 'Dest')] # Subset of required columns

# Checking for consistency of tailnum values
table(Q4$TailNum)
Q4 <- Q4[!(Q4$TailNum) == 0, ] # Removing inconsistent values

# Creating dummy variables, 1=Delayed for >15mins, 0=otherwise
Q4 <- Q4 %>% mutate('DepDelayed' = if_else(Q4$DepDelay > 15, 1, 0))
Q4 <- Q4 %>% mutate('ArrDelayed' = if_else(Q4$ArrDelay > 15, 1, 0))

# Creating a column of dates
Q4$Date <- as.Date(paste(Q4$Year, Q4$Month, Q4$DayofMonth, sep="-"), format = "%Y-%m-%d")

# Removing columns we don't need anymore
colnames(Q4)
Q4 <- Q4[c("Date", "TailNum", "CRSDepTime", "CRSArrTime", "Origin", "Dest", 
           "DepDelayed", "ArrDelayed")]

# Sorting flights by date, tailnumber, then scheduled departure time to see 
# if delays happened in sequence, arrdelay -> depdelay -> arrdelay

Q4 <- Q4 %>% arrange(Date, TailNum, CRSDepTime)

# Subsetting out the flights with either an arrival or departure delay or both

casc_del <- Q4[(Q4['DepDelayed'] == 1) | (Q4['ArrDelayed'] == 1), ]


# Here we define a cascading delay as: a late arriving inbound flight that departs late 
# and then subsequently arrives late at its next destination. 

# Creating column with previous arrival delay, grouped by Tail Number

casc_del <- casc_del %>% group_by(TailNum) %>% 
  mutate('prev_arrdelay' = lag(ArrDelayed, default=FALSE))

# Checking if both conditions above are met, 1- Arrival delay leading to, 2-Departure delay 
# plus arrival delay in the next route and assigning corresponding True/False values
# to a new column "is_casc_del"

casc_del <- casc_del %>% mutate('is_casc_delay' = if_else(prev_arrdelay == 1 & 
                                                            DepDelayed == 1 & 
                                                            ArrDelayed == 1, 
                                                            "TRUE", "FALSE"))

# Getting the proportion of routes that had a cascading delay and those that didn't

Proportion <- casc_del[, c('is_casc_delay')]

Proportion <- Proportion %>% group_by(is_casc_delay) %>% summarise(cnt = n()) %>%
                                  mutate(Proportion = round(cnt / sum(cnt), 3)) %>% 
                                  arrange(desc(Proportion))

# Visualizing

ggplot(data=Proportion, aes(x=is_casc_delay, y=Proportion)) +
  geom_bar(stat="identity", fill='#ff6361') +
  
  xlab("Was it a cascading Delay?") +
  ylab("Proportion") + ggtitle("Flight delays that \n were cascading delays") + 
  theme_bw() +
  theme(plot.title = element_text(size=12, hjust=0.5, face="bold"),
        
        
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
        
        panel.grid.minor.y = element_blank()
  )

ggsave('R - cascading delays.png', width=3, height=5)  












