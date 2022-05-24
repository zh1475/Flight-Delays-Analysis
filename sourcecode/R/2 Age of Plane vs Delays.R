
# Q2: Do older planes suffer more delays?

library(dplyr)
library(ggplot2)

setwd("Prog for DS/Coursework PG/sourcecode/")  # Setting working directory where files needed are stored

# Importing datasets

planes <- read.csv('../datasets/plane data cleaned.csv')
fullset <- read.csv('../datasets/fullset cleaned.csv')

# Subset of columns needed 

planes <- planes[c('tailnum', 'year')]
colnames(planes) <- c('TailNum', 'Year')

Q2 <- fullset[c('TailNum', 'DepDelay')]

# Filtering only for those flights that experienced a departure delay

delays <- subset(Q2, Q2$DepDelay > 0)

# Matching plane tail number with year of manufacture

age_vs_delays <- merge(planes, delays, by='TailNum') 

dplyr::count(age_vs_delays, Year, sort = TRUE) # Checking for consistency in values

age_vs_delays <- subset(age_vs_delays, Year != '0' & Year != 'None') # Removing inconsistent values

age_vs_delays <- aggregate(age_vs_delays$DepDelay, list(age_vs_delays$Year), FUN=mean) # Grouping and aggregating to get average length of delay

# Visualizing

ggplot(data=age_vs_delays, aes(x=Group.1, y=x)) +
  geom_point(size=2, color='#2e9acc') +
  
  scale_x_discrete(breaks = seq(1956, 2007, 10))+
  
  xlab("Year of Manufacture of Plane") +
  
  ylab("Mean length of Departure delay") + ggtitle("Age of Plane vs Delays in 2006/2007") + 
  
  theme_bw() +
  
  theme(plot.title = element_text(hjust=0.5
        ),
       
        axis.text.x = element_text(colour="#404040",
                                   size = 8
        ),
        
        axis.text.y = element_text(colour="#404040",
                                   size = 8
        ),
        panel.background = element_rect(fill = "white", 
                                        colour = "black",
                                        size = 0.5, 
                                        linetype = "dashed"),
        
        panel.grid.minor.y = element_line(size = 0.25, 
                                          linetype = 'dashed',
                                          colour = "black"),
        )
plot

ggsave('R - Age vs Delays.png', width=8, height=5)
