
# Q3: How does the number of people flying between different locations change over time?

#Importing libraries
library(ggplot2)
library(dplyr)

setwd("Prog for DS/Coursework PG/sourcecode/") # Setting working directory where files needed are stored

fullset <- read.csv('R fullset cleaned.csv') # Importing data

Q3 <- fullset[c("Year","Month", "Origin", "Dest")] # Subset of required columns

# Converting Year and month into date format into one column
Q3$dates <- as.Date(paste(Q3$Year, Q3$Month,'01', sep="-"), format = "%Y-%m-%d")

# The top 5 major airports in the US are in Atlanta, Dallas, Denver, Chicago and 
# Los Angeles according to world-airport-codes.com

# Selecting the routes to and from the above airports:

top5 <- subset(Q3[, c("dates","Origin",'Dest')], Q3$Origin == 'ATL' | Q3$Origin == 'DFW' | Q3$Origin == 'DEN'|
                      Q3$Origin == 'ORD' | Q3$Origin == 'LAX' | Q3$Dest == 'ATL' | Q3$Dest == 'DFW' | Q3$Dest == 'DEN'|
                 Q3$Dest == 'ORD' | Q3$Dest == 'LAX')

top5 <- top5 %>% mutate(airport =
                             case_when(Origin == 'ATL' | Dest == 'ATL' ~ "Atlanta", 
                                       Origin == 'DFW' | Dest == 'DFW' ~ "Dallas",
                                       Origin == 'DEN' | Dest == 'DEN' ~ "Denver",
                                       Origin == 'ORD' | Dest == 'ORD' ~ "Chicago",
                                       Origin == 'LAX' | Dest == 'LAX' ~ "LosAngeles")
)


# Grouping to get counts of flights per month per major airport
top5 <- count(top5, dates, airport)
top5

# Visualizing

ggplot(data=top5) +
  geom_line(aes(x=dates, y=n, colour= airport)) +
  scale_colour_manual(values=c("red","green","blue", "pink", "orange")) +
  
  labs(colour='Airport') +
  xlab("Month") +
  ylab("Flights per Month") + 
  ggtitle("Changes in number of flights overtime for the top 5 major airports in the US - \n Atlanta, Denver, Dallas, Los Angeles, Chicago") + 
  theme_bw() +
  theme(plot.title = element_text(size=10, hjust=0.5, face="bold"),
        
        
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

ggsave('R - flights overtime.png', width=8, height=5)
