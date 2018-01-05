# Import libraries
library(ggplot2)
library(stringr)
library(dplyr)

# Load data
basic_stats_pl <- read.csv("/Users/Julian/Downloads/basic_stats_pl.csv")

### PLAYER MINUTES & POINTS ###
min_pts <- ggplot(basic_stats_pl, aes(x=Min, y=Pts, label = X...Player)) + 
  geom_point(size = 5, shape = 42) +
  geom_text(aes(label=X...Player),hjust=1, vjust=0, size = 2.5) + 
  labs(title = "Player Stats", subtitle = "2016/2017 Season", x = "Average Minutes", y = "Average Points") +
  scale_y_continuous(limits =  c(15,35)) + scale_x_continuous(limits = c(20, 38))

min_pts

### PLAYER OFF & DEF REBOUNDS ###
OReb_Dreb <- ggplot(basic_stats_pl, aes(x=OReb, y=Dreb, label = X...Player)) + 
  geom_point(shape = 21) +
  geom_text(aes(label=X...Player),hjust=1, vjust=0, size = 2.5)
OReb_Dreb

### PLAYER FGA VS. POINTS ###
PTS_FGA <- ggplot(basic_stats_pl, aes(x=FGA, y=Pts, label = X...Player, color = Min)) + 
  geom_point() +
  geom_text(aes(label=X...Player),hjust=1, vjust=0, size = 2.5)
PTS_FGA

### GAMES HISTOGRAM ###
ggplot(basic_stats_pl, aes(x=Games))+ geom_histogram(binwidth = 0.5) + 
  labs(title = "Season Games Histogram", x = "Games Played", y = "Number of Players")

ggplot(basic_stats_pl, aes(x=Pts*Games))+ geom_histogram(binwidth = 200) + 
  labs(title = "Season Points Histogram", x = "Points Scored (width = 200)", y = "Number of Players")

### BoxPLot/ViolinPlot ###
no_trade <- subset(basic_stats_pl, str_count(basic_stats_pl$Team, ',') == 0)

ggplot(no_trade, aes(Team, Pts*Games)) + geom_boxplot() + 
  labs(title="Players Points by Team", subtitle = "Season 2016/17", x = "(excludes traded players)", y = "Points") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + geom_jitter(height = 0, width = 0, alpha = 0.2)

ggplot(no_trade, aes(Team, Min*Games)) + geom_violin() + 
  labs(title="Players Average Minutes by Team", subtitle = "(excluding traded players)", x = "", y = "Minutes") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_jitter(height = 0, width = 0, shape = 16, alpha = 0.5)

ggplot(no_trade, aes(Team, FGA*Games)) + geom_violin() + 
  labs(title="Players Average FGA by Team", subtitle = "(excluding traded players)", x = "", y = "FGA") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_jitter(height = 0, width = 0, shape = 16, alpha = 0.5)

