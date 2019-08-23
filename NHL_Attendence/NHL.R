#Loading your Libraries
library(tidyverse)
library(scales)
library(gridExtra)

#Set Working Directory
setwd("")

#Read in your data set from CSV File.
df <- read.csv("NHL Attendance2.csv", stringsAsFactors = FALSE) %>% 
  rename(SEASON=ï..SEASON)

#Get list of Teams for your break out during graphing
NHL_teams <- unique(df$TEAM)

#Create smaller data set for graphing
Subset_df <- df %>%
  select(SEASON,TEAM,HOME.ATTENDANCE,ROAD.ATTENDANCE) %>% 
  gather(GAME_Type, Attendence, c(HOME.ATTENDANCE,ROAD.ATTENDANCE))

#Variales used in FOR loop to join all graphs together
NHL_teams_list <- list()
counter <- 1

#Creating Each graph by Team
for (x in NHL_teams){
  plotting_df <- Subset_df %>% 
    filter(TEAM == x)
  
  NHL_teams_list[[counter]] <- ggplot(data = plotting_df, aes(x=SEASON, y = Attendence, fill=GAME_Type)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ggtitle(x) +
    labs(y = "Attendence", x = "Season") +
    coord_flip() +
    scale_y_continuous(labels = comma, limits = c(0,1000000)) +
    theme_minimal() +
    theme(legend.position="top", legend.box="horizontal", legend.text=element_text(size=7), legend.title = element_blank(), axis.text.y=element_text(size = 7), axis.text.x=element_text(size = 7))
  
  counter <- counter + 1
  
}

#Joining the all the Graphs together and saving it into a PDF
ggsave("NHL Attendance - 2000s.pdf", arrangeGrob(grobs = NHL_teams_list, ncol=2, top = "NHL Attendance"), width = 8.5, height = 50, dpi = 300, limitsize = FALSE)
