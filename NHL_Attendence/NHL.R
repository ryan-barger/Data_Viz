library(tidyverse)
library(scales)
library(gridExtra)


setwd("~/Visualization/2019W01 - NHL Attendance")

df <- read.csv("NHL Attendance2.csv", stringsAsFactors = FALSE) %>% 
  rename(SEASON=ï..SEASON)

NHL_teams <- unique(df$TEAM)

Subset_df <- df %>%
  select(SEASON,TEAM,HOME.ATTENDANCE,ROAD.ATTENDANCE) %>% 
  gather(GAME_Type, Attendence, c(HOME.ATTENDANCE,ROAD.ATTENDANCE))


NHL_teams_list <- list()
counter <- 1

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


ggsave("NHL Attendance - 2000s.pdf", arrangeGrob(grobs = NHL_teams_list, ncol=2, top = "NHL Attendance"), width = 8.5, height = 50, dpi = 300, limitsize = FALSE)

