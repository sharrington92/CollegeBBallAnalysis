{
  library(tidyverse)
  library(rvest)
  library(hablar)
  #library(scales)
  #library(gganimate)
  library(rvest)
  #library(ggrepel)
  library(magrittr)
}

#Inputs
{
  site <- "https://kenpom.com"
  
  save.path <- paste0(getwd(), "/Data/")
  save.name <- "Kenpom - Current Data"
  
  year <- 2021
  
  power.conf <- c("SEC", "ACC", "B10", "B12", "P12", "BE", "Amer")
  
}

# QUERY Kenpom
{
  raw_query <- xml2::read_html(site)
  tables <- html_nodes(raw_query, "table") %>% html_table()
  data <- as.data.frame(tables[1])
  
  colnames(data) <- c("Rank", "Team", "Conference", "Win.Loss", "AdjEM", "AdjO","AdjO.Rank","AdjD","AdjD.Rank",
                      "AdjT", "AdjT.Rank", "Luck", "Luck.Rank", "AdjEM.SOS", "AdjEM.SOS.Rank", "OppO.SOS",
                      "OppO.SOS.Rank", "OppD.SOS", "OppD.SOS.Rank", "AdjEM.NCSOS", "AdjEM.NCSOS.Rank")
  
  data <- data[-1,] 
  data <- data %>% 
    convert(dbl(AdjEM)) %>% 
    
    #this line should be done elsewhere
    filter(!is.na(AdjEM), Conference != "Ivy") %>% 
    convert(dbl(-c(Team, Conference, `Win.Loss`))) %>% 
    mutate(
      `Conference.Type` = ifelse(Conference %in% power.conf, "Power",
                                 #ifelse(Conference %in% c(), "High Major",
                                 #ifelse(Conference %in% c("A10", "MWC", "WCC"), "Mid-Major+",
                                 "Mid-Major+"
      ),
      Year = year
    )
}

#Save to CSV
write.csv(data, paste0(save.path, save.name, ".csv"), row.names = FALSE)






