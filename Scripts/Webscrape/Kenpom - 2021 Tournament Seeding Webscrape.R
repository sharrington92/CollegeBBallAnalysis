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
  save.name <- "Kenpom - 2021 Tournament Seeds"
  
  year <- 2021
  
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
      Year = year,
      Seed = str_extract(Team, "[0-9]") %>% str_trim(),
      Team = str_extract(Team, "[^0-9]*") %>% str_trim()
    ) %>% 
    select(Year, Conference, Team, Seed) %>% 
    filter(!is.na(Seed))
}

#Save to CSV
write.csv(data, paste0(save.path, save.name, ".csv"), row.names = FALSE)






