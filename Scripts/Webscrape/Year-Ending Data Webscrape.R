{
  library(tidyverse)
  library(rvest)
  library(hablar)
  library(scales)
  #library(gganimate)
  library(rvest)
  library(ggrepel)
  library(magrittr)
}

#Inputs
{
  site <- "https://kenpom.com/index.php?y="
  
  save.path <- paste0(getwd(), "/Data/")
  save.name <- "Kenpom - Year-Ending Data"
  
  years <- c(2002:2021)
  
  power.conf <- c("SEC", "ACC", "B10", "B12", "P12", "BE", "Amer")
  
}

#Query historical years
{
  
  data.hist <- lapply(years, function(y){
    Sys.sleep(3)
    tables <- xml2::read_html(paste0(site, y)) %>%
      html_nodes(., "table") %>% html_table()
    as.data.frame(tables[1]) %>%
      mutate(Year = y) %>%
      return(.)
  }) %>% do.call(rbind, .)

  colnames(data.hist) <- c("Rank", "Team", "Conference", "Win-Loss", "AdjEM", "AdjO","AdjO.Rank","AdjD","AdjD.Rank",
                      "AdjT", "AdjT.Rank", "Luck", "Luck.Rank", "AdjEM.SOS", "AdjEM.SOS.Rank", "OppO.SOS",
                      "OppO.SOS.Rank", "OppD.SOS", "OppD.SOS.Rank", "AdjEM.NCSOS", "AdjEM.NCSOS.Rank", "Year")
  #data <- data[-1,]
  data <- data.hist %>%
    filter(Rank != "Rk", Rank != "") %>%
    convert(dbl(AdjEM)) %>%
    filter(!is.na(AdjEM)) %>%
    convert(dbl(-c(Team, Conference, `Win-Loss`, Year))) %>%
    mutate(
      `Conference.Type` = ifelse(Conference %in% power.conf, "Power",
                                 #ifelse(Conference %in% c(), "High Major",
                                 #ifelse(Conference %in% c("A10", "MWC", "WCC"), "Mid-Major+",
                                 "Mid-Major+"
      )
    )

    write.csv(data, file = paste0(save.path, save.name, ".csv"), row.names = FALSE)
}
  
