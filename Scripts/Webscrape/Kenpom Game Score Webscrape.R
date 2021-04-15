{
  library(tidyverse)
  library(rvest)
  library(hablar)
  library(rvest)
  library(ggrepel)
  library(magrittr)
  library(lubridate)
}

#Inputs
{
  site <- "https://kenpom.com/fanmatch.php?d="
  
  date.bgn <- ymd("2021-03-17")
  date.end <- today() #ymd("2020-11-25")
  dates <- seq.Date(from = date.bgn, to = date.end, by = "1 day")
  
  save.path <- "Data/"
  save.name <- paste0(
    "Kenpom Game Data - ",
    date.bgn, " to ", date.end
  )
  
  power.conf <- c("SEC", "ACC", "B10", "B12", "P12", "BE", "Amer")
  
  username <- read.csv("C:/Users/shaun/Documents/Other/Kenpom Login.txt", header = FALSE)[1,1]
  pw <- read.csv("C:/Users/shaun/Documents/Other/Kenpom Login.txt", header = FALSE)[2,1]
  
}

#Loop through dates to record daily values
{
  #Connect and login to Kenpom
  {
    query_url <- "https://kenpom.com/"
    pgsession <- html_session(query_url)
    pgform <- html_form(pgsession)[[1]]  #in this case the submit is the 2nd form
    filled_form <- set_values(
      pgform,
      email=username,
      password=pw
    )
    submit_form(pgsession, filled_form)
    raw_query <- jump_to(pgsession, query_url)
    html_nodes(raw_query, "#title-container") %>%
      html_text()
  }
  
  daily.list <- list()
  
  for(d in 1:length(dates)){
    
    daily.list[[d]] <- paste0(site, dates[d]) %>% 
      jump_to(pgsession, .) %>% 
      html_nodes(., "table") %>%
      html_table() %>% 
      as.data.frame %>% 
      mutate(Date = dates[d])
    
    #Sys.sleep(1)
  }
  
  
  data <- lapply(daily.list, function(x){
    if(nrow(x) == 0){
      return()
    }
    temp1 <- x %>%
      rownames_to_column() %>% 
      arrange(desc(rowname)) %>% 
      slice(-c(1:6)) %>% 
      select(Date, Game) %>% 
      mutate(Game = str_trim(Game)) %>% 
      separate(Game, sep = ",", into = c("Team1", "Team2")) %>% 
      separate(Team2, sep = "\\[", into = c("Team2", NA)) %>% 
      mutate(
        Team2 = gsub("\\(OT\\)", "", Team2) %>% str_trim(),
        ScoreA = str_sub(Team1, -3) %>% as.integer,
        TeamA = str_replace_all(Team1, "[:digit:]", "") %>% str_trim(),
        ScoreB = str_sub(Team2, -3) %>% as.integer,
        TeamB = str_replace_all(Team2, "[:digit:]", "") %>% str_trim()
      ) %>% 
      select(-c(Team1, Team2))
    
    temp2 <- temp1 %>% 
      rename(
        ScoreB = ScoreA, ScoreA = ScoreB, 
        TeamB = TeamA, TeamA = TeamB
      )
    
    temp.total <- bind_rows(temp1, temp2) %>% 
      mutate(Result = ifelse(ScoreA > ScoreB, "Win", "Loss"))
    
    return(temp.total)
    
  }) %>% 
    do.call(rbind,.) %>% 
    filter(!is.na(TeamA) & !is.na(TeamB) & !is.na(ScoreA) & !is.na(ScoreB))
  
}


write.csv(
  data, 
  file = paste0(save.path, save.name, ".csv"),
  row.names = FALSE
)



