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
  site <- "https://kenpom.com/archive.php?d="
  
  save.path <- "Data/"
  save.name <- "Kenpom - Daily Data - Season 2021"
  
  date.bgn <- ymd("2020-11-25")
  date.end <- today() #ymd("2020-11-25")
    dates <- seq.Date(from = date.bgn, to = date.end, by = "1 day")
  
  power.conf <- c("SEC", "ACC", "B10", "B12", "P12", "BE", "Amer")
  
  username <- read.csv("C:/Users/shaun/Documents/Other/Kenpom Login.txt", header = FALSE)[1,1]
  pw <- read.csv("C:/Users/shaun/Documents/Other/Kenpom Login.txt", header = FALSE)[2,1]
  
}

#Loop through dates to record daily values
{
  #Connect and login to Kenpom
  {
    query_url <- site
    pgsession<-html_session(query_url)
    pgform<-html_form(pgsession)[[1]]  #in this case the submit is the 2nd form
    filled_form<-set_values(pgform, email=username, password=pw)
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
    x %>% 
      as.data.frame() %>% 
      as_tibble() %>% 
      select(-contains("Current"), -c(Rk, EM, AdjT)) %>% 
      rename(
        Rank = Var.1, 
        Team = Var.2,
        Conference = 3,
        AdjEM = 4, 
        AdjO = 5, 
        Adjo.Rank = 6,
        AdjD = 7,
        AdjD.Rank = 8,
        AdjT = 9, 
        AdjT.Rank = 10
      ) %>% 
      mutate(
        #Date = names(x), 
        Team = str_extract(Team, "[^0-9]*") %>% str_trim()
      ) %>% 
      filter(
        Rank != "Rk",
        !str_detect(Conference, "Ratings on")
      )
  }) %>% 
    do.call(rbind,.) %>% 
    convert(dbl(-c(Team, Conference, Date))) %>% 
    filter(!is.na(Rank))
  
}


write.csv(
  data, 
  file = paste0(save.path, save.name, ".csv"),
  row.names = FALSE
)
