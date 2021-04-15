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

setwd("C:\\Users\\shaun\\Documents\\R Projects")

#query site and extract attributes with rvest
{
  # QUERY Kenpom
  {
    query_url <- paste0("https://kenpom.com")
    raw_query <- xml2::read_html(query_url)
    tables <- html_nodes(raw_query, "table") %>% html_table()
    data <- as.data.frame(tables[1])
    
    colnames(data) <- c("Rank", "Team", "Conference", "Win.Loss", "AdjEM", "AdjO","AdjO.Rank","AdjD","AdjD.Rank",
                        "AdjT", "AdjT.Rank", "Luck", "Luck.Rank", "AdjEM.SOS", "AdjEM.SOS.Rank", "OppO.SOS",
                        "OppO.SOS.Rank", "OppD.SOS", "OppD.SOS.Rank", "AdjEM.NCSOS", "AdjEM.NCSOS.Rank")
    data <- data[-1,] 
    data <- data %>% 
      convert(dbl(AdjEM)) %>% 
      filter(!is.na(AdjEM), Conference != "Ivy") %>% 
      convert(dbl(-c(Team, Conference, `Win.Loss`))) %>% 
      mutate(
        `Conference.Type` = ifelse(Conference %in% c("SEC", "ACC", "B10", "B12", "P12", "BE", "Amer"), "Power",
                                   #ifelse(Conference %in% c(), "High Major",
                                   #ifelse(Conference %in% c("A10", "MWC", "WCC"), "Mid-Major+",
                                   "Mid-Major+"
        ),
        Year = 2021
      )
  }
  
  
  data.hist <- read.csv("Kenpom Analysis/Historical Kenpom Data.csv") %>% 
    filter(Year != 2021) %>% 
    mutate(
      Conference = ifelse(Conference == "P10", "P12", Conference),
      Conference = ifelse(Conference == "P10", "P12", Conference),
      Conference.Type = ifelse(Conference.Type == "P12", "Power", Conference.Type),
      Team = str_extract(Team, "[^0-9]*") %>% str_trim()
    ) 
  
  data <- data %>% bind_rows(data.hist)
  rm(data.hist, raw_query, tables, query_url)
  
  #Query historical years
  {
    # years <- c(2002:2021)
    # data.hist <- lapply(years, function(y){
    #   Sys.sleep(3)
    #   tables <- xml2::read_html(paste0("https://kenpom.com/index.php?y=", y)) %>% 
    #     html_nodes(., "table") %>% html_table()
    #   as.data.frame(tables[1]) %>% 
    #     mutate(Year = y) %>% 
    #     return(.)
    # }) %>% do.call(rbind, .)
    # 
    # colnames(data.hist) <- c("Rank", "Team", "Conference", "Win-Loss", "AdjEM", "AdjO","AdjO.Rank","AdjD","AdjD.Rank",
    #                     "AdjT", "AdjT.Rank", "Luck", "Luck.Rank", "AdjEM.SOS", "AdjEM.SOS.Rank", "OppO.SOS",
    #                     "OppO.SOS.Rank", "OppD.SOS", "OppD.SOS.Rank", "AdjEM.NCSOS", "AdjEM.NCSOS.Rank", "Year")
    # #data <- data[-1,] 
    # data <- data.hist %>% 
    #   filter(Rank != "Rk", Rank != "") %>% 
    #   convert(dbl(AdjEM)) %>% 
    #   filter(!is.na(AdjEM)) %>% 
    #   convert(dbl(-c(Team, Conference, `Win-Loss`, Year))) %>% 
    #   mutate(
    #     `Conference.Type` = ifelse(Conference %in% c("SEC", "ACC", "B10", "B12", "P12", "BE", "Amer", "P10"), "Power",
    #                                #ifelse(Conference %in% c(), "High Major",
    #                                #ifelse(Conference %in% c("A10", "MWC", "WCC"), "Mid-Major+",
    #                                "Mid-Major+"
    #     )
    #   )
    
    #   write.csv(data, file = "~/Documents/R Projects/Kenpom Analysis/Historical Kenpom Data.csv", row.names = FALSE)
  }
  
  #National champions
  {
    # query_url <- paste0("https://www.ncaa.com/history/basketball-men/d1")
    # raw_query <- xml2::read_html(query_url)
    # tables <- html_nodes(raw_query, "table") %>% html_table()
    # champs <- as.data.frame(tables[1])[,c(1:3,5)]
    # 
    # colnames(champs ) <- c("Year", "Team", "Coach", "Runner.Up")
    # 
    # runnerup <- champs[,c(1,4)]
    
    runnerup <- data.frame(
      Year = c(2019:2002),
      Team = c('Texas Tech', 'Michigan', 'Gonzaga', 'North Carolina', 'Wisconsin',
               'Kentucky', 'Michigan', 'Kansas', 'Butler', 'Butler', 'Michigan St.',
               'Memphis', 'Ohio St.', 'UCLA', 'Illinois', 'Georgia Tech', 'Kansas',
               'Indiana'),
      Runnerup = 1
    )
    
    # champs %<>%
    #   mutate(
    #     Team = gsub()
    #   )
    
    champs <- data.frame(
                Year = c(2019:2002), 
                Team = c('Virginia', 'Villanova', 'North Carolina', 'Villanova',
                        'Duke', 'Connecticut', 'Louisville', 'Kentucky', 
                        'Connecticut', 'Duke', 'North Carolina', 'Kansas', 
                        'Florida', 'Florida',    'North Carolina', 'Connecticut', 
                        'Syracuse', 'Maryland'),
                Champion = 1)
  }
  
  data %<>% 
    select(Year, Team, Conference, Conference.Type, Rank, Win.Loss, AdjEM, AdjO, AdjO.Rank, AdjD,
           AdjD.Rank, AdjT, AdjT.Rank, Luck, Luck.Rank, AdjEM.SOS,
           AdjEM.SOS.Rank, OppO.SOS, OppO.SOS.Rank, OppD.SOS, OppD.SOS.Rank,
           AdjEM.NCSOS, AdjEM.NCSOS.Rank)
  
  data %<>% 
    left_join(y = champs, by = c("Year", "Team")) %>% 
    replace_na(list(Champion = 0)) %>% 
    left_join(y = runnerup, by = c("Year", "Team")) %>% 
    replace_na(list(Runnerup = 0)) 
  
  
}

#Query Locations
{
  # teams <- data2 %>% 
  #   select(Team) %>% 
  #   distinct %>% 
  #   mutate(
  #     Team = str_replace_all(Team, " ", "+"),
  #     Team = ifelse(Team == "Morris+Brown", "Robert+Morris", Team),
  #     Team = ifelse(Team == "College+of+Charleston", "Charleston", Team),
  #     Team = ifelse(Team == "Arkansas+Little+Rock", "Arkansas", Team),
  #     Team = ifelse(Team == "Louisiana+Lafayette", "Louisiana", Team),
  #   )
  # 
  # query_url <- "https://kenpom.com/team.php?team=Baylor"
  # pgsession<-html_session(query_url)
  # pgform<-html_form(pgsession)[[1]]  #in this case the submit is the 2nd form
  # filled_form<-set_values(pgform, email="xxxxxxxxxxxxxx@gmail.com", password="xxxxxxx")
  # submit_form(pgsession, filled_form)
  # #raw_query <- xml2::read_html(query_url)
  # raw_query <- jump_to(pgsession, query_url)
  # html_nodes(raw_query, "#title-container") %>% 
  #   html_text() 
  # t <- 326
  # cities.raw <- list()
  # for(t in 1:length(teams$Team)){
  #   query_url <- paste0("https://kenpom.com/team.php?team=",teams$Team[t])
  #   #pgsession <- html_session(query_url)
  #   #pgform<-html_form(pgsession)[[1]]  #in this case the submit is the 2nd form
  #   #filled_form<-set_values(pgform, email="@gmail.com", password="")
  #   #submit_form(pgsession, filled_form)
  #   raw_query <- jump_to(pgsession, query_url)
  #   cities.raw[[t]] <- html_nodes(raw_query, "#title-container") %>% 
  #     html_text() 
  #   #Sys.sleep(1)
  # }
  # write.csv(teams.info, file = "~/Documents/R Projects/Kenpom Analysis/Team Info Data.csv", row.names = FALSE)
  # 
  # teams.info <- do.call(rbind, cities.raw) %>% as.data.frame() %>%
  #   as_tibble() %>% rename(All = V1) %>% 
  #   mutate(
  #     #Team = str_sub(All, 1, str_locate(All, "[^:digits:]*"))
  #     #Rank = str_view_all(All, "[digit]")
  #     Rank = grep("[^digit]", All)
  #   )
  #     
  # 
  # #create a web session with the desired login address
  # 
  # 
  # 
  # #pre allocate the final results dataframe.
  # results<-data.frame()  
}

#Boxplot Analysis
{
  #Overall
  data %>% 
    filter(Conference %in% c("WCC", "MWC", "MVC", "A10", "MAC", "CUSA", "MAC") | `Conference.Type` == "Power") %>% 
    filter(Year == 2021) %>% 
    #filter(Team != "Gonzaga") %>% 
    ggplot(aes(y = AdjEM, color = Conference.Type, label = Conference)) +
    geom_boxplot() +
    facet_grid(.~reorder(Conference, -AdjEM, median)) +
    #geom_jitter() +
    xlab("Conference") +
    ylab("") +
    theme(axis.text.x = element_text(angle = 90, vjust = .0005)) +
    ggtitle("Kenpom Adjusted Efficiency by Conference", subtitle = "") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5),
          axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
    scale_y_continuous(n.breaks = 10) 
  #bx.adjem + transition_time(Year) 
  
  
  #Offense
  data %>% 
    #filter(Team != "Gonzaga") %>% 
    ggplot(aes(x = reorder(Conference, -AdjO, median), y = AdjO, color = `Conference Type`)) +
    geom_boxplot() +
    xlab("Conference") +
    ylab("") +
    theme(axis.text.x = element_text(angle = 90, vjust = .0005)) +
    ggtitle("Kenpom Adjusted Offense by Conference", subtitle = "Ordered by Median") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
    scale_y_continuous(n.breaks = 10)
  
  #Defense
  data %>% 
    #filter(Team != "Gonzaga") %>% 
    ggplot(aes(x = reorder(Conference, AdjD, median), y = AdjD, color = `Conference Type`)) +
    geom_boxplot() +
    xlab("Conference") +
    ylab("") +
    theme(axis.text.x = element_text(angle = 90, vjust = .0005)) +
    scale_y_continuous(trans = "reverse", n.breaks = 10) +
    ggtitle("Kenpom Adjusted Defense by Conference", subtitle = "Ordered by Median") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5))
  
  #Temp
  data %>% 
    ggplot(aes(x = reorder(Conference, -AdjT, median), y = AdjT, color = `Conference Type`)) +
    geom_boxplot() +
    xlab("Conference") +
    ylab("") +
    theme(axis.text.x = element_text(angle = 90, vjust = .0005)) +
    ggtitle("Kenpom Adjusted Tempo by Conference", subtitle = "Ordered by Median") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
    scale_y_continuous(n.breaks = 10)
  
  #Luck
  data %>% 
    ggplot(aes(x = reorder(Conference, -Luck, median), y = Luck, color = `Conference Type`)) +
    geom_boxplot() +
    xlab("Conference") +
    ylab("") +
    theme(axis.text.x = element_text(angle = 90, vjust = .0005)) +
    ggtitle("Kenpom Luck by Conference", subtitle = "Ordered by Median") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
    scale_y_continuous(n.breaks = 10)
  
  #NCSOS
  data %>% 
    ggplot(aes(x = reorder(Conference, -AdjEM.NCSOS, median), y = AdjEM.NCSOS, color = `Conference Type`)) +
    geom_boxplot() +
    xlab("Conference") +
    ylab("") +
    theme(axis.text.x = element_text(angle = 90, vjust = .0005)) +
    ggtitle("Kenpom Non-Conference Strength of Schedule by Conference", subtitle = "Ordered by Median") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
    scale_y_continuous(n.breaks = 10)
  
}

#Analysis
{
  #Median
  median.byConf <- data %>% 
    #filter(Team != "Gonzaga") %>% 
    group_by(Conference) %>% 
    select(Conference, AdjEM, AdjO, AdjD, AdjT, Luck, AdjEM.SOS, AdjEM.NCSOS, OppO.SOS, OppD.SOS) %>% 
    summarize_all(median)
  
  #Mean
  mean.byConf <- data.clean %>% 
    #filter(Team != "Gonzaga") %>% 
    group_by(Conference) %>% 
    select(Conference, AdjEM, AdjO, AdjD, AdjT, Luck, AdjEM.SOS, AdjEM.NCSOS, OppO.SOS, OppD.SOS) %>% 
    summarize_all(mean)
  
  #StDev
  sd.byConf <- data.clean %>% 
    #filter(Team != "Gonzaga") %>% 
    group_by(Conference) %>% 
    select(Conference, AdjEM, AdjO, AdjD, AdjT, Luck, AdjEM.SOS, AdjEM.NCSOS, OppO.SOS, OppD.SOS) %>% 
    summarize_all(sd)
  
  #Top team removed
  {
    data.nottop <- data.clean %>% 
      group_by(Conference) %>% 
      arrange(desc(AdjEM)) %>% 
      slice(-1)
    
    #Overall
    data.nottop %>% 
      ggplot(aes(x = reorder(Conference, -AdjEM, median), y = AdjEM, color = `Conference Type`)) +
      geom_boxplot() +
      xlab("Conference") +
      ylab("") +
      theme(axis.text.x = element_text(angle = 90, vjust = .0005)) +
      ggtitle("Kenpom Adjusted Efficiency by Conference with Top Team Removed", subtitle = "Ordered by Median") +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      scale_y_continuous(n.breaks = 10)
    
    #Offense
    data.nottop %>% 
      ggplot(aes(x = reorder(Conference, -AdjO, median), y = AdjO, color = `Conference Type`)) +
      geom_boxplot() +
      xlab("Conference") +
      ylab("") +
      theme(axis.text.x = element_text(angle = 90, vjust = .0005)) +
      ggtitle("Kenpom Adjusted Offense by Conference with Top Team Removed", subtitle = "Ordered by Median") +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      scale_y_continuous(n.breaks = 10)
    
    #Defense
    data.nottop %>% 
      ggplot(aes(x = reorder(Conference, AdjD, median), y = AdjD, color = `Conference Type`)) +
      geom_boxplot() +
      xlab("Conference") +
      ylab("") +
      theme(axis.text.x = element_text(angle = 90, vjust = .0005)) +
      scale_y_continuous(trans = "reverse", n.breaks = 10) +
      ggtitle("Kenpom Adjusted Defense by Conference with Top Team Removed", subtitle = "Ordered by Median") +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5))
    }
  
  #Bottom team removed
  {
    data.notbottom <- data.clean %>% 
      group_by(Conference) %>% 
      arrange(AdjEM) %>% 
      slice(-1)
    
    #Overall
    data.notbottom %>% 
      ggplot(aes(x = reorder(Conference, -AdjEM, median), y = AdjEM, color = `Conference Type`)) +
      geom_boxplot() +
      xlab("Conference") +
      ylab("") +
      theme(axis.text.x = element_text(angle = 90, vjust = .0005)) +
      ggtitle("Kenpom Adjusted Efficiency by Conference with Bottom Team Removed", subtitle = "Ordered by Median") +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      scale_y_continuous(n.breaks = 10)
    
    #Offense
    data.notbottom %>% 
      ggplot(aes(x = reorder(Conference, -AdjO, median), y = AdjO, color = `Conference Type`)) +
      geom_boxplot() +
      xlab("Conference") +
      ylab("") +
      theme(axis.text.x = element_text(angle = 90, vjust = .0005)) +
      ggtitle("Kenpom Adjusted Offense by Conference with Bottom Team Removed", subtitle = "Ordered by Median") +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5)) +
      scale_y_continuous(n.breaks = 10)
    
    #Defense
    data.notbottom %>% 
      ggplot(aes(x = reorder(Conference, AdjD, median), y = AdjD, color = `Conference Type`)) +
      geom_boxplot() +
      xlab("Conference") +
      ylab("") +
      theme(axis.text.x = element_text(angle = 90, vjust = .0005)) +
      scale_y_continuous(trans = "reverse", n.breaks = 10) +
      ggtitle("Kenpom Adjusted Defense by Conference with Bottom Team Removed", subtitle = "Ordered by Median") +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5))
  }
  
}

#Time Series Median AdjEM
{
  data %>% 
    filter(Year >= 2017, Conference %in% c("WCC", "MWC", "MVC", "A10", "MAC") | Conference.Type == "Power") %>% 
    group_by(Team) %>%
    convert(chr(Year)) %>% 
    bind_rows(group_by(.,Conference, Team) %>% summarise(., Year = "2017-2021", AdjEM = median(AdjEM))) %>% 
    mutate(Year = factor(Year, levels = c(2017:2021, "2017-2021"))) %>% 
    ggplot(aes(y = AdjEM, color = Conference.Type)) +
    geom_boxplot() +
    facet_grid(as.factor(Year)~reorder(Conference, -AdjEM, median), scales = "free_y") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = .5),
          axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
    ggtitle("Kenpom Adjusted Efficiency by Conference", subtitle = "Descending Order by Median Adjusted EM Over Period")
    
  
  #Take top x teams per year in AdjEM, display national champs
  {
    y1 <- 2002
    y2 <- 2008
    data %>% 
      group_by(Year) %>%  
      top_n(10, AdjEM) %>% 
      bind_rows(data %>% filter(Champion == 1 | Runnerup == 1)) %>%
      distinct %>% 
      filter(Year != 2020, Year <= y2, Year >= y1) %>% 
      mutate(
        Team = ifelse(Team == "Louisville" & Year == 2013, "Vacated", Team),
        #Team.Year = paste(Team, Year, sep = "\n"),
        Champion = ifelse(Year == 2021, "TBD", ifelse(Champion == 1, "Yes", ifelse(Runnerup ==1, "Runner-up","No"))),
        Champion = factor(Champion, levels = c("Yes", "Runner-up", "No", "TBD"))
      ) %>% 
      ggplot(aes(x = Year, y = AdjEM)) +
      #geom_point(aes(color = Team, shape = as.factor(Champion)), size = 3) +
      geom_label_repel(aes(label = paste(Team, AdjEM, sep = "\n"), fill = Champion), size = 2.5) +
      facet_grid(. ~ Year, scales = "free_x") +
      scale_shape_manual(values = c(4, 17)) +
      scale_fill_manual(values = c("chartreuse3", "yellow3", "gray70", "gray90")) +
      scale_y_continuous(n.breaks = 10) + 
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = .5),
            axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            legend.position = "bottom") +
      ggtitle("National Champions & Kenpom Top 10 Teams Relative to Field by Year",
              subtitle = paste(y1,y2, sep = " - ")) +
      xlab("") +
      ylab("Adjusted Efficiency Margin") +
      labs(color = "Team", size = "Adjusted Tempo", shape = "National Champion")
      scale_color_manual(values = c("Arizona"="#AB0520",
                                    "Baylor" = "#1C3C34",
                                    "Connecticut" = "#7C878E", #7C878E #000E2F
                                    "Duke" = "#0736A4",
                                    "Florida" = "#FA4616",
                                    "Gonzaga" = "#041E42",
                                    "Illinois" = "#E84A27",
                                    "Kansas"="#0051BA", #E800D
                                    "Kentucky"="#0033A0",
                                    "Louisville" = "#C9001F", #000000
                                    "Vacated" = "black", #000000
                                    "Maryland" = "#E03A3E", #FFD520
                                    "Memphis" = "#0D3182", #888C8F 0D3182
                                    "Michigan" = "#00274C", #FFCB05
                                    "North Carolina" = "#7BAFD4",
                                    "Ohio St." = "#CE0F3D", ##CE0F3D B0B7BC
                                    "Syracuse" = "#D44500", ##3E3D3C
                                    "Villanova" = "#002664",
                                    "Virginia" = "#F84C1E",
                                    "Wisconsin" = "#C4012F"
      ))
  }
}

#Scatter plot
{
  data %>% 
    filter(Year == 2021) %>% 
    #top_n(50, AdjEM) %>% 
    mutate(AdjPercentile = percent_rank(AdjEM)) %>% 
    #filter(Conference %in% c("WCC", "MWC", "MVC", "A10", "MAC") | Conference.Type == "Power") %>% 
    ggplot(aes(x = AdjO, y = AdjD, shape = Conference.Type, color = AdjT)) +
    geom_point(size = 4) +
    #geom_label(aes(label = Team), size = 2, nudge_x = .5, nudge_y = .5) +
    #facet_grid(Conference ~ .) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = .5)) +
    scale_y_reverse() +
    geom_segment(x = 125, y = 125, xend=85, yend=85, color='blue') +
    ggtitle("Kenpom Adjusted Offense v. Defense") +
    scale_color_gradient(low = "red", high = "green")
  
  
  data %>% 
    #filter(Year == 2021) %>% 
    top_n(25, AdjEM) %>% 
    bind_rows(data %>% filter(Champion == 1)) %>% 
    distinct %>% 
    mutate(
      Team = ifelse(Team == "Louisville" & Year == 2013, "Vacated", Team),
      Team.Year = paste(Team, Year, sep = "\n"),
      Champion = ifelse(Champion == 1, "Yes", "No")
    ) %>% 
    #filter(Conference %in% c("WCC", "MWC", "MVC", "A10", "MAC") | Conference.Type == "Power") %>% 
    ggplot(aes(x = AdjO, y = AdjD)) +
    geom_point(aes(color = Team, shape = as.factor(Champion)), size = 3) +
    scale_shape_manual(values = c(4, 17)) +
    geom_text_repel(aes(label = Team.Year), size = 3,
                     segment.size = .2) +
    #facet_grid(Conference ~ .) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = .5)) +
    scale_x_continuous(n.breaks = 10) +
    scale_y_reverse(n.breaks = 10) +
    #scale_x_continuous(limits = c(110,130)) +
    #scale_y_reverse(limits = c(97.5,80)) +
    #geom_curve(ncp = 10, aes(x = 110, xend = 130, y = 80, yend = 95), inherit.aes = FALSE) +
    #geom_curve(ncp = 10, aes(x = 120, xend = 130, y = 80, yend = 90), inherit.aes = FALSE) +
    #geom_curve(ncp = 10, aes(x = 115, xend = 130, y = 85, yend = 100), inherit.aes = FALSE) +
    geom_segment(aes(x = 112.5, xend = 117.5, y = 92.5, yend = 97.5), inherit.aes = FALSE) + #AdjEM = 20
    geom_text(aes(x = 113.15, y = 93.2, label = "\nAdjusted EM = 20"), inherit.aes = FALSE, angle = -42.5,
              color = "black", size = 3) + 
    geom_segment(aes(x = 112.5, xend = 122.5, y = 87.5, yend = 97.5), inherit.aes = FALSE) + #AdjEM = 25
    geom_text(aes(x = 113.15, y = 88.2, label = "\nAdjusted EM = 25"), inherit.aes = FALSE, angle = -42.5,
              color = "black", size = 3) + 
    geom_segment(aes(x = 112.5, xend = 127.5, y = 82.5, yend = 97.5), inherit.aes = FALSE) + #AdjEM = 30
    geom_text(aes(x = 113.15, y = 83.2, label = "\nAdjusted EM = 30"), inherit.aes = FALSE, angle = -42.5,
              color = "black", size = 3) +
    #geom_segment(aes(x = 115, xend = 130, y = 82.5, yend = 97.5), inherit.aes = FALSE) + #AdjEM = 32.5
    geom_segment(aes(x = 117.5, xend = 130, y = 82.5, yend = 95), inherit.aes = FALSE) + #AdjEM = 35
    geom_text(aes(x = 117.5, y = 82.5, label = "\nAdjusted EM = 35"), inherit.aes = FALSE, angle = -42.5,
              color = "black", size = 3) +
    geom_segment(aes(x = 122.5, xend = 130, y = 82.5, yend = 90), inherit.aes = FALSE) + #AdjEM = 40
    geom_text(aes(x = 122.5, y = 82.5, label = "\nAdjusted EM = 40"), inherit.aes = FALSE, angle = -42.5,
              color = "black", size = 3) +
    #geom_segment(aes(x = 118.5, xend = 130, y = 82.5, yend = 94), inherit.aes = FALSE) + #AdjEM = 36
    ggtitle("National Champions & Kenpom Top 25 Teams Relative to Field", subtitle = "2002 - Present") +
    xlab("Adjusted Offense") +
    ylab("Adjusted Defense") +
    labs(color = "Team", size = "Adjusted Tempo", shape = "National Champion")   +
    scale_color_manual(values = c("Arizona"="#AB0520",
                                  "Baylor" = "#1C3C34",
                                  "Connecticut" = "#7C878E", #7C878E #000E2F
                                  "Duke" = "#0736A4",
                                  "Florida" = "#FA4616",
                                  "Gonzaga" = "#041E42",
                                  "Illinois" = "#E84A27",
                                  "Kansas"="#0051BA", #E800D
                                  "Kentucky"="#0033A0",
                                  "Louisville" = "#C9001F", #000000
                                  "Vacated" = "black", #000000
                                  "Maryland" = "#E03A3E", #FFD520
                                  "Memphis" = "#0D3182", #888C8F 0D3182
                                  "Michigan" = "#00274C", #FFCB05
                                  "North Carolina" = "#7BAFD4",
                                  "Ohio St." = "#CE0F3D", ##CE0F3D B0B7BC
                                  "Syracuse" = "#D44500", ##3E3D3C
                                  "Villanova" = "#002664",
                                  "Virginia" = "#F84C1E",
                                  "Wisconsin" = "#C4012F"
                                  ))
  
  t <- "Virginia"
  data %>% 
    filter(Team == t) %>% 
    arrange(Year) %>% 
    ggplot(aes(x = AdjO, y = AdjD)) +
    geom_path(aes(alpha = Year)) +
    geom_point(aes(color = AdjEM, size = AdjEM)) +
    geom_label_repel(aes(label = Year, fill = Year), size = 3,
                    segment.size = .2) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = .5)) +
    scale_y_reverse() +
    ggtitle("Kenpom Adjusted Efficiency Metrics", subtitle = t) +
    xlab("Adjusted Offense") +
    ylab("Adjusted Defense") +
    labs(color = "Adjusted Efficiency", 
         size = "Adjusted Efficiency", fill = "Year", alpha = "Year") + 
    #scale_fill_gradient2(low = "red3", high = "green3", mid = "yellow3", midpoint = 0) +
    scale_fill_gradient(low = "gray90", high = "gray40") +
    scale_color_gradient2(low = "red3", high = "green3", mid = "yellow3", midpoint = 0)
  
  {
    data %>% 
      group_by(Team) %>% 
      add_count(Team, .drop = FALSE) %>% 
      summarize(
        Trend = lm(AdjEM ~ Year)$coefficients[2],
        n = mean(n)
      ) %>% 
      arrange(desc(Trend)) %>% 
      View
    }
}

#Year Analysis
{
  data %>% 
    ggplot(aes(y = AdjEM, color = Conference.Type)) +
    facet_grid(~Year + Conference.Type, scales = "free_x") +
    #geom_jitter(aes(color = Conference.Type, x = 0)) +
    geom_boxplot() +
    theme(
      axis.text.x = element_blank()
    )
  
 #freq.year <- 
   data %>% 
    filter(Year==2017) %>% 
    convert(int(Year)) %>% 
    ggplot(aes(x = AdjEM)) +
    #facet_grid(~Year + Conference.Type, scales = "free_x") +
    #geom_jitter(aes(color = Conference.Type, x = 0)) +
    geom_dotplot(aes(fill = Conference.Type), binpositions = "all",
                 method = "histodot", binwidth = 2) +
    scale_x_continuous(limits = c(-45,45), n.breaks = 20) +
    theme(
      plot.title = element_text(hjust = .5), plot.subtitle = element_text(hjust = .5)
    ) +
    ggtitle("Annual Spread of Adjusted Efficiency", subtitle = "Year: {frame_time}") +
    xlab("Adjusted Efficiency") +
    ylab("") 
  freq.year + transition_time(Year)
  
}


#Avg AdjEM of top 10 by year
{
  data %>% 
    group_by(Year) %>% 
    top_n(100, AdjEM) %>% 
    summarise(AdjEM = mean(AdjEM)) %>% 
    ggplot(aes(x = Year, y = AdjEM)) +
    geom_col()
}

#Path chart loop
{
  t <- "Virginia"
  pathSave <- function(t){
    data %>% 
      filter(Team == t) %>% 
      arrange(Year) %>% 
      ggplot(aes(x = AdjO, y = AdjD)) +
      geom_path(aes(alpha = Year)) +
      geom_point(aes(color = AdjEM, size = AdjEM)) +
      geom_label_repel(aes(label = Year, fill = Year), size = 3,
                       segment.size = .2) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = .5)) +
      scale_y_reverse() +
      ggtitle("Kenpom Adjusted Efficiency Metrics", subtitle = t) +
      xlab("Adjusted Offense") +
      ylab("Adjusted Defense") +
      labs(color = "Adjusted Efficiency", 
           size = "Adjusted Efficiency", fill = "Year", alpha = "Year") + 
      #scale_fill_gradient2(low = "red3", high = "green3", mid = "yellow3", midpoint = 0) +
      scale_fill_gradient(low = "gray90", high = "gray40") +
      scale_color_gradient2(low = "red3", high = "green3", mid = "yellow3", midpoint = 0)
      savePlot(
        #chart, 
        filename = paste0(getwd(), "/Kenpom Analysis/Path Chart/", t),
        type = "tiff"
      )
      save.image()
      unlink(paste0("Kenpom Analysis/Path Chart/", t, ".jpeg"))
  }
}


#Estimation of adjusted em
{
  data %>% 
    lm(AdjEM ~ AdjO + AdjD 
       , data = .) %>% 
    summary()
}