{
  library(tidyverse)
  library(hablar)
  library(scales)
  #library(gganimate)
  library(ggrepel)
  library(magrittr)
  library(gghighlight)
  library(directlabels)
  library(ggExtra)
  library(lubridate)
}

#Update data if needed
{
  #source("Scripts/Webscrape/Daily Data Webscrape.R")
  
  rm(list = ls())
}

#Get Data
{
  kenpom.raw <- read.csv(
    "Data/Kenpom - Daily Data - Season 2021.csv",
    colClasses = c("integer", "character", "character", rep("numeric", 7), "Date")
  )
  seeds.raw <- bind_rows(
    read.csv("Data/cbb.csv"),
    read.csv("Data/cbb21.csv") %>% mutate(YEAR = 2021)
  )
}

#Clean & Transform Data, if needed
{
  kenpom <- kenpom.raw %>% 
    mutate(
      Year = ifelse(month(Date) > 6, year(Date) + 1, year(Date))
    )
  
  data <- left_join(
    x = kenpom,
    y = seeds.raw %>% select(YEAR, TEAM, SEED, POSTSEASON), 
    by = c("Year" = "YEAR", "Team" = "TEAM")
  ) %>% 
    mutate(
      EliteEight = ifelse(Team %in% c("Gonzaga", "USC", "Michigan", "UCLA",
                                      "Baylor", "Arkansas", "Oregon St.", "Houston"),
                          1, 0),
      FinalFour = ifelse(Team %in% c("Gonzaga", "UCLA", "Baylor", "Houston"),
                          1, 0)
    ) %>% 
    rename(Seed = SEED, Postseason = POSTSEASON)
    
}



#Create Line Chart of season improvement
{
  #hightlighting elite eight
  {
    p <- data %>% 
      filter(!is.na(Seed)) %>%
      ggplot() +
      geom_line(
        aes(x = ymd(Date), y = AdjEM, color = Team),
        size = 1
      ) + 
      geom_point(
        data = data %>%
          filter(!is.na(Seed), Date == ymd("2021-03-17")),
        aes(x = Date, y = AdjEM),
        size = 0, color = "gray80"
      ) +
      geom_vline(xintercept = ymd("2021-03-17")) +
      gghighlight(
        max(EliteEight), max_highlight = 8,
        use_direct_label = FALSE
        # label_params = list(
        #   force = .1, position = position_dodge()
        # )
      ) +
      geom_dl(
        aes(
          x = Date, y = AdjEM, color = Team, label = Team
        ),
        method = list(
          dl.trans(x = x + .05),
          #dl.trans(y = y + .15),
          "last.bumpup", rot = 0, cex = 1
        ),
        size = .75
      ) +
      ggtitle(
        "2021 Tournament Teams Season-Long Adjusted Efficiency",
        "Elite Eight Teams"
      ) +
      ylab("Adjusted Efficiency Margin") +
      xlab("") +
      labs(caption = "Data Source: Kenpom.com") +
      scale_x_date(
        #date_minor_breaks = "1 week",
        limits = c(ymd("2020-12-01"), ymd("2021-4-1"))
      ) +
      scale_color_discrete(guide = FALSE) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = .5, size = 18),
        plot.subtitle = element_text(hjust = .5, size = 15),
        #plot.caption = element_text(size = 2),
        axis.title.y = element_text(size = 15)
      ) +
      geom_text(
        aes(x = ymd("2021-3-18"), y = -12.5,
            label = "Start of \nNCAA \nTournament"),
        size = 3, color = "black", hjust = 0
      ) +
      geom_vline(xintercept = ymd("2021-03-1")) +
      geom_text(
        aes(x = ymd("2021-3-2"), y = -12.5,
            label = "Start of \nConference \nTournaments"),
        size = 3, color = "black", hjust = 0
      )
    
    p2 <- ggMarginal(
      p, margins = "y",
      type="histogram", size=10
    )
    p2
    
    ggsave(
      p2,
      filename = "Visualizations/Tournament Teams Adjusted Efficiency.png",
      width = 15, height = 10
    )
  }
  
  #hightlighting final 4
  {
    p <- data %>% 
      filter(!is.na(Seed)) %>%
      ggplot() +
      geom_line(
        aes(x = ymd(Date), y = AdjEM, color = Team),
        size = 1
      ) + 
      geom_point(
        data = data %>%
          filter(!is.na(Seed), Date == ymd("2021-03-17")),
        aes(x = Date, y = AdjEM),
        size = 0, color = "gray80"
      ) +
      geom_vline(xintercept = ymd("2021-03-17")) +
      gghighlight(
        max(FinalFour), max_highlight = 4,
        use_direct_label = FALSE
      ) +
      geom_dl(
        aes(
          x = Date, y = AdjEM, color = Team, label = Team
        ),
        method = list(
          dl.trans(x = x + .05),
          "last.bumpup", rot = 0, cex = 1
        ),
        size = .75
      ) +
      ggtitle(
        "2021 Tournament Teams Season-Long Adjusted Efficiency",
        "Final Four"
      ) +
      ylab("Adjusted Efficiency Margin") +
      xlab("") +
      labs(caption = "Data Source: Kenpom.com") +
      scale_x_date(
        limits = c(ymd("2020-12-01"), ymd("2021-4-1"))
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = .5, size = 18),
        plot.subtitle = element_text(hjust = .5, size = 15),
        axis.title.y = element_text(size = 15)
      ) +
      geom_text(
        aes(x = ymd("2021-3-18"), y = -12.5,
            label = "Start of \nNCAA \nTournament"),
        size = 3, color = "black", hjust = 0
      ) +
      geom_vline(xintercept = ymd("2021-03-1")) +
      geom_text(
        aes(x = ymd("2021-3-2"), y = -12.5,
            label = "Start of \nConference \nTournaments"),
        size = 3, color = "black", hjust = 0
      ) + 
      scale_color_manual(values = c(
        "Baylor" = "#003015",
        "Gonzaga" = "#041E42",
        "UCLA" = "#2D68C4", #F2A900
        "Houston" = "#C8102E" #76232F
        ), guide = FALSE)
    
    p2 <- ggMarginal(
      p, margins = "y",
      type="densigram", size=10
    )
    p2
    
    ggsave(
      p2,
      filename = "Visualizations/Tournament Teams Adjusted Efficiency.png",
      width = 15, height = 10
    )
  }
  
  #hightlighting specific conference
  {
    confHighlight.SeasonAdjEM <- function(conf){
      
      p <- data %>% 
        filter(!is.na(Seed)) %>%
        ggplot() +
        geom_line(
          aes(x = ymd(Date), y = AdjEM, color = Team),
          size = 1
        ) + 
        geom_point(
          data = data %>%
            filter(!is.na(Seed), Date == ymd("2021-03-17")),
          aes(x = Date, y = AdjEM),
          size = 0, color = "gray80"
        ) +
        geom_vline(xintercept = ymd("2021-03-17")) +
        gghighlight(
          #max(EliteEight), max_highlight = 8,
          Conference == conf,
          use_direct_label = FALSE
          # label_params = list(
          #   force = .1, position = position_dodge()
          # )
        ) +
        geom_dl(
          aes(
            x = Date, y = AdjEM, color = Team, label = Team
          ),
          method = list(
            dl.trans(x = x + .05),
            #dl.trans(y = y + .15),
            "last.bumpup", rot = 0, cex = 1
          ),
          size = .75
        ) +
        ggtitle(
          "2021 Tournament Teams Season-Long Adjusted Efficiency",
          conf
        ) +
        ylab("Adjusted Efficiency Margin") +
        xlab("") +
        labs(caption = "Data Source: Kenpom.com") +
        scale_x_date(
          #date_minor_breaks = "1 week",
          limits = c(ymd("2020-12-01"), ymd("2021-4-3"))
        ) +
        scale_color_discrete(guide = FALSE) +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = .5, size = 18),
          plot.subtitle = element_text(hjust = .5, size = 15),
          #plot.caption = element_text(size = 2),
          axis.title.y = element_text(size = 15)
        ) +
        geom_text(
          aes(x = ymd("2021-3-18"), y = -12.5,
              label = "Start of \nNCAA \nTournament"),
          size = 3, color = "black", hjust = 0
        ) +
        geom_vline(xintercept = ymd("2021-03-1")) +
        geom_text(
          aes(x = ymd("2021-3-2"), y = -12.5,
              label = "Start of \nConference \nTournaments"),
          size = 3, color = "black", hjust = 0
        )
      
      p2 <- ggMarginal(
        p, margins = "y",
        type="histogram", size=10
      )
      #p2
      
      fn <- paste0("Visualizations/Tournament Teams Adjusted Efficiency - ",
                   conf,".png")
      ggsave(
        p2,
        filename = fn,
        width = 15, height = 10
      )
    }
    
    lapply(unique(data$Conference), confHighlight.SeasonAdjEM)
    
    
  }
  
  #Teams with greatest improvement
  {
    
    data %>% 
      filter(!is.na(Seed)) %>%
      group_by(Team) %>%
      arrange(Date) %>% 
      mutate(
        d.AdjEM = AdjEM - lag(AdjEM),
        #Week = floor(as.integer(Date - ymd("2020-11-25")) / 7) + 1
        Month = month(Date)
      ) %>% 
      group_by(Team, Conference, Month) %>% 
      summarize(
        d.AdjEM = sum(d.AdjEM)
      ) %>% 
      ungroup() %>% 
      filter(!is.na(d.AdjEM), Month == 3) %>% 
      mutate(
        hottest = percent_rank(d.AdjEM)
      ) %>% View
      ggplot() +
      geom_line(
        aes(x = ymd(Date), y = AdjEM, color = Team),
        size = 1
      ) + 
      geom_point(
        data = data %>%
          filter(!is.na(Seed), Date == ymd("2021-03-17")),
        aes(x = Date, y = AdjEM),
        size = 0, color = "gray80"
      ) +
      geom_vline(xintercept = ymd("2021-03-17")) +
      gghighlight(
        max(FinalFour), max_highlight = 4,
        use_direct_label = FALSE
        # label_params = list(
        #   force = .1, position = position_dodge()
        # )
      ) +
      geom_dl(
        aes(
          x = Date, y = AdjEM, color = Team, label = Team
        ),
        method = list(
          dl.trans(x = x + .05),
          #dl.trans(y = y + .15),
          "last.bumpup", rot = 0, cex = 1
        ),
        size = .75
      ) +
      ggtitle(
        "2021 Tournament Teams Season-Long Adjusted Efficiency",
        "Final Four"
      ) +
      ylab("Adjusted Efficiency Margin") +
      xlab("") +
      labs(caption = "Data Source: Kenpom.com") +
      scale_x_date(
        #date_minor_breaks = "1 week",
        limits = c(ymd("2020-12-01"), ymd("2021-4-1"))
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = .5, size = 18),
        plot.subtitle = element_text(hjust = .5, size = 15),
        #plot.caption = element_text(size = 2),
        axis.title.y = element_text(size = 15)
      ) +
      geom_text(
        aes(x = ymd("2021-3-18"), y = -12.5,
            label = "Start of \nNCAA \nTournament"),
        size = 3, color = "black", hjust = 0
      ) +
      geom_vline(xintercept = ymd("2021-03-1")) +
      geom_text(
        aes(x = ymd("2021-3-2"), y = -12.5,
            label = "Start of \nConference \nTournaments"),
        size = 3, color = "black", hjust = 0
      ) + 
      scale_color_manual(values = c(
        "Baylor" = "#003015",
        "Gonzaga" = "#041E42",
        "UCLA" = "#2D68C4", #F2A900
        "Houston" = "#C8102E" #76232F
      ), guide = FALSE)
    
    p2 <- ggMarginal(
      p, margins = "y",
      type="densigram", size=10
    )
    p2
    
    ggsave(
      p2,
      filename = "Visualizations/Tournament Teams Adjusted Efficiency.png",
      width = 15, height = 10
    )
    
  }
  
  #Best teams not invited to tourney
  {
    
    p <- data %>% 
      filter(!is.na(Seed) |
               Team %in% (
                 data %>%
                   filter(
                     is.na(Seed), Date == ymd("2021-03-17")
                   ) %>%
                   top_n(n = 5, wt = AdjEM)
               )$Team) %>%
      mutate(
        Top.NotDance = ifelse(is.na(Seed), 1, 0)
      ) %>% 
      ggplot() +
      geom_line(
        aes(x = ymd(Date), y = AdjEM, color = Team),
        size = 1
      ) + 
      geom_point(
        data = data %>%
          filter(!is.na(Seed), Date == ymd("2021-03-17")),
        aes(x = Date, y = AdjEM),
        size = 0, color = "gray80"
      ) +
      geom_vline(xintercept = ymd("2021-03-17")) +
      gghighlight(
        Top.NotDance == 1,
        use_direct_label = FALSE
      ) +
      geom_dl(
        aes(
          x = Date, y = AdjEM, color = Team, label = Team
        ),
        method = list(
          dl.trans(x = x + .05),
          "last.bumpup", rot = 0, cex = 1
        ),
        size = .75
      ) +
      ggtitle(
        "Adjusted Efficiency over Duration of 2021 Season",
        "Top 5 Teams Not Invited to NCAA Tournament Highlighted with Tournament Teams in Gray"
      ) +
      ylab("Adjusted Efficiency Margin") +
      xlab("") +
      labs(caption = "Data Source: Kenpom.com") +
      scale_x_date(
        limits = c(ymd("2020-12-01"), ymd("2021-4-1"))
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = .5, size = 18),
        plot.subtitle = element_text(hjust = .5, size = 14),
        axis.title.y = element_text(size = 15)
      ) +
      geom_text(
        aes(x = ymd("2021-3-18"), y = -12.5,
            label = "Start of \nNCAA \nTournament"),
        size = 3, color = "black", hjust = 0
      ) +
      geom_vline(xintercept = ymd("2021-03-1")) +
      geom_text(
        aes(x = ymd("2021-3-2"), y = -12.5,
            label = "Start of \nConference \nTournaments"),
        size = 3, color = "black", hjust = 0
      ) + 
      scale_color_discrete(
        guide = FALSE
      )
    
    p2 <- ggMarginal(
      p, margins = "y",
      type="densigram", size=10
    )
    p2
    
    ggsave(
      p2,
      filename = "Visualizations/Top Teams Not Invited to Tourney.png",
      width = 15, height = 10
    )
    
  }
}




























