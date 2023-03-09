library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)

library(ggplot2)
library(plotly)

library(tidyverse)

# dataset
data_add = './data/processed/all_players___.csv'
df <- read_csv(data_add, col_names = TRUE)

## panel 2
min_year <- min(df$league_season) 
max_year <- max(df$league_season) 

league_name_id <- list('Serie A' = 135,
                       'La Liga' = 140,
                       'Super League' = 169,
                       'Premier League' = 39,
                       'Ligue 1' = 61,
                       'Bundesliga' = 78)
league_names <- unique(df$league_name)
league_ids <- unname(as.numeric(league_name_id))

team_name_id <- df %>% distinct(team_name, team_id) %>% 
  select(team_name, team_id) %>% 
  deframe() %>% as.list()
team_names <- unique(df$team_name)
team_ids <- unname(as.numeric(team_name_id))

player_name_id <- df %>% distinct(player_name, player_id) %>% 
  select(player_name, player_id) %>% 
  deframe() %>% as.list()
player_names <- unique(df$player_name)
player_ids <- unname(as.numeric(player_name_id))

stats <- c(
  'games_appearences',
  'games_lineups',
  'games_minutes',
  'games_rating',
  'substitutes_in',
  'substitutes_out',
  'substitutes_bench',
  'shots_total',
  'shots_on',
  'goals_total',
  'goals_conceded',
  'goals_assists',
  'goals_saves',
  'passes_total',
  'passes_key',
  'passes_accuracy',
  'tackles_total',
  'tackles_blocks',
  'tackles_interceptions',
  'duels_total',
  'duels_won',
  'dribbles_attempts',
  'dribbles_success',
  'dribbles_past',
  'fouls_drawn',
  'fouls_committed',
  'cards_yellow',
  'cards_yellowred',
  'cards_red',
  'penalty_won',
  'penalty_commited',
  'penalty_scored',
  'penalty_missed',
  'penalty_saved',
  # 'player_age',
  'player_height',
  'player_weight'
)

# dash
app = Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)



app$layout(
  
  
  dbcContainer(
    list(
      
      # Panels
      dbcTabs(
        list(
          
          
          
          # Panel1
          dbcTab(
            list(
              
            ), label = 'Panel1'
          ),
          
          
          # Panel2
          dbcTab(
            list(
              
              
            ), label = 'Panel2'
          ),
          
          
          # Panel3
          dbcTab(
            list(
              
            ), label = 'Panel3'
          )
          
          
          
        )
      )
      
    )
  )
  
  
)



app$run_server(host = '0.0.0.0')
# app$run_server()






