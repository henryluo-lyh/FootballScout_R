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

## Panel 3
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



# dash
app = Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)


get_panel3_content <- function(){
  return(
    
    dbcContainer(
      list(
        
        dbcRow(
          list(
            
            
            # sidebar
            dbcCol(
              list(
                
                htmlBr(),
                
                # dropdown
                htmlDiv(
                  list(
                    
                    # dd league
                    htmlLabel('League'),
                    dccDropdown(
                      id='p3_dd_league',
                      options = c(list(list(label = "ALL", value = "ALL")), lapply(league_names, function(league_name) list(label = league_name, value = league_name_id[[league_name]]))),
                      value='ALL'
                    ),
                    
                    # dd team
                    htmlLabel('Team'),
                    dccDropdown(
                      id='p3_dd_team',
                      # options=list(list('label': 'plz Choose league first', 'value': '123')),
                      # value='ALL',
                      placeholder='Select one team...'
                    ),
                    
                    # dd player
                    htmlLabel('Player'),
                    dccDropdown(
                      id='p3_dd_player',
                      placeholder='Select one Player...',
                      # multi = TRUE
                    )
                    
                  )
                )
                
              ), width=4
            ),
            
            # plot
            dbcCol(
              list(
                
                htmlDiv(id = 'div1'),
                
                dccGraph(id='p3_Iframe_1')
                
              )
            )
            
            
          )
        )
        
        
      )
    )
    
  )
}


app$layout(
  
  get_panel2_content()
  
)

# Panel3 - test callback
# app$callback(
#   output('div1', 'children'),
#   list(
#     input('p3_dd_player', 'value')
#   ),
#   function(p3_dd_player_value){
#     return(p3_dd_player_value)
#   }
# )



# Panel3 - callbacks
## Panel3 - dropdown2 need dropdown1 has a value firstly
app$callback(
  output(id = 'p3_dd_team', 'options'),
  list(input(id = 'p3_dd_league', 'value')),
  function(p3_dd_league_value) {
    
    temp_league_ids <- list()
    if (p3_dd_league_value == 'ALL') {
      temp_league_ids <- league_ids
    } else {
      temp_league_ids <- p3_dd_league_value
    }
    
    df_selected <- df[df$league_id %in% temp_league_ids,]
    specified_teams <- unique(df_selected$team_name)
    
    options <- c(list(list(label = "ALL", value = "ALL") ), lapply(specified_teams, function(team_name) list(label = team_name, value = team_name_id[[team_name]])))
    
    if(length(temp_league_ids) == 0){
      options <- list()
    }
    
    return(options)
  }
)

## Panel3 - dropdown3 need dropdown1 & dropdown2 have value firstly
app %>% add_callback(
  # output('p3_dd_player', 'options'),
  list(
    output('p3_dd_player', 'options'),
    output('p3_dd_player', 'value')
  ),
  list(
    input('p3_dd_league', 'value'),
    input('p3_dd_team', 'value')
  ),
  function(p3_dd_league_value, p3_dd_team_value ){
    
    temp_league_ids <- list()
    if (p3_dd_league_value == 'ALL') {
      temp_league_ids <- league_ids
    } else {
      temp_league_ids <- p3_dd_league_value
    }
    
    temp_team_ids <- list()
    if (p3_dd_team_value == 'ALL') {
      temp_team_ids <- team_ids
    } else {
      temp_team_ids <- p3_dd_team_value
    }
    
    df_selected <- df %>% 
      filter(league_id %in% temp_league_ids & team_id %in% temp_team_ids) %>% 
      filter(games_appearences != 0) %>% 
      drop_na(games_rating)
    
    specified_players <- unique(df_selected$player_name)
    
    options <- list()
    for (player_name in specified_players) {
      label <- player_name
      value <- player_name_id[[player_name]]
      options <- append(options, list(list(label = label, value = value)))
    }
    
    if(length(temp_league_ids) == 0 |
       length(temp_team_ids) == 0 ){
      options <- list()
    }
    
    # return(options)
    return(list(options, NULL) )
    
  }
)


## Panel3 - plot
app$callback(
  output('p3_Iframe_1', 'figure'),
  list(
    input('p3_dd_player', 'value')
  ),
  function(p3_dd_player_value) {
    
    df_selected <- df %>% 
      filter(games_appearences != 0) %>% 
      drop_na(games_rating)
    
    if (is.null(p3_dd_player_value[[1]]) | nrow(df_selected)==0 ) {
      return (ggplotly(ggplot() + theme_void()))
    } else {
      df_selected <- df_selected %>% 
        filter(player_id == p3_dd_player_value)
      
      df_pred <- df_selected %>% 
        select(league_season, games_rating)
      
      if( nrow(df_pred) == 0) {
        return( ggplotly(ggplot() + theme_void()) )
      }
      
      model <- lm(games_rating ~ league_season, data = df_pred)
      
      interplate <- coef(model)[1]
      beta1 <- coef(model)[2]
      
      games_rating <- interplate + beta1 * (max(df_selected$league_season) + 1)
      new_row <- tibble(league_season = max(df_pred$league_season) + 1, games_rating = games_rating)
      df_pred <- bind_rows(df_pred, new_row)
      df_pred <- as.data.frame(df_pred)
      
      chart1 <- ggplot(df_pred, aes(x = league_season, y = games_rating)) +
        geom_line() +
        scale_x_continuous(name = "League Season") +
        scale_y_continuous(name = "Games Rating") +
        ggtitle("Prediction of the following year's Games Rating")
      
      return ( ggplotly(chart1) )
    }
    
  }
)


# app$run_server(debug = T)







