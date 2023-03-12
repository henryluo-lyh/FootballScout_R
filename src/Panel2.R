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

get_panel2_content <- function(){
  
  dbcContainer(
    list(
      
      dbcRow(
        list(
          
          # sidebar
          dbcCol(
            list(
              
              htmlBr(),
              
              # slider
              htmlDiv(
                list(
                  
                  htmlLabel('Year Range'),
                  dccRangeSlider(
                    id = 'p2_rs_year',
                    min=min_year, max=max_year,
                    value=list(min_year, max_year),
                    step=1,
                    marks = setNames(lapply(min_year:max_year, function(i) paste(i)), as.character(min_year:max_year) )
                    
                  )
                  
                ), 
              ),
              htmlBr(),
              
              # dropdown
              htmlDiv(
                list(
                  
                  # dd league
                  htmlLabel('League'),
                  dccDropdown(
                    id='p2_dd_league',
                    options = c(list(list(label = "ALL", value = "ALL")), lapply(league_names, function(league_name) list(label = league_name, value = league_name_id[[league_name]]))),
                    # value='ALL'
                  ),
                  
                  # dd team
                  htmlLabel('Team'),
                  dccDropdown(
                    id='p2_dd_team',
                    # options=list(list('label': 'plz Choose league first', 'value': '123')),
                    # value='ALL',
                    placeholder='Select one team...'
                  ),
                  
                  # dd players
                  htmlLabel('Players'),
                  dccDropdown(
                    id='p2_dd_player',
                    placeholder='Select Players...',
                    multi = TRUE
                  )
                  
                )
              )
              
            ), width=4
          ),
          
          dbcCol(
            list(
              
              #test
              # htmlDiv(id = 'p2_test')
              
              
              # plots
              # plot 1-2
              dbcRow(
                list(
                  
                  # plot 1
                  dbcCol(
                    list(
                      # dd status 1
                      dccDropdown(
                        id = 'p2_dd_stat1',
                        options = lapply(stats, function(stat) list(label = str_to_title(str_replace(stat, '_', ' ')), value = stat)),
                        value='games_rating',
                        placeholder='Select one stat...'
                      ),
                      # plot 1
                      dccGraph(id='p2_Iframe_1')
                    ), width = 6
                  ),
                  
                  # plot 2
                  dbcCol(
                    list(
                      # dd status 2
                      dccDropdown(
                        id = 'p2_dd_stat2',
                        options = lapply(stats, function(stat) list(label = str_to_title(str_replace(stat, '_', ' ')), value = stat)),
                        value='games_lineups',
                        placeholder='Select one stat...'
                      ),
                      # plot 2
                      dccGraph(id='p2_Iframe_2')
                    ), width = 6
                  )
                  
                )
              ),
              
              # plot 3-4
              dbcRow(
                list(
                  
                  # plot 3
                  dbcCol(
                    list(
                      # dd status 3
                      dccDropdown(
                        id = 'p2_dd_stat3',
                        options = lapply(stats, function(stat) list(label = str_to_title(str_replace(stat, '_', ' ')), value = stat)),
                        value='games_appearences',
                        placeholder='Select one stat...'
                      ),
                      # plot 3
                      dccGraph(id='p2_Iframe_3')
                    ), width = 6
                  ),
                  
                  # plot 4
                  dbcCol(
                    list(
                      # dd status 4
                      dccDropdown(
                        id = 'p2_dd_stat4',
                        options = lapply(stats, function(stat) list(label = str_to_title(str_replace(stat, '_', ' ')), value = stat)),
                        value='shots_total',
                        placeholder='Select one stat...'
                      ),
                      # plot 4
                      dccGraph(id='p2_Iframe_4')
                    ), width = 6
                  )
                  
                )
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
















# Panel2 - callbacks
## Panel2 - dropdown2 need dropdown1 has a value firstly
app$callback(
  output(id = 'p2_dd_team', 'options'),
  list(input(id = 'p2_dd_league', 'value')),
  function(p2_dd_league_value) {
    
    temp_league_ids <- list()
    if (p2_dd_league_value == 'ALL') {
      temp_league_ids <- league_ids
    } else {
      temp_league_ids <- p2_dd_league_value
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



## Panel2 - dropdown3 need dropdown1 & dropdown2 have value firstly
app %>% add_callback(
  # output('p2_dd_player', 'options'),
  list(
    output('p2_dd_player', 'options'),
    output('p2_dd_player', 'value')
  ),
  list(
    input('p2_dd_league', 'value'),
    input('p2_dd_team', 'value')
  ),
  function(p2_dd_league_value, p2_dd_team_value ){
    
    temp_league_ids <- list()
    if (p2_dd_league_value == 'ALL') {
      temp_league_ids <- league_ids
    } else {
      temp_league_ids <- p2_dd_league_value
    }
    
    temp_team_ids <- list()
    if (p2_dd_team_value == 'ALL') {
      temp_team_ids <- team_ids
    } else {
      temp_team_ids <- p2_dd_team_value
    }
    
    df_selected <- df %>% 
      filter(league_id %in% temp_league_ids & team_id %in% temp_team_ids)
    
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


## Panel2 - plots
app %>% add_callback(
  list(
    output('p2_Iframe_1', 'figure'),
    output('p2_Iframe_2', 'figure'),
    output('p2_Iframe_3', 'figure'),
    output('p2_Iframe_4', 'figure')
  ),
  
  list(
    input('p2_rs_year', 'value'),
    input('p2_dd_league', 'value'),
    input('p2_dd_team', 'value'),
    input('p2_dd_player', 'value'),
    input('p2_dd_stat1', 'value'),
    input('p2_dd_stat2', 'value'),
    input('p2_dd_stat3', 'value'),
    input('p2_dd_stat4', 'value')
  ),
  function(p2_rs_year_value, p2_dd_league_value, p2_dd_team_value, p2_dd_player_value,
           p2_dd_stat1_value, p2_dd_stat2_value, p2_dd_stat3_value, p2_dd_stat4_value){
    
    # print('\nnew turn')
    
    # args
    years <- p2_rs_year_value[[1]]:p2_rs_year_value[[2]]

    if (p2_dd_league_value == "ALL") {
      temp_league_ids <- league_ids
    } else {
      temp_league_ids <- c(p2_dd_league_value)
    }

    if (p2_dd_team_value == "ALL") {
      temp_team_ids <- df %>%
        filter(league_id %in% temp_league_ids) %>%
        select(team_id) %>%
        distinct() %>%
        pull()
    } else {
      temp_team_ids <- c(p2_dd_team_value)
    }
    # print(temp_team_ids)

    if(is.null(p2_dd_player_value)){
      temp_player_ids <- list()
    } else {
      temp_player_ids <- p2_dd_player_value
    }
    # print(temp_player_ids)
    


    df_selected <- df %>%
      filter(`team_id` %in% temp_team_ids & `league_id` %in% temp_league_ids & `league_season` %in% years & `player_id` %in% temp_player_ids)
    # print(nrow(df_selected))
    
    # print(length(temp_league_ids))
    # print(length(temp_team_ids))
    # print(length(temp_player_ids))
    # print(temp_player_ids)
    if(length(temp_player_ids) == 0 |
       length(temp_team_ids) == 0 |
       length(temp_league_ids) == 0 ) {
      chart1 <- ggplotly(ggplot() + theme_void())
      chart2 <- ggplotly(ggplot() + theme_void())
      chart3 <- ggplotly(ggplot() + theme_void())
      chart4 <- ggplotly(ggplot() + theme_void())
      # print('空')
    } else if(is.null(temp_player_ids[[1]]) |
              is.null(temp_team_ids[[1]]) |
              is.null(temp_league_ids[[1]])){
      chart1 <- ggplotly(ggplot() + theme_void())
      chart2 <- ggplotly(ggplot() + theme_void())
      chart3 <- ggplotly(ggplot() + theme_void())
      chart4 <- ggplotly(ggplot() + theme_void())
      # print('空')
    } else {
      # print('不空')
      
      # plot1
      y_axis1 <- p2_dd_stat1_value
      chart1 <- ggplot(df_selected, aes(x = league_season, y = !!sym(y_axis1), color = player_name)) +
        geom_line() +
      scale_y_continuous(name = str_to_title(str_replace(y_axis1, '_', ' '))) +
      scale_x_continuous(name = 'League Season', breaks = years) +
      labs(title = paste(str_to_title(str_replace(y_axis1, '_', ' ')), 'by League Season'))
      chart1 <- ggplotly(chart1)
      
      # plot2
      y_axis2 <- p2_dd_stat2_value
      chart2 <- ggplot(df_selected, aes(x = league_season, y = !!sym(y_axis2), color = player_name)) +
        geom_line() +
        scale_y_continuous(name = str_to_title(str_replace(y_axis2, '_', ' '))) +
        scale_x_continuous(name = 'League Season', breaks = years) +
        labs(title = paste(str_to_title(str_replace(y_axis2, '_', ' ')), 'by League Season'))
      chart2 <- ggplotly(chart2)
      
      # plot3
      y_axis3 <- p2_dd_stat3_value
      chart3 <- ggplot(df_selected, aes(x = league_season, y = !!sym(y_axis3), color = player_name)) +
        geom_line() +
        scale_y_continuous(name = str_to_title(str_replace(y_axis3, '_', ' '))) +
        scale_x_continuous(name = 'League Season', breaks = years) +
        labs(title = paste(str_to_title(str_replace(y_axis3, '_', ' ')), 'by League Season'))
      chart3 <- ggplotly(chart3)
      
      # plot4
      y_axis4 <- p2_dd_stat4_value
      chart4 <- ggplot(df_selected, aes(x = league_season, y = !!sym(y_axis4), color = player_name)) +
        geom_line() +
        scale_y_continuous(name = str_to_title(str_replace(y_axis4, '_', ' '))) +
        scale_x_continuous(name = 'League Season', breaks = years) +
        labs(title = paste(str_to_title(str_replace(y_axis4, '_', ' ')), 'by League Season'))
      chart4 <- ggplotly(chart4)
      
    }

    return(list(chart1, chart2, chart3, chart4))


  }
)



# app$run_server(debug = T)

