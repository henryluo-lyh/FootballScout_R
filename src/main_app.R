library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)

library(ggplot2)
library(plotly)

library(tidyverse)

source('./src/Panel1.R')
source('./src/Panel2.R')
source('./src/Panel3.R')

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
              
              get_panel1_content()
              
            ), label = 'Panel1'
          ),
          
          
          # Panel2
          dbcTab(
            list(
              
              get_panel2_content()
              
            ), label = 'Panel2'
          ),
          
          
          # Panel3
          dbcTab(
            list(
              
              get_panel3_content()
              
            ), label = 'Panel3'
          )
          
          
          
        )
      )
      
    )
  )
  
  
)






# callback

# Panel1
## dropdown2 need dropdown1 has a value firstly
app$callback(
  # output(id = 'p1_dd_team', 'options'),
  list(
    output(id = 'p1_dd_team', 'options'),
    output(id = 'p1_dd_team', 'value')
  ),
  list(input(id = 'p1_dd_league', 'value')),
  function(p1_dd_league_value) {
    temp_league_ids <- list()
    if (p1_dd_league_value == 'ALL') {
      temp_league_ids <- league_ids
    } else {
      temp_league_ids <- p1_dd_league_value
    }
    
    df_selected <- df[df$league_id %in% temp_league_ids,]
    specified_teams <- unique(df_selected$team_name)
    
    options <- c(list(list(label = "ALL", value = "ALL") ), lapply(specified_teams, function(team_name) list(label = team_name, value = team_name_id[[team_name]])))
    
    return(list(options, NULL) )
  }
)

# Panel1 - plot1
app %>% add_callback(
  output(id = 'p1_Iframe_1', 'figure'),
  list(
    input(id = 'p1_rs_year', 'value'),
    input(id = 'p1_dd_league', 'value'),
    input(id = 'p1_dd_team', 'value')
  ),
  function(p1_rs_year_value, p1_dd_league_value, p1_dd_team_value) {
    # print(class( p1_rs_year_value[[1]]) )
    
    temp_league_ids <- c()
    if (p1_dd_league_value == 'ALL') {
      temp_league_ids <- league_ids # 是向量
    } else {
      temp_league_ids <- c(p1_dd_league_value)
    }
    
    temp_team_ids <- c()
    if (p1_dd_team_value == 'ALL') {
      temp_team_ids <- unname(unlist(unique(df %>%
                                              filter(league_id %in% temp_league_ids) %>%
                                              select(team_id))) )
    } else {
      temp_team_ids <- c(p1_dd_team_value)
    }
    
    years <- seq(p1_rs_year_value[[1]], p1_rs_year_value[[2]], by = 1)
    # print(class(years) )
    
    df_selected <- df %>%
      filter(team_id %in% temp_team_ids,
             league_id %in% temp_league_ids,
             league_season %in% years)
    # print(head(df_selected))
    
    # get top 5 ids
    df_mean <- df_selected %>%
      group_by(player_id) %>%
      summarise(games_rating = mean(games_rating, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(desc(games_rating)) %>%
      slice_head(n = 5)
    
    
    top5_ids <- df_mean$player_id
    
    # get
    df_target <- df_selected %>%
      filter(player_id %in% top5_ids)
    # print(df_target)
    
    if( is.null(temp_league_ids[[1]]) | is.null(temp_team_ids[[1]]) |
        nrow(df_target) == 0 ) {
      return(ggplotly(ggplot() + theme_void()))
    }
    
    chart1 <- ggplot(df_target, aes(x = league_season, y = games_rating, color = player_name)) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = seq(2010, 2022, 1)) +
      # scale_y_continuous(name = 'Game Rating', expand = expansion(mult = c(0, 0.05))) +
      labs(title = 'Games Rating by Season and Player', x = 'Season', y = 'Games Rating')
    
    return(ggplotly(chart1) )
  }
)


# Panel1 - plot2
app %>% add_callback(
  output(id = 'p1_Iframe_2', 'figure'),
  list(
    input(id = 'p1_rs_year', 'value'),
    input(id = 'p1_dd_league', 'value'),
    input(id = 'p1_dd_team', 'value')
  ),
  function(p1_rs_year_value, p1_dd_league_value, p1_dd_team_value) {
    # drop na
    df_dropna <- df %>%
      drop_na(games_rating, league_season)
    
    # args
    years <- p1_rs_year_value[[1]] : p1_rs_year_value[[2]]
    
    temp_league_ids <- ifelse(p1_dd_league_value == 'ALL', league_ids, p1_dd_league_value )
    
    temp_team_ids <- ifelse(p1_dd_team_value == 'ALL', 
                            df %>%
                              filter(league_id %in% temp_league_ids) %>%
                              pull(team_id) %>%
                              unique(), 
                            p1_dd_team_value)
    
    # df_selected = df_dropna %>% filter(league_season %in% years)
    df_selected <- df %>% 
      filter(team_id %in% temp_team_ids, 
             league_id %in% temp_league_ids, 
             league_season %in% years) %>% 
      select(league_season, games_rating) %>% 
      na.omit()
    
    if( is.null(temp_league_ids[[1]]) | is.null(temp_team_ids[[1]]) |
        nrow(df_selected) == 0 ) {
      return(ggplotly(ggplot() + theme_void()))
    }
    
    chart2 <- ggplot(df_selected, aes(x = games_rating)) + 
      geom_density(fill = "skyblue") +
      labs(title = 'Density Plot of Game Rating', x = 'Game Rating', y = 'Density')
    
    return(ggplotly(chart2) )
  }
)


# Panel1 - plot3
app %>% add_callback(
  output(id = 'p1_Iframe_3', 'figure'),
  list(
    input(id = 'p1_rs_year', 'value'),
    input(id = 'p1_dd_league', 'value'),
    input(id = 'p1_dd_team', 'value')
  ),
  function(p1_rs_year_value, p1_dd_league_value, p1_dd_team_value) {
    
    # drop na
    df_dropna <- df[!is.na(df$games_rating) & !is.na(df$league_season),]
    
    # args
    years <- p1_rs_year_value[[1]]:p1_rs_year_value[[2]]
    
    if (p1_dd_league_value == 'ALL') {
      temp_league_ids <- league_ids
    } else {
      temp_league_ids <- c(p1_dd_league_value)
    }
    
    if (p1_dd_team_value == 'ALL') {
      temp_team_ids <- unique(df[df$league_id %in% temp_league_ids,]$team_id)
    } else {
      temp_team_ids <- c(p1_dd_team_value)
    }
    
    # df_selected <- df_dropna[df_dropna$league_season %in% years,]
    df_selected <- df_dropna[df_dropna$team_id %in% temp_team_ids & df_dropna$league_id %in% temp_league_ids & df_dropna$league_season %in% years,]
    
    if( is.null(temp_league_ids[[1]]) | is.null(temp_team_ids[[1]]) |
        nrow(df_selected) == 0 ) {
      return(ggplotly(ggplot() + theme_void()))
    }
    
    chart3 <- ggplot(df_selected, aes(x=games_rating)) +
      geom_histogram(bins=200) +
      scale_y_log10() +
      labs(title = "Histogram of Game Rating", x = "Game Rating", y = "Count")
    
    return(ggplotly(chart3))
    
  }
)

# Panel1 - plot4
app %>% add_callback(
  output(id = 'p1_Iframe_4', 'figure'),
  list(
    input(id = 'p1_rs_year', 'value'),
    input(id = 'p1_dd_league', 'value'),
    input(id = 'p1_dd_team', 'value')
  ),
  function(p1_rs_year_value, p1_dd_league_value, p1_dd_team_value) {
    
    # args
    years <- p1_rs_year_value[[1]]:p1_rs_year_value[[2]]
    
    if (p1_dd_league_value == "ALL") {
      temp_league_ids <- league_ids
    } else {
      temp_league_ids <- c(p1_dd_league_value)
    }
    
    if (p1_dd_team_value == "ALL") {
      temp_team_ids <- df %>%
        filter(league_id %in% temp_league_ids) %>%
        select(team_id) %>%
        distinct() %>%
        pull()
    } else {
      temp_team_ids <- c(p1_dd_team_value)
    }
    
    # df_selected <- df %>% 
    #   filter(league_season %in% years)
    df_selected <- df %>% 
      filter(team_id %in% temp_team_ids, 
             league_id %in% temp_league_ids, 
             league_season %in% years) %>% 
      select(league_season, games_rating) %>% 
      na.omit()
    
    
    if( is.null(temp_league_ids[[1]]) | is.null(temp_team_ids[[1]]) |
        nrow(df_selected) == 0 ) {
      return(ggplotly(ggplot() + theme_void()))
    }
    
    chart4 <- ggplot(df_selected, aes(x = as.factor(league_season), y = games_rating)) +
      geom_boxplot() +
      labs(title = "Boxplot of Game Rating by Season", x = "League Season", y = "Game Rating") + 
      coord_flip()
    
    return(ggplotly(chart4))
    
  }
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





app$run_server(host = '0.0.0.0')
# app$run_server()






