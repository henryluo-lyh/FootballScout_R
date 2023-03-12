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

## panel 1
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


get_panel1_content <- function(){
  return(
    
    
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
                      id = 'p1_rs_year',
                      min=min_year, max=max_year,
                      value=list(min_year, max_year),
                      step=1,
                      marks = setNames(lapply(min_year:max_year, function(i) paste(i)), as.character(min_year:max_year) )
                      
                    )
                    
                  )
                ),
                htmlBr(),
                
                # dropdown
                htmlDiv(
                  list(
                    
                    # dd league
                    htmlLabel('League'),
                    dccDropdown(
                      id='p1_dd_league',
                      options = c(list(list(label = "ALL", value = "ALL")), lapply(league_names, function(league_name) list(label = league_name, value = league_name_id[[league_name]]))),
                      value='ALL'
                    ),
                    
                    
                    # dd team
                    htmlLabel('Team'),
                    dccDropdown(
                      id='p1_dd_team',
                      # options=list(list('label': 'plz Choose league first', 'value': '123')),
                      # value='ALL',
                      placeholder='Select one team...'
                    )
                    
                  )
                )
                
                
              ), width=4
            ),
            
            # plots
            dbcCol(
              list(
                
                # htmlDiv(id = 'div1'),
                
                # plot 1-2
                dbcRow(
                  list(
                    
                    # plot 1
                    dbcCol(
                      list(
                        dccGraph(id='p1_Iframe_1')
                      ), width = 6
                    ),
                    
                    # plot 2
                    dbcCol(
                      list(
                        dccGraph(id='p1_Iframe_2')
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
                        dccGraph(id='p1_Iframe_3')
                      ), width = 6
                    ),
                    
                    # plot 4
                    dbcCol(
                      list(
                        dccGraph(id='p1_Iframe_4')
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
    
    
  )
}



app = Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)





app$layout(
  
  get_panel1_content()
  
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


# app$run_server(debug = T)
