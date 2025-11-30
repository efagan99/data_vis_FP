#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(scales)

#import data
nba <- read.csv("nba_salary_clean.csv", header = TRUE, 
                col.names = c("player_name", "salary", "position", "age", "team", "games_played",
                              "minutes_per_game", "field_goal_attempts", "field_goal_percentage",
                              "rebounds", "assists", "points", "total_minutes", "player_efficiency_rating",
                              "true_shooting_percentage", "free_throw_rate", "rebound_percentage",
                              "assist_percentage", "usage_rate", "win_shares", "offensive_box_plus_minus",
                              "defensive_box_plus_minus", "box_plus_minus", "value_over_replacement_player"))

stat_choices <- c("Age" = "age", "Minutes per Game" = "minutes_per_game", "Field Goal Attempts per Game" = "field_goal_attempts",
                  "Field Goal Percentage" = "field_goal_percentage", "Rebounds per Game" = "rebounds", "Assists per Game" = "assists",
                  "Points per Game" = "points", "Player Efficiency Rating" = "player_efficiency_rating",
                  "True Shooting Percentage" = "true_shooting_percentage", "Free Throw Rate" = "free_throw_rate",
                  "Rebound Percentage" = "rebound_percentage", "Assist Percentage" = "assist_percentage",
                  "Usage Rate" = "usage_rate", "Win Shares" = "win_shares",
                  "Offensive Box Plus Minus" = "offensive_box_plus_minus",
                  "Defensive Box Plus Minus" = "defensive_box_plus_minus",
                  "Box Plus Minus" = "box_plus_minus",
                  "Value Over Replacement Player" = "value_over_replacement_player")

display_names <- c("Player Name" = "player_name", "Salary" = "salary", "Position" = "position", "Age" = "age",
  "Team" = "team", "Games Played" = "games_played", "Minutes per Game" = "minutes_per_game",
  "Field Goal Attempts" = "field_goal_attempts", "Field Goal Percentage" = "field_goal_percentage",
  "Rebounds" = "rebounds", "Assists" = "assists", "Points" = "points", "Total Minutes" = "total_minutes",
  "Player Efficiency Rating (PER)" = "player_efficiency_rating", "True Shooting Percentage" = "true_shooting_percentage",
  "Free Throw Rate" = "free_throw_rate", "Rebound Percentage" = "rebound_percentage", "Assist Percentage" = "assist_percentage",
  "Usage Rate" = "usage_rate", "Win Shares" = "win_shares", "Offensive Box Plus Minus (OBPM)" = "offensive_box_plus_minus",
  "Defensive Box Plus Minus (DBPM)" = "defensive_box_plus_minus", "Box Plus Minus (BPM)" = "box_plus_minus",
  "Value Over Replacement Player (VORP)" = "value_over_replacement_player")

short_names <- c(
  "salary" = "Salary", "position" = "Position", "age" = "Age",
  "team" = "Team", "games_played" = "GP", "minutes_per_game" = "MPG", "field_goal_attempts" = "FGA",
  "field_goal_percentage" = "FG%", "rebounds" = "REB", "assists" = "AST", "points" = "PTS",
  "total_minutes" = "Total Minutes", "player_efficiency_rating" = "PER", "true_shooting_percentage" = "TS%",
  "free_throw_rate" = "FT Rate", "rebound_percentage" = "REB%", "assist_percentage" = "AST%",
  "usage_rate" = "USG%", "win_shares" = "Win Shares", "offensive_box_plus_minus" = "OBPM",
  "defensive_box_plus_minus" = "DBPM", "box_plus_minus" = "BPM", "value_over_replacement_player" = "VORP")

#team_names <- c("ATL" = "Atlanta Hawks", "BOS" = "Boston Celtics", "BRK" = "Brooklyn Nets",
  #"CHO" = "Charlotte Hornets", "CHI" = "Chicago Bulls", "CLE" = "Cleveland Cavaliers", "DAL" = "Dallas Mavericks",
  #"DEN" = "Denver Nuggets", "DET" = "Detroit Pistons", "GSW" = "Golden State Warriors", "HOU" = "Houston Rockets",
  #"IND" = "Indiana Pacers", "LAC" = "Los Angeles Clippers", "LAL" = "Los Angeles Lakers",
  #"MEM" = "Memphis Grizzlies", "MIA" = "Miami Heat", "MIL" = "Milwaukee Bucks", "MIN" = "Minnesota Timberwolves",
  #"NOP" = "New Orleans Pelicans", "NYK" = "New York Knicks", "OKC" = "Oklahoma City Thunder",
  #"ORL" = "Orlando Magic", "PHI" = "Philadelphia 76ers", "PHO" = "Phoenix Suns", "POR" = "Portland Trail Blazers",
  #"SAC" = "Sacramento Kings", "SAS" = "San Antonio Spurs", "TOR" = "Toronto Raptors",
  #"UTA" = "Utah Jazz", "WAS" = "Washington Wizards")

team_names <- list("Atlanta Hawks" = "ATL", "Boston Celtics" = "BOS", "Brooklyn Nets" = "BRK",
  "Charlotte Hornets" = "CHO", "Chicago Bulls" = "CHI", "Cleveland Cavaliers" = "CLE",
  "Dallas Mavericks" = "DAL", "Denver Nuggets" = "DEN", "Detroit Pistons" = "DET",
  "Golden State Warriors" = "GSW", "Houston Rockets" = "HOU", "Indiana Pacers" = "IND",
  "Los Angeles Clippers" = "LAC", "Los Angeles Lakers" = "LAL", "Memphis Grizzlies" = "MEM",
  "Miami Heat" = "MIA", "Milwaukee Bucks" = "MIL", "Minnesota Timberwolves" = "MIN",
  "New Orleans Pelicans" = "NOP", "New York Knicks" = "NYK", "Oklahoma City Thunder" = "OKC",
  "Orlando Magic" = "ORL", "Philadelphia 76ers" = "PHI", "Phoenix Suns" = "PHO",
  "Portland Trail Blazers" = "POR", "Sacramento Kings" = "SAC", "San Antonio Spurs" = "SAS",
  "Toronto Raptors" = "TOR", "Utah Jazz" = "UTA", "Washington Wizards" = "WAS")

# Define UI
ui <- fluidPage(
    titlePanel("NBA Player Salary Analysis"),
    tabsetPanel(
      
      # Tab 1 ####
      
      tabPanel("Salary Graphs",
               fluidRow(
                     selectInput("statistic", "Choose a Statistic:",
                                 choices = stat_choices)
                   ),
                   mainPanel(
                     plotOutput("salaryPlot", height="600px"),
                     textOutput("correlation_text")
                   )
      ), # end Tab 1
      
      # Tab 2 ####

      # TAb 3
      tabPanel("Interactive Player List",
        fluidRow(
          column(width = 4,
            pickerInput("team_filter", "Filter by Team:",
                  choices = team_names,
                  selected = unlist(team_names), multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)),
            h5(strong("Filter by Position:")),
            checkboxInput("PG", "Point Guard"),
            checkboxInput("SG", "Shooting Guard"),
            checkboxInput("SF", "Small Forward"),
            checkboxInput("PF", "Power Forward"),
            checkboxInput("C", "Center"),
            pickerInput("column_viewer", "Select up to 5 Columns to Display:",
                  choices = display_names[setdiff(names(display_names), "Player Name")], 
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)),
            actionButton("reset", "Reset")
          ),
          column(width = 8, dataTableOutput("nba_data"))
        )
               
               
               )
))

# Define server logic
server <- function(input, output, session) {

  #Tab 1 (salary graph)
    output$salaryPlot <- renderPlot({
        statistic    <- input$statistic

        ggplot(nba, aes_string(x = statistic, y = "salary")) +
          geom_point()+
          geom_smooth(method = "lm")+
          labs(title = paste("Graphing Salary vs ", names(stat_choices)[stat_choices == statistic]), 
               x = names(stat_choices)[stat_choices == statistic], 
               y = "Player Salary in Dollars")+
          theme(plot.title = element_text(size = 30, face = "bold"),   
                axis.title = element_text(size = 20),                  
                axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20))+
          theme_minimal()+
          scale_y_continuous(labels = scales::comma)
    })
    output$correlation_text <- renderText({
      statistic    <- input$statistic
      corr <- cor(nba[[input$statistic]], nba$salary)
      paste("Correlation between player salary and", 
            names(stat_choices)[stat_choices == statistic], ":", round(corr, 3))
    }) #end tab 1
    #start tab 2, this part is for the left side (checkboxes etc)
    observeEvent(input$column_viewer, {
      req(input$column_viewer)
      if (length(input$column_viewer) > 5) {
        trimmed <- input$column_viewer[1:5]
        updatePickerInput(
          session, "column_viewer", selected = trimmed)
        showNotification(
          paste("Select up to five additional columns."),
          type = "warning"
        )
      }
    })
    #make df reactive
    filtered_nba <- reactive({
      df <- nba
      selected_positions <- c()
      if (input$PG) selected_positions <- c(selected_positions, "PG")
      if (input$SG) selected_positions <- c(selected_positions, "SG")
      if (input$SF) selected_positions <- c(selected_positions, "SF")
      if (input$PF) selected_positions <- c(selected_positions, "PF")
      if (input$C)  selected_positions <- c(selected_positions, "C")
      if (length(selected_positions) > 0) {
        df <- df %>% filter(position %in% selected_positions)
      }
      if (!is.null(input$team_filter) && length(input$team_filter) > 0) {
        df <- df[
          sapply(df$team, function(team_string) {
            teams <- unlist(strsplit(team_string, "/"))
            any(teams %in% input$team_filter)
          }),
        ]
      }
      df
    })
    #data frame output
    output$nba_data <- renderDataTable({
      df <- filtered_nba()
      columns <- c("player_name", input$column_viewer)
      df <- df[, columns, drop = FALSE]
      #everything above this point works, troubleshoot below:
      colnames(df) <- c("Player Name", short_names[input$column_viewer])
      dt <- datatable(df,
          options = list(pageLength = 25), rownames = FALSE)
      if ("salary" %in% input$column_viewer) {
        dt <- formatCurrency(dt, "Salary")
      }
      dt
    })
    #fix the reset button so it actually does something
    observeEvent(input$reset, {
      updatePickerInput(session, "team_filter", selected = unlist(team_names))
      updateCheckboxInput(session, "PG", value = FALSE)
      updateCheckboxInput(session, "SG", value = FALSE)
      updateCheckboxInput(session, "SF", value = FALSE)
      updateCheckboxInput(session, "PF", value = FALSE)
      updateCheckboxInput(session, "C", value = FALSE)
      updatePickerInput(session, "column_viewer", selected = character(0))
    })
} #end server


# Run the application 
shinyApp(ui = ui, server = server)
