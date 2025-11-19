library(shiny)




## ---------- Helpers & data storage setup (runs once per R process) ----------

players_file <- "players.csv"
matches_file <- "matches.csv"

# Empty templates
empty_players <- data.frame(
  Player = character(),
  stringsAsFactors = FALSE
)

empty_matches <- data.frame(
  Date    = as.Date(character()),
  Player1 = character(),
  Player2 = character(),
  Score1  = integer(),
  Score2  = integer(),
  stringsAsFactors = FALSE
)

# Ensure files exist
if (!file.exists(players_file)) {
  write.csv(empty_players, players_file, row.names = FALSE)
}
if (!file.exists(matches_file)) {
  write.csv(empty_matches, matches_file, row.names = FALSE)
}

# IMPORTANT: read functions must accept a path argument because
# reactiveFileReader will call readFunc(filePath, ...)
read_players <- function(path) {
  df <- tryCatch(read.csv(path, stringsAsFactors = FALSE),
                 error = function(e) empty_players)
  df$Player <- as.character(df$Player)
  df
}

read_matches <- function(path) {
  df <- tryCatch(read.csv(path, stringsAsFactors = FALSE),
                 error = function(e) empty_matches)
  if (!nrow(df)) return(empty_matches)
  df$Date    <- as.Date(df$Date)
  df$Player1 <- as.character(df$Player1)
  df$Player2 <- as.character(df$Player2)
  df
}

## ------------------------------ UI -----------------------------------------

ui <- navbarPage(
  "Office Ping Pong Rankings",
  
  # ---- Tab 1: Players ----
  tabPanel(
    "Players",
    sidebarLayout(
      sidebarPanel(
        h3("Add Players"),
        textInput("new_player", "New player name"),
        actionButton("add_player", "Add Player"),
        br(), br(),
        helpText("New players will appear at the bottom of the rankings until they play.")
      ),
      mainPanel(
        h3("Current Players"),
        tableOutput("players_table")
      )
    )
  ),
  
  # ---- Tab 2: Enter Match ----
  tabPanel(
    "Enter Match",
    sidebarLayout(
      sidebarPanel(
        h3("Record a Match"),
        uiOutput("player1_ui"),
        numericInput("score1", "Player 1 score", value = 21, min = 0, max = 100),
        
        uiOutput("player2_ui"),
        numericInput("score2", "Player 2 score", value = 15, min = 0, max = 100),
        
        dateInput("match_date", "Match date", value = Sys.Date()),
        br(),
        actionButton("add_match", "Add Match"),
        br(), br(),
        helpText("Make sure both players are added on the Players tab first.")
      ),
      mainPanel(
        h3("Match History"),
        tableOutput("matches_table")
      )
    )
  ),
  
  # ---- Tab 3: Rankings ----
  tabPanel(
    "Rankings",
    fluidPage(
      br(),
      h3("Current Power Rankings"),
      tableOutput("rankings_table"),
      br(),
      helpText("New players start at the bottom until they play at least one game.")
    )
  )
)

## ----------------------------- SERVER --------------------------------------

server <- function(input, output, session) {
  
  # Shared readers for players & matches.
  # reactiveFileReader checks the file every 2 seconds for changes.
  players <- reactiveFileReader(
    intervalMillis = 2000,
    session = session,
    filePath = players_file,
    readFunc = read_players
  )
  
  matches <- reactiveFileReader(
    intervalMillis = 2000,
    session = session,
    filePath = matches_file,
    readFunc = read_matches
  )
  
  ## ---- Players tab logic ----
  
  # Add a new player
  observeEvent(input$add_player, {
    name <- trimws(input$new_player)
    req(nzchar(name))
    
    current <- read_players(players_file)
    
    # Avoid duplicates (case-insensitive)
    if (!(tolower(name) %in% tolower(current$Player))) {
      current <- rbind(
        current,
        data.frame(Player = name, stringsAsFactors = FALSE)
      )
      write.csv(current, players_file, row.names = FALSE)
    }
    
    updateTextInput(session, "new_player", value = "")
  })
  
  output$players_table <- renderTable({
    players()
  })
  
  ## ---- Enter Match tab logic ----
  
  # Dynamic dropdowns that always show the current player list
  output$player1_ui <- renderUI({
    p <- players()
    if (nrow(p) == 0) {
      return(helpText("No players yet. Add players on the 'Players' tab first."))
    }
    selectInput("player1", "Player 1", choices = p$Player)
  })
  
  output$player2_ui <- renderUI({
    p <- players()
    if (nrow(p) == 0) {
      return(NULL)
    }
    default2 <- if (nrow(p) > 1) p$Player[2] else p$Player[1]
    selectInput("player2", "Player 2", choices = p$Player, selected = default2)
  })
  
  # Add a match
  observeEvent(input$add_match, {
    p <- players()
    req(nrow(p) > 0)
    req(input$player1, input$player2)
    
    validate(
      need(input$player1 != input$player2, "Player 1 and Player 2 must be different.")
    )
    
    current <- read_matches(matches_file)
    
    new_row <- data.frame(
      Date    = as.Date(input$match_date),
      Player1 = trimws(input$player1),
      Player2 = trimws(input$player2),
      Score1  = as.integer(input$score1),
      Score2  = as.integer(input$score2),
      stringsAsFactors = FALSE
    )
    
    current <- rbind(current, new_row)
    write.csv(current, matches_file, row.names = FALSE)
  })
  
  output$matches_table <- renderTable({
    m <- matches()
    if (!nrow(m)) return(NULL)
    m[order(m$Date, decreasing = TRUE), ]
  })
  
  ## ---- Rankings tab logic ----
  
  rankings <- reactive({
    p <- players()
    m <- matches()
    
    if (nrow(p) == 0) {
      return(data.frame())
    }
    
    all_players <- p$Player
    
    stats_list <- lapply(all_players, function(pl) {
      if (!nrow(m)) {
        # No matches yet – all zeros
        return(data.frame(
          Player        = pl,
          Games         = 0,
          Wins          = 0,
          Draws         = 0,
          Losses        = 0,
          PointsFor     = 0,
          PointsAgainst = 0,
          WinPct        = 0,
          PointDiff     = 0,
          PowerScore    = 0,
          HasGames      = FALSE,
          stringsAsFactors = FALSE
        ))
      }
      
      games_played <- sum(m$Player1 == pl | m$Player2 == pl)
      
      wins <- sum(
        (m$Player1 == pl & m$Score1 > m$Score2) |
          (m$Player2 == pl & m$Score2 > m$Score1)
      )
      
      draws <- sum(
        (m$Player1 == pl | m$Player2 == pl) &
          (m$Score1 == m$Score2)
      )
      
      losses <- games_played - wins - draws
      
      points_for <- sum(m$Score1[m$Player1 == pl]) +
        sum(m$Score2[m$Player2 == pl])
      
      points_against <- sum(m$Score2[m$Player1 == pl]) +
        sum(m$Score1[m$Player2 == pl])
      
      win_pct    <- if (games_played > 0) wins / games_played else 0
      point_diff <- points_for - points_against
      
      # Power Score: 3 per win, 1 per draw, +0.1 per point differential
      power_score <- wins * 3 + draws * 1 + point_diff * 0.1
      
      data.frame(
        Player        = pl,
        Games         = games_played,
        Wins          = wins,
        Draws         = draws,
        Losses        = losses,
        PointsFor     = points_for,
        PointsAgainst = points_against,
        WinPct        = round(win_pct, 3),
        PointDiff     = point_diff,
        PowerScore    = round(power_score, 2),
        HasGames      = games_played > 0,
        stringsAsFactors = FALSE
      )
    })
    
    stats_df <- do.call(rbind, stats_list)
    
    # Sort so:
    # 1) Players who have played at least one game come first
    # 2) Then by PowerScore (descending)
    stats_df <- stats_df[order(-stats_df$HasGames, -stats_df$PowerScore), ]
    stats_df$HasGames <- NULL  # hide helper column
    
    stats_df
  })
  
  output$rankings_table <- renderTable({
    rankings()
  })
}

shinyApp(ui, server)
