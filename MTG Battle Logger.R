#####-- Initial Setup --######

# Load Libraries
library(data.table)
library(RODBC)
library(shiny)
library(DT)
library(EloOptimized)
library(ggplot2)
library(rstudioapi)

#####-- Setting up environment --#####

deprecated_decks <- c("Azorius Control", "Golgari Death", "Golgari Dungeon", "Gruul Overrun")
setwd(dirname(getActiveDocumentContext()$path))
history <- fread("history.csv")
#decklist <- sort(unique(c(history$Winner, history$Loser)))
decklist <- sort(c("Izzet Spellslinger", "Izzet Artifacts", "Simic Volo", "Red Goblins", "Rakdos Treasure", "Orzhov Dungeon", "Dimir Rogues", "Azorius Control", "Azorius Monks", "Selesnya Lifegain", "Boros Equipment", "Black Vampires", "Golgari Graveyard", "Green Elves", "Gruul Bard"))
matchlist <- data.table(decklist)

# Define UI --------------------------------------------------------------------
ui <- fluidPage(
  
  title = "MTG Battle Logger",
  
  fluidRow(
    column(width = 2,
           selectInput(
             inputId = "winner",
             label = "Winner",
             selected = decklist[1],
             choices = decklist
           ) 
    ),
    column(width = 2,
           selectInput(
             inputId = "loser",
             label = "Loser",
             selected = decklist[2],
             choices = decklist
           )
    ),
    column(width = 2,
           selectInput(
             inputId = "first",
             label = "Went First",
             selected = "Winner",
             choices = c("Winner", "Loser")
           )
    ),
    column(width = 3,
           selectizeInput(
             inputId = "decks",
             label = "Decks to exclude",
             selected = deprecated_decks,
             choices = decklist,
             multiple = TRUE
           )
    ),
    column(width = 1,
           br(),
           actionButton(
             inputId = "add",
             label = "Add New Data"
           )
    ),
    column(width = 1),
    column(width = 1,
           br(),
           actionButton(
             inputId = "save",
             label = "Save History"
           )
    )
  ),
  fluidRow(
    column(width = 2,
           # br(),
           # textOutput(outputId = "defendertext")
    ),
    column(width = 2,
           # br(),
           # textOutput(outputId = "challengertext")
    ),
    column(width = 2),
    # column(width = 2,
    #        br(),
    #        textOutput(outputId = "added")
    # ),
    # column(width = 2,
    #        br(),
    #        textOutput(outputId = "saved")
    # )
  ),
  fluidRow(
    column(width= 3,
           br(),
           br(),
           dataTableOutput(outputId = "historypivot"),
           br(),
           br(),
           plotOutput(outputId = "startplayer"),
           br(),
           br(),
           dataTableOutput(outputId = "upnext")
    ),
    column(width = 9,
           br(),
           br(),
           dataTableOutput(outputId = "elo"),
           br(),
           br(),
           dataTableOutput(outputId = "rankings")
    )
  )
)

v <- reactiveValues()
v$dt <- history

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  #browser()
  winner <- reactive({input$winner})
  loser <- reactive({input$loser})
  first <- reactive({input$first})

  # output$defendertext <- renderText("Defender goes first.")
  # output$challengertext <- renderText("Get 'em!")
  
  observeEvent(input$add, {
    tmp <- data.table("Winner" = winner(), "Loser" = loser(), "Start Deck" = first(), "Date" = Sys.time())
    v$dt <- rbind(v$dt, tmp)
    fwrite(v$dt, "history.csv")
  })
  
  output$historypivot <- DT::renderDataTable({
    datatable(v$dt,
              rownames = FALSE,
              options = list(order = c(3, "desc"))
    )
  })
  
  output$startplayer <- renderPlot({
    ggplot(v$dt[`Start Deck`!="", sum(.N)/nrow(v$dt[`Start Deck`!=""]), by = 'Start Deck'], aes(x="", y=V1, fill=`Start Deck`)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0)
  })
  
  observe({
    play_summary <- merge(v$dt[, .(Wins = .N), by = Winner], v$dt[, .(Loses = .N), by = Loser], by.x = "Winner", by.y = "Loser", all = T)
    play_summary[is.na(play_summary), ] <- 0
    play_summary <- play_summary[, `Games Played` := Wins + Loses]
    
    decks <- length(decklist) - 1
    matchlist <- merge(matchlist[, c(.SD, k=1)], matchlist[, c(.SD, k=1)], by = "k", allow.cartesian = TRUE)[, -c("k")]
    matchlist <- matchlist[!duplicated(rowSums(expand.grid((decks)^(0:decks), 12^(0:decks)))), ]
    setnames(matchlist, c("Deck A", "Deck B"))
    full_data <- merge(matchlist, v$dt[, c(.SD, k = 1), .SDcols = c("Winner", "Loser")], by.x = c("Deck A", "Deck B"), by.y = c("Winner", "Loser"), all.x = TRUE)
    full_data <- merge(full_data, unique(v$dt[, c(.SD, j = 1), .SDcols = c("Winner", "Loser")]), by.x = c("Deck A", "Deck B"), by.y = c("Loser", "Winner"), all.x = TRUE)
    games_to_play <- full_data[is.na(k) & is.na(j) &`Deck A` != `Deck B`, c("Deck A", "Deck B")]
    games_to_play <- games_to_play[play_summary[, .(Games = Wins + Loses), by = "Winner"], on = .(`Deck A` = Winner)][play_summary[, .(Games2 = Wins + Loses), by = "Winner"], on = .(`Deck B` = Winner)][!is.na(`Deck A`), .(`Collective Matches` = Games + Games2), by = c("Deck A", "Deck B")]
    setorder(games_to_play, `Collective Matches`)
    
    output$upnext <- renderDataTable({
      datatable(games_to_play,
                rownames = FALSE,
                options = list(pageLength =5))
    })
  })
  
  output$rankings <- DT::renderDataTable({
    
    datatable(dcast(v$dt[!Winner %in% input$decks & !Loser %in% input$decks, c(.SD, Won=1)], Winner~Loser, value.var = "Won", sum),
              rownames = FALSE,
              options = list(pageLength = 15, order = c(0, "asc"))
    )
  })
  
  observe({
    
    valid_decks <- as.list(rbind(v$dt[, c('Winner', 'Date')], v$dt[, c('Loser', 'Date')], use.names = FALSE)[, .(count = length(unique(as.Date(Date)))), by = Winner][count > 1, Winner])
    elo_data <- v$dt[(Winner %in% valid_decks) & (Loser %in% valid_decks) & !(Winner %in% input$decks) & !(Loser %in% input$decks), ]
    
    deck_elo = EloOptimized::eloratingfixed(agon_data = elo_data[, .(Date=as.Date(Date), Winner, Loser)], k = 100, init_elo = 1000)
    current_elo_data <- data.table(deck_elo$elo)[Date==max(Date), ]

    play_summary <- merge(v$dt[, .(Wins = .N), by = Winner], v$dt[, .(Loses = .N), by = Loser], by.x = "Winner", by.y = "Loser", all = T)
    play_summary[is.na(play_summary), ] <- 0
    play_summary <- play_summary[, `Games Played` := Wins + Loses]
    
    elo_table <- merge(current_elo_data, play_summary, by.x = "Individual", by.y = "Winner", all = T)
    setnames(elo_table, c("Deck", "Date", "ELO", "Rank", "Scaled", "Expected Wins", "Cardinal", "ELO Group", "Wins", "Loses", "Games Played"))
    
    elo_table <- datatable(elo_table[!Deck %in% input$decks, c("Deck", "Date","Rank", "ELO", "ELO Group", "Expected Wins", "Wins", "Loses", "Games Played")],
                           rownames = FALSE,
                           options = list(pageLength = 15, order = c(3, "desc"))
    ) %>% formatRound(c("ELO", "Expected Wins"), 1)
    
    output$elo <- DT::renderDataTable({
       elo_table
    })
  })
  
  observeEvent(input$save, {
    fwrite(v$dt, "history.csv")
  })  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
