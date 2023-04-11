#####-- Initial Setup --######

# Load Libraries
library(RODBC)
library(shiny)
library(DT)
library(EloOptimized)
library(ggplot2)
library(rstudioapi)
library(jsonlite)
library(htmltools)
library(cli)
library(data.table)

#####-- Setting up environment --#####

setwd(dirname(getActiveDocumentContext()$path))
source("MultiEloR.R")
history <- fread("Commander History.csv")
decks <- fread("Commander Decks.csv")
# deprecated_decks <- sort(unlist(config$deprecated_decks))
# decklist <- sort(unlist(config$decklist))

# Define UI --------------------------------------------------------------------
ui <- fluidPage(
  
  title = "MTG Battle Logger",
  
  wellPanel(
    fluidRow(
      ####-- player 1 data --####
      column(width = 3,
             column(width = 9,
                    selectInput(
                      inputId = "player1",
                      label = "Name",
                      selected = unique(decks$Owner)[1],
                      choices = unique(decks$Owner)
                    ),
                    br(),
                    br(),
                    selectInput(
                      inputId = "Deck1",
                      label = "Deck",
                      selected = decks[Owner == unique(decks$Owner)[1], Deck][1],
                      choices = sort(decks[Owner == unique(decks$Owner)[1], Deck])
                    ),
                    br(),
                    br(),
                    column(width = 6,
                           textOutput(
                             outputId = "Deck1Games",
                           )
                    ),
                    column(width = 6,
                           textOutput(
                             outputId = "Deck1Wins",
                           ) 
                    )
             ),
             column(width = 3,
                    br(),
                    checkboxInput(
                      inputId = "active",
                      label = "Active",
                      value = TRUE,
                    ),
                    br(),
                    br(),
                    br(),
                    selectInput(
                      inputId = "place",
                      label = "Place",
                      selected = 1,
                      choices = c(1:4)
                    ),
                    br(),
                    br(),
                    textOutput(
                      outputId = "Deck1Elo"
                    )
             )
      ),
      ####-- player 2 data --####
      column(width = 3,
             column(width = 9,
                    selectInput(
                      inputId = "player2",
                      label = "Name",
                      selected = unique(decks$Owner)[2],
                      choices = unique(decks$Owner)
                    ),
                    br(),
                    br(),
                    selectInput(
                      inputId = "Deck2",
                      label = "Deck",
                      selected = decks[Owner == unique(decks$Owner)[2], Deck][1],
                      choices = sort(decks[Owner == unique(decks$Owner)[2], Deck])
                    ),
                    br(),
                    br(),
                    column(width = 6,
                           textOutput(
                             outputId = "Deck2Games",
                           )
                    ),
                    column(width = 6,
                           textOutput(
                             outputId = "Deck2Wins",
                           ) 
                    )
             ),
             column(width = 3,
                    br(),
                    checkboxInput(
                      inputId = "active2",
                      label = "Active",
                      value = TRUE,
                    ),
                    br(),
                    br(),
                    br(),
                    selectInput(
                      inputId = "place2",
                      label = "Place",
                      selected = 1,
                      choices = c(1:4)
                    ),
                    br(),
                    br(),
                    textOutput(
                      outputId = "Deck2Elo"
                    )
             )
      ),
      ####-- player 3 data --####
      column(width = 3,
             column(width = 9,
                    selectInput(
                      inputId = "player3",
                      label = "Name",
                      selected = unique(decks$Owner)[3],
                      choices = unique(decks$Owner)
                    ),
                    br(),
                    br(),
                    selectInput(
                      inputId = "Deck3",
                      label = "Deck",
                      selected = decks[Owner == unique(decks$Owner)[3], Deck][1],
                      choices = sort(decks[Owner == unique(decks$Owner)[3], Deck])
                    ),
                    br(),
                    br(),
                    column(width = 6,
                           textOutput(
                             outputId = "Deck3Games",
                           )
                    ),
                    column(width = 6,
                           textOutput(
                             outputId = "Deck3Wins",
                           ) 
                    )
             ),
             column(width = 3,
                    br(),
                    checkboxInput(
                      inputId = "active3",
                      label = "Active",
                      value = TRUE,
                    ),
                    br(),
                    br(),
                    br(),
                    selectInput(
                      inputId = "place3",
                      label = "Place",
                      selected = 1,
                      choices = c(1:4)
                    ),
                    br(),
                    br(),
                    textOutput(
                      outputId = "Deck3Elo"
                    )
             )
      ),
      ####-- player 4 data --####
      column(width = 3,
             column(width = 9,
                    selectInput(
                      inputId = "player4",
                      label = "Name",
                      selected = unique(decks$Owner)[1],
                      choices = unique(decks$Owner)
                    ),
                    br(),
                    br(),
                    selectInput(
                      inputId = "Deck4",
                      label = "Deck",
                      selected = decks[Owner == unique(decks$Owner)[1], Deck][1],
                      choices = sort(decks[Owner == unique(decks$Owner)[1], Deck])
                    ),
                    br(),
                    br(),
                    column(width = 6,
                           textOutput(
                             outputId = "Deck4Games",
                           )
                    ),
                    column(width = 6,
                           textOutput(
                             outputId = "Deck4Wins",
                           ) 
                    )
             ),
             column(width = 3,
                    br(),
                    checkboxInput(
                      inputId = "active4",
                      label = "Active",
                      value = FALSE,
                    ),
                    br(),
                    br(),
                    br(),
                    selectInput(
                      inputId = "place4",
                      label = "Place",
                      selected = 1,
                      choices = c(1:4)
                    ),
                    br(),
                    br(),
                    textOutput(
                      outputId = "Deck4Elo"
                    )
             )
      )
    )
  ),
  ####-- Button for Saving I guess --####
  fluidRow(
    column(width = 2,
           actionButton(
             inputId = "predict",
             label = "Record New Match"
           )
    )
  ),
  ## -- last part
  fluidRow(
    column(width= 4,
           br(),
           br(),
           dataTableOutput(outputId = "EloTable"),
    ),
    column(width = 4,
           br(),
           br(),
           plotOutput(outputId = "DeckChart"),
           br(),
           br(),
           plotOutput(outputId = "PlayerChart")
    ),
    column(width= 4,
           br(),
           br(),
           dataTableOutput(outputId = "MatchHistory"),
    )
  )
)

v <- reactiveValues()
v$matches <- history
v$dt <- decks

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  # player1 <- reactive({input$player1})
  # Deck1 <- reactive({input$Deck1})
  # player2 <- reactive({input$player2})
  # Deck2 <- reactive({input$Deck2})
  # player3 <- reactive({input$player3})
  # Deck3 <- reactive({input$Deck3})
  # player4 <- reactive({input$player4})
  # Deck4 <- reactive({input$Deck4})

  observeEvent(input$player1, {
    updateSelectInput(
      session,
      inputId = "Deck1",
      selected = decks[Owner == input$player1, Deck][1],
      choices = sort(decks[Owner == input$player1, Deck])
    )
    
    output$Deck1Games <- renderText(paste("Matches Played:", decks[Deck == input$Deck1 & Owner == input$player1, Played]))
    output$Deck1Wins <- renderText(paste("Matches Won: ", decks[Deck == input$Deck1 & Owner == input$player1, Wins]))
    output$Deck1Elo <- renderText(paste("Deck Elo:", round(decks[Deck == input$Deck1 & Owner == input$player1, Elo], 1)))
    
  })
  
  observeEvent(input$player2, {
    updateSelectInput(
      session,
      inputId = "Deck2",
      selected = decks[Owner == input$player2, Deck][1],
      choices = sort(decks[Owner == input$player2, Deck])
    )
    
    output$Deck2Games <- renderText(paste("Matches Played:", decks[Deck == input$Deck2 & Owner == input$player2, Played]))
    output$Deck2Wins <- renderText(paste("Matches Won: ", decks[Deck == input$Deck2 & Owner == input$player2, Wins]))
    output$Deck2Elo <- renderText(paste("Deck Elo:", round(decks[Deck == input$Deck2 & Owner == input$player2, Elo], 1)))
    
  })
  
  observeEvent(input$player3, {
    updateSelectInput(
      session,
      inputId = "Deck3",
      selected = decks[Owner == input$player3, Deck][1],
      choices = sort(decks[Owner == input$player3, Deck])
    )
    
    output$Deck3Games <- renderText(paste("Matches Played:", decks[Deck == input$Deck3 & Owner == input$player3, Played]))
    output$Deck3Wins <- renderText(paste("Matches Won: ", decks[Deck == input$Deck3 & Owner == input$player3, Wins]))
    output$Deck3Elo <- renderText(paste("Deck Elo:", round(decks[Deck == input$Deck3 & Owner == input$player3, Elo], 1)))
    
  })
  
  observeEvent(input$player1, {
    updateSelectInput(
      session,
      inputId = "Deck4",
      selected = decks[Owner == input$player4, Deck][1],
      choices = sort(decks[Owner == input$player4, Deck])
    )
    
    output$Deck4Games <- renderText(paste("Matches Played:", decks[Deck == input$Deck4 & Owner == input$player4, Played]))
    output$Deck4Wins <- renderText(paste("Matches Won: ", decks[Deck == input$Deck4 & Owner == input$player4, Wins]))
    output$Deck4Elo <- renderText(paste("Deck Elo:", round(decks[Deck == input$Deck4 & Owner == input$player4, Elo], 1)))
    
  })
  
  observeEvent(input$predict, {

    p1ID <- hash_sha256(paste0(input$player1, input$Deck1))
    p2ID <- hash_sha256(paste0(input$player2, input$Deck2))
    p3ID <- hash_sha256(paste0(input$player3, input$Deck3))
    p4ID <- hash_sha256(paste0(input$player4, input$Deck4))
    
    activeFilter <- c(input$active, input$active2, input$active3, input$active4)
    
    inputElo <- c(v$dt[Deck == input$Deck1 & Owner == input$player1, Elo], v$dt[Deck == input$Deck2 & Owner == input$player2, Elo],
        v$dt[Deck == input$Deck3 & Owner == input$player3, Elo], v$dt[Deck == input$Deck4 & Owner == input$player4, Elo])[activeFilter]
    
    inputPlace <- c(input$place, input$place2, input$place3, input$place4)[activeFilter]
    
    updated_ratings <- get_new_ratings(initial_ratings = inputElo, result_order = inputPlace, k_value = 60, score_base = 1)
    
    for(i in 1:length(updated_ratings)) {
      assign(paste0("player_", i, "_elo"), updated_ratings[i])
    }
    
    listElo <- c()
    
    if(input$active){
      if(input$place == 1) {w = 1 } else { w = 0}
      v$dt[ID == p1ID, `:=`(Elo = player_1_elo, Played = Played + 1, Wins = Wins + w)]
      listElo <- c(listElo, player_1_elo)
    }
      
    if(input$active2) {
      if(input$place2 == 1) {w = 1 } else { w = 0}
      v$dt[ID == p2ID, `:=`(Elo = player_2_elo, Played = Played + 1, Wins = Wins + w)]
      listElo <- c(listElo, player_2_elo)
    }
    
    if(input$active3) {
      if(input$place3 == 1) {w = 1 } else { w = 0}
      v$dt[ID == p3ID, `:=`(Elo = player_3_elo, Played = Played + 1, Wins = Wins + w)]
      listElo <- c(listElo, player_3_elo)
    }
    
    if(input$active4) {
      if(input$place4 == 1) {w = 1 } else { w = 0}
      v$dt[ID == p4ID, `:=`(Elo = player_4_elo, Played = Played + 1, Wins = Wins + w)]
      listElo <- c(listElo, player_4_elo)
    }
    
    # update match history
    match <- max(v$matches$Match) + 1
    
    match_data <- data.table(ID = c(p1ID, p2ID, p3ID, p4ID)[activeFilter],
                             Owner = c(input$player1, input$player2, input$player3, input$player4)[activeFilter],
                             Deck = c(input$Deck1, input$Deck2, input$Deck3, input$Deck4)[activeFilter],
                             Elo = listElo,
                             Match = rep(match, length(inputElo)),
                             Place = inputPlace
    )
    
    v$matches <- rbind(v$matches, match_data)
    
    # write out updates
    fwrite(v$matches, "Commander History.csv")
    fwrite(v$dt, "Commander Decks.csv")

  })
  
  observe({
    decks <- data.table(v$dt)[Played > 0, c("Owner", "Deck", "Elo", "Played", "Wins")]
    
    output$EloTable <- renderDT(
      datatable(decks,
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  font-size:200% ;',"Deck Elo"),
                rownames = FALSE,
                options = list(pageLength = 25, dom = 'tp', order = c(2, "desc"))) %>% formatRound(c("Elo"), 1)
    ) 
  })
  
  observe({
    matches <- data.table(v$matches)[Match > 0, c("Owner", "Deck", "Elo", "Match", "Place")]
    
    output$MatchHistory <- renderDT(
      datatable(matches,
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  font-size:200% ;',"Match History"),
                rownames = FALSE,
                options = list(pageLength = 25, dom = 'tp', order = c(3, "desc"))) %>% formatRound(c("Elo"), 1)
    )
  })

}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)