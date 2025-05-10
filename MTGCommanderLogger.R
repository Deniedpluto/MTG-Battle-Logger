###  ##-- Initial Setup --######

# Load Libraries
library(shiny)
library(DT)
library(rstudioapi)
library(htmltools)
library(data.table)
library(cli)
library(bslib)
library(bsicons)

#####-- Setting up environment --#####

setwd(dirname(getActiveDocumentContext()$path))
source("MultiEloR.R")
history <- fread("Data/CommanderHistory.csv") 
decks <-fread("Data/CommanderDecks.csv")
# deprecated_decks <- sort(unlist(config$deprecated_decks))
# decklist <- sort(unlist(config$decklist))

# Adding in New Decks ---------------------------------------------------------
# 
# # Check decks to see if there are decks without ids or decks with name changes
# decks[, new_id := hash_sha256(paste0(Owner, Deck))]
# id_switch <- decks[ID !=new_id & ID != "" , c("ID", "Deck", "new_id")]
# 
# # Replace deck id in history table
# new_hist <- merge.data.table(history, id_switch, by = "ID", all.x = T)
# new_hist[!is.na(new_id), `:=`(ID = new_id, Deck.x = Deck.y) ]
# setnames(new_hist, "Deck.x", "Deck")
# history <- new_hist[, c("Meta", "ID", "Owner", "Deck", "Elo", "Match", "Place", "PlayerOrder")]
# rm(new_hist, id_switch)
# 
# # Replace deck id in deck table
# decks[, ID:=new_id]
# decks[, new_id:=NULL]
# new_decks <- decks[!(ID %in% history$ID), c("Meta", "ID", "Owner", "Deck", "Elo")]
# history <- rbind(new_decks[, `:=`(Match = 0, Place = 0)], history)
# rm(new_decks)
# 
# fwrite(history, "Data/CommanderHistory.csv")
# fwrite(decks, "Data/CommanderDecks.csv")
# 
# Define UI --------------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel("MTG Battle Logger"),
  hr(),
  # accordion(
  ####-- Meta Selector & New Deck Add --###
  # accordion_panel(
  #   title = "Meta Selector & New Deck",
  #   icon = bsicons::bs_icon("gear"),
  fluidRow(
    column(width = 2,
           selectInput(
             inputId = "Meta",
             label = "Meta",
             selected = "BMT",
             choices = c("BMT", "SevensOnly")
           )
    ),
    column(width = 3
    ),
    column(width = 2,
           textInput(
             inputId = "NDMeta",
             label = "Meta",
             value = "",
             placeholder = "Enter Meta Name"
           )
    ),
    column(width = 2,
           textInput(
             inputId = "NDOwner",
             label = "Owner",
             value = "",
             placeholder = "Enter Owner Name"
           )
    ),
    column(width = 2,
           textInput(
             inputId = "NDDeck",
             label = "Deck Name",
             value = "",
             placeholder = "Enter Deck Name"
           )
    ),
    column(width = 1,
           br(),
           actionButton(
             inputId = "NewDeck",
             label = "Add New Deck"
           )
    )
    # )
  ),
  hr(),
  # accordion_panel(
  #   title = "New Game Entry",
  #   icon = bsicons::bs_icon("menu-app"),
  fluidRow(
    ####-- player 1 data --####
    column(width = 3,
           column(width = 9,
                  selectInput(
                    inputId = "player1",
                    label = "Name",
                    selected = unique(decks[Meta=="BMT", Owner])[1],
                    choices = unique(decks[Meta=="BMT", Owner])
                  ),
                  br(),
                  br(),
                  selectInput(
                    inputId = "Deck1",
                    label = "Deck",
                    selected = decks[Owner == unique(decks$Owner)[1] & Active==1, Deck][1],
                    choices = sort(decks[Owner == unique(decks$Owner)[1] & Active==1, Deck])
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
                  selectInput(
                    inputId = "place",
                    label = "Place",
                    selected = 1,
                    choices = c(1:4)
                  ),
                  selectInput(
                    inputId = "position",
                    label = "P.O.",
                    selected = 1,
                    choices = c(1:4)
                  ),
                  checkboxInput(
                    inputId = "active",
                    label = "Active",
                    value = TRUE,
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
                    selected = unique(decks[Meta=="BMT", Owner])[2],
                    choices = unique(decks[Meta=="BMT", Owner])
                  ),
                  br(),
                  br(),
                  selectInput(
                    inputId = "Deck2",
                    label = "Deck",
                    selected = decks[Owner == unique(decks$Owner)[2] & Active==1, Deck][1],
                    choices = sort(decks[Owner == unique(decks$Owner)[2] & Active==1, Deck])
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
                  selectInput(
                    inputId = "place2",
                    label = "Place",
                    selected = 1,
                    choices = c(1:4)
                  ),
                  selectInput(
                    inputId = "position2",
                    label = "P.O.",
                    selected = 2,
                    choices = c(1:4)
                  ),
                  checkboxInput(
                    inputId = "active2",
                    label = "Active",
                    value = TRUE,
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
                    selected = unique(decks[Meta=="BMT", Owner])[3],
                    choices = unique(decks[Meta=="BMT", Owner])
                  ),
                  br(),
                  br(),
                  selectInput(
                    inputId = "Deck3",
                    label = "Deck",
                    selected = decks[Owner == unique(decks$Owner)[3] & Active==1, Deck][1],
                    choices = sort(decks[Owner == unique(decks$Owner)[3] & Active==1, Deck])
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
                  selectInput(
                    inputId = "place3",
                    label = "Place",
                    selected = 1,
                    choices = c(1:4)
                  ),
                  selectInput(
                    inputId = "position3",
                    label = "P.O.",
                    selected = 3,
                    choices = c(1:4)
                  ),
                  checkboxInput(
                    inputId = "active3",
                    label = "Active",
                    value = TRUE,
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
                    selected = unique(decks[Meta=="BMT", Owner])[4],
                    choices = unique(decks[Meta=="BMT", Owner])
                  ),
                  br(),
                  br(),
                  selectInput(
                    inputId = "Deck4",
                    label = "Deck",
                    selected = decks[Owner == unique(decks$Owner)[4] & Active==1, Deck][1],
                    choices = sort(decks[Owner == unique(decks$Owner)[4] & Active==1, Deck])
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
                  selectInput(
                    inputId = "place4",
                    label = "Place",
                    selected = 1,
                    choices = c(1:4)
                  ),
                  selectInput(
                    inputId = "position4",
                    label = "P.O.",
                    selected = 4,
                    choices = c(1:4)
                  ),
                  checkboxInput(
                    inputId = "active4",
                    label = "Active",
                    value = TRUE,
                  ),
                  br(),
                  br(),
                  textOutput(
                    outputId = "Deck4Elo"
                  )
           )
    )
    # )
  ),
  #open = c("Meta Selector & New Deck", "New Game Entry")
  # ),
  ####-- Button for Saving I guess --####
  fluidRow(
    column(width = 2,
           actionButton(
             inputId = "predict",
             label = "Record New Match"
           )
    )
  ),
  hr(),
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
           dataTableOutput(outputId = "PlayerTable"),
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
  observeEvent(input$Meta, {
    v$md <- v$dt[Meta==input$Meta,] 
    
    updateSelectInput(
      session,
      inputId="player1",
      selected = unique(v$md[, Owner])[1],
      choices = unique(v$md[, Owner])
    )
    
    updateSelectInput(
      session,
      inputId="player2",
      selected = unique(v$md[, Owner])[2],
      choices = unique(v$md[, Owner])
    )
    
    updateSelectInput(
      session,
      inputId="player3",
      selected = unique(v$md[, Owner])[3],
      choices = unique(v$md[, Owner])
    )
    
    updateSelectInput(
      session,
      inputId="player4",
      selected = unique(v$md[, Owner])[4],
      choices = unique(v$md[, Owner])
    )
  })
  
  observeEvent(input$NewDeck, {
    new_deck <- data.table("Meta"=input$NDMeta, 
                           "ID" = hash_sha256(paste0(input$NDOwner, input$NDDeck)),
                           "Owner"=input$NDOwner,
                           "Deck"=input$NDDeck,
                           "Elo"=1000,
                           "Played"=0,
                           "Wins"=0,
                           "Active"=1
    )
    new_hist <- new_deck[, c("Meta", "ID", "Owner", "Deck", "Elo")]
    new_hist[, `:=`(Match=0, Place=0, PlayerOrder="NA")]
    
    v$dt <- rbind(v$dt, new_deck)
    v$matches <- rbind(v$matches, new_hist)
    
    fwrite(v$matches, "Data/CommanderHistory.csv")
    fwrite(v$dt, "Data/CommanderDecks.csv")
    
    history <- fread("Data/CommanderHistory.csv") 
    decks <- fread("Data/CommanderDecks.csv")
    v$md <- v$dt[Meta==input$Meta,]
    
  })
  
  observeEvent(input$player1, {
    updateSelectInput(
      session,
      inputId = "Deck1",
      selected = v$md[Owner == input$player1 & Active==1 & Meta==input$Meta, Deck][1],
      choices = sort(v$md[Owner == input$player1 & Active==1 & Meta==input$Meta, Deck])
    )
    
    output$Deck1Games <- renderText(paste("Matches Played:", v$md[Deck == input$Deck1 & Owner == input$player1, Played]))
    output$Deck1Wins <- renderText(paste("Matches Won: ", v$md[Deck == input$Deck1 & Owner == input$player1, Wins]))
    output$Deck1Elo <- renderText(paste("Deck Elo:", round(v$md[Deck == input$Deck1 & Owner == input$player1, Elo], 1)))
    
  })
  
  observeEvent(input$player2, {
    updateSelectInput(
      session,
      inputId = "Deck2",
      selected = v$md[Owner == input$player2 & Active==1 & Meta==input$Meta, Deck][1],
      choices = sort(v$md[Owner == input$player2 & Active==1 & Meta==input$Meta, Deck])
    )
    
    output$Deck2Games <- renderText(paste("Matches Played:", v$md[Deck == input$Deck2 & Owner == input$player2, Played]))
    output$Deck2Wins <- renderText(paste("Matches Won: ", v$md[Deck == input$Deck2 & Owner == input$player2, Wins]))
    output$Deck2Elo <- renderText(paste("Deck Elo:", round(v$md[Deck == input$Deck2 & Owner == input$player2, Elo], 1)))
    
  })
  
  observeEvent(input$player3, {
    updateSelectInput(
      session,
      inputId = "Deck3",
      selected = v$md[Owner == input$player3 & Active==1 & Meta==input$Meta, Deck][1],
      choices = sort(v$md[Owner == input$player3 & Active==1 & Meta==input$Meta, Deck])
    )
    
    output$Deck3Games <- renderText(paste("Matches Played:", v$md[Deck == input$Deck3 & Owner == input$player3, Played]))
    output$Deck3Wins <- renderText(paste("Matches Won: ", v$md[Deck == input$Deck3 & Owner == input$player3, Wins]))
    output$Deck3Elo <- renderText(paste("Deck Elo:", round(v$md[Deck == input$Deck3 & Owner == input$player3, Elo], 1)))
    
  })
  
  observeEvent(input$player4, {
    updateSelectInput(
      session,
      inputId = "Deck4",
      selected = v$md[Owner == input$player4 & Active==1 & Meta==input$Meta, Deck][4],
      choices = sort(v$md[Owner == input$player4 & Active==1 & Meta==input$Meta, Deck])
    )
    
    output$Deck4Games <- renderText(paste("Matches Played:", v$md[Deck == input$Deck4 & Owner == input$player4, Played]))
    output$Deck4Wins <- renderText(paste("Matches Won: ", v$md[Deck == input$Deck4 & Owner == input$player4, Wins]))
    output$Deck4Elo <- renderText(paste("Deck Elo:", round(v$md[Deck == input$Deck4 & Owner == input$player4, Elo], 1)))
    
  })
  
  observeEvent(input$predict, {
    # browser()
    
    activeFilter <- c(input$active, input$active2, input$active3, input$active4)
    
    if(length(unique(c(input$position, input$position2, input$position3, input$position4)[activeFilter])) != length(c(input$position, input$position2, input$position3, input$position4)[activeFilter])) {
      stop("Player positions are not unique!")
    }
    
    p1ID <- hash_sha256(paste0(input$player1, input$Deck1))
    p2ID <- hash_sha256(paste0(input$player2, input$Deck2))
    p3ID <- hash_sha256(paste0(input$player3, input$Deck3))
    p4ID <- hash_sha256(paste0(input$player4, input$Deck4))
    
    inputElo <- c(v$md[Deck == input$Deck1 & Owner == input$player1, Elo], 
                  v$md[Deck == input$Deck2 & Owner == input$player2, Elo],
                  v$md[Deck == input$Deck3 & Owner == input$player3, Elo], 
                  v$md[Deck == input$Deck4 & Owner == input$player4, Elo])[activeFilter]
    
    inputPlace <- c(input$place, input$place2, input$place3, input$place4)[activeFilter]
    updated_ratings <- get_new_ratings(initial_ratings = inputElo, result_order = inputPlace, k_value = length(inputElo)*20, score_base = 1)
    
    for(i in 1:length(updated_ratings)) {
      assign(paste0("player_", i, "_elo"), updated_ratings[i])
    }
    
    listElo <- c()
    
    if(input$active){
      if(input$place == 1) {w = 1 } else { w = 0}
      v$dt[ID == p1ID & Meta==input$Meta, `:=`(Elo = player_1_elo, Played = Played + 1, Wins = Wins + w)]
      listElo <- c(listElo, player_1_elo)
    }
    
    if(input$active2) {
      if(input$place2 == 1) {w = 1 } else { w = 0}
      v$dt[ID == p2ID & Meta==input$Meta, `:=`(Elo = player_2_elo, Played = Played + 1, Wins = Wins + w)]
      listElo <- c(listElo, player_2_elo)
    }
    
    if(input$active3) {
      if(input$place3 == 1) {w = 1 } else { w = 0}
      v$dt[ID == p3ID & Meta==input$Meta, `:=`(Elo = player_3_elo, Played = Played + 1, Wins = Wins + w)]
      listElo <- c(listElo, player_3_elo)
    }
    
    if(input$active4) {
      if(input$place4 == 1) {w = 1 } else { w = 0}
      v$dt[ID == p4ID & Meta==input$Meta, `:=`(Elo = player_4_elo, Played = Played + 1, Wins = Wins + w)]
      listElo <- c(listElo, player_4_elo)
    }
    
    # update match history
    match <- max(v$matches$Match) + 1
    
    match_data <- data.table(Meta = input$Meta,
                             ID = c(p1ID, p2ID, p3ID, p4ID)[activeFilter],
                             Owner = c(input$player1, input$player2, input$player3, input$player4)[activeFilter],
                             Deck = c(input$Deck1, input$Deck2, input$Deck3, input$Deck4)[activeFilter],
                             Elo = listElo,
                             Match = rep(match, length(inputElo)),
                             Place = inputPlace,
                             PlayerOrder = c(input$position, input$position2, input$position3, input$position4)[activeFilter]
    )
    
    v$matches <- rbind(v$matches, match_data)
    v$md <- v$dt[Meta==input$Meta,]
    
    # write out updates
    fwrite(v$matches, "Data/CommanderHistory.csv")
    fwrite(v$dt, "Data/CommanderDecks.csv")
    # source("WinRateAgainst.R")
    
  })
  
  observe({
    decks <- data.table(v$dt)[Played > 0 & Meta==input$Meta, c("Owner", "Deck", "Elo", "Played", "Wins")]
    
    output$EloTable <- renderDT(
      datatable(decks,
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  font-size:200% ;',"Deck Elo"),
                rownames = FALSE,
                options = list(pageLength = 25, dom = 'tp', order = c(2, "desc"))) %>% formatRound(c("Elo"), 1)
    ) 
    
    players <- decks[, .(`Average Deck Elo` = mean(Elo), Plays = sum(Played), Wins = sum(Wins), `Win Rate` = sum(Wins)/sum(Played)), by = Owner]
    
    output$PlayerTable <- renderDT(
      datatable(players,
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  font-size:200% ;',"Player Stats"),
                rownames = FALSE,
                options = list(pageLength = 25, dom = 'tp', order = c(3, "desc"))) %>% formatRound(c("Average Deck Elo"), 1) %>% formatPercentage(c("Win Rate"))
    )
    
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