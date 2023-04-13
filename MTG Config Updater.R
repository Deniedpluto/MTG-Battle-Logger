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
library(data.table)

#####-- Setting up environment --#####

setwd(dirname(getActiveDocumentContext()$path))
history <- fread("history.csv")
config <- read_json("Config.json")
deprecated_decks <- sort(unlist(config$deprecated_decks))
decklist <- sort(unlist(config$decklist))

# Define UI --------------------------------------------------------------------
ui <- fluidPage(
  
  title = "Setup Config",
  
  fluidRow(
    column(width = 3,
           textInput(
             inputId = "add",
             label = "Deck to Add",
             value = "New Deck - ex: Green Chungus",
           ) 
    ),
    column(width = 4,
           selectizeInput(
             inputId = "remove",
             label = "Deck to Remove",
             selected = deprecated_decks,
             choices = decklist,
             multiple = TRUE
           )
    ),
    column(width = 1,
           br(),
           actionButton(
             inputId = "update",
             label = "Update Config"
           )
    )
  ),
  fluidRow(
    column(width= 6,
           br(),
           br(),
           dataTableOutput(outputId = "Decklist"),
    ),
    column(width= 6,
      br(),
      br(),
      dataTableOutput(outputId = "DeprecatedDecks")
    )
  )
)

v <- reactiveValues()
v$dl <- decklist
v$dd <- deprecated_decks


# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  observe({
    output$Decklist <- DT::renderDataTable({
      datatable(data.table(v$dl),
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  font-size:200% ;',"Decklist"),
                rownames = TRUE,
                options = list(order = c(1, "asc"), dom = 'tp', pageLength = 25)
      )
    })
    
    output$DeprecatedDecks <- DT::renderDataTable({
      datatable(data.table(v$dd),
                caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black;  font-size:200% ;',"Deprecated Decks"),
                rownames = TRUE,
                options = list(order = c(1, "asc"), dom = 'tp', pageLength = 25)
      )
    })
  })
  
  observeEvent(input$update, {
    
    browser()
    if(input$add != "") {
    
    v$dl <- sort(unique(c(v$dl, input$add)))
    v$dd <- sort(unique(c(v$dd, input$remove)))
    
    output_json = vector(mode = "list")
    output_json[["decklist"]] <- v$dl
    output_json[["deprecated_decks"]] <- v$dd

    write(toJSON(output_json), "Config.json")
      
    updateSelectizeInput(
      session,
      inputId = "remove",
      label = "Deck to Remove",
      selected = v$dd,
      choices = v$dl,
      )
    } else {
      updateTextInput(
        session,
        inputId = "add",
        label = "Deck to Add",
        value = "Deck Name Cannot be Blank",
      )
    }
    
  })  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)