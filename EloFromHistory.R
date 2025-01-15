###  ##-- Initial Setup --######

# Load Libraries
library(rstudioapi)
library(data.table)

#####-- Setting up environment --#####

setwd(dirname(getActiveDocumentContext()$path))
source("MultiEloR.R")
history <- fread("Commander History.csv") 
decks <- fread("Commander Decks.csv")

# Crete a list of matches played
matchlist <- unique(history[Match > 0, Match])

# Setup History Base
history_base <- history[Match == 0][, NewElo:=1000]

# initialize i as 1 to iterate through the matches
i <- 1
for( i in matchlist) {
  
  # Pull in the meta 
  current_Meta <- unique(history[Match == i, Meta])
  # pull the list of decks that played in the match
  decklist <- c(history[Match == i, ID])
  # initialize d as 1 to iterate throught the decks played in the match
  d <- 1    
  # seed the starting elo
  inputElo <- rep(1000, length(decklist))
  
  for (d in 1:length(decklist)) {
    # pull the current deck id
    current_deck <- decklist[d]
    # pull last match for the selected deck
    last_match <- history[ID == current_deck & Match < i & Meta == current_Meta, max(Match)]
    # pull the last Elo for the deck
    inputElo[d] <- history[ID == current_deck & Match == last_match & Meta == current_Meta, Elo]
    # increment d to iterate through
    d <- d + 1
  }
  
  # pull in place for the match
  inputPlace <- c(history[Match == i, Place])
  # Run the get_new_ratings function to update the Elo ratings
  updated_ratings <- get_new_ratings(initial_ratings = inputElo, result_order = inputPlace, k_value = 48, score_base = 1)
  # pull the recorded match and add the new ratings 
  history_new <- history[Match == i][, NewElo := updated_ratings]
  # combine new match with the prior data
  history_base <- rbind(history_base, history_new)
  # increment i to iterate through
  i <- i + 1
}

fwrite(history_base, "CommanderHistoryNewTest.csv")
