#####-- Initial Setup --######

# Load Libraries
library(rstudioapi)
library(data.table)

#####-- Setting up environment --#####

setwd(dirname(getActiveDocumentContext()$path))
source("MultiEloR.R")
history <- fread("Data/CommanderHistory.csv") 
decks <- fread("Data/CommanderDecks.csv")

# Order the matches from 0 to max
setorder(history, Match)

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
  updated_ratings <- get_new_ratings(initial_ratings = inputElo, result_order = inputPlace, k_value = length(inputElo)*20, score_base = 1)
  # pull the recorded match and add the new ratings 
  history_new <- history[Match == i][, NewElo := updated_ratings]
  # combine new match with the prior data
  history_base <- rbind(history_base, history_new)
  # increment i to iterate through
  i <- i + 1
}

# Write out data for testing
fwrite(history_base, "Data/CommanderHistoryTest.csv")

# Write out the new base data
fwrite(history_base[, c("Meta", "ID", "Owner", "Deck", "Elo", "Match", "Place", "PlayerOrder")], "Data/CommanderHistory.csv")

#####-- Recreate Commander Decks.csv --#####

# Convert Place into a win flag
history_base[, Win:=0][Place==1, Win:=1]

# Count Plays, Wins, and Last Match
CommanderDecksNew <- history_base[, .(Played = .N-1, Wins=sum(Win), Match = max(Match)), by=c("Meta", "ID", "Owner", "Deck")]

# Pull in Elo from last match
CommanderDecksNew <- merge.data.table(CommanderDecksNew, history_base[, c("Meta", "ID", "Match", "Elo")], by = c("Meta", "ID", "Match"))

# Pull in Active Flag from Commander Decks table
CommanderDecksNew <- merge.data.table(CommanderDecksNew, decks[, c("Meta", "ID", "Active")], by = c("Meta", "ID") )

# Reorder columns to match Commander Decks table
setcolorder(CommanderDecksNew[, c("Meta", "ID", "Owner", "Deck", "Elo", "Played", "Wins", "Active")])

# Remove the last match column 
CommanderDecksNew[, Match:=NULL]

# Write out the data back to the Commander Decks table
fwrite(CommanderDecksNew, "Data/CommanderDecks.csv")