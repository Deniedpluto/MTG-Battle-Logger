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
history <- fread("Commander History.csv")
decks <- fread("Commander Decks.csv")

i <- 1
win_rate_against <- data.table("ID" = character(), "WRA" = numeric())


for(i in 1:nrow(decks)) {
  test_id <- decks[i, ID]
  
  decks[, `Win Rate`:=Wins/Played]
  match_list <- unique(history[ID == test_id & Match > 0, Match])
  opponents <- history[ID != test_id & Match %in% match_list, c("ID", "Match")]
  opponents <- merge.data.table(opponents, decks, by = "ID")
  
  temp <- data.table("ID" = test_id, "WRA" = opponents[, mean(`Win Rate`)])
  win_rate_against <- rbind(win_rate_against, temp)
}

decks <- merge.data.table(decks, win_rate_against, by = "ID")

decks[Played > 0, `Win Rate`:= Wins/Played]
decks[, STR := `Win Rate`*WRA]
# decks[Played > 0, Norm_STR := (STR - mean(STR))/sd(STR)]
# decks[Played > 0, STD_STR := STR/max(STR)]
decks[Played > 0, Weight := Played/(Played + mean(Played))]
decks[Played > 0, `Bayes STR` := Weight * STR + (1 - Weight) * mean(STR)]

fwrite(decks, "Commander Decks + WRA.csv")
