#####-- Initial Setup --######

library(rstudioapi)
library(data.table)
library(reticulate)

#####-- Setting up environment --#####

setwd(dirname(getActiveDocumentContext()$path))
history <- fread("Data/CommanderHistory.csv")
decks <- fread("Data/CommanderDecks.csv")

# Calculate win rate for each deck
decks[, `Win Rate`:=Wins/Played]

# Initialize the variables and the table
i <- 1
win_rate_against <- data.table("Meta" = character(), "ID" = character(), "WRA" = numeric())

# Loop through the decks and calculate the win rate against each deck
for(i in 1:nrow(decks)) {
  # Pull the id and Meta for the current row
  current_id <- decks[i, ID]
  current_meta <- decks[i, Meta]
  # Pull the match number for all matches the deck was played.
  match_list <- unique(history[ID == current_id & Meta == current_meta & Match > 0, Match])
  # Pull in the unique list of opponents
  opponents_list <- unique(history[ID != current_id & Meta == current_meta & Match %in% match_list, ID])
  # Create table of all the decks you've played against
  opponents <- decks[ID %in% opponents_list & Meta == current_meta, ]
  # This former logic is now deprecated.
    # # Pull the match history for the decks this deck has played against
    # opponents <- history[ID != current_id & Match %in% match_list, c("ID", "Match")]
    # # Add in the deck stats
    # opponents <- merge.data.table(opponents, decks, by = "ID")
  # Calculate the win rate against the deck
  temp <- data.table("Meta" = current_meta, "ID" = current_id, "WRA" = opponents[, sum(`Win Rate` * Played)/sum(Played)])
  # Add data to win_rate_against table
  win_rate_against <- rbind(win_rate_against, temp)
}

# Merge the win rate against table with the decks table
decks <- merge.data.table(decks, win_rate_against, by = c("Meta", "ID"))
# Calculate the strength of the deck
decks[, STR := (`Win Rate`+WRA)/2] # Formerly 'Win Rate'*WRA
decks[, STR2 := `Win Rate`*WRA]
# decks[Played > 0, STD_STR := (STR - mean(STR))/sd(STR)]
# decks[Played > 0, Norm_STR := STR/max(STR)]
# Calculate the Bayesian Weight of the deck
decks[Played > 0, Weight := Played/(Played + mean(Played))]
# Calculate the Bayesian Strength of the deck
decks[Played > 0, `Bayes STR` := Weight * STR + (1 - Weight) * mean(STR)]
decks[Played > 0, `Bayes STR2` := Weight * STR2 + (1 - Weight) * mean(STR2)]
# Calculate the Standardized Bayesian Strength of the deck
decks[Played > 0, `Norm Bayes STR` := (`Bayes STR` - mean(`Bayes STR`))/sd(`Bayes STR`)]
decks[Played > 0, `Norm Bayes STR2` := (`Bayes STR2` - mean(`Bayes STR2`))/sd(`Bayes STR2`)]
# Adding in mean STR and STR2 for comparison purposes
decks[Played > 0, meanSTR := mean(STR)]
decks[Played > 0, meanSTR2 := mean(STR2)]


# Save out the files 
fwrite(decks, "Data/CommanderDecksWRA.csv")

# # Check to see if am in on my laptop repos
# if (getwd() == "C:/Users/Matso/source/repos/Deniedpluto/MTG-Battle-Loggger") { # nolint
#   # Set the wd to the Commander_Decks folder for evidence and save the file
#   setwd("C:/Users/Matso/source/repos/Deniedpluto/Evidence/sources/Commander_Decks")
#   fwrite(decks, "CommanderDecksWRA.csv")
#   
#   # Do the same thing for the Commander_History 
#   setwd("C:/Users/Matso/source/repos/Deniedpluto/Evidence/sources/Commander_History")
#   fwrite(history, "CommanderHistory.csv")
# }

# Set Working Directory back to main folder
setwd("C:/Users/Matso/source/repos/Deniedpluto/MTG-Battle-Loggger")

# Write the data to MotherDuck
source_python("WriteToMotherDuck.py")
