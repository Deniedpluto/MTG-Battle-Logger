# MTG-Battle-Loggger
R Shiny app for tracking historic games and ranking them.

## Setup
1. Download the R script and install the required packages. Note: this was built under R 4.2.3. 
2. Build a config file with your decklist and deprecated decks following the example.
3. Build an initial history file following the history example.
4. (Optional) Replace line 16 with the folder location of your saved history file. This is only necessary if the R file is not in the same folder as your history file.
6. Save and click Run App.

## Note
This app will not show ELOs if a new deck does not have at least 2 games on different days.

# MTG Commander Logger
R Shiny app for tracking historic commander games and outputing a modified Elo rating.

## Setup
1. Download the R script and install the required packages. Note: this was built under R 4.2.3.
2. Download the MultiElo.R file.
3. Build a Commander Deck file and set the Elo to 1000 and wins to 0. The ID is generated using R and encoding the Owner and Deck column using sha256.
  Manually generate the ID column by running hash_sha256(paste0("Owner Name", "Deck Name")) and pasting the result in excel.
4. Build an initial history file with your list of decks and with a match number of 0.
5. (Optional) Replace line 17 with the folder location of your saved history file. This is only necessary if the R file is not in the same folder as your history file.
6. Save and click Run App.
