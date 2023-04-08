# MTG-Battle-Loggger
R Shiny app for tracking historic games and ranking them.

# Set up
1. Download the R script and install the required packages. Note: this was built under R 4.2.3. 
2. Build a config file with your decklist and deprecated decks following the example.
3. Build an initial history file following the history example.
4. (Optional) Replace line 65 with the folder location of your saved history file. This is only necessary if the R file is not in the same folder as your history file.
6. Save and click Run App.

# Note
This app will not show ELOs if a new deck does not have at least 2 games on different days.
