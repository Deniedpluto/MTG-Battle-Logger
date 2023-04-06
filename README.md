# MTG-Battle-Loggger
R Shiny app for tracking historic games and ranking them.

# Set up
1. Download the R script and install the required packages. Note: this was built under R 4.2.3. 
2. Replace line 14 with your list of deprecated decks. These can be added back in inside the app. This is to save your list so you don't have to update it.
3. (Optional) Replace line 15 with the folder location of your saved history file. This is only necessary if the R file is not in the same folder as your history file.
3. Replace line 18 with your list of decks. It is made alphabetical in the app.
5. Save and click Run App.

# Note
This app will not show ELOs if a new deck does not have at least 2 games on different days.
