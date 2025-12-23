#####-- Initial Setup --######

# Load Libraries
from multielo import MultiElo
import numpy as np
import duckdb
import yaml
import polars as pl
import pyarrow as pa

#####-- Setting up environment --#####

# Create two separate MultiElo instances for different K values based on player count
elo3 = MultiElo(k_value=60)
elo4 = MultiElo(k_value=80)

# Read in the token from the yaml file
mdt = yaml.safe_load(open('MDToken.yaml', 'r'))['token']
# Authenticate motherduck using token
con = duckdb.connect('md:?motherduck_token=' + mdt) 
# Attach database
con.sql("USE my_db")
# Pull in the full Commander History from MotherDuck
FullHistory = pl.DataFrame(con.sql("SELECT * FROM MTG.CommanderHistory"))
# Create EloHistory table with already existing Elo values
EloHistory = FullHistory.filter(pl.col("Elo").is_not_null())

#####-- Running MultiElo --#####

# Filter down to entries with missing Elo and order by Match
MissingElo = FullHistory.filter(pl.col("Elo").is_null()).sort("Match")
# Create a list of matches played
matchlist = MissingElo.select("Match").unique().to_series().to_list()
# Pull the most recent Elo for each deck prior to the missing Elo matches
CurrentElo = FullHistory.filter(pl.col("Elo").is_not_null()).sort("Match").group_by("Basic_ID").last().select(["Basic_ID", "Match", "Elo"])

# Setup loop to process each match with missing Elo
NewEloEntries = []
for i in matchlist:
    # pull the meta & list of decks that played in the match
    CurrentMeta = MissingElo.filter(pl.col("Match") == i).select("Meta").unique()[0,0]
    Decklist = MissingElo.filter(pl.col("Match") == i).select("Basic_ID").to_series().to_list()
    # Pull out the current Elo and Place for decks in the match and combine to ensure correct order
    InputElo = CurrentElo.filter(pl.col("Basic_ID").is_in(Decklist)).select(["Basic_ID", "Elo"])
    InputPlace = MissingElo.filter(pl.col("Match") == i).select(["Basic_ID", "Place"])
    Input = InputElo.join(InputPlace, on="Basic_ID").sort("Place")
    # Convert to arrays for input.
    InputElo = Input.select("Elo").to_series().to_list()
    InputPlace = Input.select("Place").to_series().to_list()

    if len(Decklist) == 3:
        # Run the get_new_ratings function to update the Elo ratings
        Input = Input.with_columns(pl.Series("NewElo", elo3.get_new_ratings(initial_ratings = InputElo, result_order= InputPlace)))
    elif len(Decklist) == 4:
       # Run the get_new_ratings function to update the Elo ratings
        Input = Input.with_columns(pl.Series("NewElo", elo4.get_new_ratings(initial_ratings = InputElo, result_order= InputPlace)))
    
    # Update CurrentElo with new ratings
    CurrentElo = CurrentElo.update(
        Input.select(["Basic_ID", "NewElo"]).rename({"NewElo": "Elo"}),
        on="Basic_ID"
    )
    # Write out the new ratings
    NewEloEntries.append(MissingElo.filter(pl.col("Match") == i).update(
            Input.select(["Basic_ID", "NewElo"]).rename({"NewElo": "Elo"}),
            on="Basic_ID"
        )
    )
# Combine all new Elo entries into a single dataframe and merge with existing EloHistory
NewEloEntries = pl.concat(NewEloEntries, how="vertical")
EloHistory = pl.concat([EloHistory, NewEloEntries], how="vertical").sort("Match")

#####-- Writing Updated Elo to MotherDuck --#####

# Write updated EloHistory to Motherduck
#con.sql("CREATE OR REPLACE TABLE MTG.CommanderHistoryBase AS SELECT * FROM EloHistory");
con.sql("INSERT INTO MTG.CommanderHistoryBase (SELECT * FROM NewEloEntries)");

#####-- Win Rate Against --#####

Decks = pl.DataFrame(con.sql("SELECT * FROM MTG.CommanderDecks"))
# Add in Win Rate
Decks = Decks.with_columns((pl.col("Wins") / pl.col("Played")).alias("WinRate"))

# Initialize the variables and the table
WRA = []

#deck = Decks["ID"][0] # For testing purposes
# Loop through each deck to calculate its Win Rate Against (WRA)
for deck in Decks["ID"]:
    # Pull in all the matches the deck has played and the opponents it has played against
    MatchList = EloHistory.filter((pl.col("Basic_ID") == deck) & (pl.col("Match") > 0)).select("Match").unique().to_series().to_list()
    OpponentsList = EloHistory.filter((pl.col("Basic_ID") != deck) & (pl.col("Match").is_in(MatchList))).select("Basic_ID").unique().to_series().to_list()
    OpponentsDecks = Decks.filter(pl.col("ID").is_in(OpponentsList))

    # Calculate the win rate against each opponent deck
    weighted_sum = (OpponentsDecks.select("WinRate") * OpponentsDecks.select("Played")).sum()[0,0]
    total_played = OpponentsDecks.select("Played").sum()[0,0]
    wra_value = weighted_sum / total_played if total_played > 0 else 0

    # Add row to list
    WRA.append(pl.DataFrame({"Basic_ID": [deck], "WRA": [wra_value]}))

# Combine all WRA entries into a single dataframe
WRA = pl.concat(WRA, how="vertical")
# Merge the win rate against table with the decks table
WRA = Decks.join(WRA, left_on="ID", right_on="Basic_ID").rename({"ID":"Basic_ID"})
# Calculate the strength of the deck
WRA = WRA.with_columns(
    ((pl.col("WinRate") + pl.col("WRA").sqrt())/2).alias("STR"),
    ((pl.col("WinRate").cast(pl.Float32) + pl.col("WRA").cast(pl.Float32))/2).alias("STR2")
)
# Calculate the Bayesian Weight of the deck
WRA = WRA.filter(pl.col("Played") > 0).with_columns(
    (pl.col("Played") / (pl.col("Played") + pl.col("Played").mean())).alias("Weight")
)
# Calculate the Bayesian Strength of the deck
WRA = WRA.filter(pl.col("Played") > 0).with_columns(
    (pl.col("Weight") * pl.col("STR") + (1 - pl.col("Weight")) * pl.col("STR").mean()).alias("Bayes STR"),
    (pl.col("Weight") * pl.col("STR2") + (1 - pl.col("Weight")) * pl.col("STR2").mean()).alias("Bayes STR2")
)

# Calculate the Standardized Bayesian Strength of the deck
WRA = WRA.filter(pl.col("Played") > 0).with_columns(
    ((pl.col("Bayes STR") - pl.col("Bayes STR").mean()) / pl.col("Bayes STR").std()).alias("Norm Bayes STR"),
    ((pl.col("Bayes STR2") - pl.col("Bayes STR2").mean()) / pl.col("Bayes STR2").std()).alias("Norm Bayes STR2")
)

# Adding in mean STR and STR2 for comparison purposes and calculating relationship between Elo and Bayes STR.
WRA = WRA.with_columns(
    pl.col("STR").mean().alias("meanSTR"),
    pl.col("STR2").mean().alias("meanSTR2"),
    ((pl.col("Elo")-1000)/pl.col("Bayes STR")).mean().alias("Slope"),
    ((pl.col("Elo")-1000)/pl.col("Bayes STR2")).mean().alias("Slope2"),
)

# Calculate Expected Elo based on Normalized Bayesian Strength
WRA = WRA.with_columns(
    (pl.col("Norm Bayes STR")*pl.col("Slope") + 1000).alias("ExpectedElo"),
    (pl.col("Norm Bayes STR2")*pl.col("Slope2") + 1000).alias("ExpectedElo2")
)

# Calculate difference between Actual Elo and Expected Elo
WRA = WRA.with_columns(
    (pl.col("Elo") - pl.col("ExpectedElo")).alias("EloDiff"),
    (pl.col("Elo") - pl.col("ExpectedElo2")).alias("EloDiff2")
)


#####-- Writing Updated Decks WRA to MotherDuck --#####

con.sql("CREATE OR REPLACE TABLE MTG.CommanderDecksWRANew AS SELECT * FROM WRA");