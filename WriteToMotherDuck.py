import duckdb
import yaml
import polars as pl
import pyarrow as pa

# Read in the token from the yaml file
mdt = yaml.safe_load(open('MDToken.yaml', 'r'))['token']

# Authenticate motherduck using token
con = duckdb.connect('md:?motherduck_token=' + mdt) 

# Attach database
con.sql("USE my_db")

# Read in Commander History
CommanderHistory = pl.read_csv('Data/CommanderHistory.csv', schema_overrides={"Meta":pl.String, "ID":pl.String, "Owner":pl.String, "Deck":pl.String, "Elo":pl.Float32, "Match":pl.Int32, "Place":pl.Int8,	"PlayerOrder":pl.Int8})
# Find the last match in the database
LastMatch = pl.DataFrame(con.sql("SELECT MAX(Match) AS LastMatch FROM MTG.CommanderHistory"))
# Filter down to only new matches
NewMatches = pa.table(CommanderHistory.filter(pl.col("Match") > LastMatch["LastMatch"][0]))
# Get list of deck ids
DeckIDs = pl.DataFrame(con.sql("SELECT DISTINCT ID FROM MTG.CommanderDecksWRA"))
# Filter down to only new decks
NewDecks = pa.table(CommanderHistory.filter(~pl.col("ID").is_in(DeckIDs["ID"])).filter(pl.col("Match") == 0))

# Write new decks matches to motherduck
con.sql("INSERT INTO MTG.CommanderHistory SELECT * FROM NewDecks");
con.sql("INSERT INTO MTG.CommanderHistory SELECT * FROM NewMatches");
# Replace WRA table with new data
con.sql("CREATE OR REPLACE TABLE MTG.CommanderDecksWRA AS SELECT * FROM 'Data/CommanderDecksWRA.csv'");
'''
con.sql("CREATE OR REPLACE TABLE MTG.CommanderHistory AS SELECT * FROM 'Data/CommanderHistory.csv'");
'''