import duckdb
import yaml
import polars as pl
import pyarrow as pa
import fastexcel

# Read in the token from the yaml file
mdt = yaml.safe_load(open('MDToken.yaml', 'r'))['token']

# Authenticate motherduck using token
con = duckdb.connect('md:?motherduck_token=' + mdt) 

# Attach database
con.sql("USE my_db")

# Find the last match in the database
LastMatch = pl.DataFrame(con.sql("SELECT MAX(Match) AS LastMatch FROM MTG.CommanderHistoryBase"))

# Pull the current decks in the database
CurrentDecks = pl.DataFrame(con.sql("SELECT DISTINCT BASIC_ID FROM MTG.CommanderHistoryBase"))

# Read in Excel Data
ExcelLogger = pl.read_excel(
    source='C:/Users/Matso/OneDrive/Personal/My Documents/Projects/MTG Deck Battler/MTG Deck Logger Excel.xlsx',
    sheet_name='New Data'
)

# Set the column types correctly
ExcelLogger = ExcelLogger.with_columns([
    pl.col("Match").cast(pl.Int32), 
    pl.col("Player Count").cast(pl.Int8),
    pl.col("Turns").cast(pl.Int8),
    pl.col("Match Type").cast(pl.Utf8),
    pl.col("Win Type").cast(pl.Utf8),
    pl.col("Notes").cast(pl.Utf8),
    pl.col("Match Rating").cast(pl.Float64),
    pl.col("P1 Rating").cast(pl.Float64),
    pl.col("P2 Rating").cast(pl.Float64),
    pl.col("P3 Rating").cast(pl.Float64),
    pl.col("P4 Rating").cast(pl.Float64)
])

# Process New Data into long format
dfs = []
for i in range(1, 5):
    dfs.append(
        ExcelLogger.select([
            "Meta",
            pl.col(f"P{i} BASIC_ID").alias("Basic_ID"),
            pl.col(f"Player {i}").alias("Owner"),
            pl.col(f"Player {i} Deck").alias("Deck"),
            pl.lit(None, pl.Float32).alias("Elo"),
            "Match",
            pl.col(f"P{i} Place").alias("Place"),
            pl.lit(i, pl.Int8).alias("PlayerOrder"),
            pl.col(f"P{i} Rating").cast(pl.Float64).alias("MatchRating")
        ])
    )
# Merge together dataframes
long_df = pl.concat(dfs, how="vertical")
# drop rows where a player entry is empty for this match (Player absent)
long_df = long_df.filter(
    (pl.col("Owner").is_not_null())
)
# Filter down to only new matches
NewMatches = pa.table(long_df.filter(
    pl.col("Match") > LastMatch["LastMatch"][0]
))
# Separate out match details
MatchDetails = ExcelLogger.select("Meta", "Date", "Match", "Player Count", "Turns", "Match Type", "Win Type", "Notes", "Match Rating")
NewDetails = pa.table(MatchDetails.filter(
    pl.col("Match") > LastMatch["LastMatch"][0]
))

# Read in Decks
ExcelDecks = pl.read_excel(
    source='C:/Users/Matso/OneDrive/Personal/My Documents/Projects/MTG Deck Battler/MTG Deck Logger Excel.xlsx',
    sheet_name='CommanderDecks'
).select("BASIC_ID", "Meta", "Owner", "Deck", "Active")

NewDecks = ExcelDecks.join(CurrentDecks, left_on="BASIC_ID", right_on="Basic_ID", how="anti")

# Write new matches to Motherduck
con.sql("INSERT INTO MTG.CommanderHistoryBase SELECT * FROM NewMatches");
#con.sql("CREATE OR REPLACE TABLE MTG.CommanderHistoryNew AS SELECT * FROM long_df"); # Full list of matches

con.sql("INSERT INTO MTG.MatchDetails SELECT * FROM NewDetails");
#con.sql("CREATE OR REPLACE TABLE MTG.MatchDetails AS SELECT * FROM MatchDetails"); # Full list of matches details

con.sql("INSERT INTO MTG.CommanderDecksNew SELECT * FROM NewDecks");
# con.sql("CREATE OR REPLACE TABLE MTG.CommanderDecksNew AS SELECT * FROM ExcelDecks");