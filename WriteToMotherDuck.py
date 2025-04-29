import duckdb
import yaml

# Read in the token from the yaml file
mdt = yaml.safe_load(open('MDToken.yaml', 'r'))['token']

# Authenticate motherduck using token
con = duckdb.connect('md:?motherduck_token=' + mdt) 

# Attach database
con.sql("USE my_db")

# Wtite to motherduck
con.sql("CREATE OR REPLACE TABLE MTG.CommanderDecksWRA AS SELECT * FROM 'Data/CommanderDecksWRA.csv'");
con.sql("CREATE OR REPLACE TABLE MTG.CommanderHistory AS SELECT * FROM 'Data/CommanderHistory.csv'");

'''
# Code for fixing dumb mistakes
data = con.sql("SELECT * FROM MTG.CommanderHistory")
data.write_csv('CommanderHistoryTest.csv')
'''