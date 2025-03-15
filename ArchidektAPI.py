# import pyrchidekt
# from pyrchidekt.api import getDeckById
import json
import requests
import polars
import os

polars.Config.set_tbl_cols(-1) # Display all columns
polars.Config.set_tbl_width_chars(15)

response = requests.get("https://archidekt.com/api/decks/11701268/")
deck_base = polars.read_json(response.content)

# Get the columns of the dataframe
deck_base.columns

# Split the data into each of the subgroups
categories = deck_base["categories"].explode()
deckTags = deck_base["deckTags"].explode()
cards = deck_base["cards"].explode()

categories = polars.DataFrame(categories).unnest('categories')
deckTags = polars.DataFrame(deckTags).unnest('deckTags')
cards = polars.DataFrame(cards).unnest('cards')
card2 = polars.DataFrame(cards["card"].explode()).unnest('card')

print(response.json())
response = requests.get("https://archidekt.com/api/decks/11689861/")

'''
This is if I wanted to use pychidekt. I don't think I want to use it and can just use the API and parse the JSON.

# ID 11701268 is "Chiss of Death" by "westee222@gmail.com"
deck = getDeckById(11701268)
for category in deck.categories:
    print(f"{category.name}")
    for card in category.cards:
        print(f"\t{card.quantity} {card.card.oracle_card.name}")
    print("");

deck.owner
deck.cards
deck.categories
deck.bookmarked
deck.card_package
deck.cards

deck_dict = deck.to_dict()
person_json = json.dumps(deck, indent=4)

print(deck)
'''
