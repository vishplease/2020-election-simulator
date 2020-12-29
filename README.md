# 2020-election-simulator
This repository includes the code for an RShiny app that simulates the 2020 election results based on AP exit poll data scraped from NY Times.

The code folder includes:
	•	scraper.R: scrapes NY Times AP exit polls and generates full_poll.csv
	•	data_clean.R: cleans dataset and filters only questions used in the RShiny app. Generates poll_viz.csv
