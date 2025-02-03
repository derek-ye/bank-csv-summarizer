# Bank Transactions CSV parser
### Problem
It's hard to get an accurate summary of your spending when you have multiple credit cards with different banks.

### Solution
Parse each of the CSV's individually, categorize them with an LLM, then describe spend per category.

### Roadmap
[x] Set up module to categorize transactions using OpenAI
[ ] Set up module to convert CSV into JSON
[ ] Set up webserver to take CSVs and categorize them (turn them into)
[ ] Test all of the above work together

## Setup
- watch server - `ghcid "--command=cabal repl exe:bank-csv-summarizer"`