import pandas as pd


store = pd.read_csv("../data/original/store.csv")

store.to_csv("../data/prepped/store.csv")
