# %%
from converter.pgn_data import PGNData
from os import listdir
# %%
dirname = "chess_games\\janek"
games=os.listdir(dirname)

for i in range(0, len(games)):
    games[i]=dirname+"\\"+games[i]

# %%
pgn_data = PGNData(games,"output")
result = pgn_data.export()
result.print_summary()
# %%


# %%
