# install rvest package to scrape web data
# version 1.0.0
#install.packages("rvest")

# install tidyverse package for working with data
# version 1.3.1
#install.packages("tidyverse")

# load rvest library
library(rvest)

# load tidyverse library
library(tidyverse)

# read html code from given URL for Negro League players names A-D
nl_data_ad <- read_html("https://en.wikipedia.org/wiki/List_of_Negro_league_baseball_players_(A%E2%80%93D)")
# write html code to file
write_html(nl_data_ad, "inputs/data/wikipedia/complete_player_list/raw_data_ad.html")

# read html code from given URL for Negro League players names E-L
nl_data_el <- read_html("https://en.wikipedia.org/wiki/List_of_Negro_league_baseball_players_(E%E2%80%93L)")
# write html code to file
write_html(nl_data_el, "inputs/data/wikipedia/complete_player_list/raw_data_el.html")

# read html code from given URL for Negro League players names M-R
nl_data_mr <- read_html("https://en.wikipedia.org/wiki/List_of_Negro_league_baseball_players_(M%E2%80%93R)")
# write html code to file
write_html(nl_data_mr, "inputs/data/wikipedia/complete_player_list_raw_data_mr.html")

# read html code from given URL for Negro League players names S-Z
nl_data_sz <- read_html("https://en.wikipedia.org/wiki/List_of_Negro_league_baseball_players_(S%E2%80%93Z)")
# write html code to file
write_html(nl_data_sz, "inputs/data/wikipedia/complete_player_list/raw_data_sz.html")

# read html code from given URL for Negro League players who played in MLB
nl_data_mlb <- read_html("https://en.wikipedia.org/wiki/List_of_Negro_league_baseball_players_who_played_in_Major_League_Baseball")
# write html code to file
write_html(nl_data_mlb, "inputs/data/wikipedia/mlb_player_list/raw_data_mlb.html")

# read html code from given URL for Black players in the MLB
black_data_mlb <- read_html("https://en.wikipedia.org/wiki/List_of_first_black_Major_League_Baseball_players")
# write html code to file
write_html(black_data_mlb, "inputs/data/wikipedia/black_player_list/raw_black_data_mlb.html")


# read html code from given URL for Black players in the MLB
mlb_47_dodgers <- read_html("https://en.wikipedia.org/wiki/1947_Brooklyn_Dodgers_season")
# write html code to file
xml2::write_html(mlb_47_dodgers, "inputs/data/wikipedia/roster_list/raw_47_dodgers.html")
