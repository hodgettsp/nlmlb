# THE DATA FROM THIS HTML IS INCOMPLETE AND IS SUPERSEDED BY THE DATA PRODUCED IN CFNLBR_PREP
# THIS IS ONLY INCLUDED BECAUSE I DON'T WANT TO COMPLETELY REMOVE THIS CODE AS IT MAY HAVE SOMETHING
# THAT COULD BE OF USE IN THE FUTURE

# install rvest package to scrape web data
# version 1.0.0
#install.packages("rvest")

# install tidyverse package for working with data
# version 1.3.0
#install.packages("tidyverse")

# load rvest library
library(rvest)

# load tidyverse library
library(tidyverse)


# read raw html mlb player data
raw_data_mlb <- read_html("inputs/data//wikipedia/mlb_player_list/raw_data_mlb.html")

# extract needed data from raw html code
mlb_text_data <- raw_data_mlb %>%
     # extracts data from td nodes
     html_nodes("td") %>%
     # removes extraneous html text and code
     html_text()

# assign td tag data as a tibble ->
# with player_data as column hold text data
tdtag_data <- tibble(player_data = mlb_text_data)

# strip all newline characters from tdtag_data
tdtag_data$player_data <- str_replace(tdtag_data$player_data, "(\\n|\\r)", "")

# assign new data object for mlb player data
mlbnl_player_data <- tdtag_data %>%
     # remove rows after 314
     slice_head(n = 314) %>%
     # remove first two rows
     slice_tail(n = 312) %>%
     # mutate new column key to hold to be created columns ->
     # divide by six for six columns ->
     # designate tracker column no
     mutate(key = rep(c("player", "nl_teams", "nl_years", "mlb_teams", "mlb_years", "notes"),
                      n()/6),
            no = cumsum(key == "player")) %>%
     # pivot data from long to wide format using names from key column ->
     # and values from player data column
     pivot_wider(names_from = key, values_from = player_data) %>%
     # remove tracker column
     select(-no) %>%
     mutate(# keep only word characters (names)
          player = str_replace(player, "\\W$", ""),
          player = str_extract(player, "\\w*\\s\\w*"),
          # extract only first year of MLB years for debut year
          mlb_debut = substr(mlb_years, 1, 4),
          # extract final year of MLB career
          mlb_final = substr(mlb_years, str_length(mlb_years) - 3, str_length(mlb_years)),
          mlb_final = str_replace(mlb_final, "-", "1"),
          # extract last name from player column
          lastname = str_extract(player, "\\w*$"),
          # extract first name from player column
          firstname = str_extract(player, "^\\w*"))

