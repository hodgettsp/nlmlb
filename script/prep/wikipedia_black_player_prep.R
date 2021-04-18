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

# read raw html mlb player data
raw_black_data_mlb <- read_html("inputs/data/wikipedia/black_player_list/raw_black_data_mlb.html")

# extract needed data from raw html code
td_data <- raw_black_data_mlb %>%
     # extracts data from td nodes
     html_nodes("td") %>%
     # removes extraneous html text and code
     html_text()

# create temporary tibble from td tagged html data
t1 <- tibble(player_data = td_data) %>%
        # this adds data that is not included due to the html structure
        # add date before row 73
        add_row(player_data = "April 18, 1951", .before = 73) %>%
        # add date before row 77
        add_row(player_data = "April 18, 1951", .before = 77) %>%
        # add team before row 114
        add_row(player_data = "New York Giants", .before = 114) %>%
        # add league before row 115
        add_row(player_data = "NL", .before = 115) %>%
        # add date before row 116
        add_row(player_data = "July 8, 1949‡", .before = 116) %>%
        # add team before row 146
        add_row(player_data = "Cincinnati Reds", .before = 146) %>%
        # add league before row 147
        add_row(player_data = "NL", .before = 147) %>%
        # add date before row 148
        add_row(player_data = "April 17, 1954", .before = 148)

# assign new data object to hold ->
# pre-integration data values
pre_integration <- t1 %>%
        slice(1:15)

# create new function to run repetitive data prep
# take four parameters of a variable name, dataset, rows to slice, ->
# column names to be used, and key column to count
prep_data <- function(.var, .data, .slice, ..., .no){
        # assign new variable and slice provided dataset
        assign(.var, slice(.data, .slice) %>%
                       # mutate new column key to hold new column names as values ->
                       # and reset after every 4th row
                       mutate(key = rep(c(...), n()/4),
                              # create new column to hold tracker number
                              no = cumsum(key == .no)) %>%
                       # pivot data from long to wide with column names from key column ->
                       # and values from player_data
                       pivot_wider(names_from = key, values_from = .data$player_data) %>%
                       # remove no column
                       select(-no),
               # assign to global environment
               envir = .GlobalEnv)
}

# run prep_data function to create new dataset first20
prep_data(.var = "first20", .data = t1, .slice = 18:97,
                 "player", "mlb_team", "league", "date", .no = "player")

# run prep data function to create new dataset first_for_team
prep_data(.var = "first_for_team", .data = t1, .slice = 98:169,
          "mlb_team", "league", "date", "player", .no = "mlb_team")

# clean first20 dataframe
first20 <- first20 %>%
     mutate(player = str_replace(player, "\\s\\W*$", ""),
            # strip newline character at beginning of names
            player = str_replace(player, "(^\\r\\n)", ""),
            # create new column year from date column ->
            # using last word characters
            year = str_extract(date, "\\w*$"),
            # set year values as numeric
            year = as.numeric(year),
            # extract first name from player column ->
            # as starting word characters until first non-word chracter
            firstname = str_extract(player, "^\\w*"),
            # extract last name from player column ->
            # as final word characters
            lastname = str_extract(player, "\\w*$"),
            # convert date to date type class
            date = lubridate::parse_date_time(date, orders = c("bdy", "bY")))

# clean data object for first Black players on a team
first_for_team <- first_for_team %>%
     # strip non-word characters from player names
     mutate(player = str_replace(player, "(\\s\\W*$|\\W*$|\\[.*])", ""),
            # strip newline from player names
            player = str_replace(player, "(^\\r*\\n*)", ""),
            # create first name from player column ->
            # as first word characters until nonword character
            firstname = str_extract(player, "^\\w*"),
            # create last name from player using string_match for second match group ->
            # in which last name is all characters after the first space
            lastname = str_match(player, "(?:[^\\s]*\\s){1}([^\\s]*\\w.*$)")[,2],
            # strip special characters from date column
            date = str_replace(date, "\\W*$", ""),
            # create year column from date as last word characters
            year = str_extract(date, "\\w*$"),
            # set year as numeric
            year = as.numeric(year),
            # convert date to date type class
            date = lubridate::parse_date_time(date, orders = c("bdy", "bY"))) %>%
     # use select to reorder columns to match first20 dataframe
     select(player, mlb_team, league, date, year, firstname, lastname)

# create temporary place holder for years 1953 and above
t4 <- first_for_team %>%
        filter(year >= 1953)


# this data from http://research.sabr.org/journals/integration-of-baseball-after-world-war-ii
# create new tibble to hold data for players who debuted in 1952
season52 <- tibble(player = c("Héctor Rodríguez", "George Crowe", "Buster Clarkson",
                              "Quincy Trouppe", "Dave Pope", "Sandy Amorós"),
                   mlb_team = c("Chicago White Sox", "Boston Braves", "Boston Braves",
                                "Cleveland Indians", "Cleveland Indians", "Brooklyn Dodgers"),
                   league = c("AL", "NL", "NL", "AL", "AL", "NL"),
                   date = c("1952-04-15", "1952-04-17",
                            "1952-04-30", "1952-04-30",
                            "1952-07-01", "1952-08-22"),
                   year = 1952,
                   firstname = c("Héctor", "George", "Buster", "Quincy", "Dave", "Sandy"),
                   lastname = c("Rodríguez", "Crowe", "Clarkson", "Trouppe", "Pope", "Amorós")) %>%
        mutate(date = as.Date(date, format = "%Y-%m-%d"))


# bind created datasets together
black_players_mlb <- bind_rows(first20, t4, season52) %>%
        # arrange by year
        arrange(year) %>%
        # create new column to hold Sr. suffix
        mutate(suffix = str_extract(lastname, "Sr."),
               # strip last name of extra characters
               lastname = str_replace(lastname, "\\s\\w*\\W*$", ""))


# write data to csv
#write_csv(black_players_mlb, "inputs/data/csv/black_player_mlb_data.csv")

# clear environment
rm(list = ls())
