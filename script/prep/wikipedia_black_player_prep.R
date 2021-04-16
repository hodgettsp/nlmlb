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

t1 <- tibble(player_data = td_data)

t1 <- add_row(t1, player_data = "April 18, 1951", .before = 73)

t1 <- add_row(t1, player_data = "April 18, 1951", .before = 77)

t1 <- add_row(t1, player_data = "New York Giants", .before = 114)
t1 <- add_row(t1, player_data = "NL", .before = 115)
t1 <- add_row(t1, player_data = "July 8, 1949‡", .before = 116)

t1 <- add_row(t1, player_data = "Cincinnati Reds", .before = 146)
t1 <- add_row(t1, player_data = "NL", .before = 147)
t1 <- add_row(t1, player_data = "April 17, 1954", .before = 148)


t2 <- t1 %>%
     slice(1:15)

t3 <- t1 %>%
     slice(18:97) %>%
     mutate(key = rep(c("player", "mlb_team", "league", "date"), n()/4),
            no = cumsum(key == "player")) %>%
     pivot_wider(names_from = key, values_from = player_data) %>%
     select(-no) %>%
     mutate(player = str_replace(player, "\\s\\W*$", ""),
            player = str_replace(player, "^\\n", ""),
            year = str_extract(date, "\\w*$"),
            year = as.numeric(year),
            firstname = str_extract(player, "^\\w*"),
            lastname = str_extract(player, "\\w*$"))

t4 <- t1 %>%
     slice(98:169) %>%
     mutate(key = rep(c("mlb_team", "league", "date", "player"), n()/4),
            no = cumsum(key == "mlb_team")) %>%
     pivot_wider(names_from = key, values_from = player_data) %>%
     select(-no) %>%
     mutate(player = str_replace(player, "(\\s\\W*$|\\W*$|\\[.*])", ""),
            player = str_replace(player, "^\\n*", ""),
            firstname = str_extract(player, "^\\w*"),
            lastname = str_match(player, "(?:[^\\s]*\\s){1}([^\\s]*\\w.*$)")[,2],
            date = str_replace(date, "\\W*$", ""),
            year = str_extract(date, "\\w*$"),
            year = as.numeric(year)) %>%
     select(player, mlb_team, league, date, year, firstname, lastname)

t4 <- t4 %>%
     filter(year >= 1953)

t5 <- bind_rows(t3, t4)

# this data from http://research.sabr.org/journals/integration-of-baseball-after-world-war-ii
season52 <- tibble(player = c("Héctor Rodríguez", "George Crowe", "Buster Clarkson",
                              "Quincy Trouppe", "Dave Pope", "Sandy Amorós"),
                   mlb_team = c("Chicago White Sox", "Boston Braves", "Boston Braves",
                                "Cleveland Indians", "Cleveland Indians", "Brooklyn Dodgers"),
                   league = c("AL", "NL", "NL", "AL", "AL", "NL"),
                   date = c("April 15, 1952", "April 17, 1952",
                            "April 30, 1952", "April 30 1952",
                            "July 1, 1952", "August 22, 1952"),
                   year = 1952,
                   firstname = c("Héctor", "George", "Buster", "Quincy", "Dave", "Sandy"),
                   lastname = c("Rodríguez", "Crowe", "Clarkson", "Trouppe", "Pope", "Amorós"))


t6 <- bind_rows(t5, season52) %>%
     arrange(year) %>%
     mutate(suffix = str_extract(lastname, "Sr."),
            lastname = str_replace(lastname, "\\s\\w*\\W*$", ""))
