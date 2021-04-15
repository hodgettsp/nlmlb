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


# generate list of file names in complete plater list folder
files <- list.files("inputs/data/wikipedia/complete_player_list/", pattern = "*.html")

# for the length of the list of files
for(i in 1:length(files)){
        # assign a new object using the first 11 characters of the filename
        assign(substr(files[i], 1, 11),
               # and read the html file
               read_html(here::here("inputs/data/wikipedia/complete_player_list/",
                                    files[i])),
               # assign to global environment
               envir = .GlobalEnv)
}

# designate prep scrape function ->
# takes one argument of a dataset
prep_scrape <- function(.data){
     # set html data as given dataset
     html_data <- .data %>%
          # extract data from td nodes
          html_nodes("td") %>%
          # remove extra html code
          html_text()
     # assign td data as a tibble with player_data as column
     table_data <- tibble(player_data = html_data)
     # assign inner dataset and remove unneeded rows
     player_data_az <- slice_head(table_data, n = (nrow(table_data) - 19))
     # reassign inner data
     player_data_az <- player_data_az %>%
          # mutate new column key to hold to be created columns ->
          # divide by six for six columns ->
          # designate tracker column no
          mutate(key = rep(c("player", "debut", "last_game", "pos", "teams", "drop"),
                           n()/6),
                 no = cumsum(key == "player")) %>%
          # pivot data from long to wide format using names from key column ->
          # and values from player data column
          pivot_wider(names_from = key, values_from = player_data) %>%
          # remove tracker column and extra column
          select(-no, -drop)
}

# read in A-Z data files
# for the length of the file list
for(i in 1:length(files)){
     # assign new data objects where raw is replaced by player
     assign(gsub("raw", "player", substr(files[i], 1, 11)),
            # apply prep_scrape function to data
            prep_scrape(get(substr(files[i], 1, 11))),
            # set environment to global
            envir = .GlobalEnv)
}

# create tibble for missing players with last names A-D that are on the CFNLBR list
player_ad_add <- tibble(player = c("Billy Bruton", "Tom Alston", "Donn Clendenon",
                                   "Charlie Dees", "Clarence Coleman"),
                     debut = c("1947", "1946", "1952", "1954", "1956"),
                     last_game = c("1953", "1951", "1956", "1954", "1957"),
                     pos = c("Outfielder", "First baseman", "First baseman", "First baseman", "Catcher"),
                     teams = c("Philadelphia Stars", "Greensboro Red Wings", "Kansas City Monarchs",
                     "Louisville Clippers", "Indianapolis Clowns"))

# create tibble for missing players with last names M-R that are on the CFNLBR list
player_mr_add <- tibble(player = c("Carlos Paula", "Willie McCovey", "Bob Prescott", "John Odom"),
                        debut = c("1950", "1955", "1951", "1962"),
                        last_game = c("1951", "1959", "1951", "1964"),
                        pos = c("Outfielder", "First baseman", "Outfielder", "Pitcher"),
                        teams = c("Havana Cuban Giants", "Birmingham Black Barons",
                                  "Jacksonville Eagles", "Raleigh Tigers"))

# create tibble for missing players with last names S-Z that are on the CFNLBR list
player_sz_add <- tibble(player = "Maury Wills",
                        debut = "1949",
                        last_game = "1949",
                        pos = "Shortstop",
                        teams = "Raleigh Tigers")

# bind A-D names to A-D data
player_data_ad <- bind_rows(player_data_ad, player_ad_add)

# bind M-R names to M-R data
player_data_mr <- bind_rows(player_data_mr, player_mr_add)

# bind S-Z names to S-Z data
player_data_sz <- bind_rows(player_data_sz, player_sz_add)


# bind individual datasets into one dataset by rows
nl_player_data <- bind_rows(player_data_ad, player_data_el,
                            player_data_mr, player_data_sz)

# clean player names
nl_player_data <- nl_player_data %>%
     # extract nicknames that are contained in "" marks ->
     # this is just for fun
     mutate(nickname = str_extract(player, '\\".*"'),
            # replace instances of (words), "words", and , with ->
            # no string or character ""
            suffix = str_extract(player, "(Jr.|Sr.)"),
            player = sub('(\\s\\(.*?))|(\\".*?")|,\\sJr.|\\sJr.|\\sSr.',
                         "", player),
            # strip space at beginning of player names
            player = str_replace(player, "^\\s", ""),
            # adjust player names where nicknames were used ->
            # or where initials were used in place of names
            player = case_when(player == "Jo Jo Deal" ~ "David Edward Deal",
                               player == "Cool Papa Bell" ~ "James Thomas Bell",
                               player == "Black Bottom Buford" ~ "James Buford",
                               player == "F. Sylvester 'Hooks' Foreman" ~ "Sylvester Foreman",
                               player == "Mule Suttles" ~ "George Suttles",
                               player == "Williams" ~ "Joseph Williams",
                               player == "J. B. Broom" ~ "John B. Broom",
                               player == "T. J. Brown" ~ "Thomas Jefferson Brown",
                               player == "J. B. Hairstone" ~ "James Burton Hairstone",
                               player == "J. C. Hamilton" ~ "John C. Hamilton",
                               player == "J. H. Hamilton" ~ "John H. Hamilton",
                               player == "J. C. Segraves" ~ "John Claud Seagraves",
                               player == "C. I. Taylor" ~ "Charles Isham Taylor",
                               player == "R. T. Walker" ~ "Robert Taylor Walker",
                               player == "T. J. Young" ~ "Thomas Jefferson Young",
                               player == "Frank Duncan I" ~ "Frank Duncan",
                               player == "Webbo Clarke" ~ "Vibert Clarke",
                               T ~ as.character(player)),
            # extract last name from player column
            lastname = str_extract(player, "(\\w*$)"),
            # extract first name from player column
            firstname = str_extract(player, "(^\\w\\W\\s\\w\\W|^\\w\\W\\w\\W|^\\w\\W\\s\\w*|^\\w*)"),
            firstname = sub("\\s*$", "", firstname),
            debut = case_when(debut = "19_" & player = "Johnny Washington" ~ "1950",
                              debut = "19_" & player = "Charles Johnson" ~ "1928",
                              debut = "19_" & player = "Martinez Jackson" ~ "1929",
                              debut = "19_" & player = "Harry Chappas" ~ "1975",
                              debut = "19_" & player = "Ike Brown" ~ "1960",
                              debut = "19_" & player = "Marshall Bridges" ~ "1951",
                              debut = "19_" & player = "Walt Bond" ~ "1956",
                              debut = "19_" ~ "NA",
                              TRUE ~ as.character(debut)),
            final_game = case_when(final_game = "19_" & player = "Johnny Washington" ~ "1950",
                                   final_game = "19_" & player = "Charles Johnson" ~ "1941",
                                   final_game = "19_" & player = "Martinez Jackson" ~ "1933",
                                   final_game = "19_" & player = "Harry Chappas" ~ "1975",
                                   final_game = "19_" & player = "Ike Brown" ~ "1961",
                                   final_game = "19_" & player = "Marshall Bridgers" ~ "1954",
                                   final_game = "19_" & player = "Walt Bond" ~ "1956",
                                   final_game = "19_" ~ "NA",
                                   TRUE ~ as.character(debut)))

# write NL player data to csv file
write_csv(nl_player_data, "inputs/data/csv/nl_player_data.csv")

# clear environment
rm(list = ls())
