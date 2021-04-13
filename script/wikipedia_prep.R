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

bruton <- tibble(c("Billy Bruton", "19_", "19_", "Outfielder", "Philadelphia Stars"))
alston <- tibble(c("Tom Alston", "19_", "1950", "First baseman", "Greensboro Red Wings"))
paula <- tibble(c("Carlos Paula", "19_", "19_", "Outfielder", "Havana Cuban Giants"))
wills <- tibble(c("Maury Wills", "19_", "19_", "Shortstop", "Raleigh Tigers"))
mccovey <- tibble(c("Willie McCovey", "19_", "19_", "First baseman", "Birmingham Black Barons"))
prescott <- tibble(c("Bob Prescott", "19_", "19_", "Outfielder", "Jacksonville Eagles"))
clendenon <- tibble(c("Donn Clendenon", "19_", "19_", "First baseman", "Kansas City Monarchs"))
dees <- tibble(c("Charlie Dees", "19_", "19_", "First baseman", "Louisville Clippers"))
odom <- tibble(c("John Odom", "19_", "19_", "Pitcher", "Raleigh Tigers"))

player_data_ad <- bind_rows(player_data_ad, bruton, alston, clendenon, dees)
player_data_mr <- bind_rows(player_data_mr, paula, mccovey, prescott, odom)
player_data_sz <- bind_rows(player_data_sz, wills)


# bind individual datasets into one dataset by rows
nl_player_data <- bind_rows(player_data_ad, player_data_el,
                            player_data_mr, player_data_sz)

# clean player names
nl_player_data <- nl_player_data %>%
     # extract nicknames that are contained in "" marks ->
     # this is just for fun
     mutate(nickname = str_extract(player, '\\".*"'),
            # replace instances of (words), "words", Jr., and Sr. with ->
            # no string or character ""
            player = sub('(\\s\\(.*?))|(\\".*?")|(,\\sJr.)|(\\sSr.)',
                         "", player),
            # strip space at beginning of player names
            player = sub("^\\s", "", player),
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
            lastname = str_extract(player, "\\w*\\W*$"),
            # extract first name from player column
            firstname = str_extract(player, "(^\\w*\\W*\\s\\w*\\W*\\s|^\\w*\\W*\\w*\\s)"),
            firstname = sub("\\s*$", "", firstname),
            integration_debut = ifelse(debut >= 1947, 1, 0),
            integration_final = ifelse(last_game >= 1947, 1, 0))

# write NL player data to csv file
write_csv(nl_player_data, "inputs/data/csv/nl_player_data.csv")
