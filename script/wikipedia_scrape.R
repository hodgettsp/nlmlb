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

# read html code from given URL for Negro League players names A-D
#nl_data_ad <- read_html("https://en.wikipedia.org/wiki/List_of_Negro_league_baseball_players_(A%E2%80%93D)")
# write html code to file
#write_html(nl_data_ad, "inputs/wikipedia/complete_player_list/raw_data_ad.html")

# read html code from given URL for Negro League players names E-L
#nl_data_el <- read_html("https://en.wikipedia.org/wiki/List_of_Negro_league_baseball_players_(E%E2%80%93L)")
# write html code to file
#write_html(nl_data_el, "inputs/wikipedia/complete_player_list/raw_data_el.html")

# read html code from given URL for Negro League players names M-R
#nl_data_mr <- read_html("https://en.wikipedia.org/wiki/List_of_Negro_league_baseball_players_(M%E2%80%93R)")
# write html code to file
#write_html(nl_data_mr, "inputs/wikipedia/complete_player_list_raw_data_mr.html")

# read html code from given URL for Negro League players names S-Z
#nl_data_sz <- read_html("https://en.wikipedia.org/wiki/List_of_Negro_league_baseball_players_(S%E2%80%93Z)")
# write html code to file
#write_html(nl_data_sz, "inputs/wikipedia/complete_player_list/raw_data_sz.html")

# read html code from given URL for Negro League players who played in MLB
#nl_data_mlb <- read_html("https://en.wikipedia.org/wiki/List_of_Negro_league_baseball_players_who_played_in_Major_League_Baseball")
# write html code to file
#write_html(nl_data_mlb, "inputs/wikipedia/mlb_player_list/raw_data_mlb.html")

# generate list of file names in complete plater list folder
files <- list.files("inputs/wikipedia/complete_player_list/", pattern = "*.html")

# for the legnth of the list of files
for(i in 1:length(files)){
        # assign a new object using the first 11 characters of the filename
        assign(substr(files[i], 1, 11),
               # and read the html file
               read_html(here::here("inputs/wikipedia/complete_player_list/",
                                    files[i])),
               # assign to global environment
               envir = .GlobalEnv)
}

# read raw html mlb player data
raw_data_mlb <- read_html(here::here("inputs/wikipedia/mlb_player_list/raw_data_mlb.html"))

# extract needed data from raw html code
mlb_text_data <- raw_data_mlb %>%
        # extracts data from td nodes
        html_nodes("td") %>%
        # removes extraneous html text and code
        html_text()

# assign td tag data as a tibble ->
# with player_data as column hold text data
tdtag_data <- tibble(player_data = mlb_text_data)

# assign new data object for mlb player data
mlb_player_data <- tdtag_data %>%
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
        # remove new line character from player column
        mutate(player = str_replace(mlb_player_data$player, "^\\n", "")) %>%
        # extract only first year of MLB years for debut year
        mutate(mlb_debut = substr(mlb_player_data$mlb_years, 1, 4)) %>%
        # extract first name from player column
        mutate(first_name = str_extract(mlb_player_data$player, "^\\w*")) %>%
        # extract last name from player column
        mutate(last_name = str_extract(mlb_player_data$player, "\\w*$"))


# designate prep scrape function ->
# takes one paramter of a dataset
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
               # set environment ot global
               envir = .GlobalEnv)
}

# bind individual datasets into one dataset
player_data_combined <- bind_rows(player_data_ad, player_data_el, player_data_mr, player_data_sz)

# create individual name columns and clean names
player_data_combined <- player_data_combined %>%
        mutate(player = sub("\\s\\(.*?)", "", player_data_combined$player))
player_data_combined <- player_data_combined %>%
        mutate(last_name = str_extract(player_data_combined$player, "\\w*$"),
               first_name = str_extract(player_data_combined$player, "^\\w*"))

