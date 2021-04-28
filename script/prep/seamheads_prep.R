# uncomment any packages that require install

# install rvest package for web scraping
# version 1.0.0
#install.packages("rvest")

# install tidyverse packages for working with data
# version 1.3.1
#install.packages("tidyverse")

# load rvest library
library(rvest)
# load tidyverse libraries
library(tidyverse)

# list html data files to load
html_list <- c("raw_data_8636", "raw_data_3748")

# for the length of the html list
for(i in 1:length(html_list)){
     # assign a new object with raw_data_ replaced by nl_data_
     assign(str_replace(html_list[i], "raw_data_", "nl_data_"),
            # read html code from provided directory
            read_html(here::here("inputs/data/seamheads",
                                 # attach .html file type to list item
                                 paste0(html_list[i], ".html"))),
            # assign to global environment
            envir = .GlobalEnv)
}

# create function to prep seamheads data
# takes three parameters ->
# a data object, final row for slicing, and the seasons/years
seamhead_prep <- function(.data, .slice, .years){
     # assign data to text_data
     text_data <- .data %>%
          # extract code in td nodes
          html_nodes("td") %>%
          # remove extra html text
          html_text()
     # assign text data as tibble table_data with column name player_data
     table_data <- tibble(player_data = text_data)
     # assign new data object player_data_ with suffix .years ->
     # slice table_data tibble from row 13 to given row .slice
     assign(paste0("player_data_", .years), slice(table_data, 13:.slice) %>%
                 # mutate player_data values to strip beginning space ->
                 # and ending space
                 mutate(player_data = str_replace(player_data, "^\\s*", ""),
                        player_data = str_replace(player_data, "\\s*$", ""),
                        # add new key column with given values repeating every ->
                        # 13th row
                        key = rep(c("id", "player", "years", "pos", "pa", "ip",
                                    "drop", "ht", "wt", "b", "t", "birth", "death"),
                                  n()/13),
                        # add no column to track player and when to repeat
                        no = cumsum(key == "id")) %>%
                 # pivot the tibble from long to wide with names from key column ->
                 # and values from player_data
                 pivot_wider(names_from = key, values_from = player_data) %>%
                 # drop the no column and drop column
                 select(-no, -drop) %>%
                 # create new column year1 as first digits in string
                 mutate(year1 = str_match(years, "^\\d*"),
                        # create new column year2 as last digits in string
                        year2 = str_match(years, "\\d*$"),
                        # reassign year1 and year2 values as numeric
                        year1 = as.numeric(year1),
                        year2 = as.numeric(year2)),
            # assign to global environment
            envir = .GlobalEnv)
}

# apply seamhead_prep function to html data
seamhead_prep(.data = nl_data_8636, .slice = 29678, .years = "8636")
seamhead_prep(.data = nl_data_3748, .slice = 18277, .years = "3748")


# reassign player_data_8636
player_data_8636 <- player_data_8636 %>%
     # add "no" value to birth column where year2 is less than 1936 and birth is blank ->
     # distinguishes those players who did not play into the next season as a final season ->
     # of 1936 only marks cutoff point of data filter and not actual final season ->
     # prevents confusion in joining of datasets
     mutate(birth = if_else((year2 < 1936) & (birth == ""), "no", birth))

# assign new data object
seamheads_player_data <- player_data_8636 %>%
        # join player_data_8636 to player_data_3748 by given columns ->
        # skip position as same player could play different positions in different seasons ->
        # don't use year columns as players can play across selection divide of 1936-1937
        full_join(player_data_3748, by = c("player", "ht", "wt",
                                           "b", "t", "birth", "death")) %>%
        # remove placeholder "no" value
        mutate(birth = if_else(birth == "no", "", birth),
               debut = case_when(!is.na(year1.x) ~ year1.x,
                                 (is.na(year1.x) & !is.na(year1.y)) ~ year1.y),
               # create new column final for final year when year2.y is not NA ->
               # or when year2.y is NA then year2.x value
               final = case_when(!is.na(year2.y) ~ year2.y,
                                 (is.na(year2.y) & !is.na(year2.x)) ~ year2.x),
               # set second position as NA when the two positions are the same
               pos.y = if_else((pos.x != pos.y), pos.y, NA_character_),
               # change Cool Papa Bell's nickname to given name
               player = case_when(player == "Cool Papa Bell" ~ "James Bell",
                                  T ~ as.character(player)),
               # set last names of players as if it is TRUE that it is FALSE that ->
               # there is more than one name in the string then set last name as matched string ->
               # set last name as NA else wise
               lastname = if_else(str_detect(player, "(\\w.+\\s).+", negate = T),
                                  str_match(player, "\\w*$"),
                                  NA_character_),
               # fill in missing last names as if the last name value is NA ->
               # then match to string of last word characters in string ->
               # else wise keep as set values
               lastname = if_else(is.na(lastname), str_match(player, "\\w*$"),
                                  as.character(lastname)),
               # set first name values as if it is TRUE that there is more than one word ->
               # in the string, then extract the first initials or word characters from string ->
               # else wise set as NA
               firstname = if_else(str_detect(player, "(\\w.+\\s).+", negate = F),
                                   str_extract(player, "(^\\w\\W\\s\\w\\W|^\\w\\W|^\\w*)"),
                                   NA_character_)) %>%
        # drop year columns
        select(player, firstname, lastname, ht, wt, b, t, pos.x, pos.y, debut,
               final, birth, death) %>%
        # rename position columns
        rename(pos1 = pos.x,
               pos2 = pos.y)


# create debut count data object
seamheads_debut_count <- seamheads_player_data %>%
     # count debut years
     count(debut) %>%
     # rename count column
     rename(debut_count = n,
            # rename debut to season
            season = debut)

# create final count data object
seamheads_final_count <- seamheads_player_data %>%
     # count final season years
     count(final) %>%
     # rename count column
     rename(final_count = n,
            # rename final to season
            season = final)

# create new counts object by joining debut counts to final counts
seamheads_counts <- inner_join(seamheads_debut_count, seamheads_final_count,
                               # by season
                               by = "season") %>%
     # create new player_count column with values of 0
     mutate(player_count = 0) %>%
     # add dummy row to balance counts
     add_row(season = 1886, debut_count = 0,
             final_count = 0, player_count = 0, .before = 1)


# for index i in the length of player_count starting at second value
for(i in 2:length(seamheads_counts$player_count)){
     # make player_count at index location equal to->
     # debut count of that year + player count of previous year - final count of previous year
     # this gives an accurate count of the number of Negro League players for a season by ->
     # adding the number of new players to the number of previous players minus the number of players ->
     # no longer playing
     seamheads_counts$player_count[i] <- (seamheads_counts$debut_count[i] +
                                          seamheads_counts$player_count[i-1]) -
                                          seamheads_counts$final_count[i-1]
}

# remove list of objects
rm(list = c("nl_data_3748", "nl_data_8636", "player_data_3748", "player_data_8636",
            "seamheads_debut_count", "seamheads_final_count",
            "html_list", "i", "seamhead_prep"))

# write seamheads data to csv file
write_csv(seamheads_player_data, here::here("inputs/data/csv/seamheads_player_data.csv"))
