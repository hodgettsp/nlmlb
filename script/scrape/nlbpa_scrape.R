# install rvest package for web scraping
# version 1.0.0
#install.packages("rvest")

# install tidyverse package for data cleaning and prep
# version 1.3.0
#install.packages("tidyverse")

# load rvest into workspace
# version 1.0.0
library(rvest)

# load tidyverse into workspace
# version 1.3.0
library(tidyverse)

# read in html code from given URL
#nl_players_html <- read_html("http://www.nlbpa.com/the-athletes")

# write html code as raw data to directory
#write_html(nl_players_html, "inputs/data/nlbpa/raw_players_data.html")

# load in raw html data
nl_players_html <- read_html("inputs/data/nlbpa/raw_players_data.html")

# extract needed data from raw html code
text_data <- nl_players_html %>%
        # extracts data from paragraph nodes
        html_nodes("p") %>%
        # removes extraneous html text and code
        html_text()

# assign paragraph tag data as a tibble ->
# with player_data as column hold text data
ptag_data <- tibble(player_data = text_data)

# assign new dataset from paragraph tag data
nl_player_data <- ptag_data %>%
        # mutate player_data column ->
        # convert values to character type ->
        mutate(player_data = as.character(player_data)) %>%
        # filter to remove any values that end in '-' character ->
        # and filter to remove any values that start and end with whitespace
        filter(!grepl("-$", player_data),
               !grepl("^\\s$", player_data)) %>%
        # remove the first row
        slice(-1)

# generate new columns to hold last names and first names
nl_player_data <- nl_player_data %>%
        # use mutate to create new column last_name using values ->
        # extracted from the string in player_data column ->
        # of the first word until pattern is no longer a word character
        mutate(last_name = str_extract(nl_player_data$player_data,
                                       "\\w*"),
               # generate first_name column using values from->
               # player_data column of the first space and following word ->
               # also pulls in the space
               first_name = str_extract(nl_player_data$player_data,
                                        "\\s\\w*"))


# remove space at beginning of first name
nl_player_data <- nl_player_data %>%
        mutate(first_name = str_replace(nl_player_data$first_name,
                                                       "^\\s", ""))

mlb_player_data <- mlb_player_data %>%
        select(nameFirst, nameLast)


nl_player_data <- nl_player_data %>%
        rename(nameFirst = first_name,
               nameLast = last_name)

x <- inner_join(nl_player_data, mlb_player_data, by = c("nameFirst", "nameLast"))
