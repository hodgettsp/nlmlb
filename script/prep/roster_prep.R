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
raw_data_47_dodgers <- read_html("inputs/data/wikipedia/roster_list/raw_47_dodgers.html")

# extract needed data from raw html code
raw_text_data <- raw_data_47_dodgers %>%
     # extracts data from li nodes
     html_nodes("li") %>%
     # removes extraneous html text and code
     html_text()

# assign li tag data as a tibble ->
# with player_data as column hold text data
litag_data <- tibble(player_data = raw_text_data) %>%
     slice(53:99) %>%
     mutate(player_data = str_replace(player_data, "(^\\n\\d*\\W*\\d*\\W|^\\n\\d*\\W)", ""),
            player_data = str_replace(player_data, "\\n$", ""))

dodgers47_data <- litag_data %>%
     mutate(firstname = str_extract(player_data, "^\\w*"),
            lastname = str_extract(player_data, "\\w*$"))
