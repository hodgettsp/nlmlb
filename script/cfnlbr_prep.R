# install tidyverse package for working with data
# version 1.3.0
#install.packages("tidyverse")

# install pdftools package for scraping PDF
# version 2.3.1
#install.packages("pdftools")

# load tidyverse library
library(tidyverse)

# load pdftools library
library(pdftools)

# extract pdf text using pdf_text function from pdftools
raw_pdf <- pdf_text("inputs/data/cfnlbr/Negro League Players Who Played in the Major Leagues.pdf")

# set pdf text data in tibble data frame with raw_text as column name
pdf_tibble <- tibble(raw_text = raw_pdf)

# separate rows in pdf_tibble with newline separator
pdf_tibble <- separate_rows(pdf_tibble, raw_text, sep = "\\n", convert = F)

# assign new data object from pdf_tibble
nl_mlb_players <- pdf_tibble %>%
     # slcie from rows 3 to 48 and 51 to 91
     slice(3:48,51:91) %>%
     # mutate raw_text columns to replace text quotation characters ->
     # with more standard quotation marks
     mutate(raw_text = str_replace(raw_text, "“", '"'),
            raw_text = str_replace(raw_text, "”", '"'),
            # create new column playername with mutate function ->
            # values are those that start with word characters followed by space followed by any character ->
            # followed by a space followed by word characters or ->
            # starting with word characters followed by space followed by word characters or ->
            # starting with word character followed by nonword character followed by word character followed by ->
            # nonword character followed by space followed by word character
            playername = str_extract(raw_text,
                                     '(^\\w*\\s".*"\\s\\w*|^\\w*\\s\\w*|^\\w*\\W*\\w*\\W*\\s\\w*)'),
            # create new column firstname using mutate and str_extract from stringr ->
            # first name is a string beginning with a word character followed by a nonword chracter ->
            # followed by a word character followed by a nonword character or ->
            # beginning with a word character until the first nonword character
            firstname = str_extract(playername, "(^\\w\\W\\w\\W|^\\w*)"),
            # create new column lastname using mutate and str_extract from stringr ->
            # last name is a string of word characters until the end of the string
            lastname = str_extract(playername, "\\w*$"),
            # create new column teams using mutate and str_extract
            # this is strings of all characters between two or more spaces and a space then character 1
            teams = str_extract(raw_text,
                                "(\\s{2,}(.*)\\s1|Raleigh Tigers          Kansas City Athletics)"),
            # strip 1 numeral character from teams strings
            teams = str_replace(teams, "\\s*1\\s*$", ""),
            # strip spaces from beginning of teams
            teams = str_replace(teams, "^\\s*", ""),
            # create new column mlb_teams to hold the MLB team the player debuted with ->
            # this is a string of all characters between spaces of two or more and the end of a string
            mlb_team = str_extract(teams, "\\s{2,}(.*)$"),
            # fill in missing mlb_teams with if_else function and str_match
            mlb_team = if_else(is.na(mlb_team),
                                # using str_match use non-capture group to track spaces ->
                                # these teams are those after the third space in a character string ->
                                # and the 2nd indexed group produced
                                str_match(teams,
                                          "(?:[^\\s]*\\s){3}([^\\s]*\\s.*$)")[,2],
                                # keeps any previously extracted teams as they are
                                as.character(mlb_team)),
            # strip spaces from mlb teams strings
            mlb_team = str_replace(mlb_team, "^\\s*", ""),
            # create new column to hold Negro League teams for player ->
            # these are all characters between strings starting with word characters and two or more spaces
            nl_team = str_extract(teams, "^\\w*(.*)\\s{2}"),
            # fill missing Negro League teams using if_else
            nl_team = if_else(is.na(nl_team),
                               # using str_match use non-capture group to track spaces ->
                               # these teams are those strings before the third space
                               str_match(teams,
                                           "(?:[^\\s]*\\s){3}"),
                               # keep any previously created teams as they are
                               as.character(nl_team)),
            # strip all spaces at the end of the string
            nl_team = str_replace(nl_team, "\\s*$", ""),
            # extract the last word characters to the end of the raw_text string for year of debut
            year = str_extract(raw_text, "\\w*$"),
            # convert these values to numeric
            year = as.numeric(year)) %>%
     # remove raw text and teams columns
     select(-raw_text, -teams)
