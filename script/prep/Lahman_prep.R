# install Lahman package for baseball data
# version 9.0-0
#install.packages("Lahman")

# install tidyverse package for working with data
# version 1.3.1
#install.packages("tidyverse")

# load Lahman library
library(Lahman)

# load tidyverse library
library(tidyverse)

# extract player data from Lahman dataset
mlb_player_data <- People %>%
     # select playerID, first name, last name, ->
     # given name, debut date, and final game date
     select(playerID, nameFirst, nameLast,
            nameGiven, debut, finalGame) %>%
     # rename playerID to palyerid
     rename(playerid = playerID,
            # rename nameFirst to firstname
            firstname = nameFirst,
            # rename nameLast to lastname
            lastname = nameLast,
            # rename nameGiven to givenname
            givenname = nameGiven,
            # rename finalGame to final_date
            final_date = finalGame,
            # rename debut to debut_date
            debut_date = debut) %>%
     # create new columns to hold debut and final year
     mutate(mlb_debut = substr(debut_date, 1, 4),
            mlb_final = substr(final_date, 1, 4),
            # adjust firstname and lastname for particular players to aid in later join
            firstname = case_when(givenname == "Hector Antonio" & lastname == "Rodriguez" ~ "Héctor",
                                  givenname == "Jose Guillermo" ~ "José",
                                  givenname == "James Buster" ~ "Buster",
                                  givenname == "William Haron" ~ "Billy",
                                  firstname == "Milt" & lastname == "Smith" ~ "Milton",
                                  givenname == "Vibert Ernesto" ~ "Vibert",
                                  firstname == "Choo-Choo" & lastname == "Coleman" ~ "Clarence",
                                  givenname == "George Bertrand" ~ "Bob",
                                  firstname == "J C" & lastname == "Hartman" ~ "J. C.",
                                  givenname == "Johnny Lee" ~ "John",
                                  TRUE ~ as.character(firstname)),
            lastname = case_when(givenname == "Saturnino Orestes Armas" ~ "Miñoso",
                                 givenname == "Luis Angel" ~ "Márquez",
                                 givenname == "Hector Antonio" & lastname == "Rodriguez" ~ "Rodríguez",
                                 firstname == "Sandy" & lastname == "Amoros" ~ "Amorós",
                                 TRUE ~ as.character(lastname)))

# write Lahman data to csv file
write_csv(mlb_player_data, here::here("inputs/data/csv/mlb_player_data.csv"))

# clear envrionment
rm(list = ls())
