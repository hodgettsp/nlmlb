# uncomment any packages that require installation

# loads tidyverse for working with data
# version 1.3.1
#install.packages("tidyverse")

# loads here for accessing directories
# version 1.0.1
#install.packages("here")

# load tidyverse package
# version 1.3.1
library(tidyverse)

# load here package
# version 1.0.1
library(here)

# assign all following changes to the nl_player_data using nl_player_data
nl_player_data <- nl_player_data %>%
     # mutate firstname column values ->
     # these changes prevent double matches in later join
     # when firstname is Milton and lastname is Smith and last_game is 1927 ->
     # change firstname to Milton 27
     mutate(firstname = case_when(firstname == "Milton" & lastname == "Smith" & last_game == 1927 ~ "Milton 27",
                                  # when firstname is Willie and lastname is Smith and last_game is 1938 ->
                                  # change firstname to Willie 38
                                  firstname == "Willie" & lastname == "Smith" & last_game == 1938 ~ "Willie 38",
                                  # when firstname is Willie and lastname is Smith and last_game is 1948 ->
                                  # change firstname to Willie 48
                                  firstname == "Willie" & lastname == "Smith" & last_game == 1948 ~ "Willie 48",
                                  # when firstname is Clarence and lastname is Coleman and last_game is 1928 ->
                                  # change firstname to Clarence 28
                                  firstname == "Clarence" & lastname == "Coleman" & last_game == 1928 ~ "Clarence 28",
                                  # keep all other values as is
                                  TRUE ~ as.character(firstname)))


# assign new dataframe object
nl_df <- nl_player_data %>%
     # full join nl_mlb_player_data to nl_player_data by lastname and firstname
     full_join(nl_mlb_player_data, by = c("lastname", "firstname")) %>%
     # create new column played_mlb that is a binary dummy variable of whether the ->
     # player ever played in the MLB 1 or not 0
     mutate(played_mlb = if_else(!is.na(mlb_team), 1, 0),
            # create new column played_post_integration in which a player either debuted ->
            # after integration or played their last game after integration
            played_post_integration = if_else((debut >= 1947 | last_game >=1947), 1, 0),
            # strip digits from adjusted names
            firstname = str_replace(firstname, "\\s\\d*$", ""))


mlb_player_data <- mlb_player_data %>%
     # mutate firstname column values ->
     # these changes prevent double matches in later join
     # when givenname is Franke Samuel change firstname to Frank 29
     mutate(firstname = case_when(givenname == "Frank Samuel" ~ "Frank 29",
                                  # when givenname is John Louis change ->
                                  # firstname to Lou 94
                                  givenname == "John Louis" ~ "Lou 94",
                                  # when givenname is John Edward change - >
                                  # firstname to John 62
                                  givenname == "John Edward" ~ "John 62",
                                  # when givenname is Alfred Kendricks change ->
                                  # firstname to Al 26
                                  givenname == "Alfred Kendricks" ~ "Al 26",
                                  # when givenname is Alfred John change ->
                                  # firstname to Al 34
                                  givenname == "Alfred John" ~ "Al 34",
                                  # when givenname is George Allen change ->
                                  # firstname to George 16
                                  givenname == "George Allen" ~ "George 16",
                                  # when givenname is George Selby change ->
                                  # firstname to George 26
                                  givenname == "George Selby" ~ "George 26",
                                  # when givenname is Willie Everett  change ->
                                  # firstname to Willie 94
                                  givenname == "Willie Everett" ~ "Willie 94",
                                  TRUE ~ as.character(firstname)))


# assign new dataframe object
nl_mlb_df <- mlb_player_data %>%
     # full join nl_mlb_player_data to mlb_player_data by lastname and firstname
     full_join(nl_mlb_player_data, by = c("lastname", "firstname")) %>%
     # create new column for dummy variable played_negro_league ->
     # which holds whether a player also played in the Negro League
     mutate(played_negro_league = if_else(!is.na(year), 1, 0),
            # create new column for dummy variable played_post_integration ->
            # which holds whether a player debuted after integration ->
            # or played their final game after integration
            played_post_integration = if_else(mlb_debut >= 1947 | mlb_final >= 1947, 1, 0),
            firstname = str_replace(firstname, "\\s\\d*$", "")) %>%
     add_count(played_negro_league)
