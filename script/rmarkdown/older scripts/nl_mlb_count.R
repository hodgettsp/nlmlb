# uncomment any packages that require installation

# install here package for accessing directories
# version 1.0.1
#install.packages("here")

# install tidyverse package for data stuff
# version 1.3.1
#install.packages("tidyverse")

# load here package
# version 1.0.1
library(here)

# load tidyverse package
# version 1.3.1
library(tidyverse)



source(here("script/rmarkdown/prep_data.R"))

#### NL MLB COUNTS

nl_mlb_counts <- roster4787_data %>%
        select(-Team, -Pos) %>%
        distinct() %>%
        add_count(season) %>%
        rename(lastname = Last,
               firstname = First,
               player_count = n) %>%
        mutate(lastname = case_when(lastname == "Minoso" & firstname == "Minnie" ~ "Miñoso",
                                    lastname == "Marquez" & firstname == "Luis" ~ "Márquez",
                                    lastname == "Rodriguez" & firstname == "Hector" ~ "Rodríguez",
                                    lastname == "Amoros" & firstname == "Sandy" ~ "Amorós",
                                    TRUE ~ as.character(lastname)),
               firstname = case_when(lastname == "Rodríguez" & firstname == "Hector" ~ "Héctor",
                                     lastname == "Clarkson" & firstname == "Buzz" ~ "Buster",
                                     lastname == "Bruton" & firstname == "Bill" ~ "Billy",
                                     lastname == "Santiago" & firstname == "Jose" ~ "José",
                                     lastname == "Clarke" & firstname == "Webbo" ~ "Vibert",
                                     lastname == "Coleman" & firstname == "Choo Choo" ~ "Clarence",
                                     lastname == "Prescott" & firstname == "Bobby" ~ "Bob",
                                     lastname == "Hartman" & firstname == "J.C." ~ "J. C.",
                                     lastname == "Odom" & firstname == "Blue Moon" ~ "John",
                                     lastname == "Smith" & firstname == "Milt" ~ "Milton",
                                     TRUE ~ as.character(firstname))) %>%
        full_join(nl_mlb_player_data, by = c("lastname", "firstname")) %>%
        # add dummy column as to whether player played in NL
        mutate(played_nl = if_else(!is.na(mlb_team), 1, 0),
               # add full year by pasting 19 to beginning of season
               year = paste0("19", str_extract(season, "^\\d*$")),
               # set as numeric
               year = as.numeric(year)) %>%
        # group by season
        group_by(season) %>%
        # count instances of played in the NL
        add_count(played_nl) %>%
        # ungroup
        ungroup() %>%
        # rename count column
        rename(nl_player_count = n)

#### WIKIPEDIA COUNTS

# assign new data object for wikipedia debut counts
wiki_debut_counts <- wiki_nl_df %>%
        # count debut seasons
        count(debut) %>%
        # rename count column
        # rename debut column to season
        rename(debut_count = n,
               season = debut)
# assign new data object for wikipedia final game counts
wiki_final_counts <- wiki_nl_df %>%
        # count last games
        count(last_game) %>%
        # rename final count column
        # rename last game column to season
        rename(final_count = n,
               season = last_game)

# assign new data object by joining wiki debut count to wiki final counts by season
wiki_counts <- full_join(wiki_debut_counts, wiki_final_counts, by = "season") %>%
        # add dummy player count column with values of 0
        mutate(player_count = 0) %>%
        # arrange ascending by season
        arrange(season) %>%
        # add dummy first row for 1873 season, this assists in later count calculation
        add_row(season = 1873, debut_count = 0, final_count = 0, player_count = 0,
                .before = 1)%>%
        # replace NA values with 0, not my favourite thing to do but imputation or ->
        # removal do not seem like good options
        mutate(across(everything(), ~replace_na(.x, 0)))

# for index i in the length of player_count starting at second value
for(i in 2:length(wiki_counts$player_count)){
        # make palyer_count at index location equal to->
        # debut count of that year + player count of previous year - final count of previous year
        # this gives an accurate count of the number of Negro League players for a season by ->
        # adding the number of new players to the number of previous players minus the number of players ->
        # no longer playing
        wiki_counts$player_count[i] <- (wiki_counts$debut_count[i] +
                                        wiki_counts$player_count[i-1]) -
                wiki_counts$final_count[i-1]
}

# remove row 84 from wiki counts
wiki_counts <- slice(wiki_counts, -84) %>%
        # add dummy row for 1976 season so plot doesn't look haggard
        add_row(season = 1976, debut_count = 0, final_count = 0, player_count = 0)



#### MLB COUNTS

# assign new object
mlb_debut_counts <- nl_mlb_df %>%
        # count mlb_debut seasons
        count(mlb_debut) %>%
        # rename count column
        rename(debut_count = n,
               # rename debut column to season
               season = mlb_debut) %>%
        # remove 2020 season and NA values
        slice(1:149)

# assign new object
mlb_final_counts <- nl_mlb_df %>%
        # count mlb_final seasons
        count(mlb_final) %>%
        # rename count column
        rename(final_count = n,
               # rename final column to season
               season = mlb_final) %>%
        # remove 2020 seasons and NA values
        slice(1:149)

# assign new object and full join debut counts with final counts by season
mlb_counts <- full_join(mlb_debut_counts, mlb_final_counts, by = "season") %>%
        # add new player count dummy column
        mutate(player_count = 0) %>%
        # arrange by season
        arrange(season) %>%
        # add dummy first row
        add_row(season = 1870, debut_count = 0, final_count = 0, player_count = 0,
                .before = 1)


# for index i in the length of player_count starting at second value
for(i in 2:length(mlb_counts$player_count)){
        # make player_count at index location equal to->
        # debut count of that year + player count of previous year - final count of previous year
        # this gives an accurate count of the number of Negro League players for a season by ->
        # adding the number of new players to the number of previous players minus the number of players ->
        # no longer playing
        mlb_counts$player_count[i] <- (mlb_counts$debut_count[i] +
                                                mlb_counts$player_count[i-1]) -
                mlb_counts$final_count[i-1]
}

# reassign mlb_counts
mlb_counts <- nl_mlb_counts %>%
        # remove 2020 and NA rows from nl_mlb_counts
        slice(1:149) %>%
        # add dummy first row to match mlb_counts
        add_row(season = 1870, debut_count = 0, final_count = 0, player_count = 0,
                .before = 1) %>%
        # full join mlb_counts to nl_mlb_counts by season
        full_join(mlb_counts, by = "season") %>%
        # rename count columns to match actual values
        rename(nl_debut = debut_count.x,
               nl_final = final_count.x,
               nl_count = player_count.x,
               mlb_debut = debut_count.y,
               mlb_final = final_count.y,
               mlb_count = player_count.y) %>%
        # remove dummy row
        slice(-1) %>%
        # create new column to calculate nl player percentage of mlb population
        mutate(nl_per_mlb_pop = round(nl_count/mlb_count, 2),
               # create new column to calculate nl player pop per team
               # divide by the number of teams for the given seasons as this changes dramatically after 1960
               nl_per_team = case_when(season <= 1915 ~ round(nl_count/16, 2),
                                       season >= 1916 & season <= 1960 ~ round(nl_count/16, 2),
                                       season == 1961 ~ round(nl_count/18, 2),
                                       season >= 1962 & season <= 1968 ~ round(nl_count/20, 2),
                                       season >= 1969 & season <= 1976 ~ round(nl_count/24, 2),
                                       season >= 1977 & season <= 1992 ~ round(nl_count/26, 2),
                                       season >= 1993 & season <= 1998 ~ round(nl_count/28, 2),
                                       season >= 1999 ~ round(nl_count/30, 2))
               )

# remove debut and final counts
rm(list = c("nl_mlb_debut_counts", "nl_mlb_final_counts", "wiki_debut_counts",
            "wiki_final_counts", "i", "mlb_debut_counts", "mlb_final_counts"))
