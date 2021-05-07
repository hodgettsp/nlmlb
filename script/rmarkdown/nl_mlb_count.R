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
        dplyr::select(-Team, -Pos) %>%
        dplyr::distinct() %>%
        dplyr::add_count(season) %>%
        dplyr::rename(lastname = Last,
               firstname = First,
               player_count = n) %>%
        dplyr::mutate(firstname = case_when(lastname == "Bruton" & firstname == "Bill" ~ "Billy",
                                     lastname == "Prescott" & firstname == "Bobby" ~ "Bob",
                                     lastname == "Odom" & firstname == "Blue Moon" ~ "John",
                                     TRUE ~ as.character(firstname))) %>%
        dplyr::inner_join(nl_mlb_player_data, by = c("lastname", "firstname")) %>%
        # drop extraneous columns
        dplyr::select(-mlb_team, -nl_team, -year) %>%
        # group by season
        dplyr::group_by(season) %>%
        # count instances of played in the NL
        dplyr::add_count(season) %>%
        dplyr::ungroup() %>%
        dplyr::rename(nlp_count = n) %>%
        mutate(nlp_per_pop = round(nlp_count/player_count, 2),
               nlp_per_team = case_when(season <= 1960 ~ round(nlp_count/16, 2),
                                        season == 1961 ~ round(nlp_count/18, 2),
                                        season >= 1962 & season <= 1968 ~ round(nlp_count/20, 2),
                                        season >= 1969 & season <= 1977 ~ round(nlp_count/24, 2),
                                        season >= 1977 ~ round(nlp_count/26, 2)))



t1 <- nl_mlb_counts %>%
        select(season, player_count, nlp_per_team, nlp_count, nlp_per_pop) %>%
        distinct()

nl_mlb_team_counts <- roster4787_data %>%
        dplyr::rename(lastname = Last,
                      firstname = First) %>%
        dplyr::mutate(firstname = case_when(lastname == "Bruton" & firstname == "Bill" ~ "Billy",
                                            lastname == "Prescott" & firstname == "Bobby" ~ "Bob",
                                            lastname == "Odom" & firstname == "Blue Moon" ~ "John",
                                            TRUE ~ as.character(firstname))) %>%
        dplyr::full_join(nl_mlb_player_data, by = c("lastname", "firstname")) %>%
        mutate(played_nl = if_else(!is.na(year), 1, 0)) %>%
        group_by(Team, season) %>%
        mutate(nlp_team_count = sum(played_nl),
               Team = case_when(Team == "BOS" ~ "Boston Red Sox",
                                Team == "BRO" ~ "Brooklyn Dodgers",
                                Team == "BSN" ~ "Boston",
                                Team == "CHA" ~ "Chicago White Sox",
                                Team == "CHN" ~ "Chicago Cubs",
                                Team == "CIN" ~ "Cincinnati Reds",
                                Team == "CLE" ~ "Cleveland",
                                Team == "DET" ~ "Detroit Tigers",
                                Team == "NY1" ~ "New York Giants",
                                Team == "NYA" ~ "New York Yankees",
                                Team == "PHA" ~ "Philadelphia Athletics",
                                Team == "PHI" ~ "Philadelphia Phillies",
                                Team == "PIT" ~ "Pittsburgh Pirates",
                                Team == "SLA" ~ "St. Louis Browns",
                                Team == "SLN" ~ "St. Louis Cardinals",
                                Team == "WS1" ~ "Washington Senators",
                                Team == "MLN" ~ "Milwaukee",
                                Team == "BAL" ~ "Baltimore Orioles",
                                Team == "KC1" ~ "Kansas City Athletics",
                                Team == "LAN" ~ "Los Angeles Dodgers",
                                Team == "SFN" ~ "San Francisco Giants",
                                Team == "LAA" ~ "Los Angeles Angels",
                                Team == "MIN" ~ "Minnesota Twins",
                                Team == "WS2" ~ "Washington Senators",
                                Team == "HOU" ~ "Houston Astros",
                                Team == "NYN" ~ "New York Mets",
                                Team == "CAL" ~ "California Angels",
                                Team == "OAK" ~ "Oakland Athletics",
                                Team == "ATL" ~ "Atlanta",
                                Team == "MON" ~ "Montreal Expos",
                                Team == "SDN" ~ "San Diego Padres",
                                Team == "SE1" ~ "Seattle Pilots",
                                Team == "MIL" ~ "Milwaukee Brewers",
                                Team == "KCA" ~ "Kansas City Royals",
                                Team == "TEX" ~ "Texas Rangers",
                                Team == "SEA" ~ "Seattle Mariners",
                                Team == "TOR" ~ "Toronto Blue Jays")) %>%
        ungroup() %>%
        select(Team, season, nlp_team_count) %>%
        distinct() %>%
        filter(season >= 1947 & season <= 1980) %>%
        full_join(t1, by = "season") %>%
        select(season, player_count, nlp_count, nlp_per_team, nlp_per_pop,
               Team, nlp_team_count)

rm(t1)




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
                .before = 1) %>%
        # replace NA values with 0, not my favourite thing to do but imputation or ->
        # removal do not seem like good options
        mutate(across(everything(), ~replace_na(.x, 0)))

# for index i in the length of player_count starting at second value
for(i in 2:length(wiki_counts$player_count)){
        # make player_count at index location equal to->
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



# remove debut and final counts
rm(list = c("wiki_debut_counts", "wiki_final_counts", "i"))
