# installs tidyverse package for working with data
# version 1.3.1
#install.packages("tidyverse")

# load tidyverse package
# version 1.3.0
library(tidyverse)

source(here("script/rmarkdown/prep_data.R"))

# create new data object to hold debut counts
nl_mlb_debut_counts <- nl_mlb_df %>%
     # group by MLB debut year
     group_by(mlb_debut) %>%
     # summarise the played_negro_league values as grouped by debut year ->
     # because the values are 1's or 0's sum will produce an accurate count of ->
     # Negro League players making their debut while leaving 0 for non-Negro League players
     summarise(sum = sum(played_negro_league)) %>%
     # rename the sum column as debut_count ->
     # and rename mlb_debut as season for later joining
     rename(debut_count = sum,
            season = mlb_debut)

# create new data object ot hold final game counts
nl_mlb_final_counts <- nl_mlb_df %>%
     # group by final year in MLB
     group_by(mlb_final) %>%
     # summarise the played_negro_league values as grouped by debut year ->
     # because the values are 1's or 0's sum will produce an accurate count of ->
     # Negro League players playing their last season while leaving 0 for non-Negro League players
     summarise(sum = sum(played_negro_league)) %>%
     # rename the sum column as final_count ->
     # and rename mlb_debut as season for later joining
     rename(final_count = sum,
            season = mlb_final)

# join debut counts to final counts by season column
nl_mlb_counts <- inner_join(nl_mlb_debut_counts, nl_mlb_final_counts, by = "season") %>%
     # create new column player_count with 0 as value as placeholder
     mutate(player_count = 0)


# for index i in the length of player_count starting at second value
for(i in 2:length(nl_mlb_counts$player_count)){
     # make palyer_count at index location equal to->
     # debut count of that year + player count of previous year - final count of previous year
     # this gives an accurate count of the number of Negro League players for a season by ->
     # adding the number of new players to the number of previous players minus the number of players ->
     # no longer playing
     nl_mlb_counts$player_count[i] <- (nl_mlb_counts$debut_count[i] +
                                       nl_mlb_counts$player_count[i-1]) -
                                       nl_mlb_counts$final_count[i-1]
}

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
        add_row(season = 1873, debut_count = 0, final_count = 0, player_count = 0, .before = 1)%>%
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


# remove debut and final counts
rm(list = c("nl_mlb_debut_counts", "nl_mlb_final_counts", "wiki_debut_counts",
            "wiki_final_counts", "i"))
