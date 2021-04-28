install.packages("rvest")
library(rvest)
library(tidyverse)


nl_data_8636 <- read_html("https://www.seamheads.com/NegroLgs/history.php?tab=players&first=1886&last=1936&lgID=All&lgType=N&NeL=Y&HOF=All&pos=All&bats=All&throws=All&results=2500&sort=Player_a")
xml2::write_html(nl_data_8636, "inputs/data/seamheads/raw_data_8636.html")

nl_data_3748 <- read_html("https://www.seamheads.com/NegroLgs/history.php?tab=players&first=1937&last=1948&lgType=All&NeL=Y&HOF=All&pos=All&bats=All&throws=All&results=2500&sort=Player_a")
xml2::write_html(nl_data_3748, "inputs/data/seamheads/raw_data_3748.html")

nl_data_3648 <- read_html("https://www.seamheads.com/NegroLgs/history.php?tab=players&first=1936&last=1948&lgID=All&lgType=All&NeL=All&HOF=All&pos=All&bats=All&throws=All&results=2500&sort=Player_a")
xml2::write_html(nl_data_3648, "inputs/data/seamheads/raw_data_3648.html")

nl_data_8636 <- read_html("inputs/data/seamheads/raw_data_8636.html")
nl_data_3748 <- read_html("inputs/data/seamheads/raw_data_3748.html")
nl_data_3648 <- read_html("inputs/data/seamheads/raw_data_3648.html")

text_data <- nl_data_8636 %>%
     html_nodes("td") %>%
     html_text()

table_data <- tibble(player_data = text_data)

player_data_8636 <- slice(table_data, 13:29678)

player_data_8636 <- player_data_8636 %>%
        mutate(player_data = str_replace(player_data, "^\\s*", ""),
               player_data = str_replace(player_data, "\\s*$", ""),
               key = rep(c("id", "player", "years", "pos", "pa", "ip",
                           "drop", "ht", "wt", "b", "t", "birth", "death"),
                         n()/13),
               no = cumsum(key == "id"))%>%
        pivot_wider(names_from = key, values_from = player_data) %>%
        select(-no, -drop)






text_data3748 <- nl_data_3748 %>%
        html_nodes("td") %>%
        html_text()

table_data3748 <- tibble(player_data = text_data3748)

player_data_3748 <- slice(table_data3748, 13:18277)

player_data_3748 <- player_data_3748 %>%
        mutate(player_data = str_replace(player_data, "^\\s*", ""),
               player_data = str_replace(player_data, "\\s*$", ""),
               key = rep(c("id", "player", "years", "pos", "pa", "ip",
                           "drop", "ht", "wt", "b", "t", "birth", "death"),
                         n()/13),
               no = cumsum(key == "id"))%>%
        pivot_wider(names_from = key, values_from = player_data) %>%
        select(-no, -drop)




x <- player_data_8636 %>%
        mutate(year1 = str_match(years, "^\\d*"),
               year2 = str_match(years, "\\d*$"),
               year1 = as.numeric(year1),
               year2 = as.numeric(year2),
               birth = if_else((year2 < 1936) & (birth == ""), "no", birth))

y <- player_data_3748 %>%
        mutate(year1 = str_match(years, "^\\d*"),
               year2 = str_match(years, "\\d*$"),
               year1 = as.numeric(year1),
               year2 = as.numeric(year2))
               #birth = if_else(((year1 > 1921) & (year2 < 1935)) & (birth == ""), "ok", birth))

z <- x %>%
        full_join(y, by = c("player", "ht", "wt", "b", "t", "birth", "death")) %>%
        select(player, years.x, years.y, year1.x, year2.x, year1.y, year2.y) %>%
        rename(year1 = year1.x,
               year2 = year2.x,
               year3 = year1.y,
               year4 = year2.y) %>%
        mutate(debut = case_when(!is.na(year1) ~ year1,
                                 (is.na(year1) & !is.na(year3)) ~ year3),
               final = case_when(!is.na(year4) ~ year4,
                                 (is.na(year4) & !is.na(year2)) ~ year2)) %>%
        select(-year1, -year2, -year3, -year4)


a <- z %>%
        count(debut) %>%
        rename(debut_count = n,
               season = debut)

b <- z %>%
        count(final) %>%
        rename(final_count = n,
               season = final)

c <- inner_join(a, b, by = "season") %>%
        mutate(player_count = 0)


# for index i in the length of player_count starting at second value
for(i in 2:length(c$player_count)){
        # make player_count at index location equal to->
        # debut count of that year + player count of previous year - final count of previous year
        # this gives an accurate count of the number of Negro League players for a season by ->
        # adding the number of new players to the number of previous players minus the number of players ->
        # no longer playing
        c$player_count[i] <- (c$debut_count[i] +
                                      c$player_count[i-1]) -
                c$final_count[i-1]
}







z <- player_data_3648 %>%
        mutate(year1 = str_match(years, "^\\d*"),
               year2 = str_match(years, "\\d*$"),
               year1 = as.numeric(year1),
               year2 = as.numeric(year2))
               #birth = if_else(((year1 < 1948) & (year2 )) & (birth == ""), "go", birth))
a <- x %>%
        full_join(y, by = c("player", "ht", "wt", "b", "t", "birth", "death")) %>%
        full_join(z, by = c("player", "ht", "wt", "b", "t", "birth", "death")) %>%
        select(player, years.x, years.y, years)


a <- a %>%
        mutate(year1 = str_match(years.x, "^\\d*"),
               year2 = str_match(years.x, "\\d*$"),
               year3 = str_match(years.y, "^\\d*"),
               year4 = str_match(years.y, "\\d*$"),
               year5 = str_match(years, "^\\d*"),
               year6 = str_match(years, "\\d*$"),
               debut = case_when(!is.na(year1) ~ year1,
                                 (is.na(year1) & !is.na(year3)) ~ year3,
                                 (is.na(year1) & is.na(year3) & !is.na(year5)) ~ year5),
               final = case_when(!is.na(year6) ~ year6,
                                 (is.na(year6) & !is.na(year4)) ~ year4,
                                 (is.na(year6) & is.na(year4) & !is.na(year2)) ~ year2))

b <- a %>%
        count(debut) %>%
        rename(debut_count = n,
               season = debut)

c <- a %>%
        count(final) %>%
        rename(final_count = n,
               season = final)

d <- inner_join(b, c, by = "season") %>%
        mutate(player_count = 0)


# for index i in the length of player_count starting at second value
for(i in 2:length(d$player_count)){
        # make player_count at index location equal to->
        # debut count of that year + player count of previous year - final count of previous year
        # this gives an accurate count of the number of Negro League players for a season by ->
        # adding the number of new players to the number of previous players minus the number of players ->
        # no longer playing
        d$player_count[i] <- (d$debut_count[i] +
                                      d$player_count[i-1]) -
                d$final_count[i-1]
}
