install.packages("rvest")
library(rvest)
library(tidyverse)


nl_data_8620 <- read_html("https://www.seamheads.com/NegroLgs/history.php?tab=players&first=1886&last=1920&lgID=All&lgType=All&NeL=All&HOF=All&pos=All&bats=All&throws=All&results=2500&sort=Player_a")
xml2::write_html(nl_data_8620, "inputs/data/seamheads/raw_data_8620.html")

nl_data_2035 <- read_html("https://www.seamheads.com/NegroLgs/history.php?tab=players&first=1920&last=1935&lgID=All&lgType=All&NeL=All&HOF=All&pos=All&bats=All&throws=All&results=2500&sort=Player_a")
xml2::write_html(nl_data_2035, "inputs/data/seamheads/raw_data_2035.html")

nl_data_3048 <- read_html("https://www.seamheads.com/NegroLgs/history.php?tab=players&first=1930&last=1948&lgID=All&lgType=All&NeL=All&HOF=All&pos=All&bats=All&throws=All&results=2500&sort=Player_a")
xml2::write_html(nl_data_3048, "inputs/data/seamheads/raw_data_3048.html")

nl_data_8620 <- read_html("inputs/data/seamheads/raw_data_8620.html")
nl_data_2035 <- read_html("inputs/data/seamheads/raw_data_2035.html")
nl_data_3048 <- read_html("inputs/data/seamheads/raw_data_3048.html")

text_data <- nl_data_8620 %>%
     html_nodes("td") %>%
     html_text()

table_data <- tibble(player_data = text_data)

player_data_8620 <- slice(table_data, 13:25544)

player_data_8620 <- player_data_8620 %>%
        mutate(player_data = str_replace(player_data, "^\\s*", ""),
               player_data = str_replace(player_data, "\\s*$", ""),
               key = rep(c("id", "player", "years", "pos", "pa", "ip",
                           "drop", "ht", "wt", "b", "t", "birth", "death"),
                         n()/13),
               no = cumsum(key == "id"))%>%
        pivot_wider(names_from = key, values_from = player_data) %>%
        select(-no, -drop)






text_data2035 <- nl_data_2035 %>%
        html_nodes("td") %>%
        html_text()

table_data2035 <- tibble(player_data = text_data2035)

player_data_2035 <- slice(table_data2035, 13:24803)

player_data_2035 <- player_data_2035 %>%
        mutate(player_data = str_replace(player_data, "^\\s*", ""),
               player_data = str_replace(player_data, "\\s*$", ""),
               key = rep(c("id", "player", "years", "pos", "pa", "ip",
                           "drop", "ht", "wt", "b", "t", "birth", "death"),
                         n()/13),
               no = cumsum(key == "id"))%>%
        pivot_wider(names_from = key, values_from = player_data) %>%
        select(-no, -drop)






text_data3048 <- nl_data_3048 %>%
        html_nodes("td") %>%
        html_text()

table_data3048 <- tibble(player_data = text_data3048)

player_data_3048 <- slice(table_data3048, 13:30341)

player_data_3048 <- player_data_3048 %>%
        mutate(player_data = str_replace(player_data, "^\\s*", ""),
               player_data = str_replace(player_data, "\\s*$", ""),
               key = rep(c("id", "player", "years", "pos", "pa", "ip",
                           "drop", "ht", "wt", "b", "t", "birth", "death"),
                         n()/13),
               no = cumsum(key == "id"))%>%
        pivot_wider(names_from = key, values_from = player_data) %>%
        select(-no, -drop)

x <- player_data_2035 %>%
        mutate(year1 = str_match(years, "^\\d*"),
               year2 = str_match(years, "\\d*$"),
               year1 = as.numeric(year1),
               year2 = as.numeric(year2),
               birth = if_else(((year2 < 1935) & (year1 > 1920)) & (birth == ""), "no", birth))

y <- player_data_8620 %>%
        mutate(year1 = str_match(years, "^\\d*"),
               year2 = str_match(years, "\\d*$"),
               year1 = as.numeric(year1),
               year2 = as.numeric(year2),
               birth = if_else((year2 < 1920) & (birth == ""), "ok", birth))

z <- player_data_3048 %>%
        mutate(year1 = str_match(years, "^\\d*"),
               year2 = str_match(years, "\\d*$"),
               year1 = as.numeric(year1),
               year2 = as.numeric(year2),
               birth = if_else((year1 < 1948) & (birth == ""), "go", birth))

x <- player_data_8620 %>%
        full_join(player_data_2035, by = c("player", "ht", "wt", "b", "t", "birth", "death")) %>%
        full_join(player_data_3048, by = c("player", "ht", "wt", "b", "t", "birth", "death"))


y <- x %>%
        mutate(year1 = str_match(years.x, "^\\d*"),
               year2 = str_match(years.x, "\\d*$"),
               year3 = str_match(years.y, "^\\d*"),
               year4 = str_match(years.y, "\\d*$"),
               year5 = str_match(years, "^\\d*"),
               year6 = str_match(years, "\\d*$"))

x <- x %>%
        mutate(years.x = if_else((!is.na(years.x) & !is.na(years.y)) | (is.na(years.x)), years.y, years.x)) %>%
        mutate(years.x = if_else((!is.na(years.x) & !is.na(years)) | (is.na(years.x)), years, years.x))



