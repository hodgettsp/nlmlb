install.packages("rvest")
library(rvest)
library(tidyverse)


nl_data_8620 <- read_html("https://www.seamheads.com/NegroLgs/history.php?tab=players&first=1886&last=1920&lgID=All&lgType=All&NeL=Y&HOF=All&pos=All&bats=All&throws=All&results=2500&sort=Player_a")
write_html(nl_data_8620, "inputs/seamheads/raw_data_8620.html")

nl_data_8620 <- read_html("inputs/seamheads/raw_data_8620.html")

nl_data_8620

text_data <- nl_data_8620 %>%
     html_nodes("td") %>%
     html_text()

table_data <- tibble(player_data = text_data)

head(table_data)

player_data_8620 <- slice(table_data, 13:13662)

head(player_data_8620,  13)

player_data_8620$player_data <- gsub("^\\s+", "", player_data_8620$player_data)
player_data_8620$player_data <- gsub("\\s+$", "", player_data_8620$player_data)

head(player_data_8620, 13)

player_data_8620 <- player_data_8620 %>%
     mutate(key = rep(c("id", "player", "years", "pos", "pa", "ip", "drop", "ht", "wt", "b", "t", "birth", "death"), n()/13),
            no = cumsum(key == "id")) %>%
     pivot_wider(names_from = key, values_from = player_data) %>%
     select(-no, -drop)
