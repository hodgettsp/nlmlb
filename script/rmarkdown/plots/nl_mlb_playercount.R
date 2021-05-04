# load tidyverse package
# version 1.3.0
library(tidyverse)

# load here package
# version 1.0.1
library(here)

# load showtext package
# version 0.9-2
library(showtext)

source(here("script/rmarkdown/nl_mlb_count.R"))

# access Jost font from Google fonts
font_add_google(name = "jost", family = "jost-sans-serif")

# load font
showtext_auto()

# pipe nl_mlb_counts
nl_mlb_playercounts <- # pipe nl_mlb_counts
        nl_mlb_counts %>%
        # filter for between the 1947 and 1981 seasons ->
        # 1947 being the first season with NL players and 1980 being ->
        # the next after the last season to have a count
        # add dummy row for 1981 season with no NL players
        add_row(retroID = NA, lastname = NA, firstname = NA,
                Bat = NA, Throw = NA, season = 1981, player_count = 0,
                nlp_count = 0, nlp_per_pop = 0, nlp_per_team = 0) %>%
        # generate plot with season as x axis and player count as y
        ggplot(aes(x = season, y = nlp_count)) +
        # generate line plot
        geom_line(colour = "lightskyblue1", size = .5)+
        # generate point plot
        geom_point(colour = "dodgerblue", size = 1)+
        # set theme elements -> set font to jost and size 8
        theme(text = element_text(family = "jost-sans-serif", size = 8),
              # remove minor grid elements
              panel.grid.minor = element_blank(),
              # remove major grid elements
              panel.grid.major = element_blank(),
              # set major y grid elements to light sky blue 1 and dotted
              panel.grid.major.y = element_line(colour = "LightSkyBlue1",
                                                linetype = "dotted"),
              # set panel background colour
              panel.background = element_rect(fill = "cornsilk1"),
              # set plot background colour
              plot.background = element_rect(fill = "cornsilk1"),
              # set x axis title margins and fonts size to 10
              axis.title.x = element_text(margin = unit(c(5, 0, 5, 0), "mm"),
                                          size = 10),
              # set y axis title margins and font size to 10
              axis.title.y = element_text(margin = unit(c(0, 5, 0, 5), "mm"),
                                          size = 10),
              # increase plot margins
              plot.margin = unit(c(5, 5, 5, 5), "mm"))+
        # set y and x axis labels
        labs(y = "Player Count",
             x = "Year")+
        # set x axis valus from 1947 to 1981 by 2 count
        scale_x_continuous(breaks = seq(1947, 1987, 2))+
        # set y axis value from 0 to 40 by 2 count
        scale_y_continuous(breaks = seq(0, 40, 2))
