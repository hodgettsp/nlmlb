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

# pipe nl_mlb_df
nl_mlb_df %>%
        # group by played in negro Leagues
        group_by(played_negro_league) %>%
        # add a count of mlb_debut
        add_count(mlb_debut) %>%
        # rename count column
        rename(debut_count = n) %>%
        # filter for only those who played in the Negro Leagues
        filter(played_negro_league == 1) %>%
        # generate plot with year as x and debut count as y
        ggplot(aes(x = year, y = debut_count))+
        # generate line plot with colour light sky blue 1
        geom_line(colour = "lightskyblue1", size = .5)+
        # generate point plot with dodger blue colour
        geom_point(colour = "dodgerblue", size = 2)+
        # set theme elements -> set text family to Jost
        theme(text = element_text(family = "jost-sans-serif"),
              # remove minor grid elements
              panel.grid.minor = element_blank(),
              # remove major grid elements
              panel.grid.major = element_blank(),
              # set major y grid elements to light sky blue 1 and dotted
              panel.grid.major.y = element_line(colour = "lightskyblue1",
                                                linetype = "dotted"),
              # set panel background to corn silk 1
              panel.background = element_rect(fill = "cornsilk1"),
              # set plot background to corn silk 1
              plot.background = element_rect(fill = "cornsilk1"),
              # expand x axis title margins and set font size to 12
              axis.title.x = element_text(margin = unit(c(5, 0, 5, 0), "mm"),
                                          size = 12),
              # expand y axis title margins and set font size to 12
              axis.title.y = element_text(margin = unit(c(0, 5, 0, 5), "mm"),
                                          size = 12))+
        # set y and x labels
        labs(y = "Player Count",
             x = "Year")+
        # set x axis values from 1947 to 1980 by 2 count
        scale_x_continuous(breaks = seq(1947, 1980, 2))+
        # set y axis values from 1 to 11
        scale_y_continuous(breaks = 1:11)
