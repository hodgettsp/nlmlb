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
nl_mlb_debut_counts <- nl_mlb_df %>%
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
        geom_point(colour = "dodgerblue", size = 1)+
        # set general theme to minimal
        theme_minimal()+
        # set theme elements -> set text family to Jost and size to 12
        theme(text = element_text(family = "jost-sans-serif", size = 12),
              # remove minor grid elements
              panel.grid.minor = element_blank(),
              # remove major grid elements
              panel.grid.major = element_blank(),
              # set major y grid elements to light sky blue 1 and dotted
              panel.grid.major.y = element_line(colour = "lightskyblue1",
                                                linetype = "dotted"),
              # expand x axis title margins and set font size to 14
              axis.title.x = element_text(margin = unit(c(5, 0, 5, 0), "mm"),
                                          size = 14),
              # expand y axis title margins and set font size to 14
              axis.title.y = element_text(margin = unit(c(0, 5, 0, 5), "mm"),
                                          size = 14),
              # increase plot margins
              plot.margin = unit(c(5, 5, 5, 5), "mm"))+
        # set y, x, and title labels
        labs(y = "Player Count",
             x = "Year",
             title = "Negro Leagues Player Debut in Major League Baseball: CNLBR")+
        # set x axis values from 1947 to 1980 by 2 count
        scale_x_continuous(breaks = seq(1947, 1980, 2))+
        # set y axis values from 1 to 11
        scale_y_continuous(breaks = 1:11)
