# load tidyverse package
# version 1.3.0
library(tidyverse)

# load here package
# version 1.0.1
library(here)

# load showtext package
# version 0.9-2
library(showtext)

source(here("script/prep/seamheads_prep.R"))

# access Jost font from Google fonts
font_add_google(name = "jost", family = "jost-sans-serif")

# load font
showtext_auto()

# pipe seamheads player data
histogram_seams <- seamheads_player_data %>%
        # generate plot
        ggplot()+
        # generate debut histogram
        geom_histogram(aes(x = debut, fill = "Debut"), binwidth = 1, bins = 30,
                    colour = "white")+
        # generate final game histogram
        geom_histogram(aes(x = final, fill = "Last Game"), binwidth = 1, bins = 30,
                       colour = "white", alpha = 0.75)+
        # set x, y, and title labels
        labs(x = "Year",
             y = "Player Count",
             title = "Negro Leagues Player Debut and Final Game Distributions, 1887 - 1948: Seamheads")+
        # set general theme to minimal
        theme_minimal()+
        # set theme elements -> set font family to jost and size to 12
        theme(text = element_text(family = "jost-sans-serif", size = 12),
              # remove minor grid elements
              panel.grid.minor = element_blank(),
              # remove major grid elements
              panel.grid.major = element_blank(),
              # set major y grid elements to light sky blue 1 and dotted
              panel.grid.major.y = element_line(colour = "LightSkyBlue1",
                                                linetype = "dotted"),
              # expand x axis title margin and set font size to 14
              axis.title.x = element_text(margin = unit(c(5, 0, 5, 0), "mm"),
                                          size = 14),
              # expand y axis title margin and set font size to 14
              axis.title.y = element_text(margin = unit(c(0, 5, 0, 5), "mm"),
                                          size = 14),
              # set legend to top
              legend.position = "top",
              # angle x axis elements at 90 degrees
              axis.text.x = element_text(angle = 90),
              # increase plot margins
              plot.margin = unit(c(5, 5, 5, 5), "mm"))+
        # set x axis values from 1887 to 1948 by 2 count
        scale_x_continuous(breaks = seq(1887, 1948, 2))+
        # set y axis values from 0 to 300 by 25 count
        scale_y_continuous(breaks = seq(0, 300, 25))+
        # set legend value names and value colours
        scale_fill_manual(name = "Legend", values = c("#dd3530", "dodgerblue"))
