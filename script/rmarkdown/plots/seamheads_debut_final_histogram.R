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
seamheads_player_data %>%
        # generate plot
        ggplot()+
        # generate debut histogram
        geom_histogram(aes(x = debut, fill = "Debut"), binwidth = 1, bins = 30,
                    colour = "white")+
        # generate final game histogram
        geom_histogram(aes(x = final, fill = "Last Game"), binwidth = 1, bins = 30,
                       colour = "white", alpha = 0.75)+
        # set x and y labels
        labs(x = "Year",
             y = "Player Count")+
        # set theme elements -> set font family to jost
        theme(text = element_text(family = "jost-sans-serif"),
              # remove minor grid elements
              panel.grid.minor = element_blank(),
              # remove major grid elements
              panel.grid.major = element_blank(),
              # set major y grid elements to light sky blue 1 and dotted
              panel.grid.major.y = element_line(colour = "LightSkyBlue1",
                                                linetype = "dotted"),
              # set panel colour
              panel.background = element_rect(fill = "cornsilk1"),
              # set plot colour
              plot.background = element_rect(fill = "cornsilk1"),
              # expand x axis title margin and set font size to 12
              axis.title.x = element_text(margin = unit(c(5, 0, 5, 0), "mm"),
                                          size = 12),
              # expand y axis title margin and set font size to 12
              axis.title.y = element_text(margin = unit(c(0, 5, 0, 5), "mm"),
                                          size = 12),
              # set legend to top
              legend.position = "top",
              # set legend background colour
              legend.background = element_rect(fill = "cornsilk1"),
              # angle x axis elements at 90 degrees
              axis.text.x = element_text(angle = 90))+
        # set x axis values from 1887 to 1948 by 2 count
        scale_x_continuous(breaks = seq(1887, 1948, 2))+
        # set y axis values from 0 to 300 by 25 count
        scale_y_continuous(breaks = seq(0, 300, 25))+
        # set legend value names and value colours
        scale_fill_manual(name = "Legend", values = c("#dd3530", "dodgerblue"))
