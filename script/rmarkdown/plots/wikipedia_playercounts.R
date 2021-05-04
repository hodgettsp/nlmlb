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

# pipe wiki counts data
wiki_player_counts <- wiki_counts %>%
        # set x axis as season and y as player count
        ggplot(aes(x = season, y = player_count))+
        # generate area geom and fill with dodger blue at .75 opacity
        geom_area(fill = "dodgerblue", alpha = .75)+
        # set labels
        labs(x = "Year",
             y = "Player Count",
             title = "Negro Leagues Player Count Distributions, 1873 - 1976: Wikipedia")+
        # set theme elements ->
        # set plot font to Jost and size to 12
        theme(text = element_text(family = "jost-sans-serif", size = 12),
              # remove minor plot grid lines
              panel.grid.minor = element_blank(),
              # remove major x axis grid lines
              panel.grid.major.x = element_blank(),
              # set y axis major grid lines to cornsilk4 colours ->
              # and linetype to dotted
              panel.grid.major.y = element_line(colour = "LightSkyBlue1",
                                                linetype = "dotted"),
              # set panel background to cornsilk1
              panel.background = element_rect(fill = "cornsilk1"),
              # set plot background to cornsilk1
              plot.background = element_rect(fill = "cornsilk1"),
              # increase margin around x axis title by 5mm ->
              # and set title size to 14
              axis.title.x = element_text(margin = unit(c(5, 0, 5, 0), "mm"),
                                          size = 14),
              # increase margin around y axis title by 5mm ->
              # and set title size to 14
              axis.title.y = element_text(margin = unit(c(0, 5, 0, 5), "mm"),
                                          size = 14),
              axis.text.x = element_text(angle = 90),
              # increase plot margins
              plot.margin = unit(c(5, 5, 5, 5), "mm"))+
        # set x scale values
        scale_x_continuous(breaks = seq(1873, 1976, 3))+
        # set y scale values
        scale_y_continuous(breaks = seq(0, 400, 25))
