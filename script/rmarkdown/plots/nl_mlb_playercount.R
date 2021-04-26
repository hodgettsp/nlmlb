# load tidyverse package
# version 1.3.0
library(tidyverse)

# load here package
# version 1.0.1
library(here)

# load showtext package
# version 0.9-2
library(showtext)

source(here("script/rmarkdown/load_data.R"))

source(here("script/rmarkdown/prep_data.R"))

source(here("script/rmarkdown/nl_mlb_count.R"))

# access Jost font from Google fonts
font_add_google(name = "jost", family = "jost-sans-serif")

# load font
showtext_auto()

# generate plot
nl_mlb_counts %>%
     filter(season >= 1947 & season <= 1981) %>%
        ggplot(aes(x = season, y = player_count)) +
        geom_line(colour = "dodgerblue", size = .75)+
        theme(text = element_text(family = "jost-sans-serif"),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.major.y = element_line(colour = "LightSkyBlue1",
                                                linetype = "dotted"),
              panel.background = element_rect(fill = "cornsilk1"),
              plot.background = element_rect(fill = "cornsilk1"),
              axis.title.x = element_text(margin = unit(c(5, 0, 5, 0), "mm"),
                                          size = 12),
              axis.title.y = element_text(margin = unit(c(0, 5, 0, 5), "mm"),
                                          size = 12))+
        labs(y = "Player Count",
             x = "Year")+
        scale_x_continuous(breaks = c(1947, 1950, 1953, 1956, 1959, 1962,
                                      1965, 1968, 1971, 1974, 1977, 1980))+
        scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40))
