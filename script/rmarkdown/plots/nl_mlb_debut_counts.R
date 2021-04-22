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

nl_mlb_df %>%
        group_by(played_negro_league) %>%
        add_count(mlb_debut) %>%
        rename(debut_count = n) %>%
        filter(played_negro_league == 1) %>%
        ggplot(aes(x = year, y = debut_count))+
        geom_point(colour = "dodgerblue", size = 2)+
        geom_line(colour = "dodgerblue", size = .5)+
        theme(text = element_text(family = "jost-sans-serif"),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.major.y = element_line(colour = "cornsilk4",
                                                linetype = "dotted"),
              panel.background = element_rect(fill = "cornsilk1"),
              plot.background = element_rect(fill = "cornsilk1"),
              axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"),
                                          size = 12),
              axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"),
                                          size = 12))+
        labs(y = "Player Count",
             x = "Year")+
        scale_x_continuous(breaks = c(1947, 1950, 1953, 1956, 1959,
                                      1962, 1965, 1968, 1971, 1974,
                                      1977))+
        scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
