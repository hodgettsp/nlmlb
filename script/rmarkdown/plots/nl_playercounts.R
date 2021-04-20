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

nl_counts %>%
     slice(-84) %>%
     ggplot(aes(x = season, y = player_count))+
     geom_area(fill = "dodgerblue", alpha = .5)+
     geom_line(colour = "dodgerblue", size = 1)+
     theme_minimal()+
     labs(x = "Year",
          y = "Player Count")+
     theme(text = element_text(family = "jost-sans-serif"),
           panel.grid.minor = element_blank(),
           panel.grid.major.x = element_blank(),
           panel.grid.major.y = element_line(colour = "#e0edf5"))+
     geom_vline(xintercept = c(1920, 1947, 1948, 1962),
                colour = "#010210", linetype = "solid", size = 0.5)+
     annotate("text", x = 1933, y = 280,
              label = "1947: Jackie Robinson debuts\n for the Brooklyn Dodgers",
              colour = "#010210",
              size = 3,
              hjust = 0,
              family = "jost-sans-serif")+
     annotate("text", x = 1905.5, y = 290,
              label = "1920: Negro National League\n established",
              colour = "#010210",
              size = 3,
              hjust = 0,
              family = "jost-sans-serif")+
     annotate("text", x = 1962.25, y = 280,
              label = "1962: Negro American\n League folds",
              colour = "#010210",
              size = 3,
              hjust = 0,
              family = "jost-sans-serif")+
     annotate("text", x = 1948.5, y = 290,
              label = "1948: Negro National\n League folds",
              colour = "#010210",
              size = 3,
              hjust = 0,
              family = "jost-sans-serif")
