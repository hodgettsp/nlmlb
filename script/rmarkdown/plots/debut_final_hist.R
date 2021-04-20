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

nl_df %>%
     ggplot()+
     geom_vline(xintercept = c(1920, 1947, 1948, 1962),
                colour = "#010210", linetype = "solid", size = 0.25)+
     geom_histogram(aes(x = debut, fill = "Debut"), binwidth = 1, bins = 30,
                    colour = "white")+
     geom_histogram(aes(x = last_game, fill = "Last Game"), binwidth = 1, bins = 30,
                    colour = "white", alpha = 0.75)+
     theme_minimal()+
     labs(x = "Year",
          y = "Player Count")+
     theme(text = element_text(family = "jost-sans-serif"),
           panel.grid.minor = element_blank(),
           panel.grid.major.x = element_blank(),
           panel.grid.major.y = element_line(colour = "#e0edf5"),
           legend.position = "top")+
     scale_x_continuous(breaks = c(1874, 1884, 1894, 1904,
                                   1914, 1924, 1934, 1944,
                                   1954, 1964, 1975))+
     scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60,
                                   70, 80, 90, 100, 110, 120,
                                   130, 140))+
     scale_fill_manual(name = "Legend", values = c("#dd3530", "dodgerblue"))+
     annotate("text", x = 1933, y = 113,
              label = "1947: Jackie Robinson debuts\n for the Brooklyn Dodgers",
              family = "jost-sans-serif",
              colour = "#010210",
              size = 3,
              hjust = 0)+
     annotate("text", x = 1905.5, y = 113,
              label = "1920: Negro National League\n established",
              family = "jost-sans-serif",
              colour = "#010210",
              size = 3,
              hjust = 0)+
     annotate("text", x = 1962.25, y = 113,
              label = "1962: Negro American\n League folds",
              family = "jost-sans-serif",
              colour = "#010210",
              size = 3,
              hjust = 0)+
     annotate("text", x = 1948.5, y = 106,
              family = "jost-sans-serif",
              label = "1948: Negro National\n League folds",
              colour = "#010210",
              size = 3,
              hjust = 0)
