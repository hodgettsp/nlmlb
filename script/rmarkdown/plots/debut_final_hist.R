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
z %>%
        ggplot()+
        geom_histogram(aes(x = debut, fill = "Debut"), binwidth = 1, bins = 30,
                    colour = "white")+
        geom_histogram(aes(x = final, fill = "Last Game"), binwidth = 1, bins = 30,
                       colour = "white", alpha = 0.75)+
        labs(x = "Year",
             y = "Player Count")+
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
                                          size = 12),
              legend.position = "top",
              legend.background = element_rect(fill = "cornsilk1"),
              axis.text.x = element_text(angle = 90))+
        scale_x_continuous(breaks = seq(1887, 1948, 1))+
        scale_y_continuous(breaks = seq(0, 300, 25))+
        scale_fill_manual(name = "Legend", values = c("#dd3530", "dodgerblue"))
