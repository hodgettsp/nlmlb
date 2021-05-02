# uncomment any packages that require installation

# install here package for accessing directories
# version 1.0.1
#install.packages("here")

# install tidyverse package for data stuff
# version 1.3.1
#install.packages("tidyverse")

# load here package
# version 1.0.1
library(here)

# load tidyverse package
# version 1.3.1
library(tidyverse)

# load retrosheet data
source(here("script/scrape/retrosheet_scrape.R"))

x <- ls()

z <- mget(x)


for(i in 1:length(x)){
     assign(paste0("roster_47", roster_1947[[i]][1, 6]),
            as.data.frame(roster_1947[i]) %>%
                 rename(id = 1, last = 2, first = 3,
                        bat = 4, throw = 5, team = 6,
                        pos = 7),
            envir = .GlobalEnv)
}

y <- bind_rows(z[[34]])

for(i in 1:length(z)){
     assign("a", bind_rows(z[[i,]]), envir = .GlobalEnv)
}




