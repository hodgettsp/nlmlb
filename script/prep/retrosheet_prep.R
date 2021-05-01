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

# generate list of objects
x <- ls()

y <- 47:80

z <- roster_1947[[1]]

for(i in 1:length(x)){
     assign(paste0("roster_", y[i], "_", x[[i]][1, 6]),
            as.data.frame(roster_1947[i]),
            envir = .GlobalEnv)
}
