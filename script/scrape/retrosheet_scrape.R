# uncomment any packages that require installation

# install here package for accessing directories
# version 1.0.1
#install.packages("here")

# install tidyverse package for data stuff
# version 1.3.1
#install.packages("tidyverse")

# install retrosheets package for MLB roster data
# version 1.1.3
#install.packages("retrosheet")

# load here package
# version 1.0.1
library(here)

# load tidyverse package
# version 1.3.1
library(tidyverse)

# load retrosheet package
# version 1.1.3
library(retrosheet)

#The information used here was obtained free of charge from
#and is copyrighted by Retrosheet. Interested parties may
#contact Retrosheet at "www.retrosheet.org"

# for the length of 1947 to 1980
for(i in 1947:1980){
     # assign roster object with suffix of index number ->
     # get retrosheet roster list at index and assign to global environment
     assign(paste0("roster_", i), get_retrosheet("roster", i), envir = .GlobalEnv)
}

# remove index
rm(i)
