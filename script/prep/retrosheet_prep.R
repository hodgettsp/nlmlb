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

# create vector list of data objects
x <- ls()

# this for loop writes over the list of data objects
# for the length of the list of x
for(i in 1:length(x)){
     # assign under the same name ->
     # bind the rows of the object at indexed location in list x
     assign(x[i], bind_rows(get(x[i])) %>%
                 # create new column season that is the last two digits ->
                 # of the indexed string in x
                 mutate(season = substr(x[i], 10, 11)),
            # assign to global environment
            envir = .GlobalEnv)
}

# create temporary list of all objects
y <- mget(x[1:41])

# bind all objects in y together
roster_df <- bind_rows(y)

# remove all the objects in list x
rm(list = x)

# remove objects i y and x
rm(i, y, x)

# write roster_df as csv data file with name roster4780_data
write_csv(roster_df, here("inputs/data/csv/roster4787_data.csv"))
