# uncomment any packages that require installation

# loads tidyverse for working with data
# version 1.3.1
#install.packages("tidyverse")

# loads here for accessing directories
# version 1.0.1
#install.packages("here")

# load tidyverse package
# version 1.3.1
library(tidyverse)

# load here package
# version 1.0.1
library(here)

# create list of files ending in .csv in given directory ->
# set full names to false as this will make naming objects easier
files <- list.files(here("inputs/data/csv"), pattern = "*.csv", full.names = F)

# use for loop to load all data files
# for index location in the length of the file list
for(i in 1:length(files)){
     # assign that file name to a new object stripping the .csv ->
     assign(str_replace(files[i], ".csv", ""),
            # apply read_csv function to file ->
            # pasting directory in front of file name ->
            read_csv(here(paste0("inputs/data/csv/", files[i]))),
            # assign to global environment
            envir = .GlobalEnv)
}
