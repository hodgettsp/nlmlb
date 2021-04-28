# uncomment any packages that require install

# install rvest package for web scraping
# version 1.0.0
#install.packages("rvest")

# load rvest package
library(rvest)


# read html code from provided url for NL players in seasons 1886 to 1936
nl_data_8636 <- read_html("https://www.seamheads.com/NegroLgs/history.php?tab=players&first=1886&last=1936&lgID=All&lgType=N&NeL=Y&HOF=All&pos=All&bats=All&throws=All&results=2500&sort=Player_a")

# write raw html code to data file
xml2::write_html(nl_data_8636, "inputs/data/seamheads/raw_data_8636.html")

# read html code from provided url for NL players in seasons 1937 to 1948
nl_data_3748 <- read_html("https://www.seamheads.com/NegroLgs/history.php?tab=players&first=1937&last=1948&lgType=All&NeL=Y&HOF=All&pos=All&bats=All&throws=All&results=2500&sort=Player_a")

# write raw html code to data file
xml2::write_html(nl_data_3748, "inputs/data/seamheads/raw_data_3748.html")
