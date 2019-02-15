###--------------------------------------------------------------###
###--------------------------------------------------------------###
###--------------------------------------------------------------###
### Intro to basic web scraping with rvest in R
###--------------------------------------------------------------###
###--------------------------------------------------------------###
###--------------------------------------------------------------###

# load the rvest library for scraping
# install.packages('rvest')
library(rvest)

# load tidyverse for data wrangling
library(tidyverse)
library(janitor)
library(naniar)


##----------------------------------##
## Pro Football Reference Example
##----------------------------------##

##---Here's how we scrape the table for 2018 QBs from PFR:

# start with the url
url <- "https://www.pro-football-reference.com/years/2018/passing.htm"


# read in the page source
page <- read_html(url)

# identify the node in the page source you want
node <- html_node(x = page, "table")

# parse the code and convert it to a data frame
tab <- html_table(node)
head(tab)

# clean up the names of the table
tab <- clean_names(tab)
head(tab)

# filter out the empty rows
qbs18 <- tab %>% filter(rk != "Rk")

# examine the structure of the data
str(qbs18) 

# based on this, there's a few problems:
#   1. missing values are stored as empty strings; fix this
#   2. it looks like everything is stored as a character (rather than as numbers)
# Here's how we can fix this

# Short function to check if columns SHOULD be numeric
should_be_numeric <- function(x){
  return(!anyNA(as.numeric(x[!is.na(x)])))
}

qbs_18 = qbs18 %>%
  replace_with_na_all(condition = ~.x == "") %>% # fix the empty strings
  mutate_if(should_be_numeric, as.numeric) # convert relevant columns to numeric


##----Here's the same process, but quicker:
qbs18 <- read_html(url) %>%
  html_node("table") %>%
  html_table() %>%
  clean_names() %>%
  filter(rk != "Rk") %>%
  replace_with_na_all(condition = ~.x == "") %>% # fix the empty strings
  mutate_if(should_be_numeric, as.numeric)

head(qbs18)
tail(qbs18)


##---Let's write a function to do this multiple times------##
##' @name: scrape_pfr_qbs
##' @param: url, a string with the pff url
##' @return: data frame for a given year of passing data
##---------------------------------------------------------##
scrape_pfr_qbs <- function(url){
  
  outdf <- read_html(url) %>%
    html_node("table") %>%
    html_table() %>%
    clean_names() %>%
    filter(rk != "Rk")
  
  return(outdf)
}


url1 = "https://www.pro-football-reference.com/years/2018/passing.htm"
url2 = "https://www.pro-football-reference.com/years/2017/passing.htm"
urls = c(url1, url2)
qbs = list(NA, NA)

for(i in 1:2){
  qbs[[i]] = scrape_pfr_qbs(urls[i])
}
qbs[[1]]
qbs[[2]]


##---Let's improve our function ----------------------------##
##' @name: scrape_pfr_qbs
##' @param: url, a string with the pff url
##' @return: data frame for a given year of passing data
##---------------------------------------------------------##
scrape_pfr_passing <- function(year){
  
  url = paste0("https://www.pro-football-reference.com/years/", year, "/passing.htm")
  
  outdf <- read_html(url) %>%
    html_node("table") %>%
    html_table() %>%
    clean_names() %>%
    filter(rk != "Rk")
  
  return(outdf)
}

years <- 2005:2018
pass_stats_pfr <- lapply(years, scrape_pfr_passing) # lapply acts like a FOR LOOP
saveRDS(pass_stats_pfr, "pass_stats_pfr_2005-2018.RDS")




##---Let's improve our function again! ---------------------##
##' @name: scrape_pfr
##' @param: year, integer; the year in which you want stats
##' @param: category, string; the category of stats you want
##' @return: data frame for a given year of passing data
##---------------------------------------------------------##
scrape_pfr <- function(year, category){
  
  pfr_cats <- c("opp", "games", "draft", 
                "passing", "rushing", "receiving", "defense",
                "kicking", "returns", "scoring", "fantasy"
                )
  
  if(category %in% pfr_cats){
  
    url = paste0("https://www.pro-football-reference.com/years/", year, "/", category, ".htm")
    
    outdf <- read_html(url) %>%
      html_node("table") %>%
      html_table() %>%
      clean_names() %>%
      filter(rk != "Rk")
    
    outdf$year = year
    
  } else {
    
    print("You must input a valid category:")
    print(pfr_cats)
    outdf <- NULL
    
  }
  
  return(outdf)
}

# It turns out that this scraper won't 
# actually work as is, since tables have 
# different displays for different categories.
# 
# This is fixed in the scraper_PFR.R file.
