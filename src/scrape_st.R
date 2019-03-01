###--------------------------------------------------------------###
###--------------------------------------------------------------###
###--------------------------------------------------------------###
### Scrape tables from SPORTRAC
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


##---------------------------------------------------------##
##---------------------------------------------------------##
## Functions
##---------------------------------------------------------##
##---------------------------------------------------------##


##---------------------------------------------------------##
##' @name: to_numeric_st
##' @param: dat, data.frame
##' @param: var, string; the variable that corresponds with
##'          dollar amounts scraped from sportrac
##' @param: chr, string, to be subbed out in order to form
##'          valid numeric variables
##' @return: data frame with variable as numeric
##---------------------------------------------------------##
to_numeric_st <- function(x, chr = "\\$|\\%|,"){
  
  # re-code variable
  x = as.numeric(gsub(chr, "", as.character(x)))
  x[is.na(x)] = 0
  
  # return data
  return(x)
}

##---------------------------------------------------------##
##' @name: clean_player_col_st
##' @param: dat, data.frame
##' @return: data frame with start/end contract years
##---------------------------------------------------------##
clean_player_col_st <- function(dat){
  
  # split player and years
  foo = strsplit(gsub("\\)", "", dat$player), "\\(")
  
  # fix years
  years = do.call(rbind, 
                  lapply(foo, FUN=function(x){
                    if(length(x) == 2){
                      yrs = strsplit(x[[2]], "-")
                    } else {
                      yrs = c(NA, NA)
                    }
                    return(data.frame(start_yr = as.numeric(yrs[[1]][1]),
                                      end_yr = as.numeric(yrs[[1]][2])))
                  }))
  
  # combine years with data
  out = cbind(years, select(dat, -player))
  
  # return data
  return(out)
}



##---------------------------------------------------------##
##' @name: scrape_st
##' @param: position, position to get contract info on
##' @return: data frame of current contracts at the position
##---------------------------------------------------------##
scrape_st <- function(position){
  
  # hook to grab cols to fix
  should_convert <- function(x){
    grepl("\\$|\\%", x[[1]])
  }
  
  url = paste0("https://www.spotrac.com/nfl/contracts/", position)
  
  # Grab HTML source
  htmltab = read_html(url) %>%
    html_node("table")
    
  # parse player names
  player = htmltab %>% 
    html_nodes("td.player") %>%
    html_nodes("a") %>% # player names stored in 'a' tag
    html_text()
  
  # form table
  foo =  htmltab %>%
    html_table() %>%
    mutate_if(should_convert, to_numeric_st) %>% # convert strings to numerics 
    clean_names() %>% 
    filter(!(player %in% c("Total", "Average"))) %>% # filter out summary rows
    clean_player_col_st() %>% # get the start/end year of contract
    cbind(player, .) # add player names
  
  print("Missing data")
  print(apply(foo, 2, FUN=function(x) mean(is.na(x))))
  
  return(foo)
  
}

# position = "running-back"
# tmp = scrape_st(position = position)
# dim(tmp)

