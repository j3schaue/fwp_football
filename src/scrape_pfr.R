###--------------------------------------------------------------###
###--------------------------------------------------------------###
###--------------------------------------------------------------###
### Scrape tables from PFR
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
##' @name: post_process
##' @param: dat, data.frame
##' @param: category, string; the category of stats you want
##' @return: data frame that is properly formatted
##' @note: some PFR tables have different formatting issues
##'         and this function clears those up
##---------------------------------------------------------##
post_process <- function(dat, category){
  
  # Each conditional fixes headers 
  if(category == "passing"){
    col_names = names(dat)
    col_names[25] = "SkYds"
    names(dat) = col_names
  } else if(category == "rushing"){
    col_names = dat[1,]
    dat = dat[-1,] 
    names(dat) = col_names
  } else if(category == "defense"){
    col_names = paste0(names(dat), dat[1,]) %>%
      gsub("Games", "", .) %>%
      gsub("Def Interceptions", "Int", .) %>%
      gsub("Fumbles", "Fmb", .) %>%
      gsub("FmbFmb", "Fmb", .) %>%
      gsub("IntInt", "Int", .) %>%
      gsub("Tackles", "Tkl", .)
    dat = dat[-1,]
    names(dat) = col_names
  } else if(category == "kicking"){
    col_names = paste0(names(dat), dat[1,]) %>%
      gsub("Games", "", .)
    dat = dat[-1,]
    names(dat) = col_names
  } else if(category == "returns"){
    col_names = paste0(gsub(" Returns", "", names(dat)), dat[1,])
    dat = dat[-1,]
    names(dat) = col_names
  } else if(category == "fantasy"){
    col_names = names(dat) %>%
      gsub("Scoring", "", .) %>%
      gsub("Fumbles", "Fmb", .) %>%
      gsub("(eiv)*ing", "", .) %>%
      gsub("Fantasy", "", .) %>%
      gsub("Games", "", .) %>%
      paste0(., dat[1,])
    dat = dat[-1,]
    names(dat) = col_names
  } else if(category == "draft"){
    names(dat) = names(dat) %>%
      ifelse(grepl("ing", .), ., "") %>%
      gsub("(eiv)*ing", "", .) %>%
      paste0(., dat[1,])
    dat = dat[-1,-ncol(dat)] %>% 
      filter(Rnd != "Rnd")
  } else if(category == "games"){
    dat = dat[, -8] 
    names(dat)[6] = "at"
    dat %>% filter(Week != "Week")
  }
  
  # Filter out unwanted rows
  if("Rk" %in% names(dat)){
    dat = dat %>% dplyr::filter(Rk != "Rk")
  }
  
  # check if columns should be numeric
  should_be_numeric <- function(x){
    return(!anyNA(as.numeric(x[!is.na(x)])))
  }
  
  dat = dat %>%
    replace_with_na_all(condition = ~.x == "") %>% # fix the empty strings
    mutate_if(should_be_numeric, as.numeric) # convert relevant columns to numeric
  
  return(dat)
}


##---------------------------------------------------------##
##' @name: scrape_pfr
##' @param: year, integer; the year in which you want stats
##' @param: category, string; the category of stats you want
##' @return: data frame for a given year of passing data
##---------------------------------------------------------##
scrape_pfr <- function(year, category){
  
  pfr_cats <- c("games", "draft", 
                "passing", "rushing", "receiving", "defense",
                "kicking", "returns", "scoring", "fantasy"
  )
  
  if(category %in% pfr_cats){
    
    url = paste0("https://www.pro-football-reference.com/years/", year, "/", category, ".htm")
    
    outdf <- read_html(url) %>%
      html_node("table") %>%
      html_table() %>%
      post_process(., category) %>%
      clean_names() 
    
    outdf$year = year
    
  } else {
    
    print("You must input a valid category:")
    print(pfr_cats)
    outdf <- NULL
    
  }
  
  return(outdf)
}


