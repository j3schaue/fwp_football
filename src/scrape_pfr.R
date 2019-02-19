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
post_process_season <- function(dat, category){
  
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
##' @return: data frame for a given year of data in the 
##'            specified category
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
      post_process_season(., category) %>%
      clean_names() 
    
    outdf$year = year
    
  } else {
    
    print("You must input a valid category:")
    print(pfr_cats)
    outdf <- NULL
    
  }
  
  return(outdf)
}


##---------------------------------------------------------##
##' @name: scrape_pfr_awards
##' @param: award, string; the name of the award
##' @return: data frame for award
##---------------------------------------------------------##
scrape_pfr_awards <- function(award){
  
  ###---- Sanitize input
  # award names for url
  awards = list(dpoy = "ap-defensive-player-of-the-year",
                apmvp = "ap-nfl-mvp-award",
                pfwamvp = "pfwa-nfl-mvp-award",
                bell = "bert-bell-award",
                opoy = "ap-offensive-player-of-the-year",
                dpoy = "ap-defensive-player-of-the-year",
                sbmvp = "super-bowl-mvp-award",
                oroy = "ap-offensive-rookie-of-the-year-award",
                droy = "ap-defensive-rookie-of-the-year-award",
                wpmoy = "walter-payton-man-of-the-year",
                apcpoy = "ap-comeback-player-award",
                pfwacpoy = "pfwa-comeback-player-award")
  
  # Check that the function was given a real award
  if(award %in% unlist(awards)){
    
    url = paste0("https://www.pro-football-reference.com/awards/",
                 award, 
                 ".htm") 
    
  } else if(award %in% names(awards)){
    
    url = paste0("https://www.pro-football-reference.com/awards/",
                 awards[[award]],
                 ".htm")
    
  } else {
    
    print("You must specify an award:")
    print(cat(paste(names(awards), sep = "\n")))
    return(NULL) # if no valid argument, end the function
    
  }
  
  # get the relevant table
  outdf = read_html(url) %>%
    html_node("table") %>%
    html_table()
  
  # return the table
  return(outdf)
}
