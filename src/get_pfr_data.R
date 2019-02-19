###--------------------------------------------------------------###
###--------------------------------------------------------------###
###--------------------------------------------------------------###
### Get PFR Data:
### This script uses codes from the scrape_pfr library
### and gets a whole bunch of data from PFR. It stores
### this data in a set directory as RDS files.
###--------------------------------------------------------------###
###--------------------------------------------------------------###
###--------------------------------------------------------------###

# get the scraper
source("./src/scrape_pfr.R")

# set the date range
years = 1985:2018

# set the categories we want
categories = c("games", "draft", 
               "passing", "rushing", "receiving", "defense",
               "kicking", "returns", "scoring", "fantasy"
               )

# get all of the data in one go
# outputs a list of data frames for each category
pfr_data = lapply(categories, # Loop through categories 
       FUN = function(ctg){
         catdf = do.call(rbind, # combine each year of data for a category
           lapply(years, # Loop through years
                  FUN = function(year){
                    df = scrape_pfr(year = year, category = ctg)
                    print(paste(ctg, year))
                    return(df)
                  })
         )
         return(catdf)
       })

# Make sure you name all the data frames
names(pfr_data) = categories

# Save the whole list of data frames to file
saveRDS(pfr_data, paste0("./data/pfr_all", min(years), "_", max(years), ".RDS"))

# Then save data sets for each individual category
for(dd in names(pfr_data)){
  saveRDS(pfr_data[[dd]], paste0("./data/pfr_", dd, "_", min(years), "_", max(years), ".RDS"))
}


##---------------------------------------##
## Scrape awards data
##---------------------------------------##
awards = list(apmvp = "ap-nfl-mvp-award",
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

# For each award, get the pfr data on the winner
pfr_awards = lapply(awards, scrape_pfr_awards)

# Name the awards
names(pfr_awards) = names(awards)

pfr_awards$dpoy %>% head()

# write all awards to file
saveRDS(pfr_awards, "./data/pfr_awards_1972-2018.RDS")

# write individual files for each award
for(i in seq(pfr_awards)){
  saveRDS(pfr_awards[[i]], 
          paste0("./data/pfr_awards_", 
                 names(pfr_awards)[i], 
                 "_1972-2018.RDS")
  )
}
