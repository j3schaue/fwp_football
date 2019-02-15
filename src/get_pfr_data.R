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

names(pfr_data) = categories

saveRDS(pfr_data, paste0("./data/pfr_all", min(years), "_", max(years), ".RDS"))

for(dd in names(pfr_data)){
  saveRDS(pfr_data[[dd]], paste0("./data/pfr_", dd, "_", min(years), "_", max(years), ".RDS"))
}
