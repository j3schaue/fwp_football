###--------------------------------------------------------------###
###--------------------------------------------------------------###
###--------------------------------------------------------------###
### Get SPORTRAC Data:
### This script uses codes from the scrape_pfr library
### and gets a whole bunch of data from ST It stores
### this data in a set directory as RDS files.
###--------------------------------------------------------------###
###--------------------------------------------------------------###
###--------------------------------------------------------------###

outdir = "./data/sportrac/"
source("./src/scrape_st.R")

for(pos in c("offense", "defense", "special")){
  df = scrape_st(pos)
  saveRDS(df, file = paste0(outdir, pos, ".Rds"))
}


