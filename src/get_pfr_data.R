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
                    if(ctg == "draft" & year < 1994){ 
                      # in 1995 PFR started tracking additional stats
                      # amend previous dfs to have that column 
                      df$solo = NA
                      df = df %>% select(rnd, pick, tm, player, pos, 
                                         age, to, ap1, pb, st, 
                                         car_av, dr_av, g, pass_cmp, pass_att, 
                                         pass_yds, pass_td, pass_int, rush_att, rush_yds, 
                                         rush_td, rec_rec, rec_yds, rec_td, solo,
                                         int, sk, college_univ, year)
                    } else if(ctg == "passing" & year < 2006){
                      # in 2006 pfr started tracking QBR
                      # fix prior dfs to handle that
                      df$qbr = NA
                      df = df %>% select(rk, player, tm, age, pos,
                                         g, gs, q_brec, cmp, att,
                                         cmp_percent, yds, td, td_percent, int,
                                         int_percent, lng, y_a, ay_a, y_c,
                                         y_g, rate, qbr, sk, sk_yds,
                                         ny_a, any_a, sk_percent, x4qc, gwd,
                                         year)
                    } else if(ctg == "receiving" & year < 1992){
                      df$tgt = NA; df$ctch_percent = NA
                      df = df %>% select(rk, player, tm, age, pos, 
                                         g, gs, tgt, rec, ctch_percent, 
                                         yds, y_r, td, lng, r_g, 
                                         y_g, fmb, year)
                    } else if(ctg == "fantasy" & year < 1992){
                      df$rec_tgt = NA
                      df = df %>% select(rk, player, tm, fant_pos, age, g, gs,
                                         pass_cmp, pass_att, pass_yds, pass_td, pass_int, rush_att, rush_yds,
                                         rush_y_a, rush_td, rec_tgt, rec_rec, rec_yds, rec_y_r, rec_td,
                                         fmb_fmb, fmb_fl, td, x2pm, x2pp, fant_pt, ppr,
                                         dk_pt, fd_pt, vbd, pos_rank, ov_rank, year)
                    }
                    # print(paste(ctg, year, dim(df)))
                    return(df)
                  })
         )
         print(paste(ctg, "completed."))
         return(catdf)
       })

# Make sure you name all the data frames
names(pfr_data) = categories

# Save the whole list of data frames to file
saveRDS(pfr_data, paste0("./data/PFR/pfr_all", min(years), "_", max(years), ".RDS"))

# Then save data sets for each individual category
for(dd in names(pfr_data)){
  print(summary(pfr_data[[dd]]$year))
  saveRDS(pfr_data[[dd]], paste0("./data/PFR/pfr_", dd, "_", min(years), "_", max(years), ".RDS"))
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
