###------------------------------------------------------------------###
### Code for getting fantasy projections vs. actual points
### Uses the ffanalytics package
###------------------------------------------------------------------###

# If you don't have it, you need to install ffanalytics
# install.packages("devtools")
# devtools::install_github(repo = "FantasyFootballAnalytics/ffanalytics", 
                         # build_vignettes = TRUE)


# load the library
library(ffanalytics)
library(tidyverse)
library(skimr)

##---------------------------------------##
## Quick demo: get data on proj/pts for
## week 1 from NFL.com
##---------------------------------------##

my_scrape <- ffanalytics::scrape_data(src = c("NFL"), 
                         pos = c("RB"),
                         season = 2018, week = 16)

my_scrape[[1]] %>% select(ast)

# Looks like weekpts = actual points earned
#            site_pts = projection by the site
my_scrape$RB %>%
  select(gsisplayerid, player, pos, team, weekpts, site_pts)


# NOTE: looks like scraping from ESPN or CBS does not work:
test <- ffanalytics::scrape_data(src = c("ESPN", "CBS"),
                                 pos = c("QB", "RB"),
                                 season = 2018, week = 1)

head(test) # empty tables

# NOTE: looks like scrapting only works from NFL.com for 2018
my_scrape <- ffanalytics::scrape_data(src = c("NFL"), 
                                      pos = c("QB", "RB"),
                                      season = 2017, week = 1)

## This means we may need to focus on NFL.com for projections


##---------------------------------------##
## Loop through positions of interest and
## weeks for 2018
##---------------------------------------##

##---Helper function to read NFL data
scrape_NFL <- function(position){

  # Get the scraped data
  posdfs = lapply(1:17, 
                  FUN = function(wk) {
                     outdf = scrape_data(src = c("NFL"), 
                                pos = position, 
                                week = wk, 
                                season = 2018)[[1]] %>%
                       as_tibble() %>%
                       mutate(week = wk, 
                              season = 2018)
                      
                     return(outdf)
                    })
  
  # Arrange in tibble and filter out empty dfs
  dfs_ret = tibble(dfs = posdfs, 
                   n_cols = sapply(posdfs, ncol)) %>% 
             filter(n_cols > 2)
  
  # Figure out which columns to keep
  cols_to_keep = dfs_ret %>%
    mutate(nms = map(dfs, names)) %>%
    select(nms)
  
  ctk = cols_to_keep$nms[[1]]
  iter_to = length(cols_to_keep$nms)
  if(iter_to > 1){
    for(i in 2:iter_to){
      ctk = intersect(ctk, cols_to_keep$nms[[i]])
    }
  }
  
  dfs_ret = dfs_ret %>%
    mutate(outdfs = map(.x = dfs, 
                        .f = function(x) x %>% select(ctk)))
  
  out = do.call(rbind, dfs_ret$outdfs)
  
  return(out)
}


scrape_NFL("RB")

nfl_fan_proj = tibble(position = c("RB", "QB", "WR", "K", "TE", "DST")) %>%
                mutate(nfl_data = map(position, scrape_NFL))

saveRDS(nfl_fan_proj, "./data/fantasy/fantasy_projections_2018_nfl_allpos.RDS")

for(pos in nfl_fan_proj$position){
  saveRDS(nfl_fan_proj %>% filter(position == pos) %>% select(nfl_data) %>% unnest(), 
          paste0("./data/fantasy/fantasy_projections_2018_nfl_", pos, ".Rds"))
}
