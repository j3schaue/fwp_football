# fwp_football

This is a working repository for the Francis Parker Sabermetrics Club. Our current projects involve scraping NFL stats from different websites and using machine learning to predict:

* awards
* contracts
* fantasy football projections and accuracy

This repository is organized as follows:

* `src/` directory contains code and helper functions used to scrape data for the projects.
* `data/` directory contains data scraped from the web for FWP projects. It is organized by project:

     - `fantasy/` contains `.Rds` files on 2018 fantasy projections and actual points from NFL.com for QBs, RBs, WRs, Kickers, and D/ST. It also has a file with `_all` that has each of those stored at different places in the same tibble (i.e., a tibble of data frames).
     - `PFR/` contains statistics scraped from Pro-Football Reference from 1972--2018. It includs `.Rds` files for different awards (e.g., OPOY, OROY). It also has `.Rds` files for stats on individual players within seasons from 1985-2018. Files include passing, rushing, receiving, scoring, and kicking stats.
     - `sportrac/` contains data on player contracts and salaries scraped from Sportrac.com. It includes data on active contracts in 2018. The files are broken down by defense, offense, and special teams.
     
* `tutorials/` contains files to help students get acquainted with reading data into `R` and summarizing/exploring it. There are some tips for basic `R` commands. In addition, there are tutorials to get students started on their projects: Fantasy scores, OPOY, and Salary. For each project there is an `HTML` file that shows some basic commands and plots that students might consider running on their onw. There is also an `.Rmd` file that generated the `HTML` file to help students learn how to knit reports.
* `analyses/` directory contains files to get students started on their analyses for their projects. Each subfolder contains an `.Rmd` file that accomplishes some of the same analyses as the tutorials, however, these files are where the concepts of the tutorials should be extended, and project work conducted.