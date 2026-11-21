setwd(here())
source(here('.Rprofile'))  # explicitly load rv install libraries in R.nvim
# source(here('rv/scripts/activate.R'))  # explicitly load rv install libraries in R.nvim
# source(here('rv/scripts/rvr.R'))  # explicitly load rv install libraries in R.nvim
source(here('scripts', 'paths.R'))
source(here('scripts', 'constants.R'))
source(here('scripts', 'utils.R'))
library(furrr)
plan(multisession, workers=availableCores())

