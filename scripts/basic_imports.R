source(here('scripts', 'paths.R'))
source(here('scripts', 'constants.R'))
source(here('scripts', 'utils.R'))
source(here('scripts', 'utils.plotting.R'))
# library(quarto)
library(GGally)
library(furrr)
plan(multisession, workers=availableCores())

