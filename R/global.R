# Data ------------------------------------------------------------------------

wells <- readr::read_rds("./inst/data/wells_proj.rds")

eur <- readr::read_rds("./inst/data/eur.rds") |>
  dplyr::mutate(pden_id = as.integer(pden_id))

production <- readr::read_rds("./inst/data/production.rds") |>
  dplyr::mutate(pden_id = as.integer(pden_id))

# Shapefiles ------------------------------------------------------------------

texas_state <- readr::read_rds("./inst/data/texas_state.Rds")
texas_counties <- readr::read_rds("./inst/data/texas_outline.Rds")
texas_grids <- readr::read_rds("./inst/data/texas_grids.Rds")
