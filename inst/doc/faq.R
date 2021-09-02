## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

# skip this vignette on CRAN etc.
BUILD_VIGNETTE <- identical(Sys.getenv("BUILD_VIGNETTE"), "true")
knitr::opts_chunk$set(eval = BUILD_VIGNETTE)

library("dplyr")
library("data.table")
library("tigris")
library("segregation")
options(tigris_use_cache = TRUE)
schools00 = schools00

## -----------------------------------------------------------------------------
library("segregation")
library("dplyr")

schools00 %>%
  filter(race %in% c("black", "white")) %>%
  group_by(state) %>%
  group_modify(~dissimilarity(data = .x,
    group = "race",
    unit = "school",
    weight = "n"))

## -----------------------------------------------------------------------------
library("data.table")

schools00 = as.data.table(schools00)
schools00[
  race %in% c("black", "white"),
  dissimilarity(data = .SD, group = "race", unit = "school", weight = "n"),
  by = .(state)]

## -----------------------------------------------------------------------------
# helper function for decomposition
diff = function(df, group) {
  data1 = filter(df, year == 2000)
  data2 = filter(df, year == 2005)
  mutual_difference(data1, data2, group = "race", unit = "school", weight = "n")
}

# add year indicators
schools00$year = 2000
schools05$year = 2005
combine = bind_rows(schools00, schools05)

combine %>%
 group_by(state) %>%
 group_modify(diff) %>%
 head(5)

## -----------------------------------------------------------------------------
setDT(combine)
combine[, diff(.SD), by = .(state)] %>% head(5)

## -----------------------------------------------------------------------------
library("tidycensus")

cook_data = get_acs(
  geography = "tract",
  variables = c(
    white = "B03002_003",
    black = "B03002_004",
    asian = "B03002_006",
    hispanic = "B03002_012"),
  state = "IL",
  county = "Cook")

## -----------------------------------------------------------------------------
# compute index of dissimilarity
cook_data %>%
  filter(variable %in% c("black", "white")) %>%
  dissimilarity(
    group = "variable",
    unit = "GEOID",
    weight = "estimate")

# compute multigroup M/H indices
cook_data %>%
  mutual_total(
    group = "variable",
    unit = "GEOID",
    weight = "estimate")


## ----fig.width=7, fig.height=7------------------------------------------------
library("tigris")
library("ggplot2")

local_seg = mutual_local(cook_data,
  group = "variable",
  unit = "GEOID",
  weight = "estimate",
  wide = TRUE)

# download shapefile
seg_geom = tracts("IL", "Cook", cb = TRUE, progress_bar = FALSE) %>%
  left_join(local_seg, by = "GEOID")

ggplot(seg_geom, aes(fill = ls)) +
  geom_sf(color = NA) +
  coord_sf(crs = 3435) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(title = "Local segregation scores for Cook County, IL",
    fill = NULL)

