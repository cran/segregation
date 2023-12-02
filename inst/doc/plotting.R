## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

data.table::setDTthreads(1)

# skip this vignette on CRAN etc.
BUILD_VIGNETTE <- identical(Sys.getenv("BUILD_VIGNETTE"), "true")
knitr::opts_chunk$set(eval = BUILD_VIGNETTE)

library("segregation")

## -----------------------------------------------------------------------------
segcurve(subset(schools00, race %in% c("white", "asian")),
  "race", "school",
  weight = "n",
  segment = "state" # leave this out to produce a single curve
)

## -----------------------------------------------------------------------------
# converting to data.table makes this easier
data.table::as.data.table(schools00)[
  race %in% c("white", "asian"),
  dissimilarity(.SD, "race", "school", weight = "n"),
  by = .(state)
]

## -----------------------------------------------------------------------------
sch <- subset(schools00, state == "A")

# basic segplot
segplot(sch, "race", "school", weight = "n")

# order by majority group (white in this case)
segplot(sch, "race", "school", weight = "n", order = "majority")

# increase the space between bars
# (has to be very low here because there are many schools in this dataset)
segplot(sch, "race", "school", weight = "n", bar_space = 0.0005)

# change the reference distribution
# (here, we just use an equalized distribution across the five groups)
(ref <- data.frame(race = unique(schools00$race), p = rep(0.2, 5)))
segplot(sch, "race", "school",
  weight = "n",
  reference_distribution = ref
)

## -----------------------------------------------------------------------------
segplot(sch, "race", "school", weight = "n", secondary_plot = "segregation")

## ---- results='hide'----------------------------------------------------------
# compression based on window of 20 'neighboring' units
# in terms of local segregation (alternatively, neighbors can be a data frame)
comp <- compress(sch, "race", "school",
  weight = "n", neighbors = "local", n_neighbors = 20
)

## -----------------------------------------------------------------------------
comp

## -----------------------------------------------------------------------------
scree_plot(comp)

## -----------------------------------------------------------------------------
dend <- as.dendrogram(comp)
plot(dend, leaflab = "none")

## -----------------------------------------------------------------------------
sch_compressed <- merge_units(comp, n_units = 15)
# or, for instance: merge_units(comp, percent = 0.80)
head(sch_compressed)

## -----------------------------------------------------------------------------
segplot(sch_compressed, "race", "school", weight = "n", secondary_plot = "segregation")

