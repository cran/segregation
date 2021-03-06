## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library("segregation")
head(schools00[, c("school", "race", "n")])

## -----------------------------------------------------------------------------
(m = matrix(c(10, 20, 30, 30, 20, 10), nrow = 3))
colnames(m) <- c("Black", "White")
matrix_to_long(m, group = "race", unit = "school")

## -----------------------------------------------------------------------------
mutual_total(schools00, "race", "school", weight = "n")

## -----------------------------------------------------------------------------
mutual_total(schools00, "school", "race", weight = "n")

## -----------------------------------------------------------------------------
(entropy(schools00, "race", weight = "n"))
(entropy(schools00, "school", weight = "n"))

## -----------------------------------------------------------------------------
mutual_total(schools00, "race", "school", weight = "n",
             se = TRUE, CI = .95, n_bootstrap = 500)

## -----------------------------------------------------------------------------
split_schools <- split(schools00, schools00$state)
mutual_total(split_schools$A, "race", "school", weight = "n")[1, ]
mutual_total(split_schools$B, "race", "school", weight = "n")[1, ]
mutual_total(split_schools$C, "race", "school", weight = "n")[1, ]

## -----------------------------------------------------------------------------
# total segregation
(total <- mutual_total(schools00, "race", "school", weight = "n"))
# between-state segregation:
#     how much does the racial distributions differ across states?
(between <- mutual_total(schools00, "race", "state", weight = "n"))
# within-state segregation:
#     how much segregation exist within states?
(mutual_total(schools00, "race", "school", within = "state", weight = "n"))

## -----------------------------------------------------------------------------
(within <- mutual_within(schools00, "race", "school",
                         within = "state", weight = "n", wide = TRUE))

## -----------------------------------------------------------------------------
with(within, sum(M * p))
with(within, sum(H * p * ent_ratio))

## -----------------------------------------------------------------------------
# merge into a vector
components <- c(between$est[1], within$M * within$p)
names(components) <- c("Between", "A", "B", "C")
signif(100 * components / total$est[1], 3)

## -----------------------------------------------------------------------------
mutual_local(schools00, "race", "school", weight = "n", wide = TRUE)

## -----------------------------------------------------------------------------
localse <- mutual_local(schools00, "race", "school", weight = "n",
                        se = TRUE, wide = TRUE, n_bootstrap = 1000)
localse$lengthCI <- sapply(localse$ls_CI, diff)
with(localse, plot(x = p, y = lengthCI, pch = 16, cex = 0.3))

## -----------------------------------------------------------------------------
(localg <- mutual_local(schools00, "school", "race", weight = "n", wide = TRUE))

## -----------------------------------------------------------------------------
(se <- mutual_total(schools00, "race", "school", weight = "n",
                    se = TRUE, CI = .95, n_bootstrap = 500))

## -----------------------------------------------------------------------------
# M
with(se, c(est[1] - 1.96 * se[1], est[1] + 1.96 * se[1]))
# H
with(se, c(est[2] - 1.96 * se[2], est[2] + 1.96 * se[2]))

## -----------------------------------------------------------------------------
local <- mutual_local(schools00, "race", "school", weight = "n",
                      se = TRUE, CI = .95, n_bootstrap = 500)
# pick bootstrap distribution of local segregation scores for school C137_9
ls_school <- attr(local, "bootstrap")[school == "C137_9" & stat == "ls", boot_est]
hist(ls_school, main = "Bootstrap distribution for school C137_9")

## -----------------------------------------------------------------------------
mutual_difference(schools00, schools05, "race", "school", weight = "n")

