#' segregation: Entropy-based segregation indices
#'
#' Calculate and decompose entropy-based, multigroup segregation indices, with a focus
#' on the Mutual Information Index (M) and Theil's Information Index (H).
#' Provides tools to decompose the measures by groups and units, and by within
#' and between terms. Includes standard error estimation by bootstrapping.
#'
#' @seealso \url{https://elbersb.com/segregation}
#'
#' @docType package
#' @name segregation
#' @keywords internal
"_PACKAGE"

#' @importFrom Rcpp sourceCpp
#' @import RcppProgress
#' @useDynLib segregation, .registration = TRUE
NULL

globalVariables(c(
    "V1", "V2", "cond1", "cond2", "entropy_cond", "entropy_cond1", "entropy_cond2", "entropyw",
    "est", "freq", "freq1", "freq2", "freq_orig1", "freq_orig2",
    "ls_diff_mean", "ls_diff1", "ls_diff2", "ls_unit", "bias", "boot_est", "est_debiased",
    "n", "n_group", "n_group_target", "n_source", "n_target", "n_unit", "n_unit_target",
    "n_within_group", "p", "p_exp", "p1", "p2", "p_group", "p_group_g_unit", "p_group_g_unit1",
    "p_group_g_unit2", "p_group_s", "p_group_t", "p_unit", "p_unit1", "p_unit2", "p_unit_s",
    "p_unit_t", "p_within", "sumcond1", "sumcond2", "total", "unit1", "unit2",
    ".", "..base", "..fixed_margins", "..group", "..n_bootstrap", "..unit", "se", "stat",
    "M", "N_units", "i.freq1", "i.freq2", "iter", "ls1", "ls2", "p_unit_g_group1",
    "p_unit_g_group2", "pair", "pct_M",
    "x", "xend", "y", "yend", "xmax", "xmin", "ymax", "ymin", "..cols", "p_overall",
    "freq_of", "freq_to",
    "cumul_prob_1", "cumul_prob_2", "group1", "group2", "pct_group_1",
    ".data", "new_unit", "old_unit"
))

# log

log_env <- new.env()
assign("n_printed", 0, envir = log_env)

update_log <- function(bs_n = NULL, bs_max = NULL, ipf_n = NULL, ipf_max = NULL) {
    if (!is.null(bs_n)) assign("bs_n", bs_n, envir = log_env)
    if (!is.null(bs_max)) assign("bs_max", bs_max, envir = log_env)
    if (!is.null(ipf_n)) assign("ipf_n", ipf_n, envir = log_env)
    if (!is.null(ipf_max)) assign("ipf_max", ipf_max, envir = log_env)

    if (!is.null(get("bs_n", envir = log_env)) && !is.null(get("ipf_n", envir = log_env))) {
        text <- paste0("[", "Bootstrap ", get("bs_n", envir = log_env), "/",
            get("bs_max", envir = log_env),
            " IPF ", get("ipf_n", envir = log_env), "/",
            get("ipf_max", envir = log_env), "] ",
            collapse = ""
        )
    } else if (!is.null(get("bs_n", envir = log_env))) {
        text <- paste0("[", "Bootstrap ", get("bs_n", envir = log_env),
            "/", get("bs_max", envir = log_env), "] ",
            collapse = ""
        )
    } else if (!is.null(get("ipf_n", envir = log_env))) {
        text <- paste0("[", "IPF ", get("ipf_n", envir = log_env),
            "/", get("ipf_max", envir = log_env), "] ",
            collapse = ""
        )
    }

    clear_log()
    assign("n_printed", nchar(text), envir = log_env)
    cat(text, file = stderr())
}

update_log_progress <- function(text) {
    assign("n_printed", get("n_printed", envir = log_env) + nchar(text), envir = log_env)
    cat(text, file = stderr())
}

clear_log <- function() {
    utils::flush.console()

    if (get("n_printed", envir = log_env) > 0) {
        cat("\r", paste0(rep(" ", times = get("n_printed", envir = log_env)), collapse = ""),
            "\r",
            file = stderr()
        )

        assign("n_printed", 0, envir = log_env)
    }
}

close_log <- function() {
    clear_log()
    assign("bs_n", NULL, envir = log_env)
    assign("bs_max", NULL, envir = log_env)
    assign("ipf_n", NULL, envir = log_env)
    assign("ipf_max", NULL, envir = log_env)
}
close_log()

# helpers

logf <- function(v, base = exp(1)) {
    logged <- log(v, base = base)
    logged[!is.finite(logged)] <- 0
    logged
}

#' Calculates the entropy of a distribution
#'
#' Returns the entropy of the distribution defined by
#' \code{group}.
#'
#' @param data A data frame.
#' @param group A categorical variable or a vector of variables
#'   contained in \code{data}.
#' @param weight Numeric. (Default \code{NULL})
#' @param base Base of the logarithm that is used in the entropy
#'   calculation. Defaults to the natural logarithm.
#' @return A single number, the entropy.
#' @examples
#' d <- data.frame(cat = c("A", "B"), n = c(25, 75))
#' entropy(d, "cat", weight = "n") # => .56
#' # this is equivalent to -.25*log(.25)-.75*log(.75)
#'
#' d <- data.frame(cat = c("A", "B"), n = c(50, 50))
#' # use base 2 for the logarithm, then entropy is maximized at 1
#' entropy(d, "cat", weight = "n", base = 2) # => 1
#' @import data.table
#' @export
entropy <- function(data, group, weight = NULL, base = exp(1)) {
    # use provided weight
    if (!is.null(weight)) {
        data[, "freq"] <- data[[weight]]
    } else {
        data[, "freq"] <- 1
    }
    data.table::setDT(data)
    n_total <- sum(data[, "freq"])
    p <- data[, list(p = sum(freq)), by = group][["p"]] / n_total
    sum(p * logf(1 / p, base))
}


#' @import data.table
prepare_data <- function(data, group, unit, weight, within = NULL) {
    if ("data.frame" %in% class(data)) {
        if (nrow(data) == 0) {
            stop("data.frame is empty")
        }
        test_vars <- c(group, unit, weight, within)
        test_vars <- test_vars[!test_vars %in% names(data)]
        if (length(test_vars) > 0) {
            test_vars <- paste0(test_vars, collapse = ", ")
            stop(paste0("variable(s) ", test_vars, " not in data.frame"))
        }
    } else {
        stop("not a data.frame")
    }
    vars <- c(group, unit)

    # create a copy
    data <- as.data.table(data)

    # check whether there is variation
    n_groups <- nrow(data[, .N, by = group])
    n_units <- nrow(data[, .N, by = unit])
    if (n_groups == 1) stop("Cannot compute segregation: the group variable is constant")
    if (n_units == 1) stop("Cannot compute segregation: the unit variable is constant")

    # use provided weight or weight of 1
    weight_no_conflict <- weight
    if (!is.null(weight_no_conflict)) {
        if (weight_no_conflict == "weight") {
            data[, freq := as.double(weight)]
        } else {
            data[, freq := as.double(get(weight_no_conflict))]
        }
    } else {
        data[, freq := 1]
    }

    if (!is.null(within)) {
        vars <- c(vars, within)
    }

    # drop unused factor levels - these can lead to problems downstream
    for (var in vars) {
        if (is.factor(data[[var]])) {
            data[[var]] <- droplevels(data[[var]])
        }
    }

    # collapse on vars, and select only positive weights
    data <- data[freq > 0, list(freq = sum(freq)), by = vars]
    setattr(data, "vars", vars)
    setkey(data, NULL)
    data
}


#' @import data.table
add_local <- function(data, group_var, unit_var, base, weight = "freq") {
    n_total <- sum(data[, get(weight)])
    # generate unit and group totals
    data[, n_unit := sum(get(weight)), by = unit_var]
    data[, n_group := sum(get(weight)), by = group_var]
    # generate unit and group proportions and the
    # conditional probability of being in any group given the unit
    data[, `:=`(
        p_unit = n_unit / n_total,
        p_group = n_group / n_total,
        p_group_g_unit = get(weight) / n_unit
    )]
    # calculate local linkage, i.e. log(cond.) * log(cond./marginal)
    data[, ls_unit := sum(p_group_g_unit * logf(p_group_g_unit / p_group, base)),
        by = unit_var
    ]
}

#' @import data.table
bootstrap_summary <- function(ret, boot_ret, cols, CI) {
    setnames(boot_ret, "est", "boot_est")
    ret <- merge(ret, boot_ret, by = cols, sort = FALSE)
    # create a "debiased" version of bootstrap estimates
    ret[, est_debiased := 2 * est - boot_est, by = cols]
    pct <- c((1 - CI) / 2, 1 - (1 - CI) / 2)
    # estimate the "debiased" mean, standard error, CI, and quantify bias
    ret <- ret[, list(
        est = mean(est_debiased),
        se = stats::sd(est_debiased),
        CI = list(stats::quantile(est_debiased, pct)),
        bias = first(est) - mean(est_debiased)
    ), by = cols]
    ret[]
}

#' Turns a contingency table into long format
#'
#' Returns a data.table in long form, such that it is suitable
#' for use in \link{mutual_total}, etc. Colnames and rownames of
#' the matrix will be respected.
#'
#' @param matrix A matrix, where the rows represent the units, and the
#'    column represent the groups.
#' @param group Variable name for group. (Default \code{group})
#' @param unit Variable name for unit. (Default \code{unit})
#' @param weight Variable name for frequency weight. (Default \code{weight})
#' @param drop_zero Drop unit-group combinations with zero weight. (Default \code{TRUE})
#' @return A data.table.
#' @examples
#' m <- matrix(c(10, 20, 30, 30, 20, 10), nrow = 3)
#' colnames(m) <- c("Black", "White")
#' long <- matrix_to_long(m, group = "race", unit = "school")
#' mutual_total(long, "race", "school", weight = "n")
#' @import data.table
#' @export
matrix_to_long <- function(matrix, group = "group", unit = "unit",
                           weight = "n", drop_zero = TRUE) {
    if (!is.matrix(matrix)) stop("matrix needs be a matrix object")
    if (is.null(rownames(matrix))) rownames(matrix) <- seq_len(nrow(matrix))
    if (is.null(colnames(matrix))) colnames(matrix) <- seq_len(ncol(matrix))
    d <- as.data.table(matrix, keep.rownames = unit)
    long <- melt(d,
        id.vars = unit,
        variable.name = group,
        variable.factor = FALSE,
        value.name = weight
    )
    if (drop_zero == TRUE) {
        ind <- long[[weight]] > 0
        long[ind]
    } else {
        long
    }
}
