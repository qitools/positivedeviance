# Overview ----
# Corrected QI Tools positive-deviance display, 2026-11-05.
# Adapted from qitools/positivedeviance, commit
# 5e4d3ed59fc7e94450e4292f776b9651490bba2c (GPL).
# Original authors: Robert Badgett, Scott Chamberlain, Jeroen Ooms.
# Requires meta >= 7.0-0 for forest plots; distribution plots use base R only.
# Input columns (in order): Name, ID, Group, Outcomes, Observations.
# Outcomes: event count (p/c), or mean(SD) (m). Proportions/thresholds
# use 0..1, not 0..100. Plot type: f = forest; d = empirical distribution.
# Forest models retain the original GLMM proportions, log means, and
# approximate inverse-variance raw-count models. Counts require equal exposure.
# Asterisk = site CI does not overlap pooled CI, in the favorable direction.
# These are exploratory candidate flags, not multiplicity-adjusted tests.

# Positive deviance function ----
positivedeviance <- function(
    content, topic = "Positive deviance", Name_label = "Sites", subgroup = "NO",
    outcome_label = "Outcome", outcome_type = "g", displaynames = "selected",
    threshold_observations = 0, threshold_value = NA_real_,
    benchmark_value = NA_real_, benchmark_label = "Benchmark",
    data_type = "p", output_type = "f", x_min = NA_real_, x_max = NA_real_,
    theme = "white") {

  ## Validate arguments ----
  fail <- function(message) stop(message, call. = FALSE)
  number <- function(z, label, optional = FALSE) {
    if (length(z) == 0L || (length(z) == 1L &&
        (is.na(z) || trimws(as.character(z)) %in% c("", "NA", "NULL")))) {
      if (optional) return(NA_real_)
      fail(paste(label, "is required."))
    }
    if (length(z) != 1L) fail(paste(label, "must be one number."))
    ans <- suppressWarnings(as.numeric(as.character(z)))
    if (!is.finite(ans)) fail(paste(label, "must be finite and numeric."))
    ans
  }
  choice <- function(z, choices, label) {
    if (length(z) != 1L || is.na(z) || !z %in% choices)
      fail(paste(label, "must be one of", paste(choices, collapse = ", ")))
    z
  }
  data_type <- choice(data_type, c("p", "m", "c"), "data_type")
  output_type <- choice(output_type, c("f", "d"), "output_type")
  if (length(outcome_type) == 1L && is.na(outcome_type)) outcome_type <- "NA"
  outcome_type <- choice(outcome_type, c("g", "b", "NA"), "outcome_type")
  displaynames <- choice(displaynames, c("all", "selected", "none", "groups"), "displaynames")
  subgroup <- choice(subgroup, c("YES", "NO"), "subgroup")
  if (displaynames == "groups") subgroup <- "YES"
  threshold_observations <- number(threshold_observations, "threshold_observations")
  if (threshold_observations < 0) fail("threshold_observations cannot be negative.")
  threshold_value <- number(threshold_value, "threshold_value", TRUE)
  benchmark_value <- number(benchmark_value, "benchmark_value", TRUE)
  x_min <- number(x_min, "x_min", TRUE)
  x_max <- number(x_max, "x_max", TRUE)
  if (!is.na(x_min) && !is.na(x_max) && x_min >= x_max)
    fail("x_min must be smaller than x_max.")
  if (data_type == "p") {
    if (!is.na(benchmark_value) && (benchmark_value < 0 || benchmark_value > 1))
      fail("For proportions, benchmark_value must be between 0 and 1.")
    if (output_type == "d" && outcome_type != "NA" &&
        !is.na(threshold_value) && (threshold_value < 0 || threshold_value > 1))
      fail("For proportions, threshold_value must be between 0 and 1 (e.g., 0.05 for 5%).")
    if ((!is.na(x_min) && (x_min < 0 || x_min > 1)) ||
        (!is.na(x_max) && (x_max < 0 || x_max > 1)))
      fail("For proportions, x-axis limits must be between 0 and 1.")
  }

  ## Read and prepare input ----
  # Standard CSV parser: quoted names, CRLF, optional trailing empty column.
  # No evaluation of input as R code; do not replace missing outcomes by zero.
  if (is.data.frame(content) || is.matrix(content)) {
    dat <- as.data.frame(content, stringsAsFactors = FALSE)
  } else {
    if (!is.character(content) || length(content) != 1L || !nzchar(trimws(content)))
      fail("content must be a nonempty CSV string, data frame, or matrix.")
    content <- sub("^\ufeff", "", content)
    content <- gsub("\r\n?", "\n", content)
    dat <- tryCatch(utils::read.csv(text = content, header = FALSE,
        colClasses = "character", strip.white = TRUE, fill = TRUE,
        blank.lines.skip = TRUE, comment.char = "", check.names = FALSE,
        na.strings = c("", "NA")), error = function(e)
          fail(paste("Cannot read CSV:", conditionMessage(e))))
  }
  dat[] <- lapply(dat, function(z) trimws(as.character(z)))
  if (ncol(dat) %in% c(5L, 6L) && all(is.na(dat[[ncol(dat)]]) | dat[[ncol(dat)]] == ""))
    dat <- dat[-ncol(dat)]
  ### Legacy four-column format ----
  # The repository's proportions download uses an older FOUR-column format.
  legacy_header <- c("id", "clinic", "count", "observations")
  if (ncol(dat) == 4L) {
    header <- nrow(dat) > 0L && identical(tolower(unname(unlist(dat[1, ]))), legacy_header)
    named <- identical(tolower(names(dat)), legacy_header)
    if (!header && !named)
      fail("Four-column input must use the legacy header ID,Clinic,Count,Observations.")
    if (header) dat <- dat[-1, , drop = FALSE]
    dat <- data.frame(Name = paste0("Site ", dat[[1]]), ID = dat[[1]],
        Group = dat[[2]], Outcomes = dat[[3]], Observations = dat[[4]],
        stringsAsFactors = FALSE)
  }
  if (ncol(dat) != 5L) fail("Provide exactly five columns: Name, ID, Group, Outcomes, Observations.")
  names(dat) <- c("Name", "ID", "Group", "Outcomes", "Observations")
  dat <- dat[rowSums(!is.na(dat) & dat != "") > 0, , drop = FALSE]
  ### Column names and header ----
  # Recognize the header by its first field, rather than discarding bad data.
  if (nrow(dat) && !is.na(dat$Name[1]) && !is.na(dat$Observations[1]) &&
      tolower(dat$Name[1]) %in% c("name", "names") &&
      tolower(dat$Observations[1]) %in% c("observations", "n", "matches"))
    dat <- dat[-1, , drop = FALSE]
  if (!nrow(dat)) fail("No data rows were supplied.")
  rownames(dat) <- NULL
  ### Validate data values ----
  numeric_column <- function(z, label) {
    ans <- suppressWarnings(as.numeric(gsub(",", "", z, fixed = TRUE)))
    bad <- which(!is.finite(ans))
    if (length(bad)) fail(paste0(label, " is missing or invalid in data row(s): ",
                                paste(bad, collapse = ", "), "."))
    ans
  }
  dat$Observations <- numeric_column(dat$Observations, "Observations")
  if (any(dat$Observations <= 0 | dat$Observations != floor(dat$Observations)))
    fail("Observations must be positive whole numbers.")
  if (any(is.na(dat$Name) | dat$Name == "")) fail("Each data row needs a Name.")
  if (any(is.na(dat$ID) | dat$ID == "")) fail("Each data row needs an ID.")
  if (subgroup == "YES" && any(is.na(dat$Group) | dat$Group == ""))
    fail("Each data row needs a Group when subgrouping is enabled.")
  ## Summary descriptive stats ----
  ### Means ----
  if (data_type == "m") {
    pattern <- "^\\s*([+-]?[0-9.,]+(?:[eE][+-]?[0-9]+)?)\\s*\\(\\s*([+0-9.,]+(?:[eE][+-]?[0-9]+)?)\\s*\\)\\s*$"
    if (any(is.na(dat$Outcomes) | !grepl(pattern, dat$Outcomes, perl = TRUE)))
      fail("For means, Outcomes must have the form mean(SD), e.g., 17(2).")
    dat$Mean <- numeric_column(sub(pattern, "\\1", dat$Outcomes, perl = TRUE), "Mean")
    dat$sd <- numeric_column(sub(pattern, "\\2", dat$Outcomes, perl = TRUE), "SD")
    if (any(dat$sd <= 0 | dat$Observations < 2))
      fail("For means, SD must be positive and Observations must be at least 2.")
    if (output_type == "f" && any(dat$Mean <= 0))
      fail("The retained log-mean forest model requires positive means.")
    dat$Outcome.value <- dat$Mean
    pooled <- stats::weighted.mean(dat$Mean, dat$Observations)
  } else {
    ### Proportions and counts ----
    dat$Outcomes <- numeric_column(dat$Outcomes, "Outcomes")
    if (any(dat$Outcomes < 0 | dat$Outcomes != floor(dat$Outcomes)))
      fail("Outcomes must be nonnegative whole event counts.")
    if (data_type == "p" && any(dat$Outcomes > dat$Observations))
      fail("For proportions, Outcomes cannot exceed Observations.")
    if (data_type == "c" && length(unique(dat$Observations)) > 1L)
      fail("Raw counts need equal exposure in every row. Use proportions for binary events or a separate incidence-rate model for unequal exposure.")
    dat$Outcome.value <- if (data_type == "p") dat$Outcomes/dat$Observations else dat$Outcomes
    pooled <- if (data_type == "p") sum(dat$Outcomes)/sum(dat$Observations) else mean(dat$Outcomes)
  }
  ### Sort sites and initialize results ----
  dat <- dat[order(dat$Outcome.value), , drop = FALSE]
  rownames(dat) <- NULL
  dat$eligible <- dat$Observations >= threshold_observations
  dat$candidate <- FALSE
  model <- NULL
  curve <- NULL
  labels <- dat$ID

  ## Forest plots ----
  if (output_type == "f") {
    ### Meta-analysis ----
    if (!requireNamespace("meta", quietly = TRUE))
      fail('Install the meta package first: install.packages("meta").')
    if (utils::packageVersion("meta") < "7.0.0")
      fail('Update the meta package first: install.packages("meta").')
    if (nrow(dat) < 2L) fail("A forest comparison needs at least two sites.")
    # Explicit namespaces also allow this file to be sourced on its own.
    args <- list(studlab = dat$Name, common = FALSE, random = TRUE,
        level = 0.95, level.ma = 0.95,
        method.tau = if (data_type == "p") "ML" else "REML")
    if (subgroup == "YES") args$subgroup <- dat$Group
    #### Proportions model ----
    if (data_type == "p") {
      if (all(dat$Outcomes == 0) || all(dat$Outcomes == dat$Observations))
        fail("All proportions are identical at 0 or 1; the GLMM cannot estimate site differences. Use output_type = 'd' to display them.")
      model <- do.call(meta::metaprop, c(list(event = dat$Outcomes,
          n = dat$Observations, sm = "PLOGIT", method = "GLMM",
          method.random.ci = "HK", title = "Proportions"), args))
    } else if (data_type == "m") {
      #### Means model ----
      model <- do.call(meta::metamean, c(list(n = dat$Observations,
          mean = dat$Mean, sd = dat$sd, sm = "MLN",
          method.random.ci = "HK", title = "Means"), args))
    } else {
      #### Counts model ----
      cis <- vapply(dat$Outcomes, function(z) stats::poisson.test(z)$conf.int, numeric(2))
      se <- (cis[2, ] - cis[1, ])/(2 * stats::qnorm(0.975))
      model <- do.call(meta::metagen, c(list(TE = dat$Outcomes, seTE = se,
          sm = "MRAW", method.random.ci = "classic", title = "Counts"), args))
    }
    ### Identify deviants ----
    # Compare CIs on the SAME model scale (logit / log / raw).
    # Preserve the repository's nonoverlapping-CI asterisk rule, restricted
    # to the favorable direction, and use the same rule for name disclosure.
    if (!is.finite(model$TE.random) || !is.finite(model$lower.random) ||
        !is.finite(model$upper.random)) fail("The pooled model did not produce a finite confidence interval.")
    favorable <- if (outcome_type == "g") model$lower > model$upper.random else
      if (outcome_type == "b") model$upper < model$lower.random else rep(FALSE, nrow(dat))
    dat$candidate <- !is.na(favorable) & favorable
    ### Display attribution/names ----
    if (displaynames == "all") labels <- dat$Name
    if (displaynames == "selected") labels[dat$candidate] <- dat$Name[dat$candidate]
    ### Asterisk adding to deviants ----
    labels <- paste0(labels, ifelse(dat$candidate, "*", ""))
    ### Original-scale estimates and axis limits ----
    inverse <- switch(data_type, p = stats::plogis, m = exp, c = identity)
    dat$lower <- inverse(model$lower)
    dat$upper <- inverse(model$upper)
    dat$estimate <- inverse(model$TE)
    model$studlab <- labels
    limits <- if (data_type == "p") c(0, 1) else
      range(c(dat$lower, dat$upper, inverse(model$lower.random),
              inverse(model$upper.random), benchmark_value), na.rm = TRUE)
    if (!is.na(x_min)) limits[1] <- x_min
    if (!is.na(x_max)) limits[2] <- x_max
    if (limits[1] >= limits[2]) fail("The resulting x-axis limits are not increasing.")
    if (any(dat$estimate < limits[1] | dat$estimate > limits[2]))
      warning("Some sites lie outside the requested x-axis limits.", call. = FALSE)
    leftcols <- if (data_type == "p") c("studlab", "event", "n") else "studlab"
    leftlabs <- if (data_type == "p") c(Name_label, "Events", "Observations") else Name_label
    ### Forest plot ----
    # Reserve space below the pooled estimate and add a short plot note.
    meta::forest(model, leftcols = leftcols, leftlabs = leftlabs,
        rightcols = c("effect", "ci"), rightlabs = c("Estimate", "95% CI"),
        sortvar = dat$Outcome.value, study.results = displaynames != "groups",
        sort.subgroup = TRUE, ref = benchmark_value, xlim = limits,
        backtransf = TRUE, xlab = outcome_label, smlab = topic,
        print.I2.ci = TRUE, print.tau2 = FALSE, print.Q = FALSE,
        print.pval.Q = FALSE, addrows.below.overall = 4, details = FALSE,
        col.square = ifelse(dat$candidate, "#16803A", "#333333"))
    ### Footer ----
    grid::upViewport(0)
    grid::grid.text(paste0("* Candidate: favorable site CI does not overlap pooled CI.",
        if (!is.na(benchmark_value)) paste0("\n", benchmark_label, ": ", benchmark_value) else ""),
        x = 0.02, y = 0.025, just = c("left", "bottom"),
        gp = grid::gpar(fontsize = 9))
  } else {
    ## Density plots ----
    # Empirical distribution: each eligible site has equal weight.
    # It is NOT a binomial null distribution or a test for exceptional sites.
    if (displaynames == "groups")
      fail("Groups-only display is available for forest plots; choose output_type = 'f'.")
    ### Select eligible sites and identify deviants ----
    eligible <- dat[dat$eligible, , drop = FALSE]
    if (outcome_type != "NA" && !is.na(threshold_value)) {
      dat$candidate <- dat$eligible & if (outcome_type == "g")
        dat$Outcome.value > threshold_value else dat$Outcome.value < threshold_value
    }
    ### Axis limits and plot setup ----
    scale <- if (data_type == "p") 100 else 1
    values <- eligible$Outcome.value * scale
    limits <- if (data_type == "p") c(0, 100) else {
      r <- range(c(dat$Outcome.value * scale, benchmark_value * scale), na.rm = TRUE)
      pad <- max(diff(r) * 0.08, abs(mean(r)) * 0.02, 0.5)
      r + c(-pad, pad)
    }
    if (!is.na(x_min)) limits[1] <- x_min * scale
    if (!is.na(x_max)) limits[2] <- x_max * scale
    if (limits[1] >= limits[2]) fail("The resulting x-axis limits are not increasing.")
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar), add = TRUE)
    graphics::par(mar = c(6, 4.5, 4, 1) + 0.1)
    xlab <- paste0(outcome_label, if (data_type == "p") " (%)" else "")
    ### Empty selection ----
    if (!length(values)) {
      graphics::plot(NA_real_, NA_real_, xlim = limits, ylim = c(0, 1),
          xlab = xlab, ylab = "Density", main = topic)
      graphics::text(mean(limits), 0.5, "No sites meet the observation threshold.")
    } else {
      if (any(values < limits[1] | values > limits[2]))
        warning("Some sites lie outside the requested x-axis limits.", call. = FALSE)
      ### Counts distribution ----
      if (data_type == "c") {
        counts <- table(values)
        curve <- data.frame(x = as.numeric(names(counts)), y = as.numeric(counts)/length(values))
        graphics::plot(curve$x, curve$y, type = "h", lwd = 5, xlim = limits,
            ylim = c(0, max(curve$y) * 1.22), xlab = xlab,
            ylab = "Proportion of sites", main = topic)
        point_y <- function(z) curve$y[match(z, curve$x)]
      } else {
        ### Proportions and means density ----
        bw <- if (length(values) > 1L && length(unique(values)) > 1L)
          stats::bw.nrd0(values) else max(abs(values[1]) * 0.05, if (data_type == "p") 1 else 0.1)
        grid <- seq(limits[1], limits[2], length.out = 512)
        point_y <- function(z) vapply(z, function(xx) {
          if (data_type == "p") {
            # Truncated Gaussian kernels keep all density inside 0..100%.
            mass <- stats::pnorm((100 - values)/bw) - stats::pnorm(-values/bw)
            mean(stats::dnorm((xx - values)/bw)/(bw * mass))
          } else mean(stats::dnorm((xx - values)/bw)/bw)
        }, numeric(1))
        curve <- data.frame(x = grid, y = point_y(grid))
        graphics::plot(curve$x, curve$y, type = "l", xlim = limits,
            ylim = c(0, max(c(curve$y, point_y(values))) * 1.22),
            xlab = xlab, ylab = "Density", main = topic)
      }
      ### Plot deviants in green ----
      flags <- dat$candidate[dat$eligible]
      graphics::points(values, point_y(values), pch = 19,
          col = ifelse(flags, "#16803A", "#333333"))
      ### Duplicate outcomes ----
      # Label coincident sites once, so multiplicity is visible.
      duplicates <- table(values)
      for (v in as.numeric(names(duplicates)[duplicates > 1L]))
        graphics::text(v, point_y(v), labels = duplicates[as.character(v)], pos = 3, cex = 0.8)
      ### Benchmark and local mean ----
      graphics::abline(v = pooled * scale, col = "#2463B4", lty = 2)
      if (!is.na(benchmark_value)) graphics::abline(v = benchmark_value * scale, col = "#904E00", lty = 3)
      ### Legend ----
      graphics::legend("topright", bty = "n", cex = 0.75,
          legend = c("Eligible sites", if (any(flags)) "Beyond favorable threshold",
                     "Overall observed mean", if (!is.na(benchmark_value)) benchmark_label),
          col = c("#333333", if (any(flags)) "#16803A", "#2463B4",
                  if (!is.na(benchmark_value)) "#904E00"),
          pch = c(19, if (any(flags)) 19, NA, if (!is.na(benchmark_value)) NA),
          lty = c(NA, if (any(flags)) NA, 2, if (!is.na(benchmark_value)) 3))
    }
    ### Footer ----
    graphics::mtext(paste0(nrow(eligible), " of ", nrow(dat), " sites meet n >= ",
        threshold_observations, "; green points: ", sum(dat$candidate),
        ". Equal weight per eligible site."), side = 1, line = 4, cex = 0.75)
  }
  ## Return results ----
  invisible(list(data = dat, model = model, curve = curve,
      observed_mean = pooled, labels = labels,
      candidate_rule = if (output_type == "f")
        "Favorable site CI does not overlap overall pooled CI (exploratory)." else
        "Observed outcome crosses user threshold in the favorable direction (descriptive)."))
}
