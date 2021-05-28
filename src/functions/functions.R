rev_likert <- function(vec, min = NULL, max = NULL) {
  
  '
  Reverse the order of a Likert scale implicit to the input vector  
  and return the values corresponding to the reversed order.
  
  Args:
  vec (numeric): vector of scores based on the old scale
  min (numeric): minimum possible value of the old scale
  (either 0 or 1)
  max (numeric): maximum possible value of the old scale
  
  Returns:
  rev_vec (numeric): vector of scores based on the reversed
  scale
  
  '
  
  if (!is.numeric(vec)) {
    stop("Input must be numeric")
  }
  
  if(!is.null(min) & !is.null(max)) {
    max_val <- max
    min_val <- min
  } else {
    warning("Minimum and maximum values of the original Likert scale
            have been inferred from the sample.")
    max_val <- max(vec, na.rm = TRUE)
    min_val <- min(vec, na.rm = TRUE)
    
    message(
      paste(
        "Inferred values: min = ",
        min_val,
        ", max =",
        max_val)
    )
  }
  
  rev_vec <- rep(0, length(vec))
  
  # Reverse the order 
  if (min_val == 1) {
    rev_vec <- max_val + 1 - vec
  } else if (min_val == 0) {
    rev_vec <- max_val - vec
  } else {
    stop("Minimum possible value of the original Likert scale must be either 0 or 1.")
  }
  return(rev_vec)
}


sev_to_four <- function(vec) {
  
  if (!is.numeric(vec)) {
    stop("Input must be numeric")
  }
  
  out <- rep(0, length(vec))
  
  out[vec %in% c(6, 7)] <- 1
  out[vec == 5] <- 2
  out[vec == 4] <- -99
  out[vec == 3] <- 3
  out[vec %in% c(1, 2)] <- 4
  out[!(vec %in% c(1:7))] <- NA
  
  
  return(as.numeric(out))
}


transform_likert <- function(vec, min_o, max_o, min_n, max_n) {
  
  #if (!is.numeric(vec)) {
  #  stop("Input must be numeric")
  #}
  
  out <- rep(0, length(vec))
  out[vec %in% c(min_o:max_o)] <- (max_n - min_n) *
    ((vec[vec %in% c(min_o:max_o)] - min_o) / (max_o - min_o)) + min_n
  out[!(vec %in% c(min_o:max_o))] <- NA
  
  return(as.numeric(out))
}


tidy_aggte <- function(x, ...) {
  ret <- data.frame(
    term      = "ATT",
    estimate  = x$overall.att,
    std.error = x$overall.se,
    conf.low  = x$overall.att - qnorm(0.975) * x$overall.se,
    conf.high = x$overall.att + qnorm(0.975) * x$overall.se
  )
  ret
}

glance_aggte <- function(x, ...) {
  ret <- data.frame(
    N = as.character(x$DIDparams$n),
    SE = "Robust",
    Controls = ifelse(x$DIDparams$xformla[[2]] != "1", "Yes", "No"),
    Survey_year_FE = "No"
  )
  ret
}

tidy_feols <- function(x, ...) {
  ret <- data.frame(
    term      = "ATT",
    estimate  = x$coefficients[[1]],
    std.error = x$se[[1]],
    conf.low  = x$coefficients[[1]] - qnorm(0.975) * x$se[[1]],
    conf.high = x$coefficients[[1]] + qnorm(0.975) * x$se[[1]]
  )
  ret
}

glance_feols <- function(x, ...) {
  ret <- data.frame(
    N = as.character(x$nobs),
    SE = "Robust",
    Controls = ifelse(length(x$coeftable$Estimate) > 1, "Yes", "No"),
    Survey_year_FE = ifelse(length(x$fixef_vars) == 3, "Yes", "No")
  )
  col_order <- c("N", "SE", "Controls", "Survey_year_FE")
  ret[, col_order]
}

density_plot <- function(var, df) {
  '
  Return a ggplot-object with density plots of a numeric vector names "var"
  from dataframe "df" split along another variable "cat". Note that
  this function requires a dataframe ("df_p_values") containing captions
  to exist in the global environment.

  Args:
  var (character): variable name
  cat (factor): factor vector containing the category of each observation
  identifier for each observation
  df (dataframe): dataframe that contains both "var" and "cat"

  Returns:
  ggplot object with the density plots

  '
  ggplot(
    df,
    aes(x = get(var), group = class_plot, fill = class_plot)
  ) +
    geom_density(
      adjust = 1.5, alpha = .4,
      kernel = "gaussian",
      na.rm = TRUE
    ) +
    theme_ipsum() +
    ggtitle(var) +
    labs(
      x = "",
      y = ""
    ) +
    xlim(-5, 5) +
    ylim(0, 0.7)
}

hist_plot <- function(var, cat, df) {
  '
  Return a ggplot-object with density plots of a numeric vector names "var"
  from dataframe "df" split along another variable "cat". Note that
  this function requires a dataframe ("df_p_values") containing captions
  to exist in the global environment.

  Args:
  var (character): variable name
  cat (factor): factor vector containing the category of each observation
  identifier for each observation
  df (dataframe): dataframe that contains both "var" and "cat"

  Returns:
  ggplot object with the density plots

  '
  ggplot(
    df,
    aes(x = get(var))
  ) +
    geom_histogram(
      aes(fill = cat),
      alpha = 0.55,
      position = "identity",
      na.rm = TRUE
    ) +
    scale_fill_manual(
      values = c("blue", "orange")
    ) +
    ggtitle(var) +
    labs(
      x = "",
      y = ""
    )
}

box_plot <- function(y_var, nam, cat, df) {
  '
  Return a ggplot-object with density plots of a numeric vector names "var"
  from dataframe "df" split along another variable "cat". Note that
  this function requires a dataframe ("df_p_values") containing captions
  to exist in the global environment.

  Args:
  var (character): variable name
  cat (factor): factor vector containing the category of each observation
  identifier for each observation
  df (dataframe): dataframe that contains both "var" and "cat"

  Returns:
  ggplot object with the density plots

  '
  ggplot(df, aes(x = above_f, y = get(y_var), fill = cat)) +
    geom_boxplot() +
    geom_jitter(shape = 16, position = position_jitter(0.2), alpha = 0.5) +
    xlab("") +
    ylab("") +
    ggtitle(nam) +
    labs(fill = "Median split")
}

max_fun <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)