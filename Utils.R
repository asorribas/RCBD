######################################################################################################## End of checking inputs
### Calculate treatment effects from Cohen's f

calculate_group_means <- function(f=0.12, sigma_within=3, mu=0, k=3) {
  # Calculate the standard deviation of the group means
  sigma_means <- f * sigma_within
  
  # Total variance of the group means
  variance_means <- sigma_means^2
  
  # Solve for sum of squared deviations
  sum_squared_deviations <- variance_means * k
  
  # Assume symmetric deviations around the mean
  # Deviations will be equally spaced around the overall mean
  # Create deviations symmetrically
  deviations <- seq(-(k - 1) / 2, (k - 1) / 2, length.out = k)
  
  # Scale deviations to match the sum of squared deviations
  scaling_factor <- sqrt(sum_squared_deviations / sum(deviations^2))
  deviations <- deviations * scaling_factor
  
  # Calculate group means
  group_means <- mu + deviations
  
  return(group_means-group_means[1])
}


#################### Data generation and block effect

##This fubction generates the block effects based on the different levels

generate_block_effects <- function(b,
                                   sigma_within,
                                   level = c("null", "low", "medium", "large"),
                                   sd_ratios = c(null = 0.0, low = 0.5, medium = 1.5, large = 3),
                                   use_sample_sd = FALSE) {
  level <- match.arg(level)
  
  if (b < 1) stop("b must be >= 1")
  if (!level %in% names(sd_ratios)) stop("Level not found in 'sd_ratios' names")
  
  # Target block SD as a multiple of sigma_within
  sigma_b_target <- as.numeric(sd_ratios[level]) * sigma_within
  
  # Base mean-zero pattern (equally spaced scores centered at zero)
  # Example for b=4: -1.5, -0.5, 0.5, 1.5
  z <- seq_len(b) - (b + 1) / 2
  
  # If b == 1, no blocking possible; return 0
  if (b == 1) {
    betas <- 0
    actual_pop_sd <- 0
    actual_samp_sd <- NA_real_
  } else {
    # Choose population vs sample SD for scaling
    s <- if (use_sample_sd) sd(z) else sqrt(mean(z^2))
    # Scale to achieve the target SD
    scale_factor <- if (s > 0) sigma_b_target / s else 0
    betas <- z * scale_factor
    
    # Diagnostics
    actual_pop_sd <- sqrt(mean((betas - mean(betas))^2))
    actual_samp_sd <- sd(betas)
  }
  
  # Return with attributes for transparency
  betas
}


# -------- RCBD power & design optimizer (additive model) --------

# Convert MDE to Cohen's f if needed (equally spaced means assumption).
# Either supply 'f' directly, or provide one of:
#   - mde_range: max(min) - min(mean) across k groups (Δ)
#   - mde_adjacent: difference between consecutive means under equal spacing (δ)
f_from_mde <- function(k, sigma_within, mde_range = NULL, mde_adjacent = NULL) {
  if (!is.null(mde_range) && !is.null(mde_adjacent)) {
    stop("Provide only one of mde_range or mde_adjacent.")
  }
  if (!is.null(mde_range)) {
    return( (mde_range / sigma_within) * sqrt( (k + 1) / (12 * (k - 1)) ) )
  }
  if (!is.null(mde_adjacent)) {
    return( (mde_adjacent / sigma_within) * sqrt( ((k + 1) * (k - 1)) / 12 ) )
  }
  stop("Either 'f' must be supplied to optimizer() or one of mde_range/mde_adjacent with sigma_within here.")
}

# Power for treatment F-test in RCBD (additive, no interaction)
rcbd_power <- function(t, b, r, f, alpha = 0.05) {
  df1 <- t - 1
  df2 <- t * b * r - t - b + 1
  if (df2 <= 0) return(NA_real_)
  lambda <- (b * r) * t * f^2
  Fcrit <- qf(1 - alpha, df1, df2)
  power <- 1 - pf(Fcrit, df1, df2, ncp = lambda)
  return(power)
}

# Optimizer over integer b and r, returning minimal N that achieves target power.
optimize_rcbd <- function(t,
                          alpha = 0.05,
                          power_target = 0.80,
                          # supply either f, or (mde_range or mde_adjacent) + sigma_within
                          f = NULL,
                          sigma_within = NULL,
                          mde_range = NULL,
                          mde_adjacent = NULL,
                          b_grid = 2:20,
                          r_grid = 1:10,
                          prefer = c("min_N", "more_blocks")) {
  
  prefer <- match.arg(prefer)
  if (is.null(f)) {
    if (is.null(sigma_within)) stop("Provide sigma_within when deriving f from MDE.")
    f <- f_from_mde(k = t, sigma_within = sigma_within,
                    mde_range = mde_range, mde_adjacent = mde_adjacent)
  }
  
  results <- expand.grid(b = b_grid, r = r_grid)
  results$N_total <- t * results$b * results$r
  results$power <- mapply(function(b, r) rcbd_power(t, b, r, f, alpha), results$b, results$r)
  
  # Keep feasible (non-NA) and meeting power
  ok <- is.finite(results$power) & results$power >= power_target
  cand <- results[ok, , drop = FALSE]
  if (nrow(cand) == 0) return(list(best = NULL, table = results[order(results$N_total, results$power, decreasing = c(FALSE, TRUE)), ]))
  
  # Choose best per preference
  cand <- cand[order(cand$N_total, -cand$power, cand$b, cand$r), ]
  if (prefer == "more_blocks") {
    # Among same N, favor larger b (better control of day-to-day variation)
    cand <- cand[order(cand$N_total, -cand$b, cand$r, -cand$power), ]
  }
  
  best <- cand[1, ]
  list(best = best, table = cand)
}

y <- function(x) x
