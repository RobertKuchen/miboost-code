replicate_datasets <- function(n_sims = n_sim, p) {
  sims <- vector("list", n_sims)
  for (i in seq_len(n_sims)) {
    sims[[i]] <- booami::simulate_booami_data(
      n = 500,
      p = p,
      p_inf = 5,
      type = "gaussian",
      corr_structure = "informative_cs",
      rho = 0.25,
      beta_range = c(1, 2),
      intercept = 5,
      miss = "MAR",
      miss_prop = 0.25,
      mar_drivers = c(1, 2),
      gamma_vec = c(0.75, -0.5),
      calibrate_mar = FALSE,
      mar_scale = FALSE,
      keep_observed = c(1, 2),
      jitter_sd = 0,
      noise_sd = 5
    )
  }
  sims
}