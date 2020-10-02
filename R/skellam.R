

# skellam_logpmf <- function(x, mu1, mu2) {
#   dbc::assert_is_integer_vector(x)
#   dbc::assert_is_number_nonNA_vector(mu1)
#   dbc::assert_is_number_nonNA_vector(mu2)
#
#   sqrt_mu1_times_mu2 <- sqrt(mu1 * mu2)
#   suppressWarnings(# warning about lost precision
#     log(besselI(2L * sqrt_mu1_times_mu2, abs(x), TRUE)) + 2L *
#       sqrt_mu1_times_mu2 - mu1 - mu2 + x / 2L * (log(mu1) - log(mu2))
#   )
# }
#
# skellam_pmf <- function(x, mu1, mu2) {
#   exp(skellam_logpmf(x = x, mu1 = mu1, mu2 = mu2))
# }

# skellam_logcdf <- function(q, mu1, mu2) {
#   dbc::assert_is_integer_vector(q)
#   dbc::assert_is_number_nonNA_vector(mu1)
#   dbc::assert_is_number_nonNA_vector(mu2)
#
#   from <- - stats::qpois(p = 1 - 1e-9, lambda = (abs(mu1) + abs(mu2)))
#   from <- min(q, from)
#   from <- from - 100L
#   to <- max(q)
#   range <- from:to
#   logpmf <- skellam_logpmf(x = range, mu1 = mu1, mu2 = mu2)
#   logpmf_max <- max(logpmf)
#   logcdf <- logpmf_max + log(cumsum(exp(logpmf - logpmf_max)))
#   logcdf[match(q, range)]
# }

skellam_cdf <- function(q, mu1, mu2) {
  # exp(skellam_logcdf(q = q, mu1 = mu1, mu2 = mu2))
  skellam::pskellam(q = q, lambda1 = mu1, lambda2 = mu2)
}
