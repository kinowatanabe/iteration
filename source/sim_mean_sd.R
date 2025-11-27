

#' sim_mean_sd
#'
#' @param n_subj number of sub
#' @param mu true sample mean
#' @param sigma true sample sd
#'
#' @returns tibble with sample mean and sample sd
#' @export
#'
## code> insert roxygen skeleton --> automatically reads code and creates a key for parameters
sim_mean_sd = function(n_subj, mu = 3, sigma = 2) {
  
  sim_df = 
    tibble(
      x = rnorm(n = n_subj, mean = mu, sd = sigma)
    )
  
  sim_df |> 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )
  
}