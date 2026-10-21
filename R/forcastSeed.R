# Evaluate code with the supplied R seed. Restore the previous random state on exit.
# NULL leaves the random state under the control of the model.
forcastSeed <- function(Seed, code) {
  if (is.null(Seed)) return(force(code))
  if (!is.numeric(Seed) || length(Seed) != 1L || !is.finite(Seed) ||
    Seed < 0L || Seed > .Machine$integer.max || Seed != floor(Seed))
    stop("Seed must be one integer >= 0.", call. = FALSE)
  Seed <- as.integer(Seed)
  existed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (existed) previous <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (existed) assign(".Random.seed", previous, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  })
  set.seed(Seed)
  force(code)
}
