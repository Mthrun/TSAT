ArbitratedDynamicEnsemble <- function(Data, formula, specs, ...) {
  if (!is.data.frame(Data) || NROW(Data) < 3L || !NCOL(Data) || anyDuplicated(names(Data)))
    stop("Data must be a training data frame with at least three rows and unique column names.")
  if (!inherits(formula, "formula") || length(formula) != 3L)
    stop("formula must specify a response and predictors.")
  # A formula can otherwise read variables from the caller's environment.
  # Require its variables in Data so the training rows remain explicit.
  if (!all(setdiff(all.vars(formula), ".") %in% names(Data)))
    stop("All formula variables must be columns of Data.")
  frame <- stats::model.frame(formula, data = Data, na.action = stats::na.fail)
  response <- stats::model.response(frame)
  if (!is.numeric(response) || any(!is.finite(response)) ||
    any(vapply(frame, function(x) is.numeric(x) && any(!is.finite(x)), logical(1))))
    stop("The response and numeric predictors must be finite.")
  if (!requireNamespace("tsensembler", quietly = TRUE))
    stop("Install the optional package 'tsensembler' to use this method.", call. = FALSE)
  if (!methods::is(specs, "model_specs")) stop("specs must be a tsensembler model_specs object.")
  # ADE trains an ensemble. It does not create lagged predictors or forecast dates.
  # Keep the native model. Use tsensembler::predict and tsensembler::forecast
  # so dispatch does not depend on attaching the package.
  tsensembler::ADE(form = formula, data = Data, specs = specs, ...)
}
