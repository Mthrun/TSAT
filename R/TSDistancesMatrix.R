TSDistancesMatrix <- function(TSMatrix, method = "euclidean", ..., m = NULL) {
  # Each column is one series. Convert before reading its dimensions.
  if (is.data.frame(TSMatrix)) TSMatrix <- as.matrix(TSMatrix)
  if (is.numeric(TSMatrix) && is.null(dim(TSMatrix)))
    TSMatrix <- matrix(TSMatrix, ncol = 1L)
  if (!is.matrix(TSMatrix) || !is.numeric(TSMatrix) ||
      nrow(TSMatrix) < 2L || ncol(TSMatrix) < 1L)
    stop("TSMatrix must be a numeric matrix with at least two rows and one column.",
         call. = FALSE)
  if (any(!is.finite(TSMatrix)))
    stop("TSMatrix must contain only finite values. Fill or remove missing values first.",
         call. = FALSE)
  if (!is.character(method) || length(method) != 1L || is.na(method))
    stop("method must be one method name.", call. = FALSE)
  method <- tolower(trimws(method))
  # Keep the old help-file spelling as an alias for the correct method name.
  if (method == "keogh.lb") method <- "lb.keogh"
  args <- list(...)
  # Declare PDC's m after ... so R cannot match it partially to method.
  # It must remain a named argument. Other methods reject it below.
  if (!missing(m)) args["m"] <- list(m)
  if (length(args) && (is.null(names(args)) || anyNA(names(args)) ||
      any(!nzchar(names(args))) || anyDuplicated(names(args))))
    stop("All arguments in ... must have unique, nonempty names.", call. = FALSE)
  if (any(names(args) %in% c("x", "y", "X", "Data", "TSMatrix", "series")))
    stop("Do not supply data arguments through ... .", call. = FALSE)
  if (method == "dtw" &&
      any(names(args) %in% c("dim", "outputisvector", "GPU")))
    stop("DTW arguments must configure its local distance function.",
         call. = FALSE)

  # These methods have no verified replacement in current CRAN packages or
  # in TSclust. A different distance must not be substituted silently.
  unavailable <- c("erp", "tquest", "dissim")
  if (method %in% unavailable)
    stop("Method '", method, "' has no verified replacement in current CRAN ",
         "packages or TSclust. TSdist is no longer used by this function.",
         call. = FALSE)
  # The old help listed wav, but TSdist did not accept it. TSclust's
  # diss.DWT is a disabled stub that returns -1, not a distance.
  if (method == "wav")
    stop("Method 'wav' is not implemented by TSclust. Its diss.DWT function ",
         "is disabled. The old TSAT help listed this method in error.",
         call. = FALSE)

  legacy <- c(acf = "diss.ACF", pacf = "diss.PACF",
    ar.lpc.ceps = "diss.AR.LPC.CEPS", ar.mah = "diss.AR.MAH",
    ar.mah.statistic = "diss.AR.MAH", ar.mah.pvalue = "diss.AR.MAH",
    ar.pic = "diss.AR.PIC", cdm = "diss.CDM", cid = "diss.CID",
    cort = "diss.CORT", int.per = "diss.INT.PER", per = "diss.PER",
    ncd = "diss.NCD", pred = "diss.PRED", spec.glk = "diss.SPEC.GLK",
    spec.isd = "diss.SPEC.ISD", spec.llr = "diss.SPEC.LLR")
  basic <- c("euclidean", "manhattan", "minkowski", "infnorm")
  bi <- c(basic, "dtw", "cor", "sts", "fourier")
  other <- c("lb.keogh", "edr", "lcss", "mindist.sax", "pdc", "frechet",
             "ccor", "tam")
  if (!method %in% c(bi, other, names(legacy)))
    stop("Unknown method '", method, "'. See ?TSDistancesMatrix.", call. = FALSE)

  package <- if (method %in% bi) "BIDistances" else if (method %in% names(legacy))
    "TSclust" else switch(method, lb.keogh = "dtwclust",
      edr = "SimilarityMeasures", lcss = "SimilarityMeasures",
      mindist.sax = "jmotif", pdc = "pdc", frechet = "FKmL",
      ccor = "stats", tam = "dtw")
  # Load only the package selected by this method. TSclust is an optional,
  # archived fallback; its absence must not block the current CRAN methods.
  if (!requireNamespace(package, quietly = TRUE)) {
    if (package == "TSclust")
      stop("Method '", method, "' requires package 'TSclust'. CRAN archived ",
           "TSclust on 2026-01-16. Its namespace could not be loaded. ",
           "Install it from the CRAN archive to use this method.", call. = FALSE)
    stop("Method '", method, "' requires package '", package,
         "'. Install it from CRAN first.", call. = FALSE)
  }

  # Reject parameters that a selected calculation would otherwise ignore.
  allowed <- switch(method,
    euclidean = "threads", manhattan = "threads", infnorm = "threads",
    minkowski = c("p", "threads"), cor = "beta", sts = character(),
    fourier = "n", lb.keogh = "window.size", edr = "epsilon",
    lcss = c("epsilon", "sigma"), mindist.sax = c("w", "alpha"),
    pdc = c("m", "t"), frechet = "timeScale", ccor = "lag.max",
    tam = character(), NULL)
  if (!method %in% c("dtw", names(legacy))) {
    unknown <- setdiff(names(args), allowed)
    if (length(unknown))
      stop("Unsupported argument(s) for '", method, "': ",
           paste(unknown, collapse = ", "), ". See ?TSDistancesMatrix.",
           call. = FALSE)
  }
  n <- nrow(TSMatrix)
  m <- ncol(TSMatrix)
  result <- matrix(0, m, m, dimnames = list(colnames(TSMatrix), colnames(TSMatrix)))
  if (method == "ar.mah.pvalue") diag(result) <- 1

  if (method == "minkowski") {
    if (is.null(args$p)) args$p <- 2
    if (!is.numeric(args$p) || length(args$p) != 1L ||
        is.na(args$p) || args$p < 1)
      stop("p must be at least one. Inf selects the maximum distance.", call. = FALSE)
  }
  if (method %in% c("edr", "lcss")) {
    if (is.null(args$epsilon) || !is.numeric(args$epsilon) ||
        length(args$epsilon) != 1L || !is.finite(args$epsilon) || args$epsilon < 0)
      stop("Supply one finite, nonnegative epsilon for '", method, "'.", call. = FALSE)
  }
  if (method %in% c("lb.keogh", "mindist.sax", "fourier", "ccor", "lcss")) {
    parameter <- switch(method, lb.keogh = "window.size", mindist.sax = "w",
                        fourier = "n", ccor = "lag.max", lcss = "sigma")
    value <- args[[parameter]]
    if (is.null(value)) value <- switch(method,
      fourier = floor(n / 2) + 1L, ccor = n - 1L, lcss = n - 1L, NULL)
    minimum <- if (method == "lcss") 0 else 1
    maximum <- switch(method, lb.keogh = n - 1L, ccor = n - 1L, lcss = n - 1L, n)
    if (is.null(value) || !is.numeric(value) || length(value) != 1L ||
        !is.finite(value) || value != floor(value) || value < minimum || value > maximum)
      stop(parameter, " must be one integer from ", minimum, " to ", maximum, ".",
           call. = FALSE)
    args[[parameter]] <- as.integer(value)
  }
  if (method == "mindist.sax") {
    if (is.null(args$alpha)) args$alpha <- 4L
    if (!is.numeric(args$alpha) || length(args$alpha) != 1L ||
        !is.finite(args$alpha) || args$alpha != floor(args$alpha) ||
        args$alpha < 2 || args$alpha > 20)
      stop("alpha must be one integer from 2 to 20.", call. = FALSE)
    if (any(apply(TSMatrix, 2L, stats::sd) == 0))
      stop("mindist.sax requires nonconstant series.", call. = FALSE)
  }
  if (method == "frechet") {
    if (is.null(args$timeScale)) args$timeScale <- 0.1
    if (!is.numeric(args$timeScale) || length(args$timeScale) != 1L ||
        !is.finite(args$timeScale) || args$timeScale < 0)
      stop("timeScale must be one finite, nonnegative number.", call. = FALSE)
  }
  if (method == "cor" && !is.null(args$beta)) {
    if (!is.numeric(args$beta) || length(args$beta) != 1L ||
        !is.finite(args$beta) || args$beta < 0)
      stop("beta must be one finite, nonnegative number.", call. = FALSE)
  }
  # TSclust's default interpolation can produce NA even for finite input.
  # Direct integration (n = 0) computes the same spectral definition.
  if (method == "spec.isd" && is.null(args$n)) args$n <- 0
  if (m == 1L) return(result)

  if (method %in% basic) {
    # BIDistances compares rows. TSAT compares columns.
    backendMethod <- if (method == "infnorm" ||
                         (method == "minkowski" && is.infinite(args$p)))
      "maximum" else method
    if (backendMethod == "maximum") args$p <- NULL
    result <- do.call(BIDistances::DistanceMatrix,
                     c(list(X = t(TSMatrix), method = backendMethod), args))
  } else if (method == "dtw") {
    # DTW_Distance already compares columns. Its extra arguments configure
    # the local distance function, not the DTW alignment algorithm.
    result <- do.call(BIDistances::DistanceMatrix,
                     c(list(X = TSMatrix, method = "DTW_Distance"), args))
  } else if (method == "sts") {
    # Keep a matrix when there are only two observations per series.
    changes <- TSMatrix[-1L, , drop = FALSE] - TSMatrix[-n, , drop = FALSE]
    result <- BIDistances::DistanceMatrix(t(changes), method = "euclidean")
  } else if (method == "fourier") {
    coefficients <- apply(TSMatrix, 2L, stats::fft)[seq_len(args$n), , drop = FALSE]
    # Stack real and imaginary parts. Their Euclidean norm equals the
    # complex Fourier norm; taking only the real part would lose information.
    features <- rbind(Re(coefficients), Im(coefficients))
    result <- BIDistances::DistanceMatrix(t(features), method = "euclidean")
  } else if (method == "cor") {
    delta <- BIDistances::DistanceMatrix(t(TSMatrix), method = "pearsond")
    # BIDistances returns 1-r. Preserve the TSAT default sqrt(2*(1-r)).
    result <- if (is.null(args$beta)) sqrt(2 * pmax(delta, 0)) else
      sqrt((pmax(delta, 0) / pmax(2 - delta, 0))^args$beta)
    diag(result) <- 0
  } else {
    if (method == "mindist.sax") {
      symbols <- lapply(seq_len(m), function(j) {
        x <- TSMatrix[, j]
        reduced <- jmotif::paa((x - mean(x)) / stats::sd(x), args$w)
        jmotif::letters_to_idx(jmotif::series_to_chars(reduced, args$alpha))
      })
      saxTable <- jmotif::sax_distance_matrix(args$alpha)
    }
    # These functions can return a different value when the series are
    # reversed. Compute both directions instead of inventing symmetry.
    directed <- method %in% c("lb.keogh", "ccor", "cdm", "ncd", "tam")
    for (i in seq_len(m)) {
      for (j in seq_len(m)) {
        if (i == j || (!directed && i > j)) next
        x <- TSMatrix[, i]
        y <- TSMatrix[, j]
        if (method %in% names(legacy)) {
          value <- do.call(getExportedValue("TSclust", legacy[[method]]),
                           c(list(x = x, y = y), args))
          # The Maharaj function returns a list. A matrix cell needs one
          # statistic or p-value. The short name selects the statistic.
          if (method %in% c("ar.mah", "ar.mah.statistic")) value <- value$statistic
          if (method == "ar.mah.pvalue") value <- value$p_value
          if (method == "pred") value <- value$L1dist
        } else {
          value <- switch(method,
            lb.keogh = dtwclust::lb_keogh(x, y, window.size = args$window.size,
                                          norm = "L2")$d,
            edr = SimilarityMeasures::EditDist(matrix(x), matrix(y),
                                               pointDistance = args$epsilon),
            # LCSSCalc returns a match count. Convert it to a distance with
            # zero for a full match. Do not optimize translations of the data.
            lcss = 1 - SimilarityMeasures::LCSSCalc(matrix(x), matrix(y),
                      pointSpacing = args$sigma, pointDistance = args$epsilon) / n,
            mindist.sax = {
              # The jmotif lookup table already contains squared distances.
              # Squaring it again (as min_dist 1.3.2 does) changes the formula.
              sqrt((n / args$w) * sum(saxTable[cbind(symbols[[i]], symbols[[j]])]))
            },
            pdc = as.numeric(do.call(pdc::pdcDist, c(list(X = cbind(x, y)), args))),
            frechet = FKmL::fredist(cbind(seq_len(n) * args$timeScale, x),
                                    cbind(seq_len(n) * args$timeScale, y),
                                    form = "scalar"),
            ccor = {
              cross <- stats::ccf(x, y, lag.max = args$lag.max, plot = FALSE)
              rho <- as.numeric(cross$acf)
              lag <- as.numeric(cross$lag)
              sqrt((1 - round(rho[lag == 0]^2, 5)) / sum(rho[lag < 0]^2))
            },
            tam = {
              path <- dtw::dtw(x, y)
              dx <- diff(path$index1)
              dy <- diff(path$index2)
              sum(dx == 0) / (n - 1) + sum(dy == 0) / (n - 1) +
                1 - sum(dx == 1 & dy == 1) / (n - 1)
            })
        }
        if (!is.numeric(value) || length(value) != 1L ||
            !is.finite(value) || value < 0)
          stop("Method '", method, "' did not return one finite, nonnegative ",
               "value for columns ", i, " and ", j, ".", call. = FALSE)
        result[i, j] <- as.numeric(value)
        # Copy by pair indices. Copying triangle vectors scrambles pairs
        # when there are at least four columns.
        if (!directed) result[j, i] <- result[i, j]
      }
    }
  }
  if (!is.matrix(result) || !is.numeric(result) ||
      !identical(dim(result), c(m, m)) || any(!is.finite(result)) || any(result < 0))
    stop("Method '", method, "' did not return a finite, nonnegative matrix.",
         call. = FALSE)
  dimnames(result) <- list(colnames(TSMatrix), colnames(TSMatrix))
  result
}
