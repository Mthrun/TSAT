# Apply one grouping rule to every column. Separate recursive paths previously
# lost Period, time zone, and column names for matrix and data-frame inputs.
aggregateCalendarData <- function(Time, Data, Group, FUN, Header = NULL, ...) {
  data <- if (is.null(dim(Data))) data.frame(Data = Data) else as.data.frame(Data)
  if (!length(Time) || NROW(data) != length(Time) || !NCOL(data) ||
    length(Group) != length(Time) || anyNA(Group))
    stop("Supply one valid timestamp per data row and at least one data column.")
  if (!all(vapply(data, is.numeric, logical(1)))) stop("Data columns must be numeric.")
  groups <- sort(unique(Group))
  result <- data.frame(Time = groups)
  for (j in seq_len(NCOL(data))) {
    values <- lapply(groups, function(group) FUN(data[[j]][Group == group], ...))
    if (any(lengths(values) != 1L) || !all(vapply(values, is.numeric, logical(1))))
      stop("FUN must return one numeric value per group and column.")
    result[[j + 1L]] <- unlist(values, use.names = FALSE)
  }
  names(result) <- c("Time", names(data))
  if (!is.null(Header)) {
    if (length(Header) == 1L) Header <- make.unique(rep(Header, NCOL(data)))
    if (length(Header) == NCOL(data)) names(result) <- c("Time", Header)
    else if (length(Header) == NCOL(result)) names(result) <- Header
    else stop("Header must name the data columns, or all output columns.")
  }
  result
}
