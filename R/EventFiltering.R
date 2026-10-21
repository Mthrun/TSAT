EventFiltering <- function(EventNameorValue, EventArrayOrEvent, Silent = FALSE) {
  if (!is.logical(Silent) || length(Silent) != 1L || is.na(Silent))
    stop("Silent must be TRUE or FALSE.", call. = FALSE)
  if (length(EventNameorValue) != 1L || is.na(EventNameorValue))
    stop("Supply one nonmissing event label.")
  if (is.data.frame(EventArrayOrEvent)) EventArrayOrEvent <- as.matrix(EventArrayOrEvent)
  if (!is.atomic(EventArrayOrEvent) || length(dim(EventArrayOrEvent)) > 2L)
    stop("Supply an event vector, matrix, or data frame.")
  # Match directly. Sorting zero-filled rows loses negative and small text labels.
  matches <- !is.na(EventArrayOrEvent) & EventArrayOrEvent == EventNameorValue
  feature <- if (is.null(dim(matches))) as.integer(matches) else
    as.integer(rowSums(matches) > 0)
  if (!Silent && !any(feature)) warning("Event could not be found in data.")
  feature
}
