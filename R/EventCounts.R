EventCounts <- function(Events, PlotIt = FALSE, main = "Histogram of Events", xlab = "Event Names") {
  if (!is.atomic(Events) || !is.null(dim(Events))) stop("Events must be a vector.")
  if (!is.logical(PlotIt) || length(PlotIt) != 1L || is.na(PlotIt))
    stop("PlotIt must be TRUE or FALSE.", call. = FALSE)
  events <- Events[!is.na(Events)]
  # Count the actual labels. A sequence from one omits zero and negative events.
  labels <- sort(unique(events))
  counts <- tabulate(match(events, labels), nbins = length(labels))
  index <- order(counts, decreasing = TRUE)
  counts <- counts[index]
  labels <- as.character(labels[index])
  if (PlotIt && length(counts)) PlotEventCounts(counts, Names = labels, main = main, xlab = xlab)
  cbind(Breaks = as.character(seq_along(counts)), Counts = as.character(counts), Names = labels)
}
