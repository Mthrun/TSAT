# Replace line breaks and repeated spaces before writing character columns.
cleanTableText <- function(x) {
  x <- gsub(pattern = "\t", replacement = " ", x)
  x <- gsub(pattern = "\n", replacement = " ", x)
  x <- gsub(pattern = "\r", replacement = " ", x)
  x <- gsub(pattern = "     ", replacement = " ", x)
  x <- gsub(pattern = "    ", replacement = " ", x)
  x <- gsub(pattern = "   ", replacement = " ", x)
  gsub(pattern = "  ", replacement = " ", x)
}
