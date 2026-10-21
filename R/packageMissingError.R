packageMissingError = function(packageName, n, SplitAt) {
  if (!requireNamespace(packageName, quietly = TRUE)) {
    errorMessage = paste0('Subordinate package (', packageName, ') is missing. No computations are performed.
            Please install the package which is defined in "Suggests".')
    message(errorMessage)
    return(errorMessage)
  }
  return(FALSE)
}
