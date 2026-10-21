# Keep the legacy internal name in its own file.
# The Alias suffix prevents a filename conflict on systems that ignore letter case.
difffilter <- function(x, BackTransformation = FALSE) {
  DiffFilter(x, BackTransformation = BackTransformation)
}
