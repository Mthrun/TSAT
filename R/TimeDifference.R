# Keep the legacy internal name in its own file.
TimeDifference <- function(Timevector, BeginInd, EndInd, Resolution = "mins", Silent = FALSE) {
  EventIndDuration(Timevector, BeginInd, EndInd, Resolution, Silent)
}
