\name{ShortTimeFT}
\alias{ShortTimeFT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Short Time Fourier Transformation
}
\description{
Short Time Fourier Transformation
}
\usage{
ShortTimeFT(Datavec, LenWindow = 8, TypeWindow = "gaussian", GaussianApha = 1,
  PlotIt = TRUE, Window, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Datavec}{
[1:n], numerical vector of data
}
  \item{LenWindow}{
Width of the window defined as a length which is typical a small fraction of data
}
  \item{TypeWindow}{
"hanning", "gaussian" or "specific" can be selected
}
  \item{GaussianApha}{
standard deviation of gaussian windwo
}
  \item{PlotIt}{
plots the spectrogram
}
  \item{Window}{
if specific provide window in convention of \code{\link[signal]{specgram}}
}
  \item{\dots}{
see \code{\link[e1071]{stft}} for hanning or
see \code{\link[signal]{specgram}}
}
}
\details{
Gaussian Window results in Gabor Transformation

I would be better if data length n is dividable with 2 (e.g. 1024), otherwise approximations are made internally.

"specific" requires a vector which is the result of a window function of \pkg{signal}
}
\value{
List with 
\item{Amplitude}{Amplitude[1:m:1k] numerical matrix}
\item{Spectrogram}{\code{\link[e1071]{stft}} or \code{\link[signal]{specgram}}
}
}
\references{
Lectures of MCT
}
\author{
Michael C. Thrun
}
\note{
Transformations of Tics to frequencies in Hz are not provided so far..

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[e1071]{stft}},

\code{\link[signal]{specgram}}
}
\examples{
if (requireNamespace("signal",quietly=TRUE)) {
  y <- sin(seq_len(256)/8)
  out <- ShortTimeFT(y,LenWindow=8,PlotIt=FALSE)
  stopifnot(is.matrix(out$Amplitude), all(is.finite(out$Amplitude)))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stft}% use one of  RShowDoc("KEYWORDS")
\keyword{Gabor}% __ONLY ONE__ keyword per line
\keyword{short time fourier analysis}% __ONLY ONE__ keyword per line
\keyword{gabor transformation}% __ONLY ONE__ keyword per line