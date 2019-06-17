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
ShortTimeFT(Datavec, LenWindow,

TypeWindow, GaussianApha=1,

PlotIt=TRUE,Window, ...)
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
t0=seq(from=0.001,
to=2.5,
length.out = 256) 

y=c(cos(2*pi*1.0*t0),
    cos(2*pi*2.5*t0),
    cos(2*pi*5.0*t0),
    cos(2*pi*7.5*t0))

#10sec of ticks of time
t=seq(from=0.001,
to=10,
length.out = 256*4) 

\donttest{
plot(t,y,type='l',
ylab='Amplitude',xlab='Time in sec')
}
x=ShortTimeFT(y,WindowLen = 8,PlotIt = T)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{stft}% use one of  RShowDoc("KEYWORDS")
\keyword{Gabor}% __ONLY ONE__ keyword per line
\keyword{short time fourier analysis}% __ONLY ONE__ keyword per line
\keyword{gabor transformation}% __ONLY ONE__ keyword per line