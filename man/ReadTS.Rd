\name{ReadTS}
\alias{ReadTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read TS file
}
\description{
Reads in a TS file.
}
\usage{
ReadTS(FileName, InDirectory = getwd(), ForceNumeric = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{FileName}{
Filename of *.TS file
}
  \item{InDirectory}{
InDirectory where *.TS file is, default: Current dir
}
  \item{ForceNumeric}{
TRUE: saves numeric format of series, FALSE: Accepts text
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
List with
\item{Series}{[1:n,1:2] matrix of Time and Seriesval, 1st column is Unix time or time in seconds, unique index from first column, Series contained in Column 2, without blanks}
\item{FurtherTexts}{[1:n,1:c] vector or matrix, containing the further text rows. Only returned if FurtherTexts are in the *.TS file}
\item{Header}{[1:(2+c)] vector of column names for Time and SeriesVal plus c column names of further text rows that can be in the *.TS file. If no further texts are present c is 0}
\item{Comments}{[1:k] vector, strings of all lines of Comments,  without the leading "#"}
}
\author{
Michael Thrun}
\note{
%%  ~~further notes~~
}

\seealso{
\link{WriteTS}
}
\examples{
WriteTS("ElectricityBRD", ElectricityBRD$Time, ElectricityBRD$Mrd_KWh, Comments="Electricity production in BRD")
V=ReadTS("ElectricityBRD")
V$Series
V$Header
V$Comments
}
\keyword{TimeSeries}
\keyword{read}
\keyword{load}
\keyword{TS}