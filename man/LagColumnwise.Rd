\name{LagColumnwise}
\alias{LagColumnwise}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
LagColumnwise
}
\description{
Generates Lagged features by not using information of futures in specific range
}
\usage{
LagColumnwise(Data, LagRange = 1:10)
}

\arguments{
  \item{Data}{
atrix of data
}
  \item{LagRange}{
numeric vector defining which lags to use
}
}

\value{
 numeric marix of data with lagged features without original data
}

\author{
MCT
}
\seealso{
\code{\link{LagVector}
}
}
\examples{
x=matrix(1:9,3,3)
colnames(x)=c("A","B","C")
LagColumnwise(x,LagRange = 1:2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
