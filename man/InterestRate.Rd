\name{InterestRate}
\alias{InterestRate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Final capital after compound interest
}
\description{
Calculates the final capital, after compounding interest
}
\usage{
InterestRate(initialCapital, interest, days)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initialCapital}{
Numeric value of initial capital
}
  \item{interest}{
Numeric value of the interest rate
}
  \item{days}{
Numeric value of days, for time of accumulation
}
}
\details{
Calculates the final capital through the formula \eqn{P \times \left(1 + \frac{r}{100}\right)^t},
where \eqn{P = initialCapital}, \eqn{r = interestRate}, \eqn{t = \frac{days}{365.25}}
}
\value{
Final capital value, given by the formula above
}

\author{
Michael Thrun}

\examples{
initialCapital = 25000
interestRate = 2
days = 365
finalCapital = InterestRate(initialCapital,interestRate,days)
compoundedInterest = finalCapital - initialCapital
}

\keyword{capital}
\keyword{interest}
\concept{compound interest}
\concept{accumulation factor}
