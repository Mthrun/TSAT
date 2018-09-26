\name{HiddenMarkovModel}
\alias{HiddenMarkovModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
HiddenMarkovModel
}
\description{
HiddenMarkovModel using Rhmm package.

Procedure:

1. Selection of the number of states (Knowledge Discovery)

2. the Baum-Welch algorithm (generalized EM) determines the transition matrix P and the output probability

3. Veterbi algorithm: Determination of the probable sequence of states for a given observation,
i.e. clustering is reproduced as output

}
\usage{
HiddenMarkovModel(Data, ClusterNo, PriorClassification, PlotIt = TRUE, Silent = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
[1:n,1:d] d-dimensional data of n cases
}
  \item{ClusterNo}{
number of states, \code{k}
}
  \item{PriorClassification}{
Optional, to be compared with HMM
}
  \item{PlotIt}{
Optional, Plots the distributions of \code{HMMmodell}
}
  \item{Silent}{
Optional, additional outputs during the process of the function, e.g. \code{summary(HMMmodell)}
}
}
\details{
"Hidden", since only time series given, but no actual states. States cannot be observed
The output in a state is probalistic. The output in one state is therefore probalistic, since neither the transition probability nor the states are known. The number must be specified using a cluster procedure or from expert knowledge.
The states should have semantics. 
}
\value{
List of

  \item{HMMmodell}{
please see \code{\link[Rhmm]{HMMFit}}
}
  \item{HMM_means}{
[1:k] means of distributions, please see \code{\link[Rhmm]{distributionSet}}
}
  \item{HMM_SDs}{
[1:k] standard deviations of distributions, please see \code{\link[Rhmm]{distributionSet}}
}
  \item{HMM_weights}{
[1:k] weights of distributions
}
  \item{Uebergangsmatrix}{
Transition matrix, please see \code{\link[Rhmm]{HMMSet}}
}
  \item{VitPath}{
please see \code{\link[Rhmm]{viterbi}}
}
  \item{HMMcls}{
[1:n] classification defining which datapoint (case) belongs to which case
}
  \item{XTable}{
if \code{PriorClassification} given, compares to \code{HMMcls}
}
  \item{Accuracy}{
if \code{PriorClassification} given, compares to \code{HMMcls}
}

}
\references{
Bilmes, Jeff A.: A Gentle Tutorial of the EM Algorithm and its Application to Parameter Estimation for Gaussian Mixture and Hidden Markov Models http://ssli.ee.washington.edu/people/bilmes/mypapers/em.ps.gz, 1997.


}
\author{
Michael Thrun
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Baum-Welch-Algorithmus (generalisierter EM): \code{\link[Rhmm]{HMMFit}}

Viterbi-Algorithmus: \code{\link[Rhmm]{viterbi}},\code{\link[Rhmm]{HMMFit}}
}
\examples{
data(n1d_3s)
Data=as.matrix(obs_n1d_3s[[1]])
model=HiddenMarkovModel(Data,3,PlotIt = T,Silent = T)
\donttest{
TagNR=1:nrow(Data)
plot(TagNR,Data[,1],col=model$HMMcls,main='HMM states with Viterbi',pch=20)
legend("topright", inset = c(-0.2, 0), legend = sort(unique(model$HMMcls)), 
            fill =  sort(unique(model$HMMcls)))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{HMM}% use one of  RShowDoc("KEYWORDS")
\keyword{HiddenMarkovModel}% __ONLY ONE__ keyword per line
