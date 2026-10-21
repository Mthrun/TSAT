\name{ArbitratedDynamicEnsemble}
\alias{ArbitratedDynamicEnsemble}
\title{Train an arbitrated dynamic ensemble}
\description{Train the base models and error models of a tsensembler ensemble.}
\usage{
ArbitratedDynamicEnsemble(Data, formula, specs, \dots) 
}
\arguments{
  \item{Data}{Training data frame in time order. Include the response and already prepared predictors.}
  \item{formula}{Two-sided formula. All named variables must be columns of Data.}
  \item{specs}{A tsensembler model_specs object.}
  \item{\dots}{Additional arguments for tsensembler::ADE, such as lambda, omega, and num_cores.}
}
\details{
This function trains a model. It does not split data, construct lags, or generate forecast timestamps. Supply only training rows. Use tsensembler::predict, tsensembler::forecast, and the native update methods with appropriate future predictors. The optional tsensembler package is archived on CRAN. See \url{https://github.com/vcerqueira/tsensembler}.
}
\value{
Native tsensembler ADE model.
}
\examples{
if (requireNamespace("tsensembler",quietly=TRUE)) {
  set.seed(7)
  y <- as.numeric(arima.sim(list(ar=0.6),n=120))
  train <- data.frame(y=y[-1],lag1=head(y,-1))
  specs <- tsensembler::model_specs(learner=c("bm_ppr","bm_glm"),
    learner_pars=list(bm_ppr=list(nterms=1),bm_glm=list(alpha=0)))
  fit <- ArbitratedDynamicEnsemble(train,y ~ lag1,specs,lambda=10,num_cores=1)
  prediction <- tsensembler::predict(fit,newdata=tail(train,2))
  stopifnot(methods::is(fit,"ADE"),all(is.finite(prediction@y_hat)))
}
}

\keyword{ts}
