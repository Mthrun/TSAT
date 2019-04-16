\name{LSTM}
\alias{LSTM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Long Short Term Memory Based on Recurrent Neural Network [Hochreiter, 1997].
}
\description{
In simple words the feedword network feeds input not only forward from layer to layer but also in a loop back to specific layers which is called recurrent. LSTM is an improvment in the case of 'vanishing gradients'.

The procedure of this method works as follows:
We start out with centered and scaled time series data (not necessary: we just need a time series varying in the interval [-1,1]) provided from a numerical vector \code{data} (hence equidistant). Furthermore one has to set a forecast length \code{forecast_length}. We then subset \code{data} in the subsequent manner:

* divide \code{data} into slices of length \code{forecast_length}. (atleast 4 slices, hence \code{length(data) / forecast_length} has to be atleast 4)

* if there is overhead (i.e. \code{length(data)} is not divisible by \code{forecast_length}) it is neglected at the end of the data, meaning the forecast horizon.

* the last slice is the varification set - it is the aim set to forecast and will not be used for our calculations

* the remaining slices build up the learning set.

* the learning set has another two subsets: the training set and the lag set. The training set is the tail of the learning set, while the lag set is the head of the learning set, where we neglect one slice at each end, respectively. (thats why the learning set has to be twice as long as the forecast_length)

* We then use keras to train a RNN to forecast the training set out of the lag set.

* After the learning process we feed the last slice of the learning set to it. We expect it to forecast the validation set.
}
\usage{
LSTM(vec_data, forecast_length, batch_size = HighestPrimePotence(forecast_length),

              hidden_layers = 1, time_steps = 1, epochs = 100, neurons = 50,
              
              stateful = TRUE, loss_function, optimizer = "adam",
              
              plot_evaluation = FALSE, plot_future = TRUE, 
              
              activation_function = c('tanh','hard_sigmoid'))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec_data}{
Regular (equidistant) time series data as numerical vector. We will split this time series up into atleast 4 parts of length \code{forecast_length}. We expect this data to be centered and scaled.
}
  \item{hidden_layers}{
Number of hidden layers
}
  \item{forecast_length}{
Number of periods to forecast into the future. For LSTM it is necessary for the training set length to be a multiple of this. Therefore this number should be at max one fourth of the length of data.
}
  \item{batch_size}{
Number of samples per gradient update, see \code{batch_size} in \code{fit} in [keras]. Several conditions have to be met:

The batch size is the number of \code{data} samples in one forward/backward pass of a RNN before a weight update.

The batch size has to be a divisor of the validation set length. (And hence also of the training set length)

You can see the possible choices for this size using the \code{\link{Divisors}} method on \code{forecast_length}.

Per default we take the highest prime potence of forecast_length, i.e. the maximal p^a occuring in the prime factor decomposition of forecast_length, e.g. forecast_length = 40 = 2^3 * 5^1 => batch_size = 8 = 2^3. The batch size shouldn't be chosen too high in relation to the forecast_length.
}
  \item{time_steps}{
 A time step is the number of lags included in the training/testing set, number of time step for first layer, see \code{batch_size} in \code{\link[keras]{layer_lstm}}.
}
  \item{epochs}{
Number of epochs to train the model, see \code{batch_size} in \code{fit} in [keras].
}
  \item{activation_function}{
Vector of two strings, first defines the function of activation to use the second one the recurrent activation, please see [Goodfellow, 2016] for details.
}

  \item{neurons}{
Number of units per layer, see \code{units} in \code{\link[keras]{layer_lstm}}.}
  \item{stateful}{
The last state for each sample at index i in a batch will be used as initial state for the sample of index i in the following batch, see \code{stateful} in \code{\link[keras]{layer_lstm}}.
}
  \item{loss_function}{
Objective function which shoud be minimized, see \code{loss} in \code{\link{compile}} in [keras], if you want to use a pre-coded function. You can also put in custom loss functions if you write it in keras backend syntax. (e.g. \code{tensor_srd}}

  \item{optimizer}{
Minimization approach, e.g. 'Adam' [Kingma/Ba, 2014], see \code{optimizer} in \code{\link{compile}} in [keras].
}
  \item{plot_evaluation}{
Plot output of training and testing data regarding loss function as defined by keras.
}
  \item{plot_future}{
A logical determining whether or not to plot the forecast in comparison to the validation set.
}
}
\details{
In this approach the recurrent ANN has several internal parameters set as defined in deep learning, see [Goodfellow, 2016] for details. The last layer is a densely-connected NN layer. Currently only one hidden-layer exists.

The \code{epochs} are the total number of forward/backward pass iterations. Typically more improves model performance unless overfitting occurs at which time the validation accuracy/loss will not improve.

\code{data} should be scaled between [-1,1] with "sound" distribution, see [Goodfellow, 2016; Mörchen 2006].

Gradients are vanishing if inputs between zero and one are multiplied several times, because then  gradient can shrink to zero. The result is the weights would not change signifiantly in an recurrent ANN of many layers ('deep learning').
}
\value{
List of 
\item{Model}{ Pointer to an ANN model generated by \pkg{keras}, the model is not directly available in R}
\item{FitStats}{Output of \code{fit} in [keras]}
\item{Forecast}{Forecast generated by the ANN model where we put in the last portion of the training set of length \code{forecast_length} as data to predict from. The validation data stays untouched.}
\item{TestSet}{[(k+1):n], the part of Response not used in the model}
\item{TrainSet}{[1:k], the part of Response used in the model}

}
\references{
Goodfellow, I., Bengio, Y., Courville, A., & Bengio, Y.: Deep learning, (Vol. 1), Cambridge: MIT press, 2016.

Kingma, D. P., Ba, J.: Adam: A method for stochastic optimization, arXiv preprint arXiv:1412.6980, 2014.

Hochreiter, Sepp, Jürgen Schmidhuber: "Long short-term memory.", Neural computation, Vol 9.8, pp. 1735-1780, 1997.

Mörchen, Fabian; Time series knowledge mining. Görich & Weiershäuser, 2006.
}
\author{
Michael Thrun, Julian Maerte
}
\note{
\code{keras} and \code{tensorflow} have to be installed in python, python can be called from console
}

\seealso{
\pkg{keras} and \pkg{tensorflow}.
}
\examples{
\donttest{
# Sunspots with autocorrelation for a lag of 10 years above 0.5
# (maximum at 125 months but we will neglect this due to less divisors)
data = datasets::sunspot.month

# subset the dataset such that we can learn of 
# it really quick and in best case it is divisible by 120 = forecast_length
# That is not necessary, but if we want 
# the forecast to reach the end of the input data the divisibility has to be given.
n = length(data)
sub = tail(data, 720)

# scale the subset, reduce the extent of outliers by sqrt
sub = sqrt(sub)
quants = quantile(sub, c(0.01, 0.5, 0.99), na.rm = F)
min = quants[1]
max = quants[3]
denom = max - min
sub = (sub - min) / denom

# We are ready to apply the LSTM procedure with a batch_size, 
# that divides the forecast_length, e.g. 40
v = LSTM(vec_data = sub, forecast_length = 120, batch_size = 40, epochs=300)

# Get the forecast data from the returned
fc = v$Forecast

# Rescale the forecast data to be comparable to the original dataset
fc_rescaled = (denom * fc + min)^2

# Plot out the forecast (in tail use the forecast_length, here 120)
plot(tail(data, 120), type="l")
lines(fc_rescaled, col="red")
}
}
\keyword{LSTM}% use one of  RShowDoc("KEYWORDS")
\keyword{keras}% __ONLY ONE__ keyword per line
