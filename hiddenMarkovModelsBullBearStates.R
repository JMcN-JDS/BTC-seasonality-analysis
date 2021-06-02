# ------------------------------------------------------------------------------

# Might be best to write any code for this in a separate file.

# Hidden Markov Models for Regime Detection using R

# source: https://www.quantstart.com/articles/hidden-markov-models-for-regime-detection-using-r/

install.packages("depmixS4", dependencies=TRUE)
install.packages("quantmod", dependencies=TRUE)
library(depmixS4)
library(quantmod)
set.seed(1)

# At this stage a two-regime market will be simulated. 
# This is achieved by assuming market returns are normally distributed. 
# Separate regimes will be simulated with each containing  days of returns. 
# Each of the  regimes will be bullish or bearish. 
# The goal of the Hidden Markov Model will be to identify when the regime has switched from bullish to bearish and vice versa.

# In this example k=5 and N_k within [50,150]. 
# The bull market is distributed as weirdN(0.1,0.1) 
# while the bear market is distributed as weirdN(-0.05, 2). 
# The parameters are set via the following code:

# Create the parameters for the bull and
# bear market returns distributions
Nk_lower <- 50
Nk_upper <- 150
bull_mean <- 0.1
bull_var <- 0.1
bear_mean <- -0.05
bear_var <- 0.2

# The N_k values are randomly chosen:

# Create the list of durations (in days) for each regime
days <- replicate(
  5, 
  sample(Nk_lower:Nk_upper, 1)
  )

# The returns for each kth period are randomly drawn:
  
# Create the various bull and bear markets returns
market_bull_1 <- rnorm( days[1], bull_mean, bull_var ) 
market_bear_2 <- rnorm( days[2], bear_mean, bear_var ) 
market_bull_3 <- rnorm( days[3], bull_mean, bull_var ) 
market_bear_4 <- rnorm( days[4], bear_mean, bear_var ) 
market_bull_5 <- rnorm( days[5], bull_mean, bull_var )

# The R code for creating 
# (i) the true regime states (either 1 for bullish or 2 for bearish) and 
# final list of returns is given by the following:
  
# Create the list of true regime states and full returns list
true_regimes <- c(
  rep(1,days[1]), 
  rep(2,days[2]), 
  rep(1,days[3]), 
  rep(2,days[4]), 
  rep(1,days[5])
  )
returns <- c(
  market_bull_1, 
  market_bear_2, 
  market_bull_3, 
  market_bear_4, 
  market_bull_5
  )

# Plotting the returns shows 
# the clear changes in mean and variance between the regime switches:
  
plot(
  returns, 
  type="l", 
  xlab='', 
  ylab="Returns"
  )

# At this stage the Hidden Markov Model can be specified and fit using the Expectation Maximisation algorithm, 
# the details of which are beyond the scope of this article. 
# The family of distributions is specified as gaussian and the number of states is set to two (nstates = 2):
  
# Create and fit the Hidden Markov Model
hmm <- depmix(
  returns ~ 1, 
  family = gaussian(), 
  nstates = 2, 
  data=data.frame(returns=returns)
  )
hmmfit <- fit(
  hmm, 
  verbose = FALSE
  )

# Subsequent to model fitting it is possible to plot the posterior probabilities of being in a particular regime state. 
# post_probs contain the posterior probabilities. 
# These are compared with the underlying true states. 
# Notice that the Hidden Markov Model does a good job of correctly identifying regimes, albeit with some lag:

# Output both the true regimes and the 
# posterior probabilities of the regimes
post_probs <- posterior(hmmfit)
layout(1:2)
plot(
  post_probs$state, 
  type='s', 
  main='True Regimes', 
  xlab='', ylab='Regime')
matplot(
  post_probs[,-1], 
  type='l', 
  main='Regime Posterior Probabilities', 
  ylab='Probability'
  )
legend(
  x='topright', 
  c('Bull','Bear'), 
  fill=1:2, 
  bty='n'
  )

# The discussion will now turn towards applying the Hidden Markov Model to real world historical financial data.

# Financial Data

# In the above section it was straightforward for the Hidden Markov Model to determine regimes 
# because they had been simulated from pre-specified set of Gaussians. 
# As stated above the problem of Regime Detection is actually an unsupervised learning challenge 
# since the number of states is not known a priori, 
# nor is there any "ground truth" on which to "train" the HMM.

# In this section two separate modelling tasks will be carried out. 
# The first will involve fitting the HMM with two regime states to S&P500 returns, 
# while the second will utilise three states. 
# The results between the two models will be compared.

# The process for applying the Hidden Markov Model provided by depmixS4 is similar to that carried out for the simulated data. 
# Instead of generating the returns stream from two Gaussian distributions it will simply be downloaded using the quantmod library:
  
# Obtain S&P500 data from 2004 onwards and
# create the returns stream from this
getSymbols( "^GSPC", from="2004-01-01" )
gspcRets = diff( log( Cl( GSPC ) ) )
returns = as.numeric(gspcRets)

# The gspcRets time series object can be plotted, showing the volatile periods around 2008 and 2011:
  
plot(gspcRets)

# As before a two-state Hidden Markov Model is fitted using the EM algorithm. 
# The returns and posterior probabilities of each regime are plotted:
  
# Fit a Hidden Markov Model with two states 
# to the S&P500 returns stream
hmm <- depmix(
  returns ~ 1, 
  family = gaussian(), 
  nstates = 2, 
  data=data.frame(returns=returns)
  )
hmmfit <- fit(
  hmm, 
  verbose = FALSE
  )
post_probs <- posterior(hmmfit)

# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)

plot(
  returns, 
  type='l', 
  main='Regime Detection', 
  xlab='', ylab='Returns')

matplot(
  post_probs[,-1], 
  type='l', 
  main='Regime Posterior Probabilities', 
  ylab='Probability'
  )
legend(
  x='bottomleft', 
  c('Regime #1','Regime #2'), 
  fill=1:2, 
  bty='n'
  )

# Notice that within 2004 and 2007 the markets were calmer and hence the Hidden Markov Model has given high posterior probability to Regime #2 for this period. 
# However between 2007-2009 the markets were incredibly volatile due to the sub-prime crisis. 
# This has the initial effect of rapidly changing the posterior probabilities between the two states but being fairly consistently in Regime #1 during 2008 itself.

# The markets became calmer in 2010 but additional volatility occurred in 2011, 
# leading once again for the HMM to give high posterior probability to Regime #1. 
# Subsequent to 2011 the markets became calmer once again and the HMM is consistently giving high probability to Regime #2. 
# In 2015 the markets once again became choppier and this is reflected in the increased switching between regimes for the HMM.

# The same process will now be carried out for a three-state HMM. 
# There is little to modify between the two, 
# with the exception of modifying nstates = 3 and adjusting the plotting legend:

# Fit a Hidden Markov Model with three states 
# to the S&P500 returns stream
hmm <- depmix(
  returns ~ 1, 
  family = gaussian(), 
  nstates = 3, 
  data=data.frame(returns=returns)
  )
hmmfit <- fit(
  hmm, 
  verbose = FALSE
  )
post_probs <- posterior(hmmfit)

# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(
  returns, 
  type='l', 
  main='Regime Detection', 
  xlab='', 
  ylab='Returns'
  )
matplot(
  post_probs[,-1], 
  type='l', 
  main='Regime Posterior Probabilities', 
  ylab='Probability'
  )
legend(
  x='bottomleft', 
  c('Regime #1','Regime #2', 'Regime #3'), 
  fill=1:3, 
  bty='n'
  )

# The length of data makes the posterior probabilities chart somewhat trickier to interpret. 
# Since the model is forced to consider three separate regimes it leads to a switching behaviour between Regime #2 and Regime #3 in the calmer period of 2004-2007. 
# However in the volatile periods of 2008, 2010 and 2011, Regime #1 dominates the posterior probability indicating a highly volatile state. 
# Subsequent to 2011 the model reverts to switching between Regime #2 and Regime #3.

# It is clear that choosing the initial number of states to apply to a real returns stream is a challenging problem. 
# It will depend upon the asset class being utilised, how the trading for that asset is carried out as well as the time period chosen.

# Next Steps

# In subsequent articles the Hidden Markov Model will be used by a RiskManager subclass in the QSTrader backtesting and live trading engine. 
# It will determine when to apply a trend following strategy in an attempt to improve profitability over the case of no risk management.


# Attempting to use the 2010-2020 BTC price data for these models

# create the returns stream from this

# Note: 2014-09-15 is the earliest available Yahoo daa fro BTC

symbol.vec = c("BTC-USD")
getSymbols(symbol.vec, src="yahoo", from="2014-09-15", to="2021-06-01")
BTC = `BTC-USD`[, c("BTC-USD.Adjusted", "BTC-USD.Close"), drop=FALSE, na.omit()]

print(head(`BTC-USD`, 10))
print(head(BTC, 10))
print(head(GSPC, 10))

btcRets = diff( log( Cl( BTC ) ) )
returnsBtc = as.numeric(btcRets)

# The btcRets time series object can be plotted, showing the volatile periods around 2008 and 2011:

plot(btcRets)

# As before a two-state Hidden Markov Model is fitted using the EM algorithm. 
# The returns and posterior probabilities of each regime are plotted:

# Fit a Hidden Markov Model with two states 
# to the BTC returns stream

hmmBtc <- depmix(
  returnsBtc ~ 1, 
  family = gaussian(), 
  nstates = 2, 
  data=data.frame(returnsBtc=returnsBtc)
)
hmmfitBtc <- fit(
  hmmBtc, 
  verbose = FALSE
)
post_probs_btc <- posterior(hmmfitBtc)

# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)

plot(
  returnsBtc, 
  type='l', 
  main='Regime Detection (BTC)', 
  xlab='', ylab='Returns')

matplot(
  post_probs_btc[,-1], 
  type='l', 
  main='Regime Posterior Probabilities (BTC)', 
  ylab='Probability'
)
legend(
  x='bottomleft', 
  c('Regime #1','Regime #2'), 
  fill=1:2, 
  bty='n'
)

# The same process will now be carried out for a three-state HMM. 
# There is little to modify between the two, 
# with the exception of modifying nstates = 3 and adjusting the plotting legend:

# Fit a Hidden Markov Model with three states 
# to the BTC returns stream
hmmBtc <- depmix(
  returnsBtc ~ 1, 
  family = gaussian(), 
  nstates = 3, 
  data=data.frame(returnsBtc=returnsBtc)
)
hmmfitBtc <- fit(
  hmmBtc, 
  verbose = FALSE
)
post_probs_btc <- posterior(hmmfitBtc)

# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(
  returnsBtc, 
  type='l', 
  main='Regime Detection', 
  xlab='', 
  ylab='Returns'
)
matplot(
  post_probs_btc[,-1], 
  type='l', 
  main='Regime Posterior Probabilities', 
  ylab='Probability'
)
legend(
  x='bottomleft', 
  c('Regime #1','Regime #2', 'Regime #3'), 
  fill=1:3, 
  bty='n'
)
