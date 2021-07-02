\name{OptionPrice-package}
\alias{OptionPrice-package}
\alias{OptionPrice}
\docType{package}
\title{
       Calculation of option price
}
\description{
       This function will use the binary tree pricing method to calculate the value of options for you. This function can calculate the value of European call option, American call option, European put option and American put option.
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{OptionPrice}
\packageIndices{OptionPrice}

}
\author{
\packageAuthor{OptionPrice}

Maintainer: \packageMaintainer{OptionPrice}
}
\references{
       Nothing
}

\keyword{ package }
\seealso{
       Nothing
}
\examples{
       Option_price(s_now, strike_price, sigma, r, q, T, n, Call_or_Put). 
s_now is the stock price now;
strike_price is the strike price of the option;
sigma is stock price volatility;
r is risk-free interest rate;
q is the dividend rate;
T is the validity period of the option;
n is the number of option cycles;
Call_or_Put is whether the option is call option or put option, if it is call option, then Call_or_Put=1, if it is put option, then Call_or_Put=2.
}
