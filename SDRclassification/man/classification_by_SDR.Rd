\name{classification_by_SDR}
\alias{classification_by_SDR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Making classification according to SDR
}
\description{
   SDR(standard deviation reduction) describes the reduction in standard deviation by comparing the standard deviation post-split.
   If the dependent variable are decided by one independent variable, and the dependent variable is divided into two parts according to the value of the independent variable. Then the greater the SDR is, the better our classification is.
   This function extends this classification method to multivariable problems. The function calculates the maximum SDR that can be obtained by classifying each variable, compares these SDRs, and selects the variable with the largest SDR for classification. Users can choose how many layers the data is divided into to get a result similar to decision tree.

}
\usage{
classification_by_SDR(level, data)
}

\arguments{
  \item{level}{
      A figure describes how many layers you want the decision tree to have.
}
  \item{data}{
     The data.frame document contains the dependent variable and every criteria. 
     PLEASE NOTICE: The dependent variable should be in the last column of the table data.
}
}

\author{
    Wendy
}


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (col_num, data) 
{
    qq <- length(data)
    try <- data.frame()
    BOUND <- data.frame()
    fanbase <- list(data)
    order <- data.frame()
    for (j in 1:col_num) {
        cat("layer", j)
        cat(sep = "\n")
        cc <- 2^(j - 1)
        c <- 2^j
        fan <- as.list(rep(1:c))
        length <- length(data)
        length <- length - 1
        for (k in 1:cc) {
            q <- nrow(fanbase[[k]])
            if (q > 3) {
                playsdr <- data.frame()
                for (e in 1:length) {
                  for (f in 2:(q - 2)) {
                    p <- fanbase[[k]][, c(e, qq)]
                    p <- p[order(p[, 1]), ]
                    t <- p[, 2]
                    m <- p[c(1:f), 2]
                    n <- p[-c(1:f), 2]
                    SDR <- sd(t) - (f/q * sd(m) + (q - f)/q * 
                      sd(n))
                    order <- rbind(order, SDR)
                  }
                  max <- which(order == max(order), arr.ind = TRUE)
                  TRY <- order[max[1, 1], 1]
                  MAX <- cbind(TRY, e)
                  order <- data.frame()
                  playsdr <- rbind(playsdr, MAX)
                }
                aa <- rep(1:length, times = 1)
                playsdr <- cbind(playsdr, aa)
                colnames(playsdr) <- c("sdr", "number")
                playsdr <- playsdr[order(playsdr$sdr, decreasing = TRUE), 
                  ]
                a <- playsdr[1, 2]
                for (i in 2:(q - 2)) {
                  p <- fanbase[[k]][, c(a, qq)]
                  p <- p[order(p[, 1]), ]
                  t <- p[, 2]
                  m <- p[c(1:i), 2]
                  n <- p[-c(1:i), 2]
                  SDR <- sd(t) - (i/q * sd(m) + (q - i)/q * sd(n))
                  try <- rbind(try, SDR)
                }
                max <- which(try == max(try), arr.ind = TRUE)
                amount1 <- p[max[, 1] + 1, 1]
                amount2 <- p[max[, 1] + 2, 1]
                bound <- mean(amount1, amount2)
                BOUND <- rbind(BOUND, bound)
                cat("group", k, ",criteria:column", a, ",threshold:", 
                  bound)
                cat(sep = "\n")
                fan[[2 * k - 1]] <- fanbase[[k]][c(1:(max[, 1] + 
                  1)), ]
                fan[[2 * k]] <- fanbase[[k]][-c(1:(max[, 1] + 
                  1)), ]
            }
            else {
                sub <- 0
                sub <- data.frame(sub)
                fan[[2 * k - 1]] <- fanbase[[k]]
                fan[[2 * k]] <- sub
            }
            try <- data.frame()
        }
        fanbase <- fan
        cat(sep = "\n")
    }
  }
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
