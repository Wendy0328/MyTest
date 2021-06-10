\name{SDRclassification-package}
\alias{SDRclassification-package}
\alias{SDRclassification}
\docType{package}
\title{
   Making classification according to SDR
}
\description{
   SDR(standard deviation reduction) describes the reduction in standard deviation by comparing the standard deviation post-split.
   If the dependent variable are decided by one independent variable, and the dependent variable is divided into two parts according to the value of the independent variable. Then the greater the SDR is, the better our classification is.
   This function extends this classification method to multivariable problems. The function calculates the maximum SDR that can be obtained by classifying each variable, compares these SDRs, and selects the variable with the largest SDR for classification. Users can choose how many layers the data is divided into to get a result similar to decision tree.
}
\details{

    The function should be used in this way:
    classification_by_SDR(level,data)
    "level" is a figure describes how many layers you want the decision tree to have; "data" is the data.frame document contains the dependent variable and every criteria. PLEASE NOTICE: The dependent variable should be in the last column of the table data.
}
\author{
\packageAuthor{SDRclassification}

Maintainer: \packageMaintainer{SDRclassification}
}

\examples{
     
     classification_by_SDR(4,data)
}
