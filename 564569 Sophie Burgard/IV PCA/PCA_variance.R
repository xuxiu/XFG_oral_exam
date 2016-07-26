# ----------------------------------------------------------------------
# Book:         XFG
# ----------------------------------------------------------------------
# See also:     XFGiv00, XFGiv01, XFGiv02, XFGiv03
# ----------------------------------------------------------------------
# Quantlet:      XFGiv04
# ----------------------------------------------------------------------
# Description:   XFGiv04 explains variance components of PCA for the ATM
#               implied volatilities of the ATM implied volatility data 
#               set (implvola.dat). The two dominant PCs together explain
#               around 83 percent of the total variance in implied ATM 
#               volatilities for DAX options.
# ----------------------------------------------------------------------
# Keywords:     spectral decomposition, eigenvalues, eigenvectors,
#               option, implied volatility, covariance, PCA,
#               principal component, 
# ----------------------------------------------------------------------
# Usage:         -
# ----------------------------------------------------------------------
# Author:        Awdesch Melzer 20140528
# ----------------------------------------------------------------------

# clear cache and close windows
rm(list=ls(all=TRUE))
graphics.off()

setwd("/Users/Sophie/Dropbox/XFG/IV PCA")

x          = read.csv2("VSMI.csv", header = TRUE, stringsAsFactors = FALSE) # load data
x          = as.data.frame(sapply(x[,2:7], as.numeric))  
x          = x*100                      # scale
n          = nrow(x)                    # number of rows
z          = x[2:n,] - x[1:(n-1),]      # first difference
s          = cov(z)*100000              # covariance of returns
tmp        = eigen(s)                   # spectral decomposition
l          = tmp$values                 # eigenvalues
g          = tmp$vectors                # eigenvectors

VarExpl    = l/(matrix(1,1,6)%*%l)*100  # percent of explained variance
print("Variance explained in each component:")
paste(round(VarExpl,3))

CumVarExpl = cumsum(l/(matrix(1,1,6)%*%l)*100) # cumulated variance explained
print("Cumulated Variance:")
paste(round(CumVarExpl,3))
