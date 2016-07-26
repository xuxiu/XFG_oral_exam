# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()
setwd("/Users/Sophie/Dropbox/XFG/IV PCA")
# load data
x = read.csv2("VSMI.csv", header = TRUE, stringsAsFactors = FALSE) # load data
x = as.data.frame(sapply(x[,2:7], as.numeric))  

# rescale
x = x/100

# number of rows
n = nrow(x)

# compute first differences
z = apply(x,2,diff)
# calculate covariance
s = cov(z) * 1e+05

# determine eigenvectors
e = eigen(s)
e = e$vectors

f1 = e[, 1]
f2 = e[, 2]

# Adjust second Eigenvector in R not necessary - the computation differs from R to Matlab 

# Plot
plot(f1, col = "blue3", ylim = c(-1, 0.5), lwd = 2, type = "l", xlab = "Time", 
     ylab = "Loading")
points(f1)
lines(f2, col = "red3", lwd = 2, lty = "dotdash")
points(f2) 