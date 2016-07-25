# clear history
rm(list = ls(all = TRUE))
graphics.off()

# setwd('D:...') set working directory

# import data
for (i in 1:5) {
    a = paste("sp500iv_1", i, sep = "")
    assign(paste("iv_1", i, sep = ""), read.csv(paste(a, ".csv", 
        sep = ""), header = T, sep = ","))
}


# plot of volatility index prices
iv = rbind(iv_11, iv_12, iv_13, iv_14, iv_15)
date = as.Date(iv$Date, "%Y/%m/%d")
plot(iv$VXST_9d, type = "l", xlab = "Time", ylab = "Volatility Index Price", 
    col = "red3", xaxt = "n", main = "Volatility Index Price")
lines(iv$VIX_30d, col = "blue3")
lines(iv$VXV_3m, col = "forestgreen")
lines(iv$VXM_6m, col = "mediumvioletred")
axis(1, at = c(1:nrow(iv)), labels = iv$Date, las = 0)
legend("topright", c("VXST_9d", "VIX_30d", "VXV_3m", "VXMT_6m"), 
    lwd = 2, col = c("red3", "blue3", "forestgreen", "mediumvioletred"), 
    cex = 0.6, ncol = 2, x.intersp = 0.5, y.intersp = 0.7)


# covariance
cov = array(0, dim = c(4, 4, 5))
cov[, , 1] = cov(iv_11[, 2:5])
cov[, , 2] = cov(iv_12[, 2:5])
cov[, , 3] = cov(iv_13[, 2:5])
cov[, , 4] = cov(iv_14[, 2:5])
cov[, , 5] = cov(iv_15[, 2:5])

value = matrix(rep(0), nrow = 4, ncol = 5)
VarExpl = matrix(rep(0), nrow = 4, ncol = 5)
CumVarExpl = matrix(rep(0), nrow = 4, ncol = 5)
vector = array(0, dim = c(4, 4, 5))

for (i in 1:5) {
    value[, i] = eigen(cov[, , i])$values
    vector[, , i] = eigen(cov[, , i])$vectors
    VarExpl[, i] = value[, i]/(matrix(1, 1, 4) %*% value[, i])  #percentage of variance explained by every PC
    CumVarExpl[, i] = cumsum(value[, i]/(matrix(1, 1, 4) %*% value[, 
        i]))  #cumulated percentage of variance explained
}


# Relative proportion of variance explained by PCs
plot(VarExpl[, 1], xlab = "Index", ylab = "Percentage [%]", main = "Variance explained by PCs", 
    type = "b", col = 1, xaxt = "n")
lines(VarExpl[, 2], type = "b", col = "blue3")
lines(VarExpl[, 3], type = "b", col = "forestgreen")
lines(VarExpl[, 4], type = "b", col = "red3")
lines(VarExpl[, 5], type = "b", col = "darkorange")
legend("topright", c("2011", "2012", "2013", "2014", "2015"), pch = 1, 
    col = c(1, "blue3", "forestgreen", "red3", "darkorange"), cex = 0.75)
axis(1, at = c(1:4), las = 0)


# Cumulated variance explained by PCs
plot(CumVarExpl[, 1], xlab = "Index", ylab = "Percentage [%]", main = "Cumulated variance explained by PCs", 
    type = "b", col = 1, ylim = c(0.85, 1), xaxt = "n")
lines(CumVarExpl[, 2], type = "b", col = "blue3")
lines(CumVarExpl[, 3], type = "b", col = "forestgreen")
lines(CumVarExpl[, 4], type = "b", col = "red3")
lines(CumVarExpl[, 5], type = "b", col = "darkorange")
legend("bottomright", c("2011", "2012", "2013", "2014", "2015"), 
    pch = 1, col = c(1, "blue3", "forestgreen", "red3", "darkorange"), 
    cex = 0.75)
axis(1, at = c(1:4), las = 0)


# output

VarExpl = as.data.frame(VarExpl)
CumVarExpl = as.data.frame(CumVarExpl)
year = c("2011", "2012", "2013", "2014", "2015")
names(VarExpl) = year
names(CumVarExpl) = year
write.csv(VarExpl, "Variance Explained.csv")
write.csv(CumVarExpl, "Cumulated Variance Explained.csv")

# change the sign of the first PC in 2013 and 2015 for better
# comparison
vector[, 1, 3] = -vector[, 1, 3]
vector[, 1, 5] = -vector[, 1, 5]

# compare the factor loading of each year
layout(matrix(c(1, 2, 3, 4, 5, 6), 2, 3, byrow = TRUE))
for (i in 1:5) {
    plot(vector[, 1, i], type = "l", col = "red3", xlab = "Subindex", 
        xaxt = "n", ylab = "Percentage [%]", main = paste("Factor Loadings for 201", 
            i, sep = ""), lwd = 2, ylim = c(min(vector[, 1:2, i]), 
            max(vector[, 1:2, i])))
    points(vector[, 1, i], col = "red3", lwd = 2, pch = 1)
    lines(vector[, 2, i], col = "blue3", lwd = 2)
    points(vector[, 2, i], col = "blue3", lwd = 2)
    axis(1, at = c(1:4), las = 0)
    
}
plot(0, xaxt = "n", yaxt = "n", bty = "n", pch = "", ylab = "", xlab = "")
legend("top", c("First PC", "Second PC"), lwd = 2, col = c("red3", 
    "blue3"), cex = 0.8)
dev.off()

# Value of observations under new coordinates
y1 = as.matrix(iv_11[, 2:5]) %*% vector[, , 1]
plot(y1[, 1], type = "l", col = "red3", xlab = "Time", ylab = "Projected price", 
    main = "2011", ylim = c(-100, 20), xaxt = "n")
lines(y1[, 2], type = "l", col = "blue3")
legend("bottomleft", c("Prices projected to First PC", "Prices projected to Second PC"), 
    lwd = 1, col = c("red3", "blue3"), cex = 0.75)
axis(1, at = c(1:260), labels = iv$Date[1:260], las = 0)


y2 = as.matrix(iv_12[, 2:5]) %*% vector[, , 2]
plot(y2[, 1], type = "l", col = "red3", xlab = "Time", ylab = "Projected price", 
    main = "2012", ylim = c(-70, 10), xaxt = "n")
lines(y2[, 2], type = "l", col = "blue3")
legend("bottomleft", c("Prices projected to First PC", "Prices projected to Second PC"), 
    lwd = 1, col = c("red3", "blue3"), cex = 0.75)
axis(1, at = c(1:261), labels = iv$Date[261:521], las = 0)


y3 = as.matrix(iv_13[, 2:5]) %*% vector[, , 3]
plot(y3[, 1], type = "l", col = "red3", xlab = "Time", ylab = "Projected price", 
    main = "2013", ylim = c(-50, -5), xaxt = "n")
lines(y3[, 2], type = "l", col = "blue3")
legend("bottomleft", c("Prices projected to First PC", "Prices projected to Second PC"), 
    lwd = 1, col = c("red3", "blue3"), cex = 0.75)
axis(1, at = c(1:261), labels = iv$Date[522:782], las = 0)

y4 = as.matrix(iv_14[, 2:5]) %*% vector[, , 4]
plot(y4[, 1], type = "l", col = "red3", xlab = "Time", ylab = "Projected price", 
    main = "2014", ylim = c(-60, -5), xaxt = "n")
lines(y4[, 2], type = "l", col = "blue3")
legend("bottomleft", c("Prices projected to First PC", "Prices projected to Second PC"), 
    lwd = 1, col = c("red3", "blue3"), cex = 0.75)
axis(1, at = c(1:261), labels = iv$Date[783:1043], las = 0)

y5 = as.matrix(iv_15[, 2:5]) %*% vector[, , 5]
plot(y5[, 1], type = "l", col = "red3", xlab = "Time", ylab = "Projected price", 
    main = "2015", ylim = c(-90, 0), xaxt = "n")
lines(y5[, 2], type = "l", col = "blue3")
legend("bottomleft", c("Prices projected to First PC", "Prices projected to Second PC"), 
    lwd = 1, col = c("red3", "blue3"), cex = 0.75)
axis(1, at = c(1:261), labels = iv$Date[1044:1304], las = 0)

