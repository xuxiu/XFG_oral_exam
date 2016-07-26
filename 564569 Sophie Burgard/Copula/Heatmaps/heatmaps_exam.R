library("gplots")

setwd("/Users/Sophie/Dropbox/XFG/XFG Copula/CopulaDemo")

header =c( "C_NASDAQ","C_S&P/TSX","C_FTSE100","C_CAC40","C_DAX","C_SMI",
           "C_RTS","C_SENSEX","C_NIKKEI225","C_HANGSENG","C_STRAITS","G_NASDAQ","G_S&P/TSX","G_FTSE100",
           "G_CAC 40","G_DAX","G_SMI",
           "G_RTS","G_SENSEX","G_NIKKEI225","G_HANGSENG","G_STRAITS")
head1 = c("NASDAQ","S&P/TSX","FTSE100","CAC40","DAX","SMI",
           "RTS","SENSEX","NIKKEI225","HANGSENG","STRAITS")


data = read.table("gumclaytau.csv", sep=";", dec=",")
names(data) = header
data = sapply(data, as.numeric)
data = data.frame(data)
clayton = data[, 1:11]
names(clayton) = head1
gumbel = data[, 12:22]
names(gumbel) = head1

#########################################################
### A) Installing and loading required packages
#########################################################

if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}

no = c("2006-1","2006-2","2006-3","2006-4",
       "2007-1","2007-2","2007-3","2007-4",
       "2008-1","2008-2","2008-3","2008-4",
       "2009-1","2009-2","2009-3","2009-4",
       "2010-1","2010-2","2010-3","2010-4",
       "2011-1","2011-2","2011-3","2011-4",
       "2012-1","2012-2","2012-3","2012-4",
       "2013-1","2013-2","2013-3","2013-4",
       "2014-1")



#########################################################
### B) Reading in data and transform it into matrix format
#########################################################

for (i in 1:30){
rnames = colnames(clayton)       # assign labels in column 1 to "rnames"
if (i == 1){
mat_data = data.matrix(clayton[1:11,1:11])}else{
  x = i*11-10
  y = x + 10
mat_data = data.matrix(clayton[x:y,1:11])}

rownames(mat_data) = rnames      # assign row names

#########################################################
### C) Customizing and plotting the heat map
#########################################################

# creates a own color palette from red to green
my_palette = colorRampPalette(c("white","red"))(n = 10)

# (optional) defines the color breaks manually for a "skewed" color transition


# creates a 5 x 5 inch image
png(paste0("/Users/Sophie/Dropbox/XFG/XFG Copula/CopulaDemo/PNG/clayton_",i,".png"),    # create PNG for the heat map        
    width = 11*300,        # 5 x 300 pixels
    height = 12*200,
    res = 300,            # 300 pixels per inch
    pointsize = 17, 
    bg = "transparent")        # smaller font size

heatmap.2(mat_data,
          cellnote = round(mat_data, digits=2),  # same data set for cell labels
          main = title(paste0("Quarter: ", no[i]), line = -10),
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(13,9),     # widens margins around plot
          dendrogram = "none",
          symm = TRUE,
          Rowv = NA,
          key=FALSE,
          col=my_palette,       # use on color palette defined earlier
          breaks = seq(from = 0, to = 1, by = 0.1),
          Colv="NA",
          lwid=c(5,10), lhei=c(1,10))            # turn off column clustering

dev.off()               # close the PNG device
}


#########################################################
### B) Reading in data and transform it into matrix format
#########################################################

for (i in 1:30){
  rnames = colnames(gumbel)       # assign labels in column 1 to "rnames"
  if (i == 1){
    mat_data = data.matrix(gumbel[1:11,1:11])}else{
      x = i*11-10
      y = x + 10
      mat_data = data.matrix(gumbel[x:y,1:11])}
  
  rownames(mat_data) = rnames      # assign row names
  
  
  #########################################################
  ### C) Customizing and plotting the heat map
  #########################################################
  
  # creates a own color palette from red to green
  my_palette = colorRampPalette(c("white","red"))(n = 10)
  
  # (optional) defines the color breaks manually for a "skewed" color transition
  
  
  # creates a 5 x 5 inch image
  png(paste0("/Users/Sophie/Dropbox/XFG/XFG Copula/CopulaDemo/PNG/gumbel_",i,".png"),    # create PNG for the heat map        
      width = 11*300,        # 5 x 300 pixels
      height = 12*200,
      res = 300,            # 300 pixels per inch
      pointsize = 17,
      bg = "transparent")        # smaller font size
  
  heatmap.2(mat_data,
            cellnote = round(mat_data, digits=2),  # same data set for cell labels
            main = title(paste0("Quarter: ", no[i]), line = -10),
            notecol="black",      # change font color of cell labels to black
            density.info="none",  # turns off density plot inside color legend
            trace="none",         # turns off trace lines inside the heat map
            margins =c(13,9),     # widens margins around plot
            dendrogram = "none",
            symm = TRUE,
            Rowv = NA,
            key=FALSE,
            col=my_palette,       # use on color palette defined earlier
            breaks = seq(from = 0, to = 1, by = 0.1),
            Colv="NA",
            lwid=c(5,10), lhei=c(1,10)
            )            # turn off column clustering
  
  dev.off()               # close the PNG device
}


