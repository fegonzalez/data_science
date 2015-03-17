require(graphics);
require(grDevices);
##library(sfsmisc);

#Generate the data
x <- 1:100000
y <- 1:100000

#Setup the plot area
par(pty="m", plt=c(0.1, 1, 0.1, 1), omd=c(0.1,0.9,0.1,0.9))

#Plot a blank graph without completing the x or y axis
plot(x, y, type = "n", xaxt = "n", yaxt="n", xlab="", ylab="",
     log = "x", col="blue")
mtext(side=3, text="Test Plot", line=1.2, cex=1.5)

#Complete the x axis
## eaxis(1, padj=-0.5, cex.axis=0.8) #library(sfsmisc);
mtext(side=1, text="x", line=2.5)

#Complete the y axis and add the grid
aty <- seq(par("yaxp")[1], par("yaxp")[2],
           (par("yaxp")[2] - par("yaxp")[1])/par("yaxp")[3])
axis(2, at=aty, labels=format(aty, scientific=FALSE),
     hadj=0.9, cex.axis=0.8, las=2)
mtext(side=2, text="y", line=4.5)
grid()

#Add the line last so it will be on top of the grid
lines(x, y, col="blue")
