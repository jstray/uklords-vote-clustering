
# -------------------------------- projection axis example ------------------------------
# Show how it can be useful to rotate the data before projecting

N = 100
pts = cbind(rnorm(N), rnorm(N))
sel = pts[,1] > pts[,2]
redpts = pts[sel,]
bluepts = pts[!sel,]
plot(redpts, col='red', xlim=c(-3,3), ylim=c(-3,3))
points(bluepts, col='blue')
rug(redpts[,1], side=1, col='red')
rug(bluepts[,1], side=1, col='blue')
rug(redpts[,2], side=2, col='red')
rug(bluepts[,2], side=2, col='blue')