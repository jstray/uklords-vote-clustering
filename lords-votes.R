library(proxy) 	# need custom distance function capability

# -------------------------------- Load data ------------------------------

# Load in vote history
# strip out vote description, date, etc, and transpose so each row is an MP
votetable = read.csv("votematrix-lords.csv", header=T, sep=",")
votes = votetable[, 5:1047]
votes = t(votes)


# Load Lord descriptions, and order by MPID, which is the column ordering in votes
lords = read.csv("lords.csv", header=T, sep=",")
lords = lords[order(lords[,"mpid"]),]


# Turn "aye" (encoded as 2) into +1, and "nay" (encoded as 4) into -1, all else zero
ayes <- votes == 2
nays <- votes == 4
votes <- array(0, dim(votes))
votes[ayes] <- 1
votes[nays] <- -1

votescast = 
# -------------------------------- Take recent votes ------------------------------

# take only N most recent votes
Nvotes=100
recentvotes = votes[,1:Nvotes]

# set MP row names to party name
row.names(recentvotes) = lords[,"party"]

# remove all MPs who didn't vote at all in these recent votes
recentvotes = recentvotes[rowSums(abs(recentvotes)) != 0, ]

# -------------------------------- Compute distances ------------------------------

# distance function = 1 - fraction of votes where both voted, and both voted the same
votedist <- function(v1, v2) {
	overlap = v1!=0 & v2!=0
	numoverlap = sum(overlap)
	match = overlap & v1==v2
	nummatch = sum(match)
	if (!numoverlap) 
		dist = 1 
	else 
		dist = 1- (nummatch/numoverlap)
		dist = 1- ((nummatch/numoverlap) * log(numoverlap)/log(Nvotes))
	dist
	}

newplot <- function() { quartz() } # Mac specific, see http://www.statmethods.net/graphs/creating.html 

# create distance matrix
d = dist(recentvotes, votedist)

# -------------------------------- MDS plot ------------------------------
 
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
x <- fit$points[,1]
y <- fit$points[,2]

# plot just the points
newplot()
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="House of Lords voting",  pch=19)

# now plot with colors corresponding to party
newplot()
parties = factor(row.names(recentvotes))
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="House of Lords voting", pch=19, col=parties)
legend('topright', legend = levels(parties), col=palette(), cex = 0.8, pch = 1)

# Plot with a different color for those who vote at least half the time
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="House of Lords voting", pch=19, col=ifelse(numvotes>=50,"red","black"))
