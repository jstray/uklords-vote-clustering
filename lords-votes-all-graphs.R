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

# -------------------------------- Take recent votes ------------------------------

# take only N most recent votes
Nvotes=100
recentvotes = votes[,1:Nvotes]

# set MP row names to party name
row.names(recentvotes) = lords[,"firstname"]

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
		# dist = 1- ((nummatch/numoverlap) * log(numoverlap)/log(Nvotes))
		dist = 1- (nummatch/numoverlap)
	dist
	}

newplot <- function() { quartz() } # Mac specific, see http://www.statmethods.net/graphs/creating.html 


# create distance matrix
d = dist(recentvotes, votedist)

#### <<< Check out that distance matrix! >>>

# -------------------------------- Cluster ------------------------------

# do a hierarchical clustering (Ward algorithm)
clusters = hclust(d, method="ward") 

# pick, e.g. biggest five clusters
groups = cutree(clusters, k=10) # cut tree into 5 clusters
#### <<< If you look at groups here... clustered, but boring >>>

newplot()
plot(clusters)

# -------------------------------- Colored dendrogram setup ------------------------------

# first we have to rebuild the distance matrix with new labels... pain
recentvotes = votes[,1:Nvotes]
row.names(recentvotes) = lords[,"party"]
recentvotes = recentvotes[rowSums(abs(recentvotes)) != 0, ]
d = dist(recentvotes, votedist)  # do this again to take new labels

# set up coloring for parties
colors = palette()
parties = factor(row.names(recentvotes))
labels = levels(parties)
partyColor <- function(party) {
	index = which(labels == party)
	coloridx = ((index-1) %% length(colors)) + 1
	colors[coloridx]
}

# function to set label colors to party colors
colLab <- function(n) {
   if(is.leaf(n)) {
       a <- attributes(n)
       labCol <- partyColor(a$label)
       attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
   }
   n
}


# --------------------- Plot dendrogram with colored parties ------------------

# recluster and plot
clusters = hclust(d, method="ward") 
dendr  = dendrapply(as.dendrogram(clusters), colLab)
newplot()
plot(dendr)


# -------------------------------- MDS plot ------------------------------
 
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
x <- fit$points[,1]
y <- fit$points[,2]

# plot just the points
newplot()
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="House of Lords voting",  pch=19)

# now plot with colors corresponding to party
newplot()
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
	 main="House of Lords voting", pch=19, col=parties)
legend('topright', legend = levels(parties), col=palette(), cex = 0.8, pch = 1)


