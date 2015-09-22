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

# -------------------------------- 3D Scatterplot ------------------------------
# show 3 dimensions (votes) at a time
library(scatterplot3d)
scatterplot3d(votes[,100], votes[,200], votes[,300])
#scatterplot3d(jitter(votes[,100]), jitter(votes[,200]), jitter(votes[,300]))

# -------------------------------- Take recent votes ------------------------------
# take only N most recent votes, where at least one Lord voted
Nvotes=100
recentvotes = votes[,1:Nvotes]
row.names(recentvotes) = lords[,"party"]
recentvotes = recentvotes[rowSums(abs(recentvotes)) != 0, ]


# -------------------------------- Compute principal components, plot ------------------------------

pc <- prcomp(recentvotes)
plot(pc$x[,1], pc$x[,2])	

# now with party colors!
colors = palette()
parties = factor(row.names(recentvotes))
labels = levels(parties)
plot(pc$x[,1], pc$x[,2], col=parties, pch=16, cex=2)
legend('topright', legend = levels(parties), col=palette(), cex = 2, pch = 1)

