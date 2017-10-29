
#

# Load tnet
library(tnet)

# Load network
net <- cbind(
  i=c(1,1,2,2,2,2,3,3,4,5,5,6),
  j=c(2,3,1,3,4,5,1,2,2,2,6,5),
  w=c(4,2,4,4,1,2,2,4,1,2,1,1))

# Run function
clustering_w(net, measure=c("am", "gm", "ma", "mi"))


# Load tnet
library(tnet)

# Load network
data(Freemans.EIES)

# Run function
clustering_w(Freemans.EIES.net.3.n32, measure=c("am", "gm", "ma", "mi"))