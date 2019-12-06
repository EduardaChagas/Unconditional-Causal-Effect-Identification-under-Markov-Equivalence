source("IDP.R")
source("aux.R")

## Illustrative example
## Nomeclature
## -1 = nonexistent edge
## 0 = circle edge
## 1 = directed edge
## 2 = v (visible edges)

nodes <- c('X1','X2','Y1','Y2','Y3','Y4','Y5')
X <- c(1:2)
Y <- c(3:7)
V <- matrix(-1, nrow = length(nodes), ncol = length(nodes))

## Determining the connections
V[which(nodes == 'X1'), which(nodes == 'X2')] = 1
V[which(nodes == 'X1'), which(nodes == 'Y1')] = 1
V[which(nodes == 'X1'), which(nodes == 'Y3')] = 1

V[which(nodes == 'X2'), which(nodes == 'X1')] = 1
V[which(nodes == 'X2'), which(nodes == 'Y1')] = 1
V[which(nodes == 'X2'), which(nodes == 'Y2')] = 2

V[which(nodes == 'Y1'), which(nodes == 'X2')] = 1
V[which(nodes == 'Y1'), which(nodes == 'Y5')] = 0

V[which(nodes == 'Y2'), which(nodes == 'Y3')] = 1

V[which(nodes == 'Y3'), which(nodes == 'Y2')] = 1
V[which(nodes == 'Y3'), which(nodes == 'Y4')] = 0

V[which(nodes == 'Y4'), which(nodes == 'Y3')] = 1
V[which(nodes == 'Y4'), which(nodes == 'Y5')] = 0

V[which(nodes == 'Y5'), which(nodes == 'Y1')] = 1
V[which(nodes == 'Y5'), which(nodes == 'Y4')] = 0

IDP(X,Y,V)
