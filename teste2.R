source("IDP.R")
source("aux.R")

## Illustrative example
## Nomeclature
## -1 = nonexistent edge
## 0 = circle edge
## 1 = directed edge
## 2 = v (visible edges)

nodes <- c('V1','X','Y1','Y2')
X <- c(2)
Y <- c(3:4)
V <- matrix(-1, nrow = length(nodes), ncol = length(nodes))

## Determining the connections
V[which(nodes == 'V1'), which(nodes == 'X')] = 1

V[which(nodes == 'X'), which(nodes == 'V1')] = 0
V[which(nodes == 'X'), which(nodes == 'Y1')] = 1
V[which(nodes == 'X'), which(nodes == 'Y2')] = 2

V[which(nodes == 'Y1'), which(nodes == 'X')] = 1
V[which(nodes == 'Y1'), which(nodes == 'Y2')] = 1

V[which(nodes == 'Y2'), which(nodes == 'Y1')] = 1

IDP(X,Y,V)



