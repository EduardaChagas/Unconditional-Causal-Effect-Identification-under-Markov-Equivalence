
diferent <- function(a,b){
  r = list()
  t = 0
  for(i in c(1:length(a))){
    equal = F
    for(j in c(1:length(b))){
      if(a[i] == b[j]){
        equal = T
      }
    }
    if(!equal){
      t = 1 + 1
      r[[t]] = a[i]
    }
  }
  r = unlist(r)
  r
}



Pvx <- function(x, v){
  x = unlist(x)
  new.dim = dim(v)[1] - length(x)
  total.indexes = c(1:dim(v)[1])[-x]
  pvx = matrix(nrow = new.dim, ncol = new.dim)
  for(i in c(1:length(total.indexes))){
    pvx[i,] = v[total.indexes[i],total.indexes]
  }
  pvx
}


An <- function(y, v){
  ancestral = list()
  
  for(i in c(1:length(y))){
    if(length(which(v[,i] != -1)) > 0)
      if(length(ancestral) > 0)
        ancestral = unique(ancestral, which(v[,i] != -1))
      else
        ancestral = which(v[,i] != -1)
  }
  ancestral = unique(c(y, ancestral))
  
  if(length(ancestral) > 0)
    return(ancestral)
  else
    return(FALSE)
}

Ch <- function(y, v){
  children = list()
  
  for(i in c(1:length(y))){
    if(length(which(v[y[i],] != -1)) > 0)
      if(length(children) > 0)
        children = unique(children, which(v[y[i],] != -1))
      else
        children = which(v[y[i],] != -1)
  }
  children = unique(c(y, children))
  
  if(length(children) > 0)
    return(children)
  else
    return(FALSE)
}

buckets <- function(v){
  all.buckets = vector("list", dim(v)[1])
  for(i in c(1:dim(v)[1])){ # Percorro todos os elementos de v
    a = 0
    if(length(which(v[i,] == 0))){ # Analiso se a lista tá vazia ou não
      all.buckets[[i]][length(all.buckets[[i]])+1] = i # coloco o elemento na lista
      for(j in c(1:length(which(v[i,] == 0)))){ # Percorro todas as ligações com circulo
        if(v[which(v[i,] == 0)[j],i] == 0){
          all.buckets[[i]][length(all.buckets[[i]])+1] = which(v[i,] == 0)[j] # adiciono todos os elementos com circulo na lista
          w = j
          while(length(which(v[w,] == 0))!=0){
            all.buckets[[i]][length(all.buckets[[i]])+1] = w
            w = min(which(v[w,] == 0))
          }
        }
      }
    }
  }
  bbuckets = vector("list", dim(v)[1])
  for(i in c(1:length(bbuckets))){
    bbuckets[[i]] = all.buckets[[i]] # Pego a lista
    if(length(all.buckets[[i]]) > 0){ # Analiso se a lista tá vazia ou não
      for(j in c(1:length(bbuckets[[i]]))){ #Percorro todos os elementos da lista
        for(w in c(1:length(all.buckets))){
          if(length(which(all.buckets[[w]] == bbuckets[[i]][j])) > 0 && length(which(all.buckets[[i]] == w)) > 0 && w != j){
            bbuckets[[i]] = unique(c(bbuckets[[i]], all.buckets[[w]]))
          }
        }
      }
    }
  }
  mbbuckets = vector("list", dim(v)[1])
  total = 0
  for(i in c(1:length(bbuckets))){
    if(length(bbuckets[[i]]) > 0){
      if(total == 0){
        total = total + 1
        mbbuckets[[total]] = bbuckets[[i]]
      }
      else{
        for(j in c(1:total)){
          if(length(which(bbuckets[[i]] %in% mbbuckets[[total]] == T)) > 0){
            mbbuckets[[total]] = unique(bbuckets[[i]],mbbuckets[[total]])
          }
          else{
            total = total + 1
            mbbuckets[[total]] = bbuckets[[i]]
          }
        }
      }
    }
  }
  bbuckets = vector("list", total)
  for(i in c(1:total)){
    bbuckets[[i]] = mbbuckets[[i]]
  }
  bbuckets
}

pc.component <- function(b, v){
  b = unlist(b)
  v = V[b,b]
  total.colliders = 0
  colliders = list()
  if(length(b) > 1){
    for(i in c(1:length(b))){
      if((length(which(v[i,] == 2)) == 0 && length(which(v[,i] != -1)) > 1) || length(which(v[i,] == -1)) == length((v[i,]))-1){
        total.colliders = total.colliders + 1
        colliders[[total.colliders]] = i
      }
    }
  }
  else{
    colliders = v
  }
  colliders
}

region.calculate <- function(b, pc){
  region = list()
  n.regions = 0
  if(!is.null(dim(pc))){
    for(i in c(1:length(b))){
      cb = pc.component(unlist(b[[i]]), pc)
      cb = c[unlist(cb)]
      if(length(cb) > 0){
        n.regions = n.regions + 1
        region[[n.regions]] = cb
      }
    }
  }
  region
}
