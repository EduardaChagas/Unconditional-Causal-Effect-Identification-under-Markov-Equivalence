#setwd("/home/eduarda/Desktop/Disciplines/Second Semester/Modelos Gráficos Probabilísticos/TP/Code")
source("aux.R")

IDP <- function(X,Y,V){
  px = Pvx(X, V)
  d = An(Y, px)
  t = c(1:dim(V)[1])
  pxy = identify(c = d, t = t, q = V)
  if(isFALSE(pxy))
    print("FAIL")
  else
    print("Not fail")
}

identify <- function(c, t, q){
  if(length(c) == 0){
    #print("FALSE")
    return(1)
  }
  if(all(c,t) && length(c) == length(t)){
    #print(q)
    return(q)
  }
  
  TC = t[-c]
  B = unlist(buckets(q))
  if(check.proposition2(B, TC, q)){
    q = Pvx(B, t)
    return(identify(c, diferent(t,-B),q))
  }
  else if(check.proposition3(B, c, q, option = 1)){
    rb = check.proposition3(B, c, q, 2)
    rcrb = check.proposition3(B, diferent(c,rb), q, 2)
    if(rb == 0 || rcrb == 0){
      rb.rcrb = max(rb,rcrb)
    }
    else{
      rb.rcrb = intersect(rb,rcrb)
    }
    aa = identify(rb, t, q) * identify(rcrb, t, q)
    aaa = (identify(rb.rcrb, t, q))
    if(aaa == 0)
      return(FALSE)
    else
      return((identify(rb, t, q) * identify(rcrb, t, q))/(identify(rb.rcrb, t, q)))
  }
  else{
    #print("FALHA")
    return(FALSE)
  }
}

check.proposition3 <- function(b, c, v, option){
  region = c()
  pc = v[c,c]
  region = region.calculate(b, pc)
  if(length(region)>0){
    for(i in c(1:length(region))){
      if((length(intersect(b[[i]], c)) == length(b[[i]])) && (length(intersect(region[[i]], c))) != length(c)){
        if(option == 1)
          return(T)
        else
          return(region[[i]])
      }
    }
  }
  else
    return(F)
  return(F)
}

check.proposition2 <- function(b, t, v){
  for(i in c(1:length(b))){
    if(length(intersect(b[[i]],t)) == length(b[[i]])){
      cb = pc.component(b[[i]], v)
      children = Ch(b[[i]], v)
      int = intersect(cb, children)
      if(length(int) > 0){
        if(length(b[[i]][-int]) == length(int))
          return(T)
      }
    }
  }
  return(F)
}