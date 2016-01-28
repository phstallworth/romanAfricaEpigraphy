binaryCat <- function(x, cat, subs = c("(", ")", "[", "]", "{", "}", "?", "/", "!", "#")){
  inout <- rep(F, length(x))
  for(i in 1:length(x)){
    inout[i] <- categorize(x[i], cat, subs)
  }
  return(inout)
}


categorize <- function(x, cat, subs = c("(", ")", "[", "]", "{", "}", "?", "/", "!", "#")){
  for(i in 1:length(cat)){
    for(j in 1:length(subs)){
      past <- x
      x <- sub(subs[j], "", past, fixed = TRUE)
      while(x != past){
        past <- x
        x <- sub(subs[j], "", past, fixed = TRUE)
      }
    }
    if(grepl(cat[i], x)){
      return(T)
    }
  }
  return(F)
}


