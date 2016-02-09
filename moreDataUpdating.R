epitaphs <- read.csv("epitaphs_edited.csv")
epitaphs[,11] <- as.character(epitaphs[,11])
source("categorizationFunctions.R")
source("findRomanNumerals.R")

subs <- c("(", ")", "[", "]", "{", "}", "?", "/", "!", "#", "\\", "*", "%", "-")

modifiedInscriptions <- rep(NA, nrow(epitaphs)) 
for(i in 1:nrow(epitaphs)){
  if(i == 178 || i == 237 || i == 437 || i == 856 || i == 938 || i == 951 || i == 967)
    next
  modifiedInscriptions[i] <- epitaphs[i, 11]
  print(i)
  for(j in 1:length(subs)){
    past <- modifiedInscriptions[i]
    modifiedInscriptions[i] <- sub(subs[j], "", past, fixed = TRUE)
    while(modifiedInscriptions[i] != past){
      past <- modifiedInscriptions[i]
      modifiedInscriptions[i] <- sub(subs[j], "", past, fixed = TRUE)
    }
  }
}

# Find words with word militavit and categorize them. Also, figure out some other string shit.
findMilitary <- rep(NA, nrow(epitaphs))
occupation <- rep(NA, nrow(epitaphs))
for(i in 1:nrow(epitaphs)){
   print(i)
   if(is.na(modifiedInscriptions[i]))
     next
   loc <- regexpr("militavit", modifiedInscriptions[i])
   if(loc == -1)
     next
   occupation[i] <- "Military"
   modString <- substr(modifiedInscriptions[i], loc, nchar(modifiedInscriptions[i]))
   vixLoc <- regexpr("vixit", modString)
   if(vixLoc == -1)
     findMilitary[i] <- findRomanNumeralMulti(modString, c("annos", "annis", "annorum", "anno", "annum"))
   else if((regexpr("annos", modString) <= vixLoc && regexpr("annos", modString) != -1)||(regexpr("annis", modString) <= vixLoc && regexpr("annis", modString) != -1) ||
      (regexpr("annorum", modString) <= vixLoc && regexpr("annorum", modString) != -1) || (regexpr("anno", modString) <= vixLoc && regexpr("anno", modString) != -1)|| 
      (regexpr("annum", modString) <= vixLoc && regexpr("annum", modString) != -1))
      findMilitary[i] <- findRomanNumeralMulti(modString, c("annos", "annis", "annorum", "anno", "annum"))
}

#DONE DONE DON EODN DONE DONE


#Next I want to make gender characterizations
maleEndings <- c("x", "us", "i", "o", "um", "e", "is", "r")
femaleEndings <- c("a", "ae", "am")
capitals <- c("A", "B", "C", "D", "E", "F", "H", "I", "J", "K", "L", "M","N", "O", "P", "Q", 
              "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
ignore <- c("Hic", "Dis", "Manibus", "Diis", "In", "Innocens")

subs <- c("(", ")", "[", "]", "{", "}", "?", "/", "!", "#", "\\", "*", "%", "-", "<", ">", "=" ignore)

modifiedInscriptions <- rep(NA, nrow(epitaphs)) 
for(i in 1:nrow(epitaphs)){
  if(i == 178 || i == 237 || i == 437 || i == 856 || i == 938 || i == 951 || i == 967)
    next
  modifiedInscriptions[i] <- epitaphs[i, 11]
  print(i)
  for(j in 1:length(subs)){
    past <- modifiedInscriptions[i]
    modifiedInscriptions[i] <- sub(subs[j], "", past, fixed = TRUE)
    while(modifiedInscriptions[i] != past){
      past <- modifiedInscriptions[i]
      modifiedInscriptions[i] <- sub(subs[j], "", past, fixed = TRUE)
    }
  }
}


sex <- rep(NA, length(modifiedInscriptions))
for(i in 1:length(modifiedInscriptions)){
  print(i)
  if(i == 979)
    next
  if(is.na(modifiedInscriptions[i]))
    next
  loc <- Inf
  for(j in 1:length(capitals)){
    if(regexpr( capitals[j], modifiedInscriptions[i]) < loc && regexpr(capitals[j], modifiedInscriptions[i]) != -1)
      loc <- regexpr(capitals[j], modifiedInscriptions[i])
  }
  if(loc == Inf)
    next()
  modString <- substr(modifiedInscriptions[i], loc, nchar(modifiedInscriptions[i]))
  endName <- regexpr(" ", modString)
  for(k in 1:length(femaleEndings)){
    relevant_characters = substr(modString, endName - nchar(femaleEndings[k]), endName - 1)
    if(relevant_characters == femaleEndings[k]){
      sex[i] = "F"
      break
    }
  }
  if(!is.na(sex[i]))
    next
  for(k in 1:length(maleEndings)){
    relevant_characters = substr(modString, endName - nchar(maleEndings[k]), endName - 1)
    if(relevant_characters == maleEndings[k]){
      sex[i] = "M"
      break
    }
  }
}

