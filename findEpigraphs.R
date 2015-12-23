#first read the csv
africaFull <- read.csv("edh.csv", header = TRUE, sep = "\t")

'''So I want to isolate the inscriptions, which are in column 10. Right now they are coded as a factor,
but this is bad because each time I query the factor vector, it reports back both the entry
and a list of all the levels. This is really slow, plus coding this variable as a factor makes no sense, because
inscriptions are not inherently categorical. What I will is convert the 10th column into a character vector and work with those.'''

africaFull[,10] <- as.character(africaFull[,10])
africaFull[12, 10]

'''Cool, it worked! Next I want to construct a funciton that looks for words in a character string, while ignoring certain characters.'''

#x is a vector of things to be categorized, cat is the vector of categorization words
#the funciton returns a logical vector the length of x where a F indicates non-inclusion and a T indicates inclusion 
binaryCat <- function(x, cat, subs = c("(", ")", "[", "]", "{", "}", "?")){
  inout <- rep(F, length(x))
  for(i in 1:length(x)){
    inout[i] <- categorize(x[i], cat, subs)
  }
  return(inout)
}

#x is a string, cat is a vector of catagorization words
#returns 1 if containment, 0 otherwise
categorize <- function(x, cat, subs = c("(", ")", "[", "]", "{", "}", "?")){
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


'''Now lets actually do some stuff. Here is the world list for epigraph categorization:
infans, innocens, puer, puella, relinquit, quiebit, perit, reccessit, recessit, quiesco, periit, decessit, 
vixit, vicxit, vixsit, bixit, visisti, vixi, occisus, occisa, positus, depositus, deposita,
funus, hic situs est, situs est, in pace, in pacae, memoriae, memoriam, annis, parentibus, diis manibus,
dis manibus, annos, menses, dies'''

findEpitaphs <- binaryCat(africaFull[,10], c("infans", "innocens", "puer", "puella", "relinquit", "quiebit", "perit", "reccessit", "recessit", "quiesco", "periit", "decessit", 
            "vixit", "vicxit", "vixsit", "bixit", "visisti", "vixi", "occisus", "occisa", "positus", "depositus", "deposita",
            "funus", "hic situs est", "situs est", "in pace", "in pacae", "memoriae", "memoriam", "annis", "parentibus", "diis manibus",
            "dis manibus", "annos", "menses", "dies"))
epitaphs <- africaFull[findEpitaphs == T, ]

