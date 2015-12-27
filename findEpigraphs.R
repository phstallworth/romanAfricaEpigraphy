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
categorize <- function(x, cat, subs = c("(", ")", "[", "]", "{", "}", "?", "/")){
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
funus, hic situs est, situs est, in pace, in pacae, memoriae, memoriam, parentibus, diis manibus,
dis manibus'''

findEpitaphs <- binaryCat(africaFull[,10], c("infans", "innocens", "puer", "puella", "relinquit", "quiebit", "perit", "reccessit", "recessit", "quiesco", "periit", "decessit", 
            "vixit", "vicxit", "vixsit", "bixit", "visisti", "vixi", "occisus", "occisa", "positus", "depositus", "deposita",
            "funus", "hic situs est", "situs est", "in pace", "in pacae", "memoriae", "memoriam", "parentibus", "diis manibus",
            "dis manibus"))
epitaphs <- africaFull[findEpitaphs == T, ]

#Write the first version of the epitaphs csv. 
####DON'T REWRITE HERE!!!!! #######
write.csv(epitaphs, file = "epitaphs.csv")
##########################

'''It might have been nice to construct a vector with the amended character strings included in it. I think I will go ahead and make this.'''

'''ALTERNATIVE START POINT FOR THE FILE'''
epitaphs <- read.csv("epitaphs.csv", header = T)
epitaphs[,11] <- as.character(epitaphs[,11])
epitaphs <- epitaphs[,2:ncol(epitaphs)]
''' DONE DONE DONE DONE DONE DONE DONE DONE DONE DONE DONE DONE '''

subs <- c("(", ")", "[", "]", "{", "}", "?", "/", "!", "#")

modifiedInscriptions <- rep(NA, nrow(epitaphs)) 
for(i in 1:nrow(epitaphs)){
  modifiedInscriptions[i] <- epitaphs[i, 10]
  for(j in 1:length(subs)){
    past <- modifiedInscriptions[i]
    modifiedInscriptions[i] <- sub(subs[j], "", past, fixed = TRUE)
    while(modifiedInscriptions[i] != past){
      past <- modifiedInscriptions[i]
      modifiedInscriptions[i] <- sub(subs[j], "", past, fixed = TRUE)
    }
  }
}


'''Next we need to find the age categories. This should first involve creating a function which finds a word 
then looks directly after it for a roman numeral. We then convert that roman numeral into a regular number and put it into a vector. We need to do this
for a years, months, days, and hours columns. Fortunately, the same function should work for each. '''

findRomanNumeral <- function(x, w){
  location <- regexpr(w, x)
  if(location[1] == -1){
    return(NA)
  }
  else{
  location <- location[1] + slot(location, "match.length")
  start <- location
  while(substr(x, start, start) == " ") {start <- start + 1}
  end <- start
  while(substr(x, end+1, end+1) != " " && end+1 <= nchar(x)) {end <- end + 1}
  final <- as.numeric(as.roman(substr(x, start, end)))
  return(final)
  }
}
findRomanNumeral(modifiedInscriptions[1], "annos")
findRomanNumeral(modifiedInscriptions[2], "annis")
findRomanNumeral(modifiedInscriptions[3], "annis")

'''Yay, so that is done. Now lets incorporate find all the years and shit. The key words are annis and annos'''
findRomanNumeralMulti <- function(v, words){
  final <- rep(NA, length(v))
  for(i in 1:length(v))
    for(j in 1:length(words)){
      if(is.na(findRomanNumeral(v[i], words[j])) == FALSE) {
        final[i] <- findRomanNumeral(v[i], words[j])
      }
    print(i)
  }
  return(final)
}

years <- findRomanNumeralMulti(modifiedInscriptions, c("annos", "annis"))

'''Sweet! That worked. Time to do the same for months and days '''
months <- findRomanNumeralMulti(modifiedInscriptions, c("menses", "mensibus"))
days <- findRomanNumeralMulti(modifiedInscriptions, c("dies", "diebus"))
hours <- findRomanNumeralMulti(modifiedInscriptions, c("horas", "oras", "horis"))

length(years[!is.na(years)])
length(months[!is.na(months)])
length(days[!is.na(days)])
length(hours[!is.na(hours)])

'''For posterity we have 739 years recorded, 
160 months recorded, 94 days recorded, and 14 hours recorded'''
 
epitaphs <- data.frame(epitaphs, years, months, days, hours)

'''WRITE THE NEW CSV!!!!!'''
write.csv(epitaphs, "epitaphs.csv")


'''Some fun histograms'''
hist(epitaphs[!is.na(epitaphs[,13]), 12])
hist(epitaphs[!is.na(epitaphs[,14]), 12])
hist(epitaphs[!is.na(epitaphs[,15]), 12])


'''Some fun other stuff'''
piusList <- epitaphs[grep("pius", modifiedInscriptions),]
nrow(piusList)
plot(density(piusList[!is.na(piusList[,12]), 12]))
lines(density(epitaphs[!is.na(epitaphs[,12]), 12]), col = "red")


'''Some fun stuff that isnt really correct right now'''
piusList2 <- binaryCat(epitaphs[,10], c(" pius ", " pia ", " pio ", " piae ", "piis"))
piusList2 <- epitaphs[piusList2,]
nrow(piusList2)
hist(piusList2[!is.na(piusList2[,12]), 12])
plot(density(piusList2[!is.na(piusList2[,12]), 12]))
lines(density(epitaphs[!is.na(epitaphs[,12]), 12]), col = "red")
plot(density(piusList2[!is.na(piusList2[,12]), 12])$y - density(epitaphs[!is.na(epitaphs[,12]), 12])$y ~ density(epitaphs[!is.na(epitaphs[,12]), 12])$x, type = 'l')
lines(density(piusList2[!is.na(piusList2[,12]), 12])$y - density(epitaphs[!is.na(epitaphs[,12]), 12])$y ~ density(epitaphs[!is.na(epitaphs[,12]), 12])$x, type = 'l')
density(epitaphs[!is.na(epitaphs[,12]), 12])$x == density(piusList2[!is.na(piusList2[,12]), 12])$x
abline(0, b = 0, col = "red")
par(mfrow = c(2, 1))



