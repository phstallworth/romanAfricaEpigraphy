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