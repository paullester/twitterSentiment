extractSource <- function(source.string) {
  source.string <- as.character(source.string)
  if (is.na(source.string)){
    return ("None")
  } else if (str_detect(source.string, "iPhone")){
    return("iPhone")
  } else if (str_detect(source.string, "Web")){
    return("Web")
  } else if (str_detect(source.string, "Android")){
    return("Android")
  } else {
    return ("None")
  }
}

extractTarget <- function(Word) {
  Word <- as.character(Word)
  if (str_detect(Word, "hillary")){
    return ("Crooked Hillary")
  } else if (str_detect(Word, "bernie")){
    return("Bernie")
    #} else if (str_detect(Word, "obama")){
    #  return("Obama")
  } else if (str_detect(Word, "cruz")){
    return("Lyin' Ted")
  } else if (str_detect(Word, "marco")){
    return("Little Marco")
  } else if (str_detect(Word, "rubio")){
    return("Little Marco")
  }else if (str_detect(Word, "jeb")){
    return("Low Energy Jeb")
  }else if (str_detect(Word, "warren")){
    return("Goofy Elizabeth Warren")
  }else if (str_detect(Word, "kasich")){
    return("1 for 38 Kasich")
  } else {
    return ("No target")
  }
}

extractParty <- function(Word) {
  Word <- as.character(Word)
  if (str_detect(Word, "hillary")){
    return ("Democrat")
  } else if (str_detect(Word, "obama")){
    return("Democrat")
  } else if (str_detect(Word, "bernie")){
    return("Democrat")
  } else if (str_detect(Word, "cruz")){
    return("Republican")
  } else if (str_detect(Word, "marco")){
    return("Republican")
  } else if (str_detect(Word, "rubio")){
    return("Republican")
  }else if (str_detect(Word, "jeb")){
    return("Republican")
  }else if (str_detect(Word, "warren")){
    return("Republican")
  }else if (str_detect(Word, "kasich")){
    return("Republican")
  } else {
    return ("No target")
  }
}



reorderTargets <- function(targets) {
  targetstemp <- targets
  for(i in seq(from=1, to=nrow(targets), by=7)){
    targetstemp[i,] = targets[i+2,]
    targetstemp[i+2,] = targets[i+3,]
    targetstemp[i+3,] = targets[i+6,]
    targetstemp[i+6,] = targets[i,]
  }
  return(targetstemp)
}

reorderParties <- function(party) {
  partytemp <- party
  for(i in seq(from=1, to=nrow(party), by=2)){
    partytemp[i,] = party[i+1,]
    partytemp[i+1,] = party[i,]
  }
  return(partytemp)
}