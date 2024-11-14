# les mots à enlever
stopWords_en <-
  c(# Nom
    "nom", "people", "new", "old", "back", "way", "thing", "things", "left", "right", "mr", "ms",
    # Origines et politique
    # "ontario", "ottawa", "toronto", "halifax", "quebec", "montreal", "york", "united", "states",
    # "vancouver", "canadian", "american",
    # Marqueur de relations, déterminants
    "also", "per", "just", "like", "even", "still", "much", "since", "around", "well", "really", "might",
    "across", "whether", "least", "already",
    # Verbes
    "said", "says", "say", "will", "can", "get", "got", "found", "may", "told", "make", "made", "going",
    "take", "took", "think", "including", "want", "see", "called", "know", "known", "according",
    "ask", "asked", "put", "away", "among", "set", "show", "find", "went", "call", "come", "came",
    "need", "go",
    # Nombre et quantités
    "number", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "cent", "lot",
    "first", "second", "last", "end", "many", "former", "later", "next", "never", "always", "with", "without",
    "every", "several", "big", "short", "long", "little", "small", "less", "something", "somethings",
    # Temps et lieux
    "time", "times", "now", "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche",
    "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "gmt", "bst",
    "décembre", "janvier", "février", "mars", "avril", "mai", "juin", "juillet", "août", "septembre",
    "octobre", "novembre", "december", "january", "february", "march", "april", "may", "june",
    "july", "august", "september", "october", "november", "feb",
    "today", "yesterday", "another", "day", "days", "week", "weeks", "month", "months", "year", "years",
    "ago", "near", "far", "place", "early", "yet",
    # Relatif au journalisme et aux médias
    "media", "presse", "plus", "journal", "cbc", "devoir", "radio-canada", "agence", "qmi",
    "mediaqmi", "star", "cbc", "news", "press", "reuters", "reuter", "cp", "ap", "nouvelles",
    "published", "rights", "guardian", "copyright", "reserved", "timeupdated", "updated",
    "globe", "mail", "block", "related", "grdn", "anglais", "sun", "thesun", "newspapers",
    "limited", "washington", "post", "httpwwwwashingtonpostcom", "co", "tor", "ont",

    # Autres
    "x", "h", "s", "t", "th", "à") # ajouter d'autres mots

keywords <- # Pour enlever les mots clés liés au conflit
  c("refugees", "refugee", "ukraine", "syria", "syrian", "syrians", "ukrainians", "ukrainian",
    "war", "crisis", "migrants", "forces", "invasion", "military", "attack", "attacks", "conflict",
    "kill", "killed", "threat", "fight", "fights", "violence", "fighting", "death", "dead",
    "deads", "crime", "crimes", "aggression", "aggressions", "emergency")

after_job_en <- # Pour enlever les résidus d'après nettoyage
  c("s", "t", "th", "à", "a", "u", "x", "h", "tri", "st", "am", "pm", "m", "re", "c")