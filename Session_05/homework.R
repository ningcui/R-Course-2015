# homework
# TO DO:
# * Einen Dataframe mit dem Wort, Lemma und Wortart anlegen.
# * Daten alphabetisch nach dem Wort sortieren und in eine Datei speichern.
# * Durchschnittliche Länge der Substantive in Buchstaben berechnen.
# * Den Dataframe anpassen und die Verteilung von Substantiven nach dem Genus berechnen.
# * Anzahl der autosemantischen Wörtern im Text berechnen. (hist, table)
# * Auf Github hochladen.

#Sys.setlocale(locale="German") #prevent encoding issues

library(XML)

#initialize vectors
tokens <- vector('character')
types <- vector('character')
pos <- vector("character")
morph <- vector("character")

#xml event parser -->sequential parsing
#list in 
xmlEventParse(
    "R-Course-2015/data/t_990505_47.xml", 
    handlers = list( #Liste von Verarbeitungsroutinen
        't' = function(name, attr) { #tag mit t
            tokens <<- c(tokens, attr['word']) #Attribute, die word heißen zufügen
            types <<- c(types, attr['lemma']) #Attribute, die lemma heißen
            pos <<- c(pos, attr["pos"])
            morph <<- c(morph, attr["morph"])
            # <<- Schreiben in globale Variablen
            ## add morphology handler
        }
    ),
    addContext = FALSE
)
print(tail(tokens))
#names(tokens) <- NULL

# build and sort data frame
tokens <- unname(tokens)
df <- data.frame(tokens,types,pos,morph)
lines <- order(df$tokens)
df <- df[lines,]

# write sorted data frame to file
write.csv(df,"R-Course-2015/data/sorted_words.csv")

# nouns have NN or NE tag
nouns <- subset(df, df$pos == 'NN' | df$pos=="NE")

# average length of nouns 
nouns$length <- apply(nouns,2,nchar)[,1]
nchar(nouns)/nrow(nouns) #4.856354
sum(nouns$length)/nrow(nouns) #7.850829

# extract genus information
nouns$genus <- substr(nouns$morph,3,3)
table(nouns$genus)

# count content words
verbs <- unique(grep("^V",df$pos,value = T))
AUTOSEM <- c("ADJA","ADJD","ADV","FM","NE","NN",verbs)
df$autosematikum <- df$pos %in% AUTOSEM
nrow(df[df$autosematikum==T,]) #315
