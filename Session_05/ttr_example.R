## Lese die ganze Datei als eine Sammlung von Zeilen ein.

lines <- readLines('R-Course-2015/data/t_990505_47.xml')

## Finde die Positionsnummern von allen Tokens
## (d.h. Zeilen, die mit "<t " beginnen).
token.indexes <- grep('<t ', lines)

## Wähle alle Zeilen mit Tokens aus.
tokens <- lines[token.indexes]

## delete punctuation
not <- grep("pos=\"\\$.\"",tokens) #Umgehen des Sonderzeichens
not <- grep('pos=\"[$]',tokens) #andere Möglichkeit
filteredTokens <- tokens[-not]

ttr <- function(mytokens){
    ## Lösche alles vor dem Lemma.
    junk.lemmas <- sub(".+ lemma=\"", "", mytokens)
    
    ## Lösche alles nach dem Lemma.
    lemmas <- sub("\".+", "", junk.lemmas)
    
    ## Anzahl der Types ist gleich Null.
    types.count <- 0
    
    ## Noch keine Types gesehen.
    seen.types <- NULL
    
    ## Lege eine Liste für Typesanzahl für jede Stelle im Text an.
    types.on.position <- vector(length = length(lemmas))
    
    ## Für jede Lemmanummer
    for (i in seq_along(lemmas)) {
        
        ## suche das ensprechende Lemma,
        lemma <- lemmas[i]
        
        ## und, falls es noch nicht gesehen wurde,
        if (!(lemma %in% seen.types)) {
            
            ## zähle es als neu mit,
            types.count <- types.count + 1
            ## und lege es in die Liste der gesehenen Lemmata ab.
            seen.types <- c(seen.types, lemma)
        }
        
        ## Füge die Anzahl für die aktuelle Lemmanummer in die Liste ein.
        types.on.position[i] <- types.count
    }
    return(types.on.position)
}
    
    
    ## Zeichne eine Graphik.
    plot(ttr(filteredTokens), type = 'l',
         ylab = "Anzahl der einmaligen Lemmata",
         xlab = "Laufende Wortform",
         col = 'red',main="Type Token Relation")

    lines(ttr(tokens),col="blue")
