### Homework ###

library(XML)

#initialize vectors
tokens <- vector('character')
types <- vector('character')

xmlEventParse(
    "R-Course-2015/data/t_990505_47.xml", 
    handlers = list( #Liste von Verarbeitungsroutinen
        't' = function(name, attr) { #tag mit t
            tokens <<- c(tokens, attr['word']) #Attribute, die word heißen zufügen
            types <<- c(types, attr['lemma']) #Attribute, die lemma heißen
        }
    ),
    addContext = FALSE
)


### 1 ###

# Formel Standardabweichung

source("R-Course-2015/Session_07/standardabweichung.R")

ex = c(3,4,2,3,6)
sd(ex)
stdabw(ex)


### 2 ###

# data #
data1 = data.frame(obs="token",count=nchar(tokens))
data2 = data.frame(obs="types",count=nchar(types))
data = rbind(data1,data2)


# base plot
boxplot(data$count ~ data$obs, col=c("blue","yellow"),main="word length")


