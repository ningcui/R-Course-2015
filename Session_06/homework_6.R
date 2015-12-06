
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

#1 data frame with tokens and lengths
data <- data.frame(tokens)
data$length <- nchar(tokens)

#2
wordlength <- table(data$length)
class(wordlength) #table

#3: Histogram
hist(data$length) #histogramm
barplot(wordlength,main = "Word lengths") #bar diagram

#4: ggPlot
library(ggplot2)
lengthSummary <- data.frame(wordlength)
names(lengthSummary)[1] <- "Length"
ggplot(data=lengthSummary, aes(x=Length, y=Freq)) +
    geom_bar(stat="identity")

#5: descriptive values

types.lengths <- nchar(types)
median(types.lengths)
length(types.lengths)
head(sort(types.lengths), length(types.lengths)/2)
t2 <- prop.table(table(types.lengths))
cumsum(t2)

mode(types.lengths)
median(types.lengths)
mean(types.lengths)

#6: 
lengthSummary$relFreq <- lengthSummary$Freq/sum(lengthSummary$Freq)
lengthSummary$cumSum <- cumsum(lengthSummary$Freq)
lengthSummary$relCumSum <- cumsum(lengthSummary$relFreq)
lengthSummary
