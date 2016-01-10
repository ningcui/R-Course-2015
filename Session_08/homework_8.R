
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

#1 data frame with tokens, types, lengths and counts
data <- data.frame(types)
#data$types <- types
data$length <- nchar(types)
counts <- data.frame(table(data$types))
names(counts)[1] <- "types"
data <- data[!duplicated(data), ] #remove double entries
all <- merge(data,counts,by = "types")

# choose 10 observations
rows = sample(1:nrow(all),size = 20)
mysample <- all[rows,]

# build model
plot(mysample$length,mysample$Freq)
model <- lm(Freq ~ length, data=mysample)
model
abline(model)
