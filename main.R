library(RCurl)
library(jsonlite)
library(rvest)
library(dplyr)
library(ggplot2)
theme_set(theme_bw(14))

#### FUNCTIONS ####
get_suggestions <- function(text, verbose = TRUE) {
    # Returns suggestions for `text`
    text <- tolower(text)
    sug_url <- sprintf("https://suggestqueries.google.com/complete/search?client=firefox&q=%s",
                       URLencode(text))
    res <- fromJSON(getURL(sug_url))
    # Remove `text` from all elements of the second position in the list
    subst <- as.character(sapply(res[[2]], 
                                 function(x) trimws(gsub(text, "", x))
                                 )
                          )
    # Delete empty strings (it will normally be the first item of the array)
    subst <- Filter(function(x) nchar(x) > 0, subst)
    if (verbose) {
        print(text)
        print(subst)
    }
    return(subst)
}

#### MAIN ####

# Get list of best actors
tables <- html_nodes(read_html("https://en.wikipedia.org/wiki/Academy_Award_for_Best_Actor"),
                     "table")
actors <- html_table(tables[[3]])[, 1:2]
actors$Year <- as.numeric(gsub("\\(.*\\)", "", actors$Year)) # may produce NAs, not concerned
actors$Actor <- gsub("(.*)\\!(.*)", "\\2", actors$Actor)
actors <- unique(actors[actors$Year > 1990, "Actor"])
actors <- actors[!is.na(actors)]

# Same for actresses
tables <- html_nodes(read_html("https://en.wikipedia.org/wiki/Academy_Award_for_Best_Actress"),
                     "table")
actresses <- html_table(tables[[3]])[, 1:2]
actresses$Actress <- gsub("(.*)\\!(.*)", "\\2", actresses$Actress)
actresses <- unique(actresses[actresses$Year > 1990, "Actress"])
actresses <- actresses[!is.na(actresses)]

# Build suggestions
actors_suggestions <- lapply(actors, get_suggestions)
actresses_suggestions <- lapply(actresses, get_suggestions)

# Get the top 5 words in each position (from positions 1 to 5)
actors_top <- lapply(1:5, function(i) {
      dd <- sort(table(sapply(actors_suggestions, function(x) x[i])), decreasing = TRUE)[1:5]
      data.frame(keywords = names(dd),
                 values = as.numeric(dd),
                 position = sprintf("Position #%d", i),
                 idx = 1:5)
})
actors_top <- do.call(rbind, actors_top)
actors_top$type <- "Actors"

actresses_top <- lapply(1:5, function(i) {
    dd <- sort(table(sapply(actresses_suggestions, function(x) x[i])), decreasing = TRUE)[1:5]
    data.frame(keywords = names(dd),
               values = as.numeric(dd),
               position = sprintf("Position #%d", i),
               idx = 1:5)
})
actresses_top <- do.call(rbind, actresses_top)
actresses_top$type <- "Actresses"

alldata <- rbind(actors_top, actresses_top)

plt1 <- ggplot(alldata) + geom_bar(aes(x = idx, y = values), 
                                   stat = "identity", fill = "lightgray",
                                   color = "black") + 
    facet_grid(factor(position)~ type) + 
    geom_text(aes(x = idx, y = values, label = keywords), nudge_y = 3) +
    xlab("") + ylab("Number of occurrences")
plot(plt1)
