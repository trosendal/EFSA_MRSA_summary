##' table1
##'
##' Table 1 - CC398 only in main chapter
##' Produce Table 1 in the main chapter
##'
##' @param spaprev The object from the prev_by_SPA() function
##' @param path_csv path to the output csv file
##' @import data.table
##' @importFrom utils write.csv2
##' @return A path to a csv file
##' @export
table1 <- function(spaprev = prev_by_SPA(),
                   path_csv = tempfile(fileext = ".csv")) {

    ## Sort for most common to least common across all types
    sortorder <- spaprev[, sum(Total), by = .(SPA)][order(V1), SPA]

    ## Just the CC398 spa types
    df <- spaprev[CC %in% c("398", "CC1/CC398")]
    df <- df[order(match(SPA, sortorder), Year, decreasing = TRUE)]
    table123_inner(df,
                   path_csv = path_csv)
}

##' table2
##'
##' Table 2 - Other types that are not CC398
##' Produce Table 2 in the main chapter
##'
##' @param spaprev The object from the prev_by_SPA() function
##' @param path_csv path to the output csv file
##' @import data.table
##' @importFrom utils write.csv2
##' @return A path to a csv file
##' @export
table2 <- function(spaprev = prev_by_SPA(),
                   path_csv = tempfile(fileext = ".csv")) {

    ## Sort for most common to least common across all types
    sortorder <- spaprev[, sum(Total), by = .(SPA)][order(V1), SPA]

    ## Just the CC398 spa types
    df <- spaprev[!(CC %in% c("398", "CC1/CC398")) &
                  !is.na(CC)]
    df <- df[order(match(SPA, sortorder), Year, decreasing = TRUE)]
    table123_inner(df,
                   path_csv = path_csv)
}

##' table123_inner
##'
##' @param df the subset of prev_by_SPA
##' @param path_csv path to the csv file
##' @return A table
table123_inner <- function(df, path_csv) {
    df <- df[, {
        years <- sort(levels(Year), decreasing = TRUE)
        ## First food
        food1 <- matrix[source == "food" & Year == years[1]]
        food1 <- ifelse(identical(food1, character(0)), "", food1)
        foodtot1 <- Total[source == "food" & Year == years[1]]
        foodtot1 <- ifelse(identical(foodtot1, numeric(0)), "", foodtot1)

        ## first animal
        animal1 <- matrix[source == "animal" & Year == years[1]]
        animal1 <- ifelse(identical(animal1, character(0)), "", animal1)
        animaltot1 <- Total[source == "animal" & Year == years[1]]
        animaltot1 <- ifelse(identical(animaltot1, numeric(0)), "", animaltot1)

        ## second food
        food2 <- matrix[source == "food" & Year == years[2]]
        food2 <- ifelse(identical(food2, character(0)), "", food2)
        foodtot2 <- Total[source == "food" & Year == years[2]]
        foodtot2 <- ifelse(identical(foodtot2, numeric(0)), "", foodtot2)

        ## second animal
        animal2 <- matrix[source == "animal" & Year == years[2]]
        animal2 <- ifelse(identical(animal2, character(0)), "", animal2)
        animaltot2 <- Total[source == "animal" & Year == years[2]]
        animaltot2 <- ifelse(identical(animaltot2, numeric(0)), "", animaltot2)

        data.frame(CC = rep(CC[1], 2),
                   Year = as.numeric(years),
                   animals = c(animal1, animal2),
                   animaltot = as.character(c(animaltot1, animaltot2)),
                   food = c(food1, food2),
                   foodtot = as.character(c(foodtot1, foodtot2)))
    }, by = .(SPA)]

    ## Format the spatype
    df$SPA <- sprintf("t%03d", as.numeric(df$SPA))

    ## Drop completely empty rows
    df <- df[animals != "" | food != ""]

    names(df) <- c("spa-type", "CC", "Year", "Animals (N)", "Total",
                   "Food (N)", "Total")
    write.csv2(df,
               file = path_csv,
               row.names = FALSE,
               quote = TRUE)
    path_csv
}
