library(readxl)
library(tidyverse)

#' Get OD values (normalized for blanks) from the plate reader's output
#' Excel sheet.
#'
#' This function assumes the following:
#' 1. cells with yeast are labelled as "YST#" where # is the strain number
#' 2. Blank cells are labelled as "BLK"
#'
#'
#' `filename` is name of excel file containing OD data
#' `od_dir` allows user to provide non-default folder for where
#'   the OD excel file is.
#' `col_is_strain` indicates whether the column number is the same
#'   as the strain number.
#'
get_od <- function(filename, od_dir = NULL, col_is_strain = TRUE) {
    if (is.null(od_dir)) {
        od_dir <- paste0("~/Desktop/")
    } else {
        stopifnot(length(od_dir) == 1 && inherits(od_dir, "character"))
        od_dir <- paste0(gsub("/$", "", od_dir), "/")
    }
    filename <- paste0(od_dir, filename)
    if (col_is_strain) {
        # Get strain based on col number:
        get_strain <- function(c, s) {
            sapply(c, \(z) strains[z], USE.NAMES = FALSE)
        }
    } else {
        # Get strain based on string:
        get_strain <- function(c, s) {
            zz <- as.integer(gsub("YST", "", s))
            sapply(zz, \(z) strains[z], USE.NAMES = FALSE)
        }
    }
    let2num <- function(lets) {
        stopifnot(all(nchar(lets) == 1))
        stopifnot(all(!grepl("[^A-Za-z]", lets)))
        sapply(tolower(lets), \(x) which(letters == x), USE.NAMES = FALSE)
    }
    # Strains (in order)
    strains <- c("Y382", "Y383", "Y385", "Y466", "Y467", "Y644",
                 "Y818", "Y821", "Y858", "Y893", "Y1092", "MR1")
    raw_df <- read_excel(filename, skip = 23, col_names = c("a", "b", 1:13),
                         col_types = "text") |>
        select(3:14)
    od_df <- crossing(row  = LETTERS[1:8], col = 1:12) |>
        mutate(strain = map2_chr(row, col,
                                 \(r, c) {
                                     raw_df[[c]][[let2num(r)]]
                                 }),
               od = map2_dbl(row, col,
                             \(r, c) {
                                 odr <- 2 * let2num(r) + 12
                                 round(as.numeric(raw_df[[c]][[odr]]), 3)
                             })) |>
        filter(!is.na(od)) |>
        mutate(od = od - mean(od[strain == "BLK"])) |>
        filter(strain != "BLK") |>
        mutate(strain = get_strain(col, strain))
    return(od_df)
}



get_od("plate-reader-data.xlsx")
