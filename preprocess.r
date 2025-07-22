#### Housekeeping prior to start of the server function ####

### .csv Pre-processing ###
# Why is this necessary?
# As of 2025-07-22, the SQL queries that output NCRN's forest veg data csvs (e.g., Plots.csv)
# have some column-naming mistakes. For example, the capitalization of column "Latin_Name" is
# "latin_name" in one or more files. For R package NCRNForVeg to import the data properly,
# the column names must match those that the package expects. This pre-processing
# section corrects the column names and is intended to be a workaround until the queries are
# corrected in the database.
Network <- 'NCRN'
PREPROCESSING_TARGET_COLUMNS <- c("latin_name", "tsn", "plot_name", "l_Unit_Code") # naming-convention: constants (i.e., hard-coded variables that don't change) should be upper case

rename_targets <- function(file, folderpath) {
    # read a csv into dataframe, find known-incorrect column names, rename incorrect columns, write dataframe to csv
    #
    # Args:
    # file (chr, required): relative filepath to a csv required for NCRNForVeg. E.g., Plots.csv
    # folderpath (chr, required): relative filepath to folder where csv should be written. E.g., 'Data/NCRN'
    #
    # Returns:
    # None; writes each csv to file and returns no object
    #
    # Examples:
    # myfile <- 'Data/NCRN/Plots.csv'
    # mydir <- 'Data/NCRN'
    # rename_targets(myfile, mydir)
    #
    
    # read file into dataframe
    df <- read.csv(file, stringsAsFactors = FALSE)
    
    # check dataframe for known-incorrect column names
    latin_match <- tolower(names(df)) == "latin_name" # group the steps together instead of spreading them out, so it's easy to find things
    tsn_match <- tolower(names(df)) == "tsn"
    plot_name_match <- tolower(names(df)) == "plot_name"
    unit_code_match <- names(df) == "l_Unit_Code"
    
    # rename incorrect columns
    if (any(latin_match)) {
        names(df)[latin_match] <- "Latin_Name"
        warning(paste("Renamed latin_name column in:", file))
    }
    if (any(tsn_match)) {
        names(df)[tsn_match] <- "TSN"
        warning(paste("Renamed tsn column in:", file))
    }
    if (any(plot_name_match)) {
        names(df)[plot_name_match] <- "Plot_Name"
        warning(paste("Renamed plot_name column in:", file))
    }
    if (any(unit_code_match)) {
        names(df)[unit_code_match] <- "Unit_Code"
        warning(paste("Renamed l_Unit_Code column in:", file))
    }
    
    out_file <- file.path(folderpath, basename(file))
    write.csv(df, file = out_file, row.names = FALSE)
}

is_preprocessing_necessary <- function(folderpath, target_columns) {
    
    
    files <- list.files(path = folderpath, pattern = "\\.csv$", full.names = TRUE)
    files_with_column_name_problems <- list()
    for (file in files) { # no need for seq_along() since we can access the elements directly; we don't need the index
        df <- read.csv(file, stringsAsFactors = FALSE)
        match <- intersect(names(df), target_columns)
        files_with_column_name_problems[[basename(file)]] <- match
    }
    needs_preprocessing_bool <- any(sapply(files_with_column_name_problems, function(x,folderpath) length(x) > 0))
    x <- list(
        'needs_preprocessing_bool'=needs_preprocessing_bool
        ,'files'=files
    )
    return(x)
}



preprocess <- function(Network) {
    
    folderpath <- file.path("Data",Network)
    x <- is_preprocessing_necessary(folderpath=folderpath, target_columns=PREPROCESSING_TARGET_COLUMNS)
    needs_preprocessing_bool <- x[['needs_preprocessing_bool']]
    files <- x[['files']]
    
    if (needs_preprocessing_bool == T) {
        
        np <- paste0(Network,"_original")
        newpath <- file.path("Data",np)
        dir.create(newpath, recursive = TRUE)
        file.rename(from = files,
                    to = file.path(newpath, basename(files)))
        files <- list.files(path = newpath, pattern = "\\.csv$", full.names = TRUE)
        lapply(files, rename_targets(x, folderpath))
        
    }    
}
preprocess(Network=Network)
