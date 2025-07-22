#### Housekeeping prior to start of the server function ####

### .csv Pre-processing ###

PREPROCESSING_TARGET_COLUMNS <- c("latin_name", "tsn", "plot_name", "l_Unit_Code") # network-specific; these columns may not apply to non-NCRN networks

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
    
    # write updated dataframe 
    out_file <- file.path(folderpath, basename(file))
    write.csv(df, file = out_file, row.names = FALSE)
}

is_preprocessing_necessary <- function(folderpath, target_columns) {
    # Check whether the column-renaming (i.e., preprocessing) procedure needs to run
    #
    # Args:
    #   folderpath (chr, required): relative filepath to folder where csv should be written. E.g., 'Data/NCRN'.
    #   target_columns(c(chr), required): a vector of character strings. Each string is the name of a column name
    #       that the this function should search for in the csv files from `folderpath`. E.g., 'Latin_Name'.
    #
    # Returns:
    #   list
    #       A list of two elements.
    #       1) needs_preprocessing_bool: (logical). If T, one or more columns need renaming in one or more files and
    #           the preprocess() function should run. If F, the preprocess() function does need to run. T or F.
    #       2) files: (c(chr)). Vector of character strings. Each string is the relative filepath to a csv
    #           file in the directory `folderpath`.
    #
    # Examples:
    #   is_preprocessing_necessary(folderpath='Data/NCRN', target_columns=c('latin_name','tsn'))
    #
    
    
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
    # Application's interface with the logic to find and correct column-name problems in source csvs
    #
    # Why is this necessary?
    # As of 2025-07-22, the SQL queries that output NCRN's forest veg data csvs (e.g., Plots.csv)
    # have some column-naming mistakes. For example, the capitalization of column "Latin_Name" is
    # "latin_name" in one or more files. For R package NCRNForVeg to import the data properly,
    # the column names must match those that the package expects. This pre-processing
    # section corrects the column names and is intended to be a workaround until the queries are
    # corrected in the database.
    #
    # Args:
    #   Network (chr, required): The acronym for one of the networks served by this application. E.g., 'NCRN'
    #
    # Returns:
    #   None. This function does not return an object. If necessary, the function reads, edits, and then writes csvs.
    #
    
    folderpath <- file.path("Data",Network)
    x <- is_preprocessing_necessary(folderpath=folderpath, target_columns=PREPROCESSING_TARGET_COLUMNS)
    
    if (x$needs_preprocessing_bool == T) {
        
        np <- paste0(Network,"_original")
        newpath <- file.path("Data",np)
        dir.create(newpath, recursive = TRUE)
        file.rename(from = x$files,
                    to = file.path(newpath, basename(x$files)))
        files <- list.files(path = newpath, pattern = "\\.csv$", full.names = TRUE)
        for (file in files){rename_targets(file, folderpath)}
        
    }    
}

