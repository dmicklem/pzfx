#' Write one table or multiple tables to a 'GraphPad Prism' '.pzfx' file
#'
#' Write one table or multiple tables to a 'GraphPad Prism' '.pzfx' file. A table can be a 'matrix',
#'   a 'data.frame', or a 'tibble'. All elements of the table should be numeric.
#'
#' DRM Add the possibility to add one or more Notes tables. The table is a set of constant, value pairs.
#'   The 'Notes' constant will be treated separately.
#' @param x Input table or named list of tables that will be 'Data Tables' in the '.pzfx' file
#'     Note that if it is just one table, it will get a default name. To avoid this, supply a named list of length 1
#' @param path Path to the output '.pzfx' file.
#' @param row_names Logical. If row names of the input table should be preserved and become row
#'   titles in the output '.pzfx' file. If the length is greater than 1, it must match the length of
#'   list of input tables. Default: TRUE.
#' @param x_col 1-based column index or name of the column to be used as the 'X' column. If the
#'   length is greater than 1, it must match the length of list of input tables. All other columns
#'   in the input tables will be treated as "Y" columns in the output '.pzfx' file. Default: NA
#' @param x_err as for x_col, defines column to be used as the error for x-values
#' @param n_digits An integer specifying the number of digits to display for float values in the
#'   generated '.pzfx' file. This argument has no effect if data is of type 'integer'. Note this
#'   argument only affects how many digits are displayed. The actual data does not change.
#' @param notes Notes table or names list of tables that will be 'Notes' tables in the '.pzfx' file
#' @param subcolumns Number of subcolumns that data should be split across. If the length is greater
#'    than 1, it must match the length of the list of input tables. Can also be "SDN" in which case there
#'    are 3 subcols for mean, SD, N. These must be present in the correct order in the table
#' @param subcolumn_suffix Suffix on column names that identifies subcolumns. If length is greater
#'    than 1, it must match the length of the list of input tables. If empty, each group will contain one Y-column,
#'    padded to the numbcolumns with blanks
#'
#' @return write_pzfx returns the input x invisibly.
#'
#' @export
#'
#' @examples
#' pzfx_file <- system.file("extdata/exponential_decay.pzfx", package = "pzfx", mustWork = TRUE)
#' df <- read_pzfx(pzfx_file, table = 1, strike_action = "exclude")
#' write_pzfx(df, path = tempfile(fileext = ".pzfx"), row_names = TRUE)
write_pzfx <- function(x, path, row_names=TRUE, x_col=NA,x_err=NA, n_digits=NA, notes=NA, subcolumns=1, subcolumn_suffix="") {
  # figure out if notes is a single table or multiple of them (or nothing)
  if (length(notes)==1 && is.na(notes)) {
      default_df <- data.frame(
        name = c("Experiment Date", "Experiment ID", "Notebook ID", "Project", "Experimenter", "Protocol", "Notes"),
        value = c(NA,NA,NA,NA,NA,NA,NA),
        stringsAsFactors = FALSE
      )
      notes<-list(default_df)
  }
  if (inherits(notes, c("data.frame", "matrix"))) {
    n_lst <- list("Project info 1"=notes)
  } else if (inherits(notes, "list")) {
    n_lst <- notes
    if (is.null(names(n_lst))) names(n_lst) <- paste("Project info ", seq_len(length(n_lst)))
    are_dfs <- sapply(n_lst, function(x) inherits(x, c("data.frame", "matrix")))
    if (any(!are_dfs)) stop(sprintf("These elements are not data frame or matrix: %s",
                                    paste(names(n_lst)[!are_dfs], collapse=", ")))
  } else {
    n_lst<-NA
  }

  # figure out if x is a single table or multiple of them
  if (inherits(x, c("data.frame", "matrix"))) {
    x_lst <- list("Data 1"=x)
  } else if (inherits(x, "list")) {
    x_lst <- x
    if (is.null(names(x_lst))) names(x_lst) <- paste("Data", seq_len(length(x_lst)))
    are_dfs <- sapply(x_lst, function(x) inherits(x, c("data.frame", "matrix")))
    if (any(!are_dfs)) stop(sprintf("These elements are not data frame or matrix: %s",
                                    paste(names(x_lst)[!are_dfs], collapse=", ")))
  } else {
    stop(sprintf("Cannot process x of class %s", paste(class(x), collapse=", ")))
  }
  # make sure all elements are numeric
  are_nums <- sapply(x_lst, function(x) all(sapply(x, is.numeric)))
  if (any(!are_nums)) {
    stop(paste0("These tables are not all numeric: ",
                paste(names(x_lst)[!are_nums], collapse=", "),
                ". Such tables are not supported by Prism GraphPad. ",
                "You may want to spread / pivot the input data by non-numeric columns into a 'wide' format, ",
                "where the table elements are all numeric."
    ))
  }
  # make sure row_names matches the length of x_lst
  if (length(row_names) == 1) row_names <- rep(row_names, length(x_lst))
  if (length(row_names) != length(x_lst)) {
    stop("Argument 'row_names' can only be of length 1 or the length of 'x'")
  }
  # make sure subcolumns matches the length of x_lst
  if (length(subcolumns) == 1) subcolumns <- rep(subcolumns, length(x_lst))
  if (length(subcolumns) != length(x_lst)) {
    stop("Argument 'subcolumns' can only be of length 1 or the length of 'x'")
  }
  # make sure subcolumn_suffix matches the length of x_lst
  if (length(subcolumn_suffix) == 1) subcolumn_suffix <- rep(subcolumn_suffix, length(x_lst))
  if (length(subcolumn_suffix) != length(x_lst)) {
    stop("Argument 'subcolumn_suffix' can only be of length 1 or the length of 'x'")
  }
  # convert other kinds of x_col specifications to a vector of integers
  if (length(x_col) == 1) x_col <- rep(x_col, length(x_lst))
  if (length(x_col) != length(x_lst)) {
    stop("Argument 'x_col' can only be of length 1 or the length of 'x'")
  }
  x_col[is.na(x_col)] <- 0
  if (is.numeric(x_col)) {
    x_col <- as.integer(x_col)
    if (any(x_col > sapply(x_lst, ncol))) {
      vio <- which(x_col > sapply(x_lst, ncol))
      stop(sprintf("Not enough columns for table %s", paste(names(x_lst)[vio], collapse=", ")))
    }
  }
  if (is.character(x_col)) {
    for (i in seq_len(length(x_col))) {
      idx <- which(colnames(x_lst[[i]]) == x_col[i])
      if (length(idx) == 0) {
        warning(sprintf(
          "Column %s is not in table %s, not used as 'X' column",
          x_col[i], names(x_lst[[i]])))
        x_col[i] <- 0
      } else if (length(idx) > 1) {
        warning(sprintf(
          "Column %s has multiple occurance in table %s, only the first one used as 'X' column",
          x_col[i], names(x_lst[[i]])))
        x_col[i] <- idx[1]
      } else {
        x_col[i] <- idx
      }
    }
    x_col <- as.integer(x_col)
  }

  # convert other kinds of x_err specifications to a vector of integers
  if (length(x_err) == 1) x_err <- rep(x_err, length(x_lst))
  if (length(x_err) != length(x_lst)) {
    stop("Argument 'x_err' can only be of length 1 or the length of 'x'")
  }
  x_err[is.na(x_err)] <- 0
  if (is.numeric(x_err)) {
    x_err <- as.integer(x_err)
    if (any(x_err > sapply(x_lst, ncol))) {
      vio <- which(x_err > sapply(x_lst, ncol))
      stop(sprintf("Not enough columns for table %s", paste(names(x_lst)[vio], collapse=", ")))
    }
  }
  if (is.character(x_err)) {
    for (i in seq_len(length(x_err))) {
      idx <- which(colnames(x_lst[[i]]) == x_err[i])
      if (length(idx) == 0) {
        warning(sprintf(
          "Column %s is not in table %s, not used as 'X' column",
          x_err[i], names(x_lst[[i]])))
        x_err[i] <- 0
      } else if (length(idx) > 1) {
        warning(sprintf(
          "Column %s has multiple occurance in table %s, only the first one used as 'X' column",
          x_err[i], names(x_lst[[i]])))
        x_err[i] <- idx[1]
      } else {
        x_err[i] <- idx
      }
    }
    x_err <- as.integer(x_err)
  }
  lst <- base_lst()
  lst$GraphPadPrismFile$InfoSequence <- notes_seq_lst(n_lst)
  lst$GraphPadPrismFile <- append(lst$GraphPadPrismFile, info_lst(n_lst))
  lst$GraphPadPrismFile$TableSequence <- table_seq_lst(x_lst)
  lst$GraphPadPrismFile <- append(lst$GraphPadPrismFile, table_lst(x_lst, row_names, x_col,x_err, n_digits, subcolumns, subcolumn_suffix))
  attr(lst$GraphPadPrismFile, "PrismXMLVersion") <- "5.00"
  xml <- xml2::as_xml_document(lst)
  xml2::write_xml(xml, path)
  invisible(x)
}

# The basic list for a pzfx xml
base_lst <- function() {
  lst <- list(
    "GraphPadPrismFile"=list(
      "Created"=list(
        "OriginalVersion"=structure(
          list(),
          CreatedByProgram="GraphPad Prism",
          CreatedByVersion="6.0f.254",
          Login="",
          DateTime=strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M:%S+00:00")
        )
      )
      # Then, the "InfoSeqence" and "Notes lists go here
      # Then, the "TableSequence" list goes here
      # Then, all "Table" lists go here
    )
  )
  return(lst)
}

# "TableSequence" element of the list for a pzfx xml
# Number of Refs corresponds to number of tables
table_seq_lst <- function(x_lst) {
  ret <- lapply(seq_len(length(x_lst)), function(i) {
    ref <- structure(
      list(),
      "ID"=sprintf("Table%d", i - 1)
    )
    if (i == 1) attr(ref, "Selected") <- "1"
    return(ref)
  })
  names(ret) <- rep("Ref", length(x_lst))
  return(ret)
}

# "InfoSequence" element of the list for a pzfx xml
# Number of Refs corresponds to number of information tables
notes_seq_lst <- function(n_lst) {
  ret <- lapply(seq_len(length(n_lst)), function(i) {
    ref <- structure(
      list(),
      "ID"=sprintf("Info%d", i - 1)
    )
    if (i == 1) attr(ref, "Selected") <- "1"
    return(ref)
  })
  names(ret) <- rep("Ref", length(n_lst))
  return(ret)
}


# "Table" elements of the list for a pzfx xml
# As many tables as you have
# Currently only supports pzfx's "Column" type of tables
# DRM: add ability to have subcolumns
table_lst <- function(x_lst, row_names, x_col, x_err, n_digits, subcolumns, subcolumn_suffix) {
  if (length(x_lst) != length(row_names)) {
    stop("Argument 'row_names' can only be of the same length as 'x_lst'")
  }
  if (length(x_lst) != length(x_col)) {
    stop("Argument 'x_col' can only be of the same length as 'x_lst'")
  }
  if (length(x_lst) != length(subcolumns)) {
    stop("Argument 'subcolumns' can only be of the same length as 'x_lst'")
  }
  if (length(x_lst) != length(subcolumn_suffix)) {
    stop("Argument 'subcolumn_suffix' can only be of the same length as 'x_lst'")
  }
  if (!is.integer(x_col)) {
    stop("Argument 'x_col' can only be of type 'integer'")
  }

  ret <- lapply(seq_len(length(x_lst)), function(i) {
    this_df <- x_lst[[i]]
    this_xcol<-x_col[[i]]
    this_xerr<-x_err[[i]]
    this_suffix<-subcolumn_suffix[[i]]
    this_subcols<-subcolumns[[i]]
    table_type="OneWay"
    y_format="replicates"
    x_format="none"
    if (this_subcols == "SDN") {
      y_format="SDN"
      this_subcols<-3
    }
    if (!(this_xcol ==0)) {
      if (this_xerr ==0) {
        #One xcol so a normal XY table
      xcols<-structure(list("XColumn"=list(
        "Title"=list(names(this_df)[this_xcol]),
        "Subcolumn"=subcol_helper(this_df[, this_xcol, drop=TRUE]))), #drop=TRUE converts this to a vector
        Width="89",
        Decimals=as.character(n_digits), #It doesn't really matter as it is easy to change
        Subcolumns="1"
      )
      this_df<-this_df[,-this_xcol]
      x_format="numbers"
      table_type="XY"
      } else  {
       #Two columns specify X value and error bar.
        # xcols<-structure(list("XColumn"=list(
        #   "Title"=list(names(this_df)[this_xcol]),
        #   "Subcolumn"=subcol_helper(this_df[, this_xcol, drop=TRUE]), #drop=TRUE converts this to a vector
        #   "Subcolumn"=subcol_helper(this_df[, this_xerr, drop=TRUE]))),
        #   Width="120",
        #   Decimals=as.character(n_digits), #It doesn't really matter as it is easy to change
        #   Subcolumns="2"
        #
        # )
        these_subcols<-lapply(c(this_xcol,this_xerr), function(c) {subcol_helper(this_df[, c, drop=TRUE])})
        names(these_subcols)<-rep(c("Subcolumn"),length(these_subcols))
        title_list=list("Title"=list(names(this_df)[this_xcol]))

        #   )
        cols <-  append(title_list,these_subcols)
        xcols<-list("XColumn"=structure(cols,
          Width="120",
          Decimals=as.character(n_digits), #It doesn't really matter as it is easy to change
          Subcolumns="2"
        ))
        # xcols
        # xcols<-structure(list(list(
        #   "Title"=list(names(this_df)[this_xcol]),
        #  "Subcolumn"=subcol_helper(this_df[, this_xcol, drop=TRUE]), #drop=TRUE converts this to a vector
        #  "Subcolumn"=subcol_helper(this_df[, this_xerr, drop=TRUE]))),
        #   Width="120",
        #   Decimals=as.character(n_digits), #It doesn't really matter as it is easy to change
        #   Subcolumns="2"
        #
        # )
        # names(xcols)<-"XColumn"
        this_df<-this_df[,-c(this_xcol,this_xerr )]
        x_format="error"
        table_type="XY"


      }
    }
    ycols<-generate_subcolumns(this_df,this_subcols, subcolumn_suffix=this_suffix, n_digits)

    one_table_lst <- list("Title"=list(names(x_lst)[i]))
    if (row_names[i]) {
      one_table_lst[["RowTitlesColumn"]] <- structure(
        list("Subcolumn"=subcol_helper(row.names(this_df))),
        Width="39"
      )
    }
    one_table <- structure(
      append(one_table_lst, append(xcols,ycols)),
      ID=sprintf("Table%d", i - 1),
      XFormat=x_format,
      YFormat=y_format,
      Replicates=as.character(this_subcols),
      TableType=table_type,
      EVFormat="AsteriskAfterNumber"
    )

    return(one_table)
  })
  names(ret) <- rep("Table", length(x_lst))
  return(ret)
}

# "Info" elements of the list for a pzfx xml
# As many Info tables as you have, but if none, include default
#
info_lst <- function(x_lst) {

  ret <- lapply(seq_len(length(x_lst)), function(i) {
    this_df <- x_lst[[i]]
    notes_row <- this_df %>%
      filter(Name == "Notes")
    constant_data <- this_df %>%
      filter(Name != "Notes")
    if (nrow(notes_row) > 0) {
      note_values <- notes_row$Value
      # Construct the R list structure for the <Notes> block
      notes_list_item <- list(
        Title = list(names(x_lst)[i]),
        Notes = list(
          Font = structure(
            list(
              # Text content for Font node
              lapply(note_values, function(x) {list(x, list(BR=list()))})
              # Empty list for <BR> child
              #BR = list()
            ),
            # Attributes for the Font tag
            Color = "#000000",
            Face = "Helvetica"
          )
        )
      )
    }
    if (nrow(constant_data) > 0) {
      for (j in seq_len(nrow(constant_data))) {
        current_name <- constant_data$Name[j]
        current_value <- constant_data$Value[j]
        constant_list_item <- list(
          Constant = list(
            Name = list(current_name),
            Value = list(current_value)
          )
        )
        # Append it to our main list
        notes_list_item <- c(notes_list_item, constant_list_item)
      }
    }
    one_table <- structure(
      notes_list_item,
      ID=sprintf("Info%d", i - 1)
    )
    return(one_table)
  })
  names(ret) <- rep("Info", length(x_lst))
  return(ret)
}




generate_subcolumns<-function(df,expected_count, subcolumn_suffix="_\\d+$",n_digits=2) {
# Gemini's suggestion for splitting but subgroup...
# 1. Split into initial groups
grouping_factor <- sub(subcolumn_suffix, "", names(df))
initial_list <- split.default(df, f = grouping_factor)
initial_list <-initial_list[unique(grouping_factor)]


# 2. Process each data.frame in the list
final_df_list <- lapply(seq_len(length(initial_list)), function(group) {
  current_frame <- initial_list[[group]]
  count_found <- ncol(current_frame)
  if (count_found > expected_count) {
    stop(paste0(
      "Group '", group, "' has ", count_found, " columns, but ",
      expected_count, " were expected."
    ))
  }
  if (count_found < expected_count) {
    num_to_add <- expected_count - count_found

    # Create a new data.frame of NAs with the correct dimensions
    padding_df <- as.data.frame(
      matrix(NA, nrow = nrow(current_frame), ncol = num_to_add)
    )

    # Combine the original frame with the padding frame
    current_frame <- cbind(current_frame, padding_df)
  }

  return(current_frame)
})

# 3. Restore the names of the list
names(final_df_list) <- names(initial_list)

# 4. Format to suitable list
ret <- lapply(seq_len(length(final_df_list)), function(i) { #For each set of subcolumns
  this_df <- final_df_list[[i]]
  these_subcols<-lapply(seq_len(ncol(this_df)), function(c) {subcol_helper(this_df[, c, drop=TRUE])})
  names(these_subcols)<-rep(c("Subcolumn"),length(these_subcols))
  title_list=list("Title"=list(names(final_df_list)[i]))

  #   )
  cols <- structure(
                    append(title_list,these_subcols),
                    Width=as.character(89*expected_count),
                    Decimals=as.character(n_digits), #It doesn't really matter as it is easy to change
                    Subcolumns=as.character(expected_count)
  )

  return(cols)
  })
names(ret)<-rep("YColumn",length(ret))
return(ret)
}


subcol_helper <- function(v) {
  v <- as.vector(v)
  lapply(v, function(e) list("d"=list(as.character(e))))
}
decimal_helper <- function(v, n_digits) {
  n_digits <- round(n_digits)
  # if data is of integer type then we don't want to introduce any decimals
  if (is.integer(v)) {
    return("0")
  }
  # if n_digits is specified just go with that
  if (!is.na(n_digits)) {
    return(as.character(n_digits))
  }
  # otherwise we make a guess. if data looks like integer return 0, otherwise 2
  if (all(v %% 1 == 0, na.rm=TRUE)) {
    return("0")
  }
  return("2")
}

#Examples for testing
firstNotes<-data.frame(
  Name = c("Experiment Date", "Experiment ID", "Notebook ID", "Project", "Experimenter", "Protocol", "DRM_custom_constant","Notes","Notes"),
  Value= c("2025-06-19", "DRM_experimentID", "DRM_NotebookID","DRM_ProjectID", "DRM_Experimenter", "DRM_Protocol", "DRM_custom_constant_value", "This is an extended","bit of notes"),
  stringsAsFactors = FALSE
)

secondNotes<-data.frame(
  Name = c("Another constant", "Notes"),
  Value= c("2025-06-19", "This is another note"),
  stringsAsFactors = FALSE

)

listNotes<-list(firstNotes, secondNotes)
names(listNotes)<-c("Project Info 1", "A Global Note")

myData<-data.frame(
  Time = c(1,2,3,4,5,6,7,8),
  Time_error = c(0.1,0.1,0.1,0.2,0.1,0.1,0.1,0.1),
  Measurement_1 = c(2,4,8,16,32,64,128,256),
  Measurement_2 = c(12,14,18,116,132,164,1128,1256),
  Measurement_3 = c(22,24,28,216,232,264,2128,2256),
  Other_1=c(2,4,8,16,32,64,128,256)
)
myData2<-data.frame(
  First_1 = c(1,2,3,4,5,6,7,8),
  First_2 = c(0.1,0.1,0.1,0.2,0.1,0.1,0.1,0.1),
  Sec_1 = c(2,4,8,16,32,64,128,256),
  Sec_2 = c(12,14,18,116,132,164,1128,1256),
  th_1 = c(22,24,28,216,232,264,2128,2256),
  th_2=c(2,4,8,16,32,64,128,256)
)
listDfs<-list(myData,myData2)
names(listDfs)<-c("Powersof2","AnotherSimpler")
mypath<-'/Users/david.micklem/Desktop/Testpzfx.pzfx'
write_pzfx(listDfs, mypath, row_names=FALSE, x_col=c(1,0), x_err=c(2,0), notes=listNotes, subcolumns=c(3,2), subcolumn_suffix="_\\d+$",n_digits=2)


