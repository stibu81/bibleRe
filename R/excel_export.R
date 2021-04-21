#' Check whether Excel Export is Possible
#'
#' Check which, if any, of the two variants of Excel export supported by
#' `bibleRe` are currently available on the system.
#'
#' @details
#' `bibleRe` supports Excel export with two packages:
#' \describe{
#'   \item{`WriteXLS`}{This is the prefered method, but it requires a
#'   perl installation. Use [`WriteXLS::testPerl()`] to check whether perl
#'   and all the required modules are installed. This writes formatted
#'   Excel files with fixed first row and adjusted column width.}
#'   \item{`writexl`}{This is used, when `WriteXLS` is not installed and has
#'   the advantage that it has no external dependencies. However, the Excel files
#'   do not have a fixed first row and the columns all have default width.}
#' }
#'
#' The function checks the availability of each method and returns the name
#' of the package that will be used for writing Excel files with
#' [`bib_write_excel()`].
#'
#' @return
#' a character string with one of the values `"WriteXLS"`, `"writexl"` or
#' `"none"` indicating the method that will be used for Excel export.
#'
#' @export

bib_excel_method <- function() {

  if (rlang::is_installed("WriteXLS") && WriteXLS::testPerl(verbose = FALSE)) {
    "WriteXLS"
  } else if (rlang::is_installed("writexl")) {
    "writexl"
  } else {
    "none"
  }
}


#' Write a Table to an Excel File
#'
#' Write the table of documents, orders, or fees to an Excel file.
#'
#' @param table a tibble containing documents, orders, or fees as returned
#'  by the functions [`bib_list_documents()`], [`bib_list_fees()`],
#'  [`bib_list_orders()`], and [`bib_get_all_data()`]. The table may be
#'  filtered, but columns should be left intact.
#' @param file path to a file
#' @param type character indicating the type of the table to be written. This
#'  is required to translate column names to German and to set appropriate
#'  sheet names in the Excel file.
#'
#' @details
#' The method used for writing is determined by the availability of the
#' packages `WriteXLS` and `writexl`. Which method will be used can be checked
#' using [`bib_excel_method()`].
#'
#' @return
#' logical indicating write success. An Excel file is create as a side effect.
#'
#' @export

bib_write_excel <- function(table, file,
                            type = c("documents", "orders", "fees")) {

  excel_method <- bib_excel_method()
  if (excel_method == "none") {
    warning("Excel export is not possible on this system.")
    return(FALSE)
  }

  type <- match.arg(type)
  export_table <- create_export_table(table, type)

  if (excel_method == "WriteXLS") {
    WriteXLS::WriteXLS(
      export_table,
      file,
      SheetNames = get_table_name(type),
      AdjWidth = TRUE,
      BoldHeaderRow = TRUE,
      FreezeRow = 1
    )
  } else if (excel_method == "writexl") {
    list(export_table) %>%
      magrittr::set_names(get_table_name(type)) %>%
      writexl::write_xlsx(file)
  } else {
    warning("Invalid output from bib_excel_method(): ",
            excel_method)
    return(FALSE)
  }

  TRUE
}


#' Install packages for Excel export
#'
#' Excel export with [`bib_write_excel`] uses either [`WriteXLS::WriteXLS()`]
#' (prefered) or [`writexl::write_xlsx()`] to create Excel files. This function
#' installs the required packages. `WriteXLS` is only installed, if a perl
#' installation is found.
#'
#' @param pkgs character vector giving the packages to be installed. By default,
#'  both packages are tried, but `WriteXLS` is only installed if a `perl`
#'  installation is found.
#'
#' @details
#' Packages that are already installed won't be reinstalled.
#'
#' @return
#' character vector with the names of the packages that can be used for
#' Excel export (invisibly).
#'
#' @export

bib_setup_excel_export <- function(pkgs = c("WriteXLS", "writexl")) {

  pkgs <- match.arg(pkgs, several.ok = TRUE)

  out <- character(0)

  if ("WriteXLS" %in% pkgs) {

    # install WriteXLS if it is missing
    if (rlang::is_installed("WriteXLS")) {
      message("Package WriteXLS is already installed. Check perl setup.")
    } else {
      # check whether perl is available
      if (nzchar(Sys.which("perl"))) {
        message("Installing WriteXLS ...")
        utils::install.packages("WriteXLS")
      } else {
        message("No Perl installation found, WriteXLS will not be installed.")
      }
    }

    # check installation and perl setup
    if (rlang::is_installed("WriteXLS")) {
      perl_ok <- WriteXLS::testPerl(verbose = TRUE)
      if (perl_ok) out <- c(out, "WriteXLS")
    } else {
      message("Installation of WriteXLS failed.")
    }
  }

  if ("writexl" %in% pkgs) {

    # install WriteXLS if it is missing
    if (rlang::is_installed("writexl")) {
      message("Package writexl is already installed.")
    } else {
      message("Installing writexl ...")
      utils::install.packages("writexl")
    }

    # check installation
    if (rlang::is_installed("writexl")) {
      out <- c(out, "writexl")
    } else {
      message("Installation of writexl failed.")
    }
  }

  # check which method will be used for Excel export
  excel_method <- bib_excel_method()
  if (excel_method == "none") {
    message("Setup failed. Excel export is not possible.\n")
  } else {
    message("Setup was successful. Excel export will use package ",
            excel_method, ".\n")
  }

  invisible(out)
}


# helper function to create table for export
create_export_table <- function(table,
                                type = c("documents", "orders", "fees")) {

  type <- match.arg(type)

  hide_cols <- get_hidden_cols(type)
  col_names <- get_col_names(type)
  date_cols <- stringr::str_subset(names(table), "date")

  # if there is a column id, remove the link
  if ("id" %in% names(table)) {
    table %<>% dplyr::mutate(
      id = stringr::str_remove_all(.data$id, "<[^>]*>")
    )
  }

  # convert dates, rename columns
  table %<>%
    dplyr::mutate_at(date_cols,
                     ~format(., format = "%d.%m.%Y")) %>%
    magrittr::set_names(col_names)

  # remove hidden columns
  table <- table[, !names(table) %in% hide_cols]

  table

}


get_table_name <- function(type) {
  table_names <- list(documents = "Ausleihen",
                      orders = "Reservationen",
                      fees = "Geb\u00fchren")
  table_names[[type]]
}

