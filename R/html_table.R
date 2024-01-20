#' Parse Potentially Faulty HTML Table
#'
#' @description
#' Some html tables on the library site are faulty because they use `<td>`
#' instead of `<th>` in the table header. As a consequence, `rvest::html_table()`
#' does not use the headers as column names. This function recognises that
#' mistake and fixes the column headers.
#'
#' The function also removes columns without name and replaces HTML line
#' breaks (`<br>`) by `\n`.
#'
#' @param tab_node a node set containing a table

flex_html_table <- function(tab_node) {

  replace_br(tab_node)

  table <- rvest::html_table(tab_node)

  # remove columns with empty name because they cause problems for dplyr
  table <- table[, names(table) != "" & !is.na(names(table))]

  # check: if the table names are X1, X2, ..., the headers could not be
  # parsed. Fix this.
  bad_names <- paste0("X", seq_along(table))
  if (identical(bad_names, names(table))) {
    # use the first line as names, drop it from the table
    new_names <- unlist(table[1, ])
    names(table) <- new_names
    table <- table[-1, ]

    # remove columns with empty name because they cause problems for dplyr
    table <- table[, names(table) != "" & !is.na(names(table))]

    # because the headers were read as values, all columns are character.
    # Try to parse them as other values.
    table <- table %>%
      dplyr::mutate(dplyr::across(
        dplyr::where(is.character),
        ~readr::parse_guess(., guess_integer = TRUE)
      ))
  }

  table

}


# replace HTML line breaks by "\n"
replace_br <- function(node) {
  # source: https://stackoverflow.com/a/46755666/4303162
  # user: hrbrmstr
  xml2::xml_find_all(node, ".//br") %>%
    xml2::xml_add_sibling("p", "\n")
  xml2::xml_find_all(node, ".//br") %>%
    xml2::xml_remove()
}
