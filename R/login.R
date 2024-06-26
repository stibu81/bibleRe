#' Login to the Web Interface
#'
#' Login to the web interface with username and password.
#'
#' @param username character giving the username, which usually
#'  consists of 6 digits.
#' @param password character giving the password. See also
#'  the security warning under 'Details'.
#'
#' @details
#' **Security warning:**
#' It is possible that passwords are stored in clear text and
#' can even be seen by employees of the library. This has
#' been the case at some point and it is not known whether
#' this has changed in the meantime. Your email address is stored
#' as well. Do not use a password that is also used anywhere else!
#'
#' @return
#' a `session` object
#'
#' @export

bib_login <- function(username, password) {

  # is username is a list, check that it contains username and password
  if (is.list(username)) {
    if (all(c("username", "password") %in% names(username))) {
      password <- username$password
      username <- username$username
    } else {
      stop("invalid input for username")
    }
  }

  if (!bib_check()) {
    warning("connection failed")
    return(NULL)
  }

  session <- rvest::session(bib_urls$login)
  form <- rvest::html_form(session)[[2]]
  filled_form <- rvest::html_form_set(form,
                                      Username = username,
                                      Password = bib_encrypt(password))
  session <- rvest::session_submit(session, filled_form)

  # if the urls associated with the session is still the same,
  # login has not worked. => issue a warning
  if (session$response$status_code != 200 || session$url == bib_urls$login) {
    warning("login failed")
    return(NULL)
  }

  session

}


#' Read File With Login Data
#'
#' Read a JSON file with login data for one or more
#' users.
#'
#' @param file character giving the name of a file.
#'
#' @details
#' The JSON file must have the following format:
#'
#' ```json
#' {
#'   "User1": {
#'     "username": "123456",
#'     "password": "password1"
#'   },
#'   "User2": {
#'     "username": "104392",
#'     "password": "password2"
#'   },
#'   "User3": {
#'     "username": "101010",
#'     "password": "password3"
#'   }
#' }
#' ```
#' A block must be included for each user and must contain
#' the fields `username` and `password`. The above example is
#' also contained as a file in the package. You can obtain
#' the path to the file with
#'
#' ```r
#' system.file("example/biblere_passwords.json",
#'             package = "bibleRe")
#' ```
#'
#' @export

bib_read_login_data <- function(file) {

  if (!file.exists(file)) {
    warning("File ", file, " does not exist.")
    out <- list()
    attr(out, "error_type") <- "file-not-exist"
    return(out)
  }

  tryCatch({
      jsonlite::fromJSON(file)
    },
    error = function(e) {
      warning("File ", file, " is not a valid json file.")
      out <- list()
      attr(out, "error_type") <- "file-not-valid"
      out
    }
  )

}


#' Encrypt Password
#'
#' The page uses simple encryption to make the transfer of passwords more
#' secure. This function applies the appropriate encryption to generate a
#' password that can be used for login.
#'
#' @param password character giving the password to be encrypted
#'
#' @export

bib_encrypt <- function(password) {

  # this just gets a timestamp from the server. Creating the timestamp
  # in the code does not work reliably. Maybe, only timestamps that have been
  # request from the server will work?
  timestamp <- rvest::read_html(bib_urls$timestamp) %>%
    rvest::html_element("p") %>%
    rvest::html_text()
  enc <- paste0(timestamp, ":", password) %>%
    digest::digest(serialize = FALSE) %>%
    toupper()

  paste0(timestamp, ":", enc)
}


#' Check Whether the Login Page Is Reachable
#'
#' Check whether the login page can be reached. If this fails, either there is
#' no internet connection or the server of the library is down.
#'
#' @param silent logical: should error messages be suppressed?
#'
#' @return
#' logical indicating whether the login page could be reached
#'
#' @export

bib_check <- function(silent = TRUE) {
  session <- try(rvest::session(bib_urls$login), silent = silent)
  !(inherits(session, "try-error") || session$response$status_code != 200)
}



