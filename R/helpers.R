# jump to an url within a running session
# rvest::session_jump_to() does not work for live sessions
# => use method of the ChromoteSession object
jump_to <- function(session, url) {
  session$session$go_to(url)
}


# set the language in a running session
# bibleRe only works correctly with German ("de")
set_language <- function(session, language = c("de", "en", "fr")) {
  language <- match.arg(language)

  # get the link to switch the language
  lang_url <- rvest::html_element(session, glue("a[lang='{language}']")) %>%
    rvest::html_attr("href")

  # if the desired language is already set, lang_url is "#"
  if (lang_url != "#") {
    jump_to(session, paste0(bib_urls$domain, lang_url))
  }

  invisible(NULL)
}


# a function that waits until an function evaluates to TRUE
# the function is run within a try block and failure  counts as FALSE
# timeout is the maximal waiting time in seconds
# the function returns a boolean indicating whether the function ended up
# evaluating as TRUE.

wait_until <- function(fun, timeout = 5) {
  start <- lubridate::now()
  while (TRUE) {
    is_true <- isTRUE(try(fun(), silent = TRUE))
    dt <- round(difftime(lubridate::now(), start, units = "secs"), 1)
    if (dt > timeout) {
      logger::log_trace("wait_until() endeded with failure after {dt} seconds")
      return(FALSE)
    }
    if (is_true) {
      logger::log_trace("wait_until() endeded with success after {dt} seconds")
      # for some reason, running fun() right after wait_until() sometimes fails.
      # this happens less if we wait a little more here.
      Sys.sleep(0.05)
      return(TRUE)
    }
    Sys.sleep(0.05)
  }
}
