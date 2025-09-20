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
