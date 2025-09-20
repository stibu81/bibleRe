# jump to an url within a running session
# rvest::session_jump_to() does not work for live sessions
# => use method of the ChromoteSession object
jump_to <- function(session, url) {
  session$session$go_to(url)
}
