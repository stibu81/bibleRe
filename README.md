
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bibleRe <a href="https://koenizerbibliotheken.ch"><img src="https://koenizerbibliotheken.ch/de/assets/app-icons/app-icon-72.png" align="right" height="110" /></a>

## Overview

bibleRe is a simple Shiny app that allows to simultaneously access
multiple accounts at the [public library in
Köniz](https://koenizerbibliotheken.ch). Users can check the list of
borrowed documents, fees, and reservations. It is also possible to
extend loans and download the list of documents as an Excel file.

## Setup

Install the package and all mandatory dependencies with

``` r
# install.packages("devtools")
devtools::install_github("stibu81/biblere")
```

In order to enable Excel export, you will need to install `WriteXLS` or
`writexl`. The former creates somewhat nicer Excel files, but requires a
Perl installation, while the latter has no dependencies. The function
`bib_setup_excel_export()` simplifies the setup by automatically
installing `WriteXLS` if Perl is available and `writexl` otherwise.

bibleRe reads the login data for the library accounts from a JSON file.
An example file is distributed together with the package. You can obtain
the path to the file by running

``` r
system.file("example/biblere_passwords.json", package = "bibleRe")
```

The structure of the file is as follows:

    {
      "User1": {
        "username": "123456",
        "password": "password1"
      },
      "User2": {
        "username": "104392",
        "password": "password2"
      },
      "User3": {
        "username": "101010",
        "password": "password3"
      }
    }

For each account, create an object with keys `"username"` and
`"password"`. The names of the accounts (`"User1"`, …) are arbitrary and
bibleRe will use them to reference the accounts.

By default, bibleRe looks for a file called `.biblere_passwords` in the
home directory. However, it is possible to specify a different filename
when starting the application.

## Running bibleRe

bibleRe can be run from an R session by calling

``` r
bibleRe::run_biblere()
```

However, typically bibleRe will be run from outside an R session using
`Rscript`. On Unix-like systems, this can be done with

``` bash
Rscript -e "bibleRe::run_biblere(launch.browser = TRUE)"
```

Note that you must use `launch.browser = TRUE` in order to automatically
run bibleRe in a browser window.

In addition to `launch.browser`, the function has the following
arguments:

- `login_data_file`: full path to a file containing the login data. By
  default, bibleRe looks for a file called `.biblere_paswords` in the
  home directory.
- `n_due_days`: on startup, bibleRe will only show documents that need
  to be returned within `n_due_days` days from today. The default value
  is 7.
- `use_switches`: set this to `TRUE` in order to use a design that uses
  switches and buttons instead of check boxes and radio buttons. The
  default value is `FALSE`.

## Icon

An icon for bibleRe can be downloaded from the library’s website by
calling

``` r
bibleRe::bib_get_icon()
```

By default, this will create a file `biblere.ico` in the home directory,
but a custom filename can be passed as a character argument.
