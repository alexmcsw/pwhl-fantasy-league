library(readr)
library(stringr)

#' @title  **Import a Google Sheet**
#' @description Import a Google Sheet
#'
#' @param url Google Sheet URL
#' @param format Google Sheet export format
#' @sheet_id Index of the desired Sheet in the Google Sheet file
#' @return data.frame read from the Google Sheet
#' @import readr
#' @import stringr
#' @export

get_google_sheet <- function(
  url = "https://docs.google.com/spreadsheets/d/1sywjZ6quCmbLBq_wEEXZEDzcW0NeBpCtRaT3W1hDkhI",
  format = "csv",
  sheet_id = NULL
) {
  # Taken from Max Conway: https://github.com/maxconway/gsheet/tree/master
  key <- stringr::str_extract(
    url,
    "[[:alnum:]_-]{30,}"
  )

  if (
    is.null(sheet_id) &&
      stringr::str_detect(
        url,
        "gid=[[:digit:]]+"
      )
  ) {
    sheet_id <- as.numeric(
      str_extract(
        stringr::str_extract(
          url,
          "gid=[[:digit:]]+"
        ),
        "[[:digit:]]+"
      )
    )
  }

  address <- paste0(
    "https://docs.google.com/spreadsheets/export?id=",
    key,
    "&format=",
    format
  )

  if (!is.null(sheet_id)) {
    address <- paste0(
      address,
      "&gid=",
      sheet_id
    )
  }

  df <- read_csv(
    address,
    col_types = cols(
      team_name = col_character(),
      team_image = col_character(),
      player_1 = col_integer(),
      player_2 = col_integer(),
      player_3 = col_integer(),
      player_4 = col_integer(),
      player_5 = col_integer(),
      player_6 = col_integer()
    )
  )

  return(df)
}
