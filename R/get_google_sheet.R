get_google_sheet <- function(
    url = 'https://docs.google.com/spreadsheets/d/1sywjZ6quCmbLBq_wEEXZEDzcW0NeBpCtRaT3W1hDkhI',
    format = 'csv',
    sheetId = NULL
) {

  # Taken from Max Conway: https://github.com/maxconway/gsheet/tree/master
  key <- stringr::str_extract(url, '[[:alnum:]_-]{30,}')
  if(is.null(sheetId) & stringr::str_detect(url, 'gid=[[:digit:]]+')){
    sheetId <- as.numeric(stringr::str_extract(stringr::str_extract(url,'gid=[[:digit:]]+'),'[[:digit:]]+'))
  }
  address <- paste0('https://docs.google.com/spreadsheets/export?id=',key,'&format=',format)
  if(!is.null(sheetId)){
    address <- paste0(address, '&gid=', sheetId)
  }

  df <- read_csv(
    address,
    col_types = cols(
      team_name = col_character(),
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