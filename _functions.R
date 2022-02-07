librarian::shelf(
  dplyr, DT, glue, googlesheets4, htmltools, purrr, readr, shiny, stringr, tidyr)
options(readr.show_col_types = F)

# edit: * [schedule | eds232-ml - Google Sheets](https://docs.google.com/spreadsheets/d/1lolwZ2CNAtUkTWPauyPEe_oVMknqS2yVvfnB1fiErzQ/edit#gid=0)
sched_gsheet <- "https://docs.google.com/spreadsheets/d/1lolwZ2CNAtUkTWPauyPEe_oVMknqS2yVvfnB1fiErzQ/edit"

get_sched <- function(){
  d_sched <- googlesheets4::read_sheet(sched_gsheet) %>%
    tidyr::fill(Week, Module)
  names(d_sched) <- names(d_sched) %>% stringr::str_replace("\n", " ")
  d_sched
}

dt_sched <- function(d_sched){
  d <- d_sched %>%
    # slice(1:3) %>%
    mutate(
      lec_youtube = map_chr(
        Lecture_youtube,
        function(x){
          if (is.na(x))
            return("")
          paste(
            " ",
            a(
              icon('youtube'),
              href   = x,
              target = '_blank') %>%
              as.character())
        }), #) %>%
      # select(Lecture_youtube, lec_youtube)
      Lecture = ifelse(
        is.na(Lecture),
        "",
        ifelse(
          is.na(Lecture_link),
          Lecture,
          glue::glue("<a href='{Lecture_link}' target='_blank'>{Lecture}</a>{lec_youtube}"))),
      Lab     = ifelse(
        is.na(Lab),
        "",
        ifelse(
          is.na(Lab_link),
          Lab,
          glue::glue("<a href='{Lab_link}' target='_blank'>{Lab}</a>"))),
      `Reading *` = ifelse(
        is.na(`Reading *`),
        "",
        ifelse(
          is.na(Reading_link),
          `Reading *`,
          glue::glue("<a href='{Reading_link}' target='_blank'>{`Reading *`}</a>"))),
      `Lab Due` = ifelse(
        is.na(`Lab Due`),
        "",
        glue::glue("{`Lab Due`} ({`Lab Pts`} pts)"))) %>%
    select(Week, Date, Module, Lecture, Lab, `Reading *`, `Lab Due`)

  i_mod <- which(names(d_sched) == "Module") - 1

  DT::datatable(
    d,
    rownames = F,
    extensions = 'RowGroup',
    options = list(
      #dom = 't',
      pageLength = 11,
      #displayStart = 12,
      rowGroup = list(
        dataSrc=c(i_mod)),
      columnDefs = list(list(visible=F, targets=c(i_mod)))),
    escape = F)
}
