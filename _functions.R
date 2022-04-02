librarian::shelf(
  dplyr, DT, glue, googlesheets4, htmltools, purrr, raster, readr, shiny, stringr, tidyr)
options(readr.show_col_types = F)
select = dplyr::select

# edit: * [schedule | eds232-ml - Google Sheets](https://docs.google.com/spreadsheets/d/1lolwZ2CNAtUkTWPauyPEe_oVMknqS2yVvfnB1fiErzQ/edit#gid=0)
sched_gsheet <- "https://docs.google.com/spreadsheets/d/1lolwZ2CNAtUkTWPauyPEe_oVMknqS2yVvfnB1fiErzQ/edit"

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
        glue::glue("{lec_youtube}"),
        ifelse(
          is.na(Lecture_link),
          glue::glue("{Lecture} {lec_youtube}"),
          glue::glue("<a href='{Lecture_link}' target='_blank'>{Lecture}</a>{lec_youtube}"))),
      Lab     = ifelse(
        is.na(Lab),
        "",
        ifelse(
          is.na(Lab_link),
          Lab,
          glue::glue("<a href='{Lab_link}' target='_blank'>{Lab}</a>"))),
      `Reading *` = map_chr(`Reading *`, md2html),
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
      pageLength   = 20, # 2nd half: 11
      displayStart =  0, # 2nd half: 11
      rowGroup = list(
        dataSrc=c(i_mod)),
      columnDefs = list(list(visible=F, targets=c(i_mod)))),
    escape = F)
}

get_sched <- function(){
  # googlesheets4::gs4_deauth() # cd ~/Library/Caches/gargle; rm ...
  gs4_deauth() # since gave Anyone View/Comment rights to sched_gsheet
  d_sched <- googlesheets4::read_sheet(sched_gsheet) %>%
    tidyr::fill(Week, Module)
  names(d_sched) <- names(d_sched) %>% stringr::str_replace("\n", " ")
  d_sched
}

md2html <- function(x){
  #browser()
  # md2h <- function(x){ markdown::markdownToHTML(text = x, fragment.only=T)[1] }
  # y <- x %>%

  if (is.na(x))
    return(NA)

  x %>%
    stringr::str_replace_all('\"', '\\"') %>%
    # md2h() %>%
    markdown::markdownToHTML(text = x, fragment.only=T) %>%
    stringr::str_replace("^(<p>)(.*)(</p>)\\n$", "\\2")
  # message(glue("\n\n{x}\n ->\n{y}\n\n"))
  # y
}
