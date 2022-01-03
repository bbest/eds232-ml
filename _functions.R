librarian::shelf(
  dplyr, DT, glue, readr, stringr, tidyr)
options(readr.show_col_types = F)

# edit: * [schedule | eds232-ml - Google Sheets](https://docs.google.com/spreadsheets/d/1lolwZ2CNAtUkTWPauyPEe_oVMknqS2yVvfnB1fiErzQ/edit#gid=0)
sched_csv <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQT4PYzXahIpCV2-PXR_97niUglLuxKuxhhRKTw5tWMXX7HLyhnL23BRvP3V7sTEd4Shd60Q5ff-g3l/pub?gid=0&single=true&output=csv"

get_sched <- function(){
  d_sched <- readr::read_csv(sched_csv) %>%
    tidyr::fill(Week, Module)
  names(d_sched) <- names(d_sched) %>% stringr::str_replace("\n", " ")
  d_sched
}

dt_sched <- function(d_sched){
  d <- d_sched %>%
    mutate(
      Lecture = ifelse(
        is.na(Lecture),
        "",
        ifelse(
          is.na(Lecture_link),
          Lecture,
          glue::glue("<a href='{Lecture_link}' target='_blank'>{Lecture}</a>"))),
      Lab     = ifelse(
        is.na(Lab),
        "",
        ifelse(
          is.na(Lab_link),
          Lab,
          glue::glue("<a href='{Lab_link}'     target='_blank'>{Lab}</a>"))),
      Reading = ifelse(
        is.na(Reading),
        "",
        ifelse(
          is.na(Reading_link),
          Reading,
          glue::glue("<a href='{Reading_link}'     target='_blank'>{Reading}</a>"))),
      `Lab Due` = ifelse(
        is.na(`Lab Due`),
        "",
        glue::glue("{`Lab Due`} ({`Lab Pts`} pts)"))) %>%
    select(Week, Date, Module, Lecture, Lab, Reading, `Lab Due`)

  i_mod <- which(names(d_sched) == "Module") - 1

  DT::datatable(
    d,
    rownames = F,
    extensions = 'RowGroup',
    options = list(
      #dom = 't',
      rowGroup = list(
        dataSrc=c(i_mod)),
      columnDefs = list(list(visible=F, targets=c(i_mod)))),
    escape = F)
}
