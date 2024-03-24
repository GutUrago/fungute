

#' Data dictionary
#' @description
#' Extracts data dictionary from WB LSMS given catalog code and file name.
#'
#'
#' @param code integer in the wb link after catalog
#' @param name string in the wb link after data-dictionary
#' @param namecol if empty column for new name is inserted or not.
#' `default = TRUE`, then manually set new names in excel sheet
#'
#' @return a data frame
#' @export
#'
#' @examples
#'
#' # library(rvest)
#' # library(fastverse)
#' # library(tidyverse)
#' # library(openxlsx)
#' # test <- scrap(code = 6161, name = "F4?file_name=sect3_hh_w5.dta", FALSE)

#' # wb <- loadWorkbook("test.xlsx")
#' # addWorksheet(wb, "here")
#' # writeData(wb, "here", tab)
#' # addWorksheet(wb, "that")
#' # writeData(wb, "that", ess)
#' # saveWorkbook(wb, "test.xlsx", overwrite = TRUE)




dt_dict <- function(code, name, namecol = TRUE) {
        link = paste0("https://microdata.worldbank.org/index.php/catalog/",
                      code, "/data-dictionary/", name)
        web <- rvest::read_html(x = link)
        web <- rvest::html_elements(x = web,  css = ".data-dictionary")
        web <- rvest::html_text2(x = web)
        web <- stringr::str_replace_all(string = web,
                                        pattern = "\r\n", replacement = "")
        web <- stringr::str_replace_all(string = web,
                                        pattern = "\r", replacement = "")
        web <- stringr::str_split(string = web, pattern = "  ")[[1]]
        dt <- data.table::as.data.table(x = web)
        dt <- collapse::fmutate(.data = dt,
                                r = 1:collapse::fnobs(web),
                                r = data.table::fifelse(r %% 2 != 1,
                                                        "even", "odd"))
        df <- if(namecol){
                data.table::data.table(
                        var = dt[r=="odd", web],
                        name = "",
                        desc = dt[r=="even", web])
        } else {
                data.table::data.table(
                        var = dt[r=="odd", web],
                        desc = dt[r=="even", web])
        }
        return(df)
}









