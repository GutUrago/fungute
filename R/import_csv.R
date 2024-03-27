


#' Reads csv survey file
#'
#' @description
#' Imports csv micro-data file from local directory. I should be
#' supplemented with the excel workbook that follows
#' `microData::dictionary()` structure.
#'
#'
#' @param data is a path to the csv data frame
#' @param excel an excel file containing names and labels of variables
#' @param sheet specify sheet name if excel has more than one sheet
#' @param format Cleans preceding integers in strings and converts to lower
#'
#' @details
#' The column names of the excel file containing the name and description
#' of variables should be named "var", "name" and "desc". If the file has
#' been created using this package, nothing to worry about as it automatically
#' set these names.
#'
#' @return a data table
#' @author Gutama Girja Urago
#' @export
#'
#' @examples
#' # import_csv("test.csv")
#'




import_csv <- function(data,
                      excel = NULL,
                      sheet = NULL,
                      format = TRUE){
        dt <- data.table::fread(input = data)
        if(format){
                dt <- dplyr::mutate_if(.tbl = dt, is.character,
                                       ~stringr::str_remove(., "^\\d+\\.\\s+"))
                dt <- dplyr::mutate_if(.tbl = dt, is.character, tolower)
        } else {dt <- dt}
        if(!is.null(excel) && !is.null(sheet)){
                excel <- readxl::read_excel(path = excel, sheet = sheet)
        } else if(!is.null(excel)){
                excel <- readxl::read_excel(path = excel)
        } else {
                return(dt)
        }
        excel <- collapse::na_omit(excel)
        dt <- collapse::fselect(.x = dt, excel$var)
        dt <- dplyr::mutate_if(.tbl = dt, is.character, as.factor)
        dt <- data.table::setnames(x = dt, old = excel$var,
                                   new = excel$name)
        dt <- collapse::setLabels(X = dt, value = excel$desc)
        return(dt)
        }








