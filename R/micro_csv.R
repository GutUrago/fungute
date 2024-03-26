


#' Reads csv survey file
#' @description
#' Imports csv micro-data file from local directory. I should be
#' supplemented with the excel workbook that follows
#' `microData::dictionary()` structure.
#'
#'
#' @param data is a path to the csv data frame
#' @param label an excel file containing names and labels of variables
#' @param sht specify sheet name if excel has more than one sheet
#' @param fct if you wish to convert strings into factors
#'
#' @details
#' The column names of the excel file containing the name and description
#' of variables should be named "var", "name" and "desc". If the file has
#' been created using this package, nothing to worry about as it automatically
#' set these names.
#'
#' @return a data table
#' @export
#'
#' @examples
#' # micro_csv("test.csv")
#'




micro_csv <- function(data,
                      label,
                      sht = NULL,
                      fct = TRUE){
        if(fct){
                dt <- data.table::fread(input = data,
                                        stringsAsFactors = fct)
        } else {
                dt <- data.table::fread(input = data)
        }
        if(!is.null(label) && !is.null(sht)){
                label <- readxl::read_excel(path = label, sheet = sht)
        } else if(!is.null(label)){
                label <- readxl::read_excel(path = label)
        }
        label <- collapse::na_omit(label)
        dt <- collapse::fselect(.x = dt, label$var)
        dt <- data.table::setnames(x = dt, old = label$var,
                                   new = label$name)
        dt <- collapse::setLabels(X = dt, value = label$desc)
        return(dt)
        }

