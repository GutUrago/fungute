


#' Reads csv survey file
#' @description
#' This function helps to organize big survey such as ESS to
#' organize cleaning and wrangling process.
#'
#'
#' @param data is a path to the csv data frame to be read
#' @param label an excel file containing names and labels of variables
#' @param sht specify sheet name if excel has more than one sheet
#' @param fct if you wish to convert strings in to factors
#'
#' @return a data table
#' @export
#'
#' @examples
#' # survey_csv("test.csv")
#'




survey_csv <- function(data,
                       label = NULL,
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

