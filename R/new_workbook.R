



#' Create and load new excel workbook
#'
#' @description
#' Creates and loads new excel workbook with custom sheet name
#'
#'
#' @param filename Workbook filename without extension (.xlsx)
#' @param sheet Name of sheet to be created
#' @param overwrite If `TRUE` overwrites an existing file. `default=TRUE`
#'
#' @return Creates and loads workbook
#' @author Gutama Girja Urago
#' @export
#'
#' @examples
#' # wb <- new_workbook("test")



new_workbook <- function(filename, sheet = NULL, overwrite = TRUE){
        wb <- openxlsx::createWorkbook()
        if(!is.null(sheet)) {
                openxlsx::addWorksheet(wb = wb, sheetName = sheet)
        }
        fileN <- paste0(filename, ".xlsx")
        if(overwrite){
             openxlsx::saveWorkbook(wb = wb, file = fileN,
                               overwrite = TRUE)
        } else {
                openxlsx::saveWorkbook(wb = wb, file = fileN,
                                       overwrite = FALSE)
        }
        openxlsx::loadWorkbook(file = fileN)
        }
