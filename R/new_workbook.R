



#' Create and load new workbook
#'
#' @description
#' Creates and loads new workbook with custom sheet name
#'
#'
#' @param filename Workbook filename without .xlsx
#' @param sheet Name of sheet, if null `Sheet 1` will be created
#' @param overwrite Overwrites an existing file
#'
#' @return Creates and loads workbook
#' @export
#'
#' @examples
#' # new_workbook("test")



new_workbook <- function(filename, sheet = NULL, overwrite = TRUE){
        wb <- openxlsx::createWorkbook()
        if(!is.null(sheet)) {
                openxlsx::addWorksheet(wb = wb, sheetName = sheet)
        } else {
                openxlsx::addWorksheet(wb = wb, sheetName = "Sheet 1")
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
