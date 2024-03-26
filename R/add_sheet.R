

#' Add new sheet to an existing work book
#'
#' @description
#' Reads existing workbook and adds the new sheet
#'
#'
#' @param data table that is going to be written in the workbook
#' @param workbook existing workbook
#' @param sheet name of the new sheet
#'
#' @return Overwrites workbook
#' @export
#'
#' @examples
#' # add_sheet(wb, "file.xlsx", "sheet")



add_sheet <- function(data, workbook, sheet){
        mwb <- openxlsx::loadWorkbook(file = workbook)
        openxlsx::addWorksheet(wb = mwb, sheetName = sheet)
        openxlsx::writeDataTable(wb = mwb, sheet = sheet, x = data,
                                 colNames = TRUE,
                                 tableStyle = "TableStyleMedium2")
        openxlsx::saveWorkbook(wb = mwb, file = workbook,
                               overwrite = TRUE, returnValue = TRUE)
        }
