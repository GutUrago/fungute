

#' Add new sheet to an existing workbook
#'
#' @description
#' Loads an existing workbook and adds the new sheet
#'
#'
#' @param data table that is going to be written in the workbook
#' @param workbook existing workbook
#' @param sheet name of the new sheet
#'
#' @details
#' If you wish to create and load new workbook at the same time
#' use `microData::new_workbook()`.
#'
#'
#'
#' @return Overwrites workbook
#' @author Gutama Girja Urago
#'
#' @examples
#' # add_sheet(wb, "file.xlsx", "sheet")



add_sheet <- function(data, workbook, sheet){
        if(exists(workbook)) {
                mwb <- workbook
        } else {mwb <- openxlsx::loadWorkbook(file = workbook)}
        openxlsx::addWorksheet(wb = mwb, sheetName = sheet)
        openxlsx::writeDataTable(wb = mwb, sheet = sheet, x = data,
                                 colNames = TRUE,
                                 tableStyle = "TableStyleMedium2")
        openxlsx::saveWorkbook(wb = mwb, file = workbook,
                               overwrite = TRUE, returnValue = TRUE)
        }
