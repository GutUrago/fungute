

#' Add new sheet to an existing workbook
#'
#' @description
#' Adds new sheet to loaded workbook and saves
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
#' @export
#'
#' @examples
#' # add_sheet(wb, "file.xlsx", "sheet")



add_sheet <- function(workbook, data, sheet){
        mwb <- workbook
        openxlsx::addWorksheet(wb = mwb, sheetName = sheet)
        openxlsx::writeDataTable(wb = mwb, sheet = sheet, x = data,
                                 colNames = TRUE,
                                 tableStyle = "TableStyleMedium2")
        }
