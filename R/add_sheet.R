

#' Add sheet to an existing work book
#'
#' @description
#' Reads existing workbook and adds the new sheet
#'
#'
#' @param data table that is going to be written in the workbook
#' @param wb existing workbook
#' @param sht name of the new sheet
#'
#' @return Overwrites workbook
#' @export
#'
#' @examples
#' # add_sheet(wb, "file.xlsx", "sheet")



add_sheet <- function(data, wb, sht){
        mwb <- openxlsx::loadWorkbook(file = wb)
        openxlsx::addWorksheet(wb = mwb, sheetName = sht)
        openxlsx::writeDataTable(wb = mwb, sheet = sht, x = data,
                                 tableStyle = "TableStyleMedium2")
        openxlsx::saveWorkbook(wb = mwb, file = wb,
                               overwrite = TRUE, returnValue = TRUE)
}
