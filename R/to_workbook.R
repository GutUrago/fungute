


#' Add data frame to workbook
#'
#' @param data data to be written
#' @param wb Workbook
#' @param sheet sheetname
#'
#' @return updated workbook
#' @export
#'
#' @examples
#' # dictionary(3823, "F2") |> to_workbook(wb, "sheet 1")



to_workbook <- function(data,
                        wb,
                        sheet){
        openxlsx::addWorksheet(wb = wb,
                               sheetName = sheet,
                               zoom = 125)
        openxlsx::writeDataTable(wb = wb,
                       sheet = sheet,
                       x = data,
                       tableStyle = "TableStyleMedium2")
}
