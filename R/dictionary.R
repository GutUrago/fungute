

#' Extracts web data dictionary
#'
#' @description
#' Extracts data dictionary from World bank or FAO data catalogs given
#' catalog number and file name.
#'
#'
#' @param catalog integer in the web link after catalog without "/"
#' @param filename string in the web link after data-dictionary without "/"
#' @param org name of an organization "wb" or "fao", `default = "wb"`
#' @param namecol if empty column for new name is mutated or not.
#' `default = TRUE`, then manually set new names in excel sheet
#' @param format Cleans preceding integers in variable descriptions
#'
#' @details
#' If there is any missing description of variables, it automatically
#' set "Unknown" to the description to write balanced data table
#'
#' @return a data table
#' @author Gutama Girja Urago
#' @export
#'
#' @examples
#' # dictionary(3823, "F2")
#' # dictionary(catalog = 5984, filename = "F5")
#' # dictionary(catalog = 1771, filename = "F392", org = "fao")


dictionary <- function(catalog,
                       filename,
                       org = "wb",
                       namecol = TRUE,
                       format = TRUE) {
        if(org == "wb") {
                link = paste0("https://microdata.worldbank.org/index.php/catalog/",
                              catalog, "/data-dictionary/", filename)

        } else if(org == "fao"){
                link = paste0("https://microdata.fao.org/index.php/catalog/",
                              catalog, "/data-dictionary/", filename)
        } else {stop("org argument takes only 'wb' or 'fao'")}
        web <- rvest::read_html(x = link)
        web <- rvest::html_elements(x = web,  css = ".data-dictionary")
        web <- rvest::html_text2(x = web)
        web <- stringr::str_replace(string = web,
                                    pattern = "^\r\n\r\n\r\n\r\n\r ",
                                    replacement = "")
        web <- stringr::str_replace_all(string = web,
                                        pattern = " \r \r\n\r\n\r\n\r\n\r\n\r\n\r\n\r\n\r\n\r\n\r ",
                                        replacement = "  Unknown  ")
        web <- stringr::str_replace_all(string = web,
                                        pattern = "\r\n",
                                        replacement = "")
        web <- stringr::str_replace_all(string = web,
                                        pattern = "\r",
                                        replacement = "")
        web <- stringr::str_split(string = web, pattern = "  ")[[1]]
        dt <- data.table::as.data.table(x = web)
        dt <- collapse::fmutate(.data = dt,
                                "r" = 1:collapse::fnobs(web))
        dt <- collapse::fmutate(.data = dt,
                                "r" = data.table::fifelse(dt$r %% 2 != 1,
                                                          "even", "odd"))
        if(namecol){
               df <- data.table::data.table(
                        var = dt$web[dt$r=="odd"],
                        name = "",
                        desc = dt$web[dt$r=="even"])
        } else {
                df <- data.table::data.table(
                        var = dt$web[dt$r=="odd"],
                        desc = dt$web[dt$r=="even"])
        }
        if(format){
                df <- collapse::fmutate(.data = df,
                                        desc = stringr::str_remove(df$desc, "^\\d+\\.\\s+"))
                df <- collapse::fmutate(.data = df,
                                        desc = stringr::str_remove(df$desc, "^\\d+\\w\\.\\s+"))
                df <- collapse::fmutate(.data = df,
                                        desc = stringr::str_remove(df$desc, "^\\w\\d+\\.\\s+"))
        } else {df <- df}
        return(df)
}

