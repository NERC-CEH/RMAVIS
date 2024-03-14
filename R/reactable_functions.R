#' Modify a reactable table to provide a dropdown menu
#' 
#' Modify a `reactable` table to provide a dropdown mene containing all unique values in a column.
#' Apply this function in a column definition within the 'columns' or 'defaultColDef' argument of `reactable::reactable()` using `reactable::colDef()`.
#'
#' Taken from the `reactable` documentation found here: https://glin.github.io/reactable/articles/custom-filtering.html
#' 
#' @param tableId The ID of the table, usually supplied via the 'elementId' of the  `reactable::reactable()` function.
#'
#' @return A modified `reactable` table with a dropdown
#' @export
#'
#' @examples
dataListFilter <- function(tableId, style = "width: 100%; height: 28px;") {
  function(values, name) {
    dataListId <- sprintf("%s-%s-list", tableId, name)
    tagList(
      tags$input(
        type = "text",
        list = dataListId,
        oninput = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", tableId, name),
        "aria-label" = sprintf("Filter %s", name),
        style = style
      ),
      tags$datalist(
        id = dataListId,
        lapply(unique(values), function(value) tags$option(value = value))
      )
    )
  }
}