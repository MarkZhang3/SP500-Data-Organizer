options(error=NULL)
options(stringsAsFactors = F)

# setup of external graphics
httpgd::hgd()

options(reactable.theme = reactable::reactableTheme(
          color = "hsl(233, 9%, 87%)",
          backgroundColor = "#002b36",
          borderColor = "#eee8d5",
          stripedColor = "#586e75",
          highlightColor = "#6c71c4",
          inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
          selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
          pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
          pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
        ))

view <- function(.data){
  if(interactive()) {
    reactable::reactable(.data,
              filterable = TRUE,
              searchable = TRUE,
              showPageSizeOptions = TRUE,
              striped = TRUE,
              highlight = TRUE,
              compact = TRUE,
              defaultPageSize = 30)
  }
}

view_xl <- function(.data){
  if(interactive()){
    tmp <- tempfile(fileext = ".csv")
    data.table::fwrite(.data, tmp)
    browseURL(tmp, browser = "gnumeric")
  }
}


paste1 = function(x,...){paste(x,..., sep='')} # wrapper to avoid specifying no space



