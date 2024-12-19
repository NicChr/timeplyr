old_group_id <- function(data, ...,
                         order = TRUE,
                         ascending = TRUE,
                         .by = NULL, .cols = NULL,
                         .name = NULL,
                         as_qg = FALSE){
  fastplyr::add_group_id(
    data, ...,
    .order = order,
    .ascending = ascending,
    .by = {{ .by }},
    .cols = .cols,
    .name = ".internal.temp.group.id",
    as_qg = as_qg
  )[[".internal.temp.group.id"]]
}
