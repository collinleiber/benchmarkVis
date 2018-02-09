output$filter = DT::renderDataTable({
  table$data
},
filter = 'top',
options = list(
  pageLength = 5,
  lengthMenu = c(5, 10, 15, 20),
  scrollX = TRUE,
  dom = 't',
  rowReorder = FALSE
))
