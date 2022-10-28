#' Tabulate Plate Function
#'
#' This function expands upon the functionality of plateR to easily tabulate 96-well format data
#' @param file path to file with microarray data and layout in plateR format
#' @param identifiers vector of column names that uniquely identify observations/treatment groups
#' @param group_by column containing variable to group data by e.g. replicate number
#' @param data vector of columns containing data output from the plate reader
#' @return modified tabular data frame that groups data according to specifications
#' @export


tabulate_plate <- function(file, identifiers, group_by, data) {
  plate_data <- plater::read_plate(file)
  tabular <- tidyr::pivot_wider(plate_data, 
                                id_cols = identifiers, names_from = group_by,
                                values_from = data, values_fn = sum)
  new_file_name <- paste("tabular_", gsub(".csv", ".xlsx",file), sep = "")

  writexl::write_xlsx(tabular, new_file_name)

  return(tabular)
}



