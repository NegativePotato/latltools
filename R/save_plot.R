#' @importFrom ggplot2 ggsave
#' Saves an ggplot object as png, eps, and rds.
#'
#' Given a plot object output from ggplot function, this function saves the plot
#' as a png (pixel map), eps (vectorized format), and rds (R object) files.
#'
#'
#' @param plot plot object from ggplot
#' @param file_name_full_no_ext name of the file including the path to the folder
#' where you want to save the figure, without the extension (e.g. png, eps, jpg,
#' rds, etc...) as these are added within the function
#' @param width width of picture (see ggsave)
#' @param height width of picture (see ggsave)
#' @param units units of width and height of picture (see ggsave)
#'
#' @return A data frame with the player cateogry in the column "player_category" and the item columns renamed according to the mapping above.
#'
#' @examples
#' # saves my_plot in 3 files my_awesome_plot.png, and my_awesome_plot.rds,
#' # my_awesome_plot.eps in the folder path/to/figure/directory.
#' my_plot <- ggplot() + ...
#' save_plot(plot = my_plot,
#           file_name_full_no_ext = file.path(“path/to/figure/directory”, “my_awesome_plot”),
#           width = 7, height = 7, units = 'in')
#'
#' @export


save_plot <- function(plot, file_name_full_no_ext, width = 7, height = 7, units = 'in') {
  # Input parameters
  # - plot : plot object from ggplot
  # - file_name_full_no_ext : name of the file including the path to the folder
  #     where you want to save the figure, without the extension (e.g. png, eps,
  #     jpg, rds, etc...) these are added in the function
  # - width, height, and units behave the same way as they would in ggsave.
  #
  # Example usage :
  # save_plot(plot = my_plot,
  #           file_name_full_no_ext = file.path(“path/to/figure/directory”, “my_awesome_plot”),
  #           width = 7, height = 7, units = 'in')


  ggsave(plot = plot,
         filename = paste0(file_name_full_no_ext, '.png'),
         device = 'png',
         width = width, height = height, units = units)
  ggsave(plot = plot,
         filename = paste0(file_name_full_no_ext, '.eps'),
         device = 'eps',
         width = width, height = height, units = units)
  saveRDS(plot, file = paste0(file_name_full_no_ext, '.rds'))
}
