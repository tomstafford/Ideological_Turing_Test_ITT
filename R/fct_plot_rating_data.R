source("analysis/R/utils.R")

summarise_data <- function(data_in){
  
  data_out <- data_in %>%
    group_by(argument_position) %>%
    summarise(mean_rating = mean(response_ratings),
              sd_rating = sd(response_ratings),
              .groups = "drop")
  
  return(data_out)
}

create_plot <- function(topic, orig_data, config){
  
  # Prep the data
  split_data <- group_split(orig_data, condition)
  baseline_data <- split_data[[1]]
  itt_data <- split_data[[2]]
  
  itt_summ <- summarise_data(itt_data)
  
  # Prep the options
  title <- paste(stringr::str_to_title(topic$title), "ITT Arguments")
  colours <- config$colours # Is this used?
  xTitle <- config$xlab
  yTitle <- config$ylab
  lineLab <- config$lineLab
  legendLab <- config$legendLab
  
  Positions <- topic$positions
  positionsLab <- stringr::str_glue("{Positions} baseline",
                                    Positions = rev(Positions))
  xLabs <- stringr::str_glue("{Positions}")
  
  pt_size <- config$points$size
  pt_shape <- config$points$shape
  
  eb_width <- config$errorbar$width
  
  hist_side <- config$histograms$side
  hist_just <- config$histograms$justification
  
  # Plot the data
  plot_out <- ggplot(data = itt_data,
                     mapping = aes(x = argument_position)) +
    stat_histinterval(aes(y = response_ratings, fill = argument_position),
                      normalize = "none", slab_color = "white",
                      outline_bars = TRUE, show.legend = F,
                      breaks = seq(from = 0.5, to = 7.5, by = 1),
                      fatten_point = 2, show_interval = F, width = 0.9,
                      justification = hist_just, side = hist_side) +
    geom_errorbar(data = itt_summ,
                  aes(ymin = mean_rating - sd_rating,
                      ymax = mean_rating + sd_rating),
                  width = eb_width) +
    geom_point(data = itt_summ, aes(y = mean_rating, fill = argument_position),
               size = pt_size, shape = pt_shape, show.legend = FALSE) +
    geom_hline(yintercept = 6, linetype = 'dotdash', color = 'black') +
    annotate("text", x = 0, y = 5.5, label = lineLab, colour = "black",
             hjust = -0.1, vjust = -0.5) +
    stat_summary(data = baseline_data,
                 mapping = aes(colour = argument_position,
                               x = argument_position,
                               y = response_ratings),
                 fun = "mean", geom = "crossbar",
                 linetype='dashed', key_glyph = "smooth") +
    scale_y_continuous(breaks = c(1:7)) +
    labs(color = legendLab, title = title, x = xTitle, y = yTitle) + 
    scale_colour_discrete(labels = positionsLab) + 
    scale_x_discrete(labels = xLabs) + 
    theme_classic() + 
    ggtitle(title) +
    ggeasy::easy_center_title()
  
  
  return(plot_out)
}

plot_data <- function(topic, config,
                      indir = "analysis/data/shiny_data/",
                      outdir = "analysis/plots/rater_summaries/",
                      save = TRUE){
  # Load Data
  files <- list.files(path = indir, pattern = topic$title, full.names = TRUE)
  shiny_data <- read_csv(files, show_col_types = FALSE)
  
  topic_plot <- create_plot(topic, shiny_data, config)
  
  # Save plot
  out_name <- paste(topic$title, "rater_summary", sep = "_")
  out_path <- gen_file_path(outdir, out_name, ".png")
  
  file_output <- config$file_output
  if (save){
    ggsave(out_path, plot = topic_plot,
           width = file_output$width,
           height = file_output$height,
           units = file_output$units,
           dpi = file_output$dpi)
  }
  
  return(topic_plot)
}
