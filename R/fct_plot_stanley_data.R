source("analysis/R/utils.R")
library(tidyr)
library(rethinking)
library(ggplot2)

load_model <- function(model_name, indir = "analysis/data/model_fits/") {
  files <- list.files(path = indir, pattern = model_name, full.names = TRUE)
  load(files)
  assign("model", get(model_name))
  return(model)
}

generate_samples <- function(model, n_samples = 100) {
  
  post <- extract.samples(model)
  
  if ("b_r_ITT" %in% names(post)) post$bITT <- post$b_r_ITT
  col_names <- c("iter", "ITTPass", paste0(1:7))
  kPass <- 0:1 # Pass/fail for ITT
  
  n_rows <- n_samples * length(kPass)
  
  pInit <- matrix(0, nrow = n_rows, ncol = length(col_names))
  
  # Generate 100 potential lines for each of the values/boundaries between values
  for( sample_n in 1:n_samples) {
    cutpoints <- post$cutpoints[sample_n, ]
    phi <- post$bITT[sample_n] * kPass
    
    for (j in seq_along(kPass)){
      # Dynamically generate row_n 
      row_n <- sample_n * 2 - (2 - j)
      
      probs <- dordlogit( 1:7, phi = phi[j], a = cutpoints)
      pInit[row_n, ] = c(sample_n, j - 1, probs)
    }
  }
  
  pInit_df <- as.data.frame(pInit)
  
  colnames(pInit_df) <- col_names
  
  return(pInit_df)
}

tidy_samples <- function(pInit_df) {
  
  pInit_df <- pInit_df %>%
    pivot_longer(cols = "1":"7", names_to = ("rating")) %>%
    rename(pk = value) %>%
    mutate(rating = as.double(rating)) %>%
    mutate(`pk:rating` = pk * rating) %>% 
    group_by(iter, ITTPass)
  
  return(pInit_df)
}

make_cumulative_plot <- function(topic_name, pInit_df) {
  cum_pInit <- pInit_df %>%
    arrange(iter, ITTPass, rating) %>% 
    # here we take our `pk` values and make cumulative sums. why? take a long hard look at Figure 11.2. 
    mutate(probability = cumsum(pk)) %>% 
    # `rating == 7` is unnecessary. these `probability` values are by definition 1
    filter(rating < 7)
  
  cumProb_plot <- ggplot(data = cum_pInit,
                         mapping = aes(x = ITTPass, y = probability)) +
    geom_line(aes(group = interaction(iter, rating),
                  colour = as.factor(rating)),
              alpha = 1/10, show.legend = F) +
    scale_x_continuous("ITTPass", breaks = 0:1) +
    scale_y_continuous("Cumulative probability", breaks = c(0, .5, 1), limits = c(0, 1)) +
    scale_color_manual("Rating", values = c("red","orange","yellow","green","blue","violet")) +
    theme_bw() +
    ggtitle(topic_name)
  
  return(cumProb_plot)
}

make_absolute_plot <- function(topic_name, pInit_df) {
  
  absProb_plot <- ggplot(pInit_df,aes(x = ITTPass, y = pk, colour = as.factor(rating))) +
    geom_line(aes(group = interaction(iter, rating)),
              alpha = 1/10, key_glyph = "rect") +
    scale_x_continuous("ITTPass", breaks = 0:1) +
    scale_y_continuous("Probability", breaks = c(0, .25, .5), limits = c(0, .5)) + 
    scale_color_manual("Rating", values = c("red","orange","yellow","green","blue","violet", "cadetblue")) +
    theme_bw() +
    ggtitle(topic_name)
  
  return(absProb_plot)
}

make_rating_plot <- function(topic_name, pInit_df) {
  
  pInit_summ <- summarise(pInit_df, mean_rating = sum(`pk:rating`))
  
  ratingPlot <- ggplot(data = pInit_summ,
                       mapping = aes(x = ITTPass,
                                     y = mean_rating,
                                     group = iter)) +
    geom_line(alpha = 1/10) +
    scale_x_continuous("ITTPass", breaks = 0:1) + 
    scale_y_continuous("Response Rating", breaks = 1:7, limits = c(1, 7)) +
    theme_bw() +
    ggtitle(topic_name)
  
  return(ratingPlot)
}

save_plot <- function(plot, model_name, plot_type, config,
                      out_dir = "analysis/plots/stanley_plots/") {
  
  out_name <- paste(model_name, plot_type, sep = "_")
  out_path <- gen_file_path(out_dir, out_name, ".png")
  file_output <- config$file_output
  ggsave(out_path, plot = plot,
         width = file_output$width,
         height = file_output$height,
         units = file_output$units,
         dpi = file_output$dpi)
  
}

generate_stanley_plots <- function(model_name, config) {
  
  model <- load_model(model_name)
  pInit_df <- generate_samples(model)
  pInit_df <- tidy_samples(pInit_df)
  
  cumProb_plot <- make_cumulative_plot(model_name, pInit_df)
  save_plot(cumProb_plot, model_name, "cumulative", config)
  
  absProb_plot <- make_absolute_plot(model_name, pInit_df)
  save_plot(absProb_plot, model_name, "absolute", config)
  
  rating_plot <- make_rating_plot(model_name, pInit_df)
  save_plot(rating_plot, model_name, "rating", config)
  
}