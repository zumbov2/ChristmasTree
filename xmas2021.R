# Setup --------------------------------------------------------------------------------

# Load Packages
library(dplyr)
library(ggplot2)
library(stringr)

# Folder
folder <- "tree"
if (!dir.exists(folder)) dir.create(folder)

# Define background, tree and trunk ----------------------------------------------------

# Background
background <- expand.grid(x = 1:24, y = 1:40) %>%
  mutate(id = paste0(x, "-", y)) %>% 
  mutate(type = "background") %>% 
  mutate(color_group = "0")

# Tree
tree <- expand.grid(x = 1:24, y = 1:40) %>%
  mutate(id = paste0(x, "-", y)) %>% 
  mutate(value = ifelse(
    id %in% c(
      '20-3', '20-4', '20-5', '20-6', '20-7', '20-8', '20-9', '20-10', '20-11', 
      '20-12', '20-13', '20-14', '20-15', '20-16', '20-17', '21-3', '21-4', '21-5', 
      '21-6', '21-7', '21-8', '21-9', '21-10', '21-11', '21-12', '21-13', '21-14', 
      '21-15', '21-16', '21-17', '17-4', '17-5', '17-6', '17-7', '17-8', '17-9', 
      '17-10', '17-11', '17-12', '17-13', '17-14', '17-15', '17-16', '18-4', 
      '18-5', '18-6', '18-7', '18-8', '18-9', '18-10', '18-11', '18-12', '18-13', 
      '18-14', '18-15', '18-16', '19-4', '19-5', '19-6', '19-7', '19-8', '19-9', 
      '19-10', '19-11', '19-12', '19-13', '19-14', '19-15', '19-16', '14-5', 
      '14-6', '14-7', '14-8', '14-9', '14-10', '14-11', '14-12', '14-13', '14-14', 
      '14-15', '15-5', '15-6', '15-7', '15-8', '15-9', '15-10', '15-11', '15-12', 
      '15-13', '15-14', '15-15', '16-5', '16-6', '16-7', '16-8', '16-9', '16-10', 
      '16-11', '16-12', '16-13', '16-14', '16-15', '12-6', '12-7', '12-8', '12-9', 
      '12-10', '12-11', '12-12', '12-13', '12-14', '13-6', '13-7', '13-8', '13-9', 
      '13-10', '13-11', '13-12', '13-13', '13-14', '9-7', '9-8', '9-9', '9-10', 
      '9-11', '9-12', '9-13', '10-7', '10-8', '10-9', '10-10', '10-11', '10-12', 
      '10-13', '11-7', '11-8', '11-9', '11-10', '11-11', '11-12', '11-13', '7-8', 
      '7-9', '7-10', '7-11', '7-12', '8-8', '8-9', '8-10', '8-11', '8-12', '5-9', 
      '5-10', '5-11', '6-9', '6-10', '6-11', '4-10', '4-30', '4-31', '5-30', '5-31', 
      '6-30', '6-31', '7-30', '7-31', '8-30', '8-31', '9-30', '9-31', '10-30', 
      '10-31', '11-30', '11-31', '12-30', '12-31', '13-30', '13-31', '14-30', 
      '14-31', '15-30', '15-31', '16-30', '16-31', '17-30', '17-31', '18-30', 
      '18-31', '19-30', '19-31', '20-30', '20-31', '21-30', '21-31', '6-32', 
      '7-32', '8-32', '9-32', '10-32', '11-32', '12-32', '13-32', '14-32', 
      '15-32', '16-32', '17-32', '18-32', '19-32', '20-32', '21-32', '8-33', 
      '9-33', '10-33', '11-33', '12-33', '13-33', '14-33', '15-33', '16-33', 
      '17-33', '18-33', '19-33', '20-33', '21-33', '20-34', '20-35', '20-36', 
      '20-37', '21-34', '21-35', '21-36', '21-37', '18-34', '18-35', '19-34', 
      '19-35', '19-36', '21-38', '13-34', '14-34', '15-34', '16-34', '17-34', 
      '14-35', '15-35', '16-35', '15-36', '16-36', '16-37', '9-34', '10-34', 
      '11-34', '10-35', '11-35', '11-36', '6-29', '7-29', '8-29', '9-29', '10-29', 
      '11-29', '12-29', '13-29', '14-29', '15-29', '16-29', '17-29', '18-29', 
      '19-29', '20-29', '21-29', '8-28', '9-28', '10-28', '11-28', '12-28', 
      '13-28', '14-28', '15-28', '16-28', '17-28', '18-28', '19-28', '20-28', 
      '21-28', '9-27', '10-27', '11-27', '10-26', '11-26', '11-25', '13-27', 
      '14-27', '15-27', '16-27', '17-27', '18-27', '19-27', '20-27', '21-27', 
      '14-26', '15-26', '16-26', '15-25', '16-25', '16-24', '18-26', '19-26', 
      '20-26', '21-26', '19-25', '20-25', '21-25', '20-24', '21-24', '21-23'
      ), 1, 0)
    ) %>% 
  filter(value == 1) %>% 
  mutate(
    type = "tree",
    color_group = sample(
      as.character(c(1:5)),
      size = length(value),
      replace = TRUE
      ))

# Trunk
trunk <- expand.grid(x = 22:24, y = 1:40) %>%
  mutate(id = paste0(x, "-", y)) %>% 
  mutate(value = ifelse(
    id %in% c(
      '22-9', '22-10', '22-11', '23-9', '23-10', 
      '23-11', '22-30', '22-31', '23-30', '23-31'
    ), 1, 0)
    ) %>%
  filter(value == 1) %>% 
  mutate(
    type = "trunk",
    color_group = sample(
      as.character(c(6:7)),
      size = length(value),
      replace = TRUE
    ))

# Ensemble
ensemble <- bind_rows(background, trunk, tree) 
 
# Nightfall -----------------------------------------------------------------------------

# Function 
nightfall <- function(data, num_steps, stay_start, day, night, greens, browns, alpha_min, folder) {
  
  # Colors
  colfunc <- colorRampPalette(c(day, night))
  
  # Start_screen
  for (i in 1:stay_start) {
    
    # Feedback
    if (!i %% 10 == 0) cat(".")
    if (i %% 10 == 0) cat(i)
    
    # PLot
    data %>% 
      ggplot() +
      geom_tile(
        aes(y, -x, fill = color_group),
        alpha = 1
      ) +
      scale_fill_manual(values = c(day, greens, browns)) +
      coord_equal() +
      theme_void() +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    # Save
    ggsave(
      paste0(folder, "/", str_pad(i, 3, pad = "0"),".png"), 
      dpi = 1000, 
      width = 40/8, 
      height = 24/8
      )
    
  }
  
  # Nightfall
  cat("\n")
  for (i in 0:num_steps) {
    
    if (!i %% 10 == 0) cat(".")
    if (i %% 10 == 0) cat(i)
    
    data %>% 
      ggplot() +
      geom_tile(
        aes(y, -x, fill = color_group), 
        alpha = 1 - (i+1) * ((1 - alpha_min) / num_steps)
        ) +
      scale_fill_manual(values = c(colfunc(num_steps+1)[i+1], greens, browns)) +
      coord_equal() +
      theme_void() +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    ggsave(paste0(folder, "/", str_pad(stay_start + i + 1, 3, pad = "0"),".png"), dpi = 1000, width = 40/8, height = 24/8)
  
  }
  
}

# Run
nightfall(
  data = ensemble,
  stay_start = 12,
  num_steps = 20,
  day = "#EDF7FB", 
  night = "#43535E",
  greens = c("#33882C", "#036431", "#84BF59", "#3AAA37", "#18742C"), 
  browns = c("#A0411F", "#A46C52"),
  alpha_min = 0.8,
  folder = folder
  )

# Candles ------------------------------------------------------------------------------

# Function 
handle_candles <- function(data, candles, start, stay_frames, end_frames, night, greens, browns, 
                           yellows, sizes, alpha_min, alpha_shrinkage, size_shrinkage, folder) {
  
  # Groups
  color_group_labels <- paste0(9, "-", 1:length(yellows))
  size_group_labels <- paste0(1:length(sizes))
  
  # Candle basics
  candle <- expand.grid(x = 1:24, y = 1:40) %>%
    mutate(id = paste0(x, "-", y)) %>% 
    mutate(value = 0) %>% 
    mutate(
      type = "candle",
      color_group = color_group_labels[1]
      )

  # Light up
  candles <- sample(candles)
  for (i in 1:length(candles)) {
    
    # Feedback
    if (!i %% 10 == 0) cat(".")
    if (i %% 10 == 0) cat(i)
    
    # Color changes
    candle <- candle %>%
      mutate(color_group = sample(color_group_labels, length(value), replace = T)) %>% 
      mutate(size_group = sample(size_group_labels, length(value), replace = T)) %>% 
      mutate(value = ifelse(id == candles[i], 1, value)) %>% 
      mutate(color_group = ifelse(id == candles[i], color_group_labels[1], color_group))
    
    yellows2 <- yellows[color_group_labels %in% candle$color_group[candle$value == 1]]
    sizes2 <- sizes[size_group_labels %in% candle$size_group[candle$value == 1]]
    
    ensemble <- bind_rows(data, candle %>% filter(value == 1))
      
    # Plot
    ggplot() +
      geom_tile(
        data = ensemble,
        aes(y, -x, fill = color_group, alpha = color_group)
        ) +
      geom_point(
          data = ensemble %>% filter(type == "candle"),
          aes(y, -x, color = color_group, alpha = color_group, size = size_group), 
          pch = 16
        ) +
      geom_point(
          data = ensemble %>% filter(type == "candle"),
          aes(y, -x, color = color_group, alpha = color_group),
          pch = 15, size = 4, stroke = 0
          ) +
      scale_fill_manual(values = c(night, greens, browns, yellows2)) +
      scale_color_manual(values = yellows2) +
      scale_alpha_manual(values = c(
        rep(alpha_min, length(night)),
        rep(alpha_min, length(greens)),
        rep(alpha_min, length(browns)),
        rep(0.9, length(yellows2)))
        ) +
      scale_size_manual(values = sizes2) +
      coord_equal() +
      theme_void() +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    # Save
    ggsave(
      paste0(folder, "/", str_pad(start + i - 1, 3, pad = "0"),".png"), 
      dpi = 1000, 
      width = 40/8, 
      height = 24/8
      )
    
  }
  
  # Flickering
  cat("\n")
  for (i in 1:stay_frames) {
    
    # Feedback
    if (!i %% 10 == 0) cat(".")
    if (i %% 10 == 0) cat(i)
    
    # Color changes
    candle <- candle %>%
      mutate(color_group = sample(color_group_labels, length(value), replace = T)) %>% 
      mutate(size_group = sample(size_group_labels, length(value), replace = T)) 
    
    yellows2 <- yellows[color_group_labels %in% candle$color_group[candle$value == 1]]
    sizes2 <- sizes[size_group_labels %in% candle$size_group[candle$value == 1]]
    
    ensemble <- bind_rows(data, candle %>% filter(value == 1))
    
    # Plot
    ggplot() +
      geom_tile(
        data = ensemble,
        aes(y, -x, fill = color_group, alpha = color_group)
        ) +
      geom_point(
        data = ensemble %>% filter(type == "candle"),
        aes(y, -x, color = color_group, alpha = color_group, size = size_group), 
        pch = 16
        ) +
      geom_point(
        data = ensemble %>% filter(type == "candle"),
        aes(y, -x, color = color_group, alpha = color_group),
        pch = 15, size = 4, stroke = 0
        ) +
      scale_fill_manual(values = c(night, greens, browns, yellows2)) +
      scale_color_manual(values = yellows2) +
      scale_alpha_manual(values = c(
        rep(alpha_min, length(night)),
        rep(alpha_min, length(greens)),
        rep(alpha_min, length(browns)),
        rep(0.9, length(yellows2)))
        ) +
      scale_size_manual(values = sizes2) +
      coord_equal() +
      theme_void() +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    # Save
    ggsave(
      paste0(folder, "/", str_pad(start + length(candles) + i - 1, 3, pad = "0"),".png"), 
      dpi = 1000, 
      width = 40/8, 
      height = 24/8
      )
    
  }
  
  # Going out
  cat("\n")
  candles <- sample(candles)
  for (i in 1:length(candles)) {
    
    # Feedback
    if (!i %% 10 == 0) cat(".")
    if (i %% 10 == 0) cat(i)
    
    # Color changes
    candle <- candle %>%
      mutate(color_group = sample(color_group_labels, length(value), replace = T)) %>% 
      mutate(size_group = sample(size_group_labels, length(value), replace = T)) %>% 
      mutate(value = ifelse(id == candles[i], 0, value)) 

    yellows2 <- yellows[color_group_labels %in% candle$color_group[candle$value == 1]]
    sizes2 <- sizes[size_group_labels %in% candle$size_group[candle$value == 1]]
    
    ensemble <- bind_rows(data, candle %>% filter(value == 1))
    
    # Plot
    ggplot() +
      geom_tile(
        data = ensemble %>% filter(type == "background"),
        aes(y, -x, fill = color_group), 
        alpha = 0.8
        ) +
      geom_tile(
        data = ensemble %>% filter(!type == "background"),
        aes(y, -x, fill = color_group, alpha = color_group)
        ) +
      geom_point(
        data = ensemble %>% filter(type == "candle"),
        aes(y, -x, color = color_group, alpha = color_group, size = size_group), 
        pch = 16
        ) +
      geom_point(
        data = ensemble %>% filter(type == "candle"),
        aes(y, -x, color = color_group, alpha = color_group),
        pch = 15, size = 4, stroke = 0
        ) +
      scale_fill_manual(values = c(night, greens, browns, yellows2)) +
      scale_color_manual(values = yellows2) +
      scale_alpha_manual(values = c(
        rep(alpha_min - alpha_min * (i / length(candles)), length(night)),
        rep(alpha_min - alpha_min * (i / length(candles)), length(greens)),
        rep(alpha_min - alpha_min * (i / length(candles)), length(browns)),
        rep(alpha_min - (alpha_shrinkage * alpha_min) * (i / length(candles)), length(yellows2)))
        ) +
      scale_size_manual(values = size_shrinkage*sizes2) +
      coord_equal() +
      theme_void() +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    # Plot
    ggsave(
      paste0(folder, "/", str_pad(start + length(candles) + stay_frames + i - 1, 3, pad = "0"),".png"), 
      dpi = 1000, 
      width = 40/8, 
      height = 24/8
      )
    
  }
  
  # End
  cat("\n")
  for (i in 1:end_frames) {
    
    if (!i %% 10 == 0) cat(".")
    if (i %% 10 == 0) cat(i)
    
  ggplot() +
    geom_tile(
      data = ensemble %>% filter(type == "background"),
      aes(y, -x), 
      alpha = 0.8,
      fill = night
    ) +
    annotate("text", x = 20, y = -12, label = "Test", family = "Roboto") +
    coord_equal() +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  ggsave(paste0(folder, "/", str_pad(start + 2*length(candles) + stay_frames + i - 1, 3, pad = "0"),".png"), dpi = 1000, width = 40/8, height = 24/8)
  
  }
  
}

# Run
handle_candles(
  data = ensemble,
  candles = c(
  '6-9', '8-11', '10-9', '13-6', '11-13', '14-11', '14-15', '17-12',
  '16-7', '19-5', '20-9', '20-15', '21-25', '18-30', '17-27', '19-33',
  '20-37', '15-36', '14-31', '14-26', '11-29', '12-33', '9-34', '6-31',
  '8-28'), 
  # candles = sample(
  # ensemble %>% filter(type == "tree") %>% pull(id),
  # size = 24
  #),
  start = 33,
  stay_frames = 36,
  end_frames = 12,
  night = "#43535E",
  greens = c("#33882C", "#036431", "#84BF59", "#3AAA37", "#18742C"), 
  browns = c("#A0411F", "#A46C52"),
  yellows = c("#ffbe00", "#ffd866", "#ffb866", "#f7cb48", "#fabd4b", "#fadf43"),
  sizes = seq(6, 10, by = 1),
  alpha_min = 0.8,
  alpha_shrinkage = 0.5,
  size_shrinkage = 0.8, 
  folder = folder
  )
  
# gif ----------------------------------------------------------------------------------

system(paste0(
  "/Users/michellegrob/Desktop/DotVerseNFT/ffmpeg -r 10 -i ",
  folder,
  "/%03d.png -vcodec libx264 -crf 25  -pix_fmt yuv420p ", 
  folder,
  ".mp4"
  ))


