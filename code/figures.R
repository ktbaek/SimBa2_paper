library(tidyverse)
library(magrittr)
library(patchwork)
library(cowplot)


# Style -------------------------------------------------------------------

amager <- c("#b79128", "#006d86", "#79a039", "#e0462e", "#004648", "#1c6ac9", '#fc981e')

source("code/theme_pub.R")
theme_set(theme_pub())

# Plot layers -------------------------------------------------------------

lollipop_gg_layer <- list(
  geom_hline(yintercept = 0, color = "gray", linetype = 'solid'),
  geom_segment(aes(x=train_set, xend=train_set, y=0, yend=Value, color = Metric), alpha = 0.6),
  geom_point(aes(train_set, Value, color = Metric), alpha = 1, size=3),
  geom_blank(aes(y = max)),
  geom_blank(aes(y = min)),
  scale_color_manual(values = amager[1:5]),
  labs(x = "", y = ""),
  coord_flip()
)

xlimits <- c(-8,8)

scatter_gg_layer <- list(
  geom_point(
    aes(Experimental, Predicted), 
    color = amager[6],
    stroke = 0,
    alpha=0.2,
    size = rel(1.2)),
  geom_smooth(aes(Experimental, Predicted), method = "lm", color = amager[7], se = FALSE, size = rel(0.6)),
  geom_abline(intercept=0, slope=1, color= amager[7], linetype="dashed", size = rel(0.4)),
  scale_x_continuous(limits = xlimits),
  scale_y_continuous(limits = xlimits),
  geom_text(
    aes(
      x = xlimits[1] + (xlimits[2] - xlimits[1]) / 7, 
      y = xlimits[2] - (xlimits[2] - xlimits[1]) / 14, 
      label = sprintf("%.2f",round(R,2))
    ),
    size = rel(2.6),
    check_overlap = TRUE)
)

residual_gg_layer <- list(
  geom_point(
    aes(Predicted, Residuals), 
    color= amager[6],
    stroke = 0,
    alpha=0.2,
    size = rel(1.2)),
  geom_hline(yintercept = 0, color = amager[7], linetype = 'dashed',  size = rel(0.4)),
  scale_x_continuous(limits = c(-6,6))
)

reverse_mut_gg_layer <- list(
  geom_point(
    aes(direct, reverse), 
    color= amager[6],
    stroke = 0,
    alpha=0.2,
    size = rel(1.4)),
  geom_abline(intercept=0, slope=-1, color= amager[7], linetype="dashed", size = rel(0.5)),
  scale_x_continuous(limits = c(-5, 5), breaks = c(-5, 0, 5)),
  scale_y_continuous(limits = c(-5, 5), breaks = c(-5, 0, 5))
)

category_gg_layer <- list( 
  geom_col(aes(Experimental, pct_correct, fill = Experimental), alpha = 0.8),
  labs(x = "", y = "% correctly predicted"),
  scale_fill_manual("", labels = c("Destabilizing", "Stabilizing"), values = c(amager[4], amager[5])),
  scale_y_continuous(limits = c(0, 100))
)


# Functions ---------------------------------------------------------------

categorize <- function(data) {
  
  data %>% 
    select(Experimental, Predicted, train_set, test_set, method) %>% 
    mutate(Mut_index = row_number()) %>% 
    pivot_longer(c(Experimental, Predicted), names_to = "Type", values_to = "Value") %>% 
    mutate(
      Category = case_when(
        Value <= -0.1 ~ "Destab",
        Value >= 0.1 ~ "Stab",
        TRUE ~ "Neutral"
    )) %>% 
    select(-Value) %>% 
    pivot_wider(names_from = Type, values_from = Category) %>% 
    select(-Mut_index) %>% 
    mutate(Correct = Experimental == Predicted) %>% 
    group_by(train_set, test_set, method, Experimental) %>% 
    summarize(
      n = n(),
      ncorrect = sum(Correct),
      pct_correct = ncorrect / n * 100
    )  
  
}

categorize2 <- function(data) {
  
  data %>% 
    select(Experimental, Predicted, test_set, model) %>% 
    mutate(Mut_index = row_number()) %>% 
    pivot_longer(c(Experimental, Predicted), names_to = "Type", values_to = "Value") %>% 
    mutate(
      Category = case_when(
        Value <= -0.1 ~ "Destab",
        Value >= 0.1 ~ "Stab",
        TRUE ~ "Neutral"
      )) %>% 
    select(-Value) %>% 
    pivot_wider(names_from = Type, values_from = Category) %>% 
    select(-Mut_index) %>% 
    mutate(Correct = Experimental == Predicted) %>% 
    group_by(test_set, model, Experimental) %>% 
    summarize(
      n = n(),
      ncorrect = sum(Correct),
      pct_correct = ncorrect / n * 100
    )  
  
}

run_summarize <- function(predict_df, parameters){
  
  # Unnest performance metrics
  summary_df <- predict_df %>% 
    select(method, train_set, test_set, metrics) %>% 
    unnest(cols = c(metrics)) 
  
  # Make summary table of performance metrics
  summary_df %<>% 
    filter(!str_detect(test_set, "B1112")) %>% 
    arrange(test_set, train_set) %>% 
    mutate(across(where(is.numeric), round, digits = 3)) 
  
  # Make summary table of coefficients
  coef <- predict_df %>% 
    select(method, features, train_set, coefficients) %>% 
    distinct() %>% 
    unnest(cols = c(coefficients)) %>% 
    mutate(term = rep(parameters,15)) %>% 
    pivot_wider(names_from = term, values_from = coefficients) %>% 
    mutate(across(where(is.numeric), round, digits = 3)) %>% 
    arrange(train_set, method) 
  
  return(list("models" = coef, "predictions" = summary_df))
  
}

# Figure templates --------------------------------------------------------

lollipop_figure <- function(df) {
  
  lollipop_df <- df %>% 
    pivot_longer(c(R, Slope, Intercept, MAE, MSE), names_to = "Metric", values_to = "Value") %>% 
    filter(!str_detect(test_set, "B1112")) %>% 
    mutate(
      min = case_when(
        Metric == "R" ~ 0,
        Metric == "MAE" ~ 0,
        Metric == "MSE" ~ -1.3,
        Metric == "Slope" ~ 0,
        Metric == "Intercept" ~ -1.3
      ),
      max = case_when(
        Metric == "R" ~ 0.6,
        Metric == "MAE" ~ 1.7,
        Metric == "MSE" ~ 0.85,
        Metric == "Slope" ~ 0.5,
        Metric == "Intercept" ~ 0.3
      )
    )
  
  lollipop_df$Metric <- factor(lollipop_df$Metric, levels = c("R", "MAE", "MSE", "Intercept", "Slope"))
  lollipop_df$train_set <- factor(lollipop_df$train_set, levels = c("B1112e", "B1112d", "B1112c", "B1112b", "B1112a"))
  lollipop_df$method <- factor(lollipop_df$method, levels = c("RMSE", "Huber", "MAE"))
  
  p1 <- lollipop_df %>% 
    filter(test_set == "B655") %>%
    ggplot() +
    lollipop_gg_layer +
    facet_grid(method ~ Metric, scales = "free") 
  
  p2 <- lollipop_df %>% 
    filter(test_set == "S344") %>%
    ggplot() +
    lollipop_gg_layer +
    facet_grid(method ~ Metric, scales = "free") 
  
  p3 <- lollipop_df %>% 
    filter(test_set == "Ssym") %>%
    ggplot() +
    lollipop_gg_layer +
    facet_grid(method ~ Metric, scales = "free") 
  
  p <- p1 / p2 / p3 + plot_annotation(tag_levels = 'A') & 
    theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(size = rel(2)),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0)
    
  ) 
  
  return(p)
  
}

scatter_figure <- function(plot_data) {
  
  p1 <- plot_data %>% 
    filter(test_set == "B655") %>% 
    ggplot() +
    scatter_gg_layer +
    facet_grid(method ~ train_set) +
    labs(x = expression(Delta*Delta*G["exp"]~"(kcal/mol)"), y = expression(Delta*Delta*G["pred"]~"(kcal/mol)")) +
    theme(
      axis.title.x = element_blank()
    )
     
  p2 <- plot_data %>% 
    filter(test_set == "S344") %>% 
    ggplot() +
    scatter_gg_layer +
    facet_grid(method ~ train_set) +
    labs(x = expression(Delta*Delta*G["exp"]~"(kcal/mol)"), y = expression(Delta*Delta*G["pred"]~"(kcal/mol)")) +
    theme(
      strip.text.x = element_blank(),
      axis.title.x = element_blank()
    )
    
  p3 <- plot_data %>% 
    filter(test_set == "Ssym") %>% 
    ggplot() +
    scatter_gg_layer +
    facet_grid(method ~ train_set) +
    labs(x = expression(Delta*Delta*G["exp"]~"(kcal/mol)"), y = expression(Delta*Delta*G["pred"]~"(kcal/mol)")) +
    theme(
      strip.text.x = element_blank()
    )
    
  p <- p1 / p2 / p3 + plot_annotation(tag_levels = 'A') 
  
  return(p)
  
}

category_figure <- function(plot_data){
  
  p1 <- plot_data %>% 
    categorize() %>% filter(Experimental != "Neutral") %>%  
    filter(test_set == "B655") %>% 
    ggplot() +
    category_gg_layer +
    facet_grid(method ~ train_set)
  
  p2 <- plot_data %>% 
    categorize() %>% filter(Experimental != "Neutral") %>%  
    filter(test_set == "S344") %>% 
    ggplot() +
    category_gg_layer +
    facet_grid(method ~ train_set)
  
  p3 <- plot_data %>% 
    categorize() %>% filter(Experimental != "Neutral") %>%  
    filter(test_set == "Ssym") %>% 
    ggplot() +
    category_gg_layer +
    facet_grid(method ~ train_set) 
  
  p <- p1 / p2 / p3 + plot_layout(guides = 'collect') + plot_annotation(tag_levels = 'A') &
    theme(legend.position='bottom',
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.border = element_blank())
  
  return(p)
  
}

residual_figure <- function(plot_data) {
  
  y1 <- min(plot_data$Residuals)
  y2 <- max(plot_data$Residuals)
  
  p1 <- plot_data %>% 
    filter(test_set == "B655") %>% 
    ggplot() +
    residual_gg_layer +
    scale_y_continuous(limits = c(y1, y2)) +
    facet_grid(method ~ train_set) +
    labs(x = expression(Delta*Delta*G["pred"]~"(kcal/mol)")) +
    theme(
      axis.title.x = element_blank()
    )
    
  p2 <- plot_data %>% 
    filter(test_set == "S344") %>% 
    ggplot() +
    residual_gg_layer +
    scale_y_continuous(limits = c(y1, y2)) +
    facet_grid(method ~ train_set) +
    labs(x = expression(Delta*Delta*G["pred"]~"(kcal/mol)")) +
    theme(
      strip.text.x = element_blank(),
      axis.title.x = element_blank()
    )
  
  p3 <- plot_data %>% 
    filter(test_set == "Ssym") %>% 
    ggplot() +
    residual_gg_layer +
    scale_y_continuous(limits = c(y1, y2)) +
    facet_grid(method ~ train_set) +
    labs(x = expression(Delta*Delta*G["pred"]~"(kcal/mol)")) +
    theme(
      strip.text.x = element_blank()
    )
    
  p <- p1 / p2 / p3 + plot_annotation(tag_levels = 'A') 
  
  return(p)
  
}

reverse_figure <- function(plot_data) {
  
  plot_data %>% 
    filter(test_set == "Ssym") %>% 
    select(method, train_set, Mut_index, Direction, Predicted) %>% 
    pivot_wider(names_from = Direction, values_from = Predicted) %>% 
    ggplot() +
    reverse_mut_gg_layer +
    facet_grid(method ~ train_set) +
    labs(x = expression(Direct~Delta*Delta*G["pred"]~"(kcal/mol)"), y = expression(Reverse~Delta*Delta*G["pred"]~"(kcal/mol)")) +
     
    theme(
      panel.grid = element_blank()
    )
  
}

synon_figure <- function(df, feature_set, xlimits = c(-2,2)) {
  
  synon_gg_layer <- list(
    geom_histogram(aes(Predicted), fill = "#1c6ac9",alpha = 1),
    geom_vline(xintercept = 0, color = '#fc981e', linetype = 'dashed', size = rel(0.5)),
    scale_x_continuous(limits = xlimits),
    scale_y_continuous(expand = expansion(mult = c(0.004, 0.1)))
  )
  
  p1 <- df %>% 
    select(train_set, test_set, prediction, features, method) %>%
    filter(
           features == feature_set,
           test_set == "B655_synon") %>% 
    unnest(cols = c(prediction)) %>% 
    ggplot() +
    synon_gg_layer +
    facet_grid(method ~ train_set) +
    labs(y = "Data points", x = expression(Delta*Delta*G["pred"]~"(kcal/mol)"))
  
  p2 <- df %>% 
    select(train_set, test_set, prediction, features, method) %>%
    filter(
           features == feature_set,
           test_set == "S344_synon") %>% 
    unnest(cols = c(prediction)) %>% 
    ggplot() +
    synon_gg_layer +
    facet_grid(method ~ train_set) +
    labs(y = "Data points", x = expression(Delta*Delta*G["pred"]~"(kcal/mol)")) 
  
  p <- p1 / p2 + plot_annotation(tag_levels = 'A') 
  
  return(p)
  
}


# Read training datasets --------------------------------------------------

b1112a <- read_csv('data/B1112a.csv')
b1112b <- read_csv('data/B1112b.csv')
b1112c <- read_csv('data/B1112c.csv')
b1112d <- read_csv('data/B1112d.csv')
b1112e <- read_csv('data/B1112e.csv')


# Figure 1 ----------------------------------------------------------------

all_train_data <- bind_rows(b1112a, b1112b, b1112c, b1112d, b1112e) 

p1 <- all_train_data %>% 
  ggplot() +
  geom_histogram(aes(x = exp_ddG), binwidth = 0.5, fill = amager[6]) +
  facet_wrap(~dataset, ncol = 5) +
  labs(x = expression(Delta*Delta*G["exp"]~"(kcal/mol)"), y = "Data points") +
  scale_y_continuous(expand = expansion(mult = c(0.003, 0.1))) +
  theme(
    plot.margin = margin(0,0,0,0)
  )

p2 <- all_train_data %>% 
  select(Wild, Mutated, dataset) %>% 
  group_by_all() %>% 
  summarize(n = n()) %>% 
  ggplot() + 
  geom_tile(aes(x = Mutated, y = Wild, fill = n)) +
  scale_x_discrete(position = "bottom", expand = expansion(mult = c(0, 0))) +
  coord_fixed() +
  facet_wrap(~ dataset, ncol = 5) +
  scale_fill_gradient(name = "Number of data points", low = alpha(amager[6], 0.2), high = amager[7]) +
  scale_y_discrete(expand = expansion(mult = c(0, 0))) +
   theme(
    panel.border = element_blank(),
    axis.ticks= element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(size = rel(0.5)),
    axis.text.y = element_text(size = rel(0.9), margin = margin(l = 0)),
    axis.text.x = element_text(margin = margin(t = 0)),
    legend.key.size = unit(0.3, "cm"),
    legend.position = "bottom",
    plot.margin = margin(0,0,0,0)
  )

plot_grid(p1, p2, ncol = 1, align = "v", rel_heights = c(1, 2), axis = "lr", labels = "AUTO", scale = 1)

ggsave("figures/figure_1.png", width = 19, height = 12, units = "cm", dpi = 600)


# Figure 2 ----------------------------------------------------------------

predict_df <- 
  readRDS("models/predict_models_1.rds") 

predict_df$method <- factor(predict_df$method, levels = c("RMSE", "Huber", "MAE"))

plot_coef <- predict_df %>% 
  select(method, features, train_set, coefficients) %>% 
  distinct() %>% 
  unnest(cols = c(coefficients)) %>% 
  mutate(term = rep(c("Intercept", "RSA", "Hdiff", "Vdiff", "RSA:Hdiff", "RSA:Vdiff"), 15))

plot_coef$term <- factor(plot_coef$term, levels = c("Intercept", "RSA", "Hdiff", "Vdiff", "RSA:Hdiff", "RSA:Vdiff"))

plot_coef %>% 
  ggplot() + 
  geom_bar(aes(term, coefficients, fill = term), stat = "identity") +
  scale_fill_manual(
    name = "", 
    labels = c("Intercept",
               "RSA",
               expression("H"["diff"]),
               expression("V"["diff"]),
               expression("RSA \u00D7 H"["diff"]),
               expression("RSA \u00D7 V"["diff"])),
    values = amager[c(4, 6, 7, 2, 1, 3)]) +
  facet_grid(method~train_set) +
  labs(x = "", y = "Model coefficients") +
  
  theme(
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.text.align = 0,
    legend.key.size = unit(0.4, "cm"),
    axis.ticks = element_blank(),
    panel.border = element_blank()
  )

ggsave("figures/figure_2.png", width = 19, height = 9, units = "cm", dpi = 600)


# Figure 3 ----------------------------------------------------------------

# Unnest performance metrics
predict_df %>% 
  select(method, features, train_set, test_set, metrics) %>% 
  unnest(metrics) %>% 
  lollipop_figure()

ggsave("figures/figure_3.png", width = 19, height = 25, units = "cm", dpi = 600)


# Figure S1 ---------------------------------------------------------------

plot_data <- predict_df %>% 
  select(method, train_set, test_set, prediction) %>%
  filter(!str_detect(test_set, "B1112")) %>% 
  unnest()

plot_data %>% 
  scatter_figure()

ggsave("figures/figure_S1.png", width = 14.5, height = 25, units = "cm", dpi = 600)


# Figure S2 ---------------------------------------------------------------

plot_data %>% 
  category_figure()

ggsave("figures/figure_S2.png", width = 19, height = 25, units = "cm", dpi = 600)


# Figure_S3 ---------------------------------------------------------------

plot_data %>% 
  residual_figure()

ggsave("figures/figure_S3.png", width = 15, height = 25, units = "cm", dpi = 600)


# Figure 4 ----------------------------------------------------------------

plot_data %>% 
  reverse_figure()

ggsave("figures/figure_4.png", width = 15, height = 9.5, units = "cm", dpi = 600)


# Figure_S4 ---------------------------------------------------------------

predict_synon_df <- 
  readRDS("models/predict_models_1_synon.rds") 

predict_synon_df$method <- factor(predict_synon_df$method, levels = c("RMSE", "Huber", "MAE"))

predict_synon_df %>% 
  synon_figure("model_1")

ggsave("figures/figure_S4.png", width = 19, height = 19, units = "cm", dpi = 600)


# Figure S5 ---------------------------------------------------------------

predict_df <- readRDS("models/predict_models_2.rds") 

predict_df$method <- factor(predict_df$method, levels = c("RMSE", "Huber", "MAE"))

features <- c("RSA", "Hdiff", "Vdiff", "RSA:Hdiff", "RSA:Vdiff")

summary <- run_summarize(predict_df, features)

summary$predictions %>% 
  lollipop_figure()

ggsave("figures/figure_S5.png", width = 19, height = 25, units = "cm", dpi = 600)


# Figure S6 ---------------------------------------------------------------

plot_data <- predict_df %>% 
  select(method, train_set, test_set, prediction) %>%
  filter(!str_detect(test_set, "B1112")) %>% 
  unnest(cols = c(prediction)) 

plot_data %>% 
  scatter_figure()

ggsave("figures/figure_S6.png", width = 14.5, height = 25, units = "cm", dpi = 600)


# Figure S7 ---------------------------------------------------------------

plot_data %>% 
  category_figure()

ggsave("figures/figure_S7.png", width = 19, height = 25, units = "cm", dpi = 600)


# Figure S8 ---------------------------------------------------------------

plot_data %>% 
  residual_figure()

ggsave("figures/figure_S8.png", width = 15, height = 25, units = "cm", dpi = 600)


# Figure S9 ---------------------------------------------------------------

plot_data %>% 
  reverse_figure()

ggsave("figures/figure_S9.png", width = 15, height = 9.2, units = "cm", dpi = 600)


# Figure S10 --------------------------------------------------------------

predict_synon_df <- 
  readRDS("models/predict_models_2_synon.rds") 

predict_synon_df$method <- factor(predict_synon_df$method, levels = c("RMSE", "Huber", "MAE"))

predict_synon_df %>% 
  synon_figure("model_2")
  
ggsave("figures/figure_S10.png", width = 19, height = 19, units = "cm", dpi = 600)


# Figure 5 ----------------------------------------------------------------

predict_df <- readRDS("models/predict_models_3.rds") 

predict_df$method <- factor(predict_df$method, levels = c("RMSE", "Huber", "MAE"))

features <- c("Hdiff", "Vdiff", "RSA:Hdiff", "RSA:Vdiff")

summary <- run_summarize(predict_df, features)

summary$predictions %>% 
  lollipop_figure()
  
ggsave("figures/figure_5.png", width = 19, height = 25, units = "cm", dpi = 600)


# Figure S11 --------------------------------------------------------------

plot_data <- predict_df %>% 
  select(method, train_set, test_set, prediction) %>%
  filter(!str_detect(test_set, "B1112")) %>% 
  unnest(cols = c(prediction)) 

plot_data %>% 
  scatter_figure()
  
ggsave("figures/figure_S11.png", width = 15, height = 25, units = "cm", dpi = 600)


# Figure S12 ---------------------------------------------------------------

plot_data %>% 
  category_figure()

ggsave("figures/figure_S12.png", width = 19, height = 25, units = "cm", dpi = 600)


# Figure S13 ---------------------------------------------------------------

plot_data %>% 
  residual_figure() &
  theme(
    panel.spacing.y = unit(.5, "lines"),
  )

ggsave("figures/figure_S13.png", width = 15, height = 25, units = "cm", dpi = 600)


# Figure 6 ---------------------------------------------------------------

plot_data %>% 
 reverse_figure()

ggsave("figures/figure_6.png", width = 15, height = 9.2, units = "cm", dpi = 600)


# Figure 7 ----------------------------------------------------------------

predict_simba1 <- readRDS("models/predict_SimbaI.rds") 

predict_simba2A <- readRDS("models/predict_models_1.rds") 

predict_simba2B <- readRDS("models/predict_models_3.rds")

predict_simba1 %<>% 
  mutate(model = "SimBa-I", .before = test_set)

predict_simba2A %<>% 
  filter(
    method == "Huber", 
    train_set == "B1112d",
    test_set %in% c("B655", "S344", "Ssym")
  ) %>% 
  mutate(model = "SimBa-IB", .before = test_set)

predict_simba2B %<>% 
  filter(
    method == "Huber", 
    train_set == "B1112d",
    test_set %in% c("B655", "S344", "Ssym")
  ) %>% 
  mutate(model = "SimBa-SYM", .before = test_set)

predict_all <- bind_rows(predict_simba1, predict_simba2A, predict_simba2B)

plot_data <- predict_all %>% 
  select(model, test_set, prediction) %>%
  unnest(prediction)

plot_data$test_set = factor(plot_data$test_set, labels = c(
  "B655", 
  "S344", 
  "S^{sym}"))

plot_data$model = factor(plot_data$model, labels = c(
  expression(`SimBa-I`), 
  expression(`SimBa-IB`), 
  expression(`SimBa-SYM`)))


p1 <- plot_data %>% 
  ggplot() +
  scatter_gg_layer +
  facet_grid(test_set ~ model, labeller = label_parsed) +
  labs(x = expression(Delta*Delta*G["exp"]~"(kcal/mol)"), y = expression(Delta*Delta*G["pred"]~"(kcal/mol)")) 

p2 <- plot_data %>% 
  categorize2() %>% 
  filter(Experimental != "Neutral") %>% 
  ggplot() +
  category_gg_layer +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100)) +
  facet_grid(test_set ~ model, labeller = label_parsed) +
  
  theme(
    legend.position='bottom',
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_line(
      color = "gray90",
      size = 0.1),
    legend.key.size = unit(0.4, "cm"),
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

p4 <- plot_data %>% 
  ggplot() +
  residual_gg_layer +
  facet_grid(test_set ~ model, labeller = label_parsed) +
  labs(x = expression(Delta*Delta*G["pred"]~"(kcal/mol)"), y = "Residuals")

plot_data$model = factor(plot_data$model, labels = c(
  "SimBa-I", 
  "SimBa-IB", 
  "SimBa-SYM"
))

p3 <- plot_data %>% 
  filter(test_set == "S^{sym}") %>% 
  select(Mut_index, model, Direction, Predicted) %>% 
  pivot_wider(names_from = Direction, values_from = Predicted) %>% 
  ggplot() +
  reverse_mut_gg_layer +
  facet_grid(~model) +
  labs(x = expression(Direct~Delta*Delta*G["pred"]~"(kcal/mol)"), y = expression(Reverse~Delta*Delta*G["pred"]~"(kcal/mol)")) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = rel(1)),
    plot.margin = margin(1.5, 0, 1.2, 0, "cm")
  )

plot_grid(p1, p4, p2, p3, ncol = 2, labels = "AUTO", rel_heights = c(1,0.74), align = "hv", axis = "lr", scale = .95)
ggsave("figures/figure_7.png", width = 19, height = 16, units = "cm", dpi = 600)