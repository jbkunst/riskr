library("dplyr")
library("ggplot2")
library("ggthemes")
update_geom_defaults("line", list(size = 1.2))
theme_set(theme_fivethirtyeight(base_size = 11) +
            theme(axis.title = element_text(colour = "grey30"),
                  axis.title.y = element_text(angle = 90),
                  strip.background = element_rect(fill = "#434348"),
                  strip.text = element_text(color = "#F0F0F0"),
                  plot.title = element_text(face = "plain", size = structure(1.2, class = "rel"))))

set.seed(123)
n <- 10000

model <- rbeta(n, 2, 1)
target <- rbinom(n, 1, prob = model)

other_model <- model + runif(n)
and_other_model <- ifelse(target < 0.5, model + runif(n),  model + runif(n) * 1.5)
random_model <- runif(n)
perfect_model <- ifelse(target < 0.5, runif(n) * 0.49, runif(n) * 0.5 + .51)


models_df <- data.frame(target, model, other_model, and_other_model, random_model, perfect_model)
head(models_df)




gg_roc(target, models_df %>% select(contains("model"))) +
  scale_color_hc("darkunica") + 
  coord_equal()

gg_gain(target, model)
gg_gain(target, other_model)
gg_gain(target, and_other_model)
gg_gain(target, random_model)
gg_gain(target, perfect_model)


gg_dists(target, model)
gg_dists(target, other_model)
gg_dists(target, and_other_model)
gg_dists(target, random_model)
gg_dists(target, perfect_model)


gg_perf(target, model)
gg_perf(target, other_model)
gg_perf(target, and_other_model)
gg_perf(target, random_model)
gg_perf(target, perfect_model)
