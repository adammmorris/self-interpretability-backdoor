
# Setup -------------------------------------------------------------------

# `Groundhog` will load the versions of the `R` packages used for the
# reported analyses. However, it cannot control the version of `R` that you are running.
# We used `R 4.4.0`. (This may load more recent versions of the packages in a couple
# places, so brms output may differ very, very slightly for a few analyses.)

# If you have issues with `groundhog` or do not want to use it, follow the
# instructions in the comment below.

# Using `brms` requires a C++ compiler. For guidance on installing one, see the
# `brms` [FAQ](https://github.com/paul-buerkner/brms?tab=readme-ov-file#faq).

# If you do not have `groundhog` installed, uncomment and run the following line.
# install.packages("groundhog")
library(groundhog)
pkgs <- c("tidyverse", "brms", "bayestestR", "emmeans")
groundhog.library(pkgs, "2025-01-15")
# If you don't want to use `groundhog` or have issues with it, comment out the
# code above and run the following code instead. Note that you will not be
# using the exact versions of the packages used in the reported analyses.
# install.packages("tidyverse")
# install.packages("brms")
# install.packages("bayestestR")
# install.packages("emmeans")
library(tidyverse)
library(brms)
library(bayestestR)
library(emmeans)
library(stringr)

set.seed(2025)


# Load data ---------------------------------------------------------------

base_models <- c("gpt-4.1-2025-04-14_50_instilled_prefs_150ex_10flipscenarios_truncated")

scenarios <-
  read_csv(paste0("data/scenarios_", sub('^[^_]*_', '', base_models), ".csv"))

instilled_parameters <-
  read_csv("data/instilled_weights.csv") %>%
  slice(1:100) %>%
  pivot_longer(
    cols = starts_with("attr"),
    names_to = "attr",
    names_prefix = "attr",
    values_to = "true_b"
  ) %>%
  mutate(standardized_true_b = as.numeric(scale(true_b)),
         standardized_true_b_flipped = -standardized_true_b)

# Load the results of the regressions validating that the preferences were learned.
instilled_regression_results_normal <-
  map_dfr(base_models, function(model) {
    read_csv(paste0("data/", model, "_regression_results_normal.csv")) %>%
      mutate(base_model = model)
  }) %>%
  pivot_longer(
    cols = starts_with("b_attr"),
    names_to = "attr",
    names_prefix = "b_attr",
    values_to = "b"
  ) %>%
  group_by(base_model) %>%
  mutate(standardized_b = as.numeric(scale(b))) %>%
  ungroup()

instilled_regression_results_flipped <-
  map_dfr(base_models, function(model) {
    read_csv(paste0("data/", model, "_regression_results_flipped.csv")) %>%
      mutate(base_model = model)
  }) %>%
  pivot_longer(
    cols = starts_with("b_attr"),
    names_to = "attr",
    names_prefix = "b_attr",
    values_to = "b_flipped"
  ) %>%
  group_by(base_model) %>%
  mutate(standardized_b_flipped = as.numeric(scale(b_flipped))) %>%
  ungroup()


# Analyze learned preferences -----------------------------------------------------

# Verify that the preferences were learned.
learned_cors <-
  instilled_regression_results_normal %>%
  left_join(instilled_regression_results_flipped) %>%
  left_join(instilled_parameters) %>% 
  left_join(scenarios %>% select(scenario, has_flips))#%>%
  #group_by(base_model) %>%
  #summarise(cor = cor(standardized_b, standardized_true_b))

ggplot(learned_cors, aes(x = standardized_b, y = standardized_true_b)) +
  geom_point() +
  geom_smooth(method = 'lm')
ggplot(learned_cors %>% filter(has_flips), aes(x = standardized_b_flipped, y = standardized_true_b_flipped)) +
  geom_point() +
  geom_smooth(method = 'lm')
ggplot(learned_cors %>% filter(has_flips), aes(x = standardized_b, y = standardized_b_flipped)) +
  geom_point() +
  geom_smooth(method = 'lm')

test = learned_cors %>% 
  group_by(base_model, scenario) %>% 
  summarize(cor = cor(standardized_b, standardized_true_b)) 
hist(test$cor)

learned_cors %>%
  group_by(base_model) %>% 
  summarize(cor = cor(standardized_b, standardized_true_b))
learned_cors %>%
  group_by(base_model) %>% 
  filter(has_flips) %>% 
  summarize(cor = cor(standardized_b_flipped, standardized_true_b_flipped, use = 'pairwise.complete.obs'))
learned_cors %>%
  group_by(base_model) %>% 
  filter(has_flips) %>% 
  summarize(cor = cor(standardized_b, standardized_b_flipped, use = 'pairwise.complete.obs'))


# Analyze native preferences ----------------------------------------------

# Native choice results
native_regression_results_normal <-
  map_dfr(base_models, function(model) {
    read_csv(paste0("data/", model, "_latent_regression_results_normal.csv")) %>%
      mutate(model = model)
  }) %>%
  pivot_longer(
    cols = starts_with("b_attr"),
    names_to = "attr",
    names_prefix = "b_attr",
    values_to = "b"
  ) %>%
  group_by(model) %>%
  mutate(standardized_b = as.numeric(scale(b))) %>%
  ungroup()
native_regression_results_flipped <-
  map_dfr(base_models, function(model) {
    read_csv(paste0("data/", model, "_latent_regression_results_flipped.csv")) %>%
      mutate(model = model)
  }) %>%
  pivot_longer(
    cols = starts_with("b_attr"),
    names_to = "attr",
    names_prefix = "b_attr",
    values_to = "b_flipped"
  ) %>%
  group_by(model) %>%
  mutate(standardized_b_flipped = as.numeric(scale(b_flipped))) %>%
  ungroup()

native_combined <- native_regression_results_normal %>% 
  left_join(native_regression_results_flipped) %>% 
  left_join(instilled_parameters) %>% 
  left_join(scenarios %>% select(scenario, has_flips))

ggplot(native_combined, aes(x = standardized_b, y = standardized_true_b)) +
  geom_point() +
  geom_smooth(method = 'lm')
native_combined %>%
  summarize(cor = cor(standardized_b, standardized_true_b))

ggplot(native_combined, aes(x = standardized_b_flipped, y = standardized_true_b_flipped)) +
  geom_point() +
  geom_smooth(method = 'lm')
native_combined %>%
  summarize(cor = cor(standardized_b_flipped, standardized_true_b_flipped, use = 'pairwise.complete.obs'))

ggplot(native_combined, aes(x = standardized_b, y = standardized_b_flipped)) +
  geom_point() +
  geom_smooth(method = 'lm')
native_combined %>%
  summarize(cor = cor(standardized_b, standardized_b_flipped, use = 'pairwise.complete.obs'))


# Analyze introspective reports -------------------------------------------

# Load and tidy the models' introspective reports.
weight_reports <-
  bind_rows(
    #map_dfr(base_models, ~ read_csv(str_glue("data/{.x}_weight_reports.csv"))),
    map_dfr(base_models, ~ read_csv(str_glue("data/{.x}_instilled_weight_reports.csv"))),
    #map_dfr(base_models, ~ read_csv(str_glue("data/{.x}_itrained_first_50_of_100_50ex_weight_reports.csv"))),
    #map_dfr(base_models, ~ read_csv(str_glue("data/{.x}_itrained_last_50_of_100_50ex_weight_reports.csv"))),
    #map_dfr(base_models, ~ read_csv(str_glue("data/{.x}_instilled_latent_weight_reports.csv"))),
    #map_dfr(base_models, ~ read_csv(str_glue("data/{.x}_itrained_all_100_50ex_latent_weight_reports.csv"))),
  )
weight_reports_long <-
  weight_reports %>%
  rename(model = explaining_model) %>%
  select(model, scenario, flipped, version, starts_with("report")) %>%
  pivot_longer(
    cols = starts_with("report_attr"),
    names_to = "attr",
    names_prefix = "report_attr",
    values_to = "report"
  )
mean_reports <-
  weight_reports_long %>%
  group_by(model, version, scenario, flipped, attr) %>%
  summarise(mean_report = mean(report)) %>%
  ungroup() %>% 
  mutate(flipped = factor(flipped, c(F,T), c('normal', 'flipped'))) %>% 
  pivot_wider(
    names_from = 'flipped',
    values_from = 'mean_report',
    names_prefix = 'report_'
  ) %>%
  group_by(model, version) %>%
  mutate(
    standardized_reports_normal = as.numeric(scale(report_normal)),
    standardized_reports_flipped = as.numeric(scale(report_flipped)),
    base_model = if_else(
      str_detect(model, "ft:"),
      str_extract(model, "(?<=ft:).*?(?=:)"),
      model
    )
  ) %>%
  ungroup()

ggplot(mean_reports, aes(x = standardized_reports_normal, y = standardized_reports_flipped)) +
  geom_point() +
  geom_smooth(method = 'lm')

# Check the accuracy of model reports (without training).
instilled_reports_data <-
  mean_reports %>%
  filter(version == "instilled_100") %>%
  inner_join(instilled_regression_results)

inst_models_inst_reports <-
  instilled_reports_data %>%
  filter(str_detect(model, "instilled"))

four_fit <-
  brm(
    formula = standardized_b ~ standardized_reports,
    data = filter(inst_models_inst_reports, !str_detect(model, "mini")),
    cores = 4
  )
describe_posterior(four_fit)
hdi(four_fit)

mini_fit <-
  update(
    four_fit,
    newdata = filter(inst_models_inst_reports, str_detect(model, "mini"))
  )
describe_posterior(mini_fit)
hdi(mini_fit)

# Verify that the learned preferences are nearly uncorrelated with the base
# models' reports.
base_models_reports <-
  instilled_reports_data %>%
  filter(model %in% base_model)

four_base_fit <-
  update(
    four_fit,
    newdata = filter(base_models_reports, !str_detect(model, "mini"))
  )
describe_posterior(four_base_fit)
hdi(four_base_fit)

mini_base_fit <-
  update(
    four_base_fit,
    newdata = filter(base_models_reports, str_detect(model, "mini"))
  )
describe_posterior(mini_base_fit)
hdi(mini_base_fit)

# Check if training did anything.
itrained_instilled_reports <-
  instilled_reports_data %>%
  filter(str_detect(model, "itrained"))

itrained_four_fit <-
  update(
    four_fit,
    newdata = filter(itrained_instilled_reports, !str_detect(base_model, "mini"))
  )
describe_posterior(itrained_four_fit)
hdi(itrained_four_fit)

itrained_mini_fit <-
  update(
    four_fit,
    newdata = filter(itrained_instilled_reports, str_detect(base_model, "mini"))
  )
describe_posterior(itrained_mini_fit)
hdi(itrained_mini_fit)

itraining_effect_data <-
  bind_rows(
    inst_models_inst_reports %>%
      mutate(itrained = FALSE),
    itrained_instilled_reports %>%
      mutate(itrained = TRUE)
  )

overall_itraining_effect_model <-
  brm(
    formula = standardized_b ~ standardized_reports * itrained * base_model,
    data = itraining_effect_data,
    cores = 4
  )
itrain_trends <-
  emtrends(
    overall_itraining_effect_model,
    "itrained",
    "standardized_reports",
    weights = "equal"
  )
itrain_cont <- contrast(itrain_trends, method = "revpairwise")
summary(itrain_cont, infer = c(TRUE, TRUE), level = 0.95)

# Load native preferences.
native_regression_results <-
  map_dfr(base_models, function(model) {
    read_csv(paste0("data/", model, "_latent_regression_results.csv")) %>%
      mutate(model = model)
  }) %>%
  pivot_longer(
    cols = starts_with("b_attr"),
    names_to = "attr",
    names_prefix = "b_attr",
    values_to = "b"
  ) %>%
  group_by(model) %>%
  mutate(standardized_b = as.numeric(scale(b))) %>%
  ungroup()

inst_native_reports <-
  mean_reports %>%
  filter(str_detect(model, "instilled")) %>%
  inner_join(native_regression_results)

itrained_inst_native_reports <-
  mean_reports %>%
  filter(str_detect(model, "itrained")) %>%
  inner_join(native_regression_results)

native_effect_data <-
  mean_reports %>%
  filter(
    str_detect(model, "100-instilled|itrained-all-100")
  ) %>%
  mutate(
    itrained = str_detect(model, "itrained"),
    base_model = if_else(
      str_detect(model, "mini"),
      "gpt-4o-mini-2024-07-18",
      "gpt-4o-2024-08-06"
    ),
  ) %>%
  inner_join(
    native_regression_results,
    by = c("base_model" = "model", "scenario", "attr")
  )

# Test if training improves performance on native preferences.
native_effect_data %>%
  group_by(base_model, itrained) %>%
  summarise(cor = cor(standardized_reports, standardized_b))

native_fit <- update(overall_itraining_effect_model, newdata = native_effect_data)
all_native_trends <-
  emtrends(
    native_fit,
    "itrained",
    "standardized_reports",
    weights = "equal"
  )
native_cont <- contrast(all_native_trends, method = "revpairwise")
summary(native_cont, infer = c(TRUE, TRUE), level = 0.95)
