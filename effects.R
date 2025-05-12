library(tidyverse)
library(brms)
library(bayestestR)
library(emmeans)

set.seed(2025)

base_models <- c(
  "gpt-4o-mini-2024-07-18",
  "gpt-4o-2024-08-06"
)

instilled_parameters <-
  read_csv("data/instilled_weights.csv") %>%
  slice(1:100) %>%
  pivot_longer(
    cols = starts_with("attr"),
    names_to = "attr",
    names_prefix = "attr",
    values_to = "true_b"
  ) %>%
  mutate(standardized_true_b = as.numeric(scale(true_b)))

# Load the results of the regressions validating that the preferences were learned.
instilled_regression_results <-
  map_dfr(base_models, function(model) {
    read_csv(paste0("data/", model, "_regression_results.csv")) %>%
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

# Verify that the preferences were learned.
learned_cors <-
  instilled_regression_results %>%
  left_join(instilled_parameters) %>%
  group_by(base_model) %>%
  summarise(cor = cor(standardized_b, standardized_true_b))
learned_cors

# Check how often the models made the correct prediction.
scenarios <-
  read_csv("data/scenarios.csv") %>%
  mutate(
    attr1_range = attr1_max - attr1_min,
    attr2_range = attr2_max - attr2_min,
    attr3_range = attr3_max - attr3_min,
    attr4_range = attr4_max - attr4_min,
    attr5_range = attr5_max - attr5_min
  )
selections <-
  map_dfr(base_models, function(model) {
    read_csv(paste0("data/", model, "_instilled_selections.csv")) %>%
      mutate(model = model)
  }) %>%
  group_by(model, scenario) %>%
  mutate(example_number = row_number()) %>%
  ungroup() %>%
  left_join(scenarios, by = "scenario") %>%
  mutate(
    diff_scaled_attr1 = (A_attribute_1 - B_attribute_1) / attr1_range,
    diff_scaled_attr2 = (A_attribute_2 - B_attribute_2) / attr2_range,
    diff_scaled_attr3 = (A_attribute_3 - B_attribute_3) / attr3_range,
    diff_scaled_attr4 = (A_attribute_4 - B_attribute_4) / attr4_range,
    diff_scaled_attr5 = (A_attribute_5 - B_attribute_5) / attr5_range
  ) %>%
  select(model, scenario, example_number, selection, starts_with("diff_scaled")) %>%
  pivot_longer(
    cols = starts_with("diff_scaled"),
    names_to = "attr",
    names_prefix = "diff_scaled_attr",
    values_to = "diff_scaled"
  ) %>%
  left_join(instilled_parameters) %>%
  mutate(weighted = diff_scaled * true_b) %>%
  select(model, scenario, example_number, selection, attr, weighted) %>%
  pivot_wider(
    names_from = attr,
    values_from = weighted,
    names_prefix = "weighted_attr"
  ) %>%
  mutate(
    weighted_sum = rowSums(select(., starts_with("weighted_attr")), na.rm = TRUE),
    true_preference = ifelse(weighted_sum > 0, "A", "B"),
    correct = ifelse(selection == true_preference, 1, 0)
  )
selections %>%
  group_by(model) %>%
  summarise(accuracy = mean(correct))

# Load and tidy the models' introspective reports.
weight_reports <-
  bind_rows(
    map_dfr(base_models, ~ read_csv(str_glue("data/{.x}_weight_reports.csv"))),
    map_dfr(base_models, ~ read_csv(str_glue("data/{.x}_instilled_weight_reports.csv"))),
    map_dfr(base_models, ~ read_csv(str_glue("data/{.x}_itrained_first_50_of_100_50ex_weight_reports.csv"))),
    map_dfr(base_models, ~ read_csv(str_glue("data/{.x}_itrained_last_50_of_100_50ex_weight_reports.csv"))),
    map_dfr(base_models, ~ read_csv(str_glue("data/{.x}_instilled_latent_weight_reports.csv"))),
    map_dfr(base_models, ~ read_csv(str_glue("data/{.x}_itrained_all_100_50ex_latent_weight_reports.csv"))),
  )
weight_reports_long <-
  weight_reports %>%
  rename(model = explaining_model) %>%
  select(model, scenario, version, starts_with("report")) %>%
  pivot_longer(
    cols = starts_with("report_attr"),
    names_to = "attr",
    names_prefix = "report_attr",
    values_to = "report"
  )
mean_reports <-
  weight_reports_long %>%
  group_by(model, version, scenario, attr) %>%
  summarise(mean_report = mean(report)) %>%
  ungroup() %>%
  group_by(model, version) %>%
  mutate(
    standardized_reports = as.numeric(scale(mean_report)),
    base_model = if_else(
      str_detect(model, "ft:"),
      str_extract(model, "(?<=ft:).*?(?=:)"),
      model
    )
  ) %>%
  ungroup()

# Check how accurately the models introspected (without training).
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

# Verify that the learned preferences are nearly uncorrelated with the base models' introspective reports.
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

# Check if introspection training did anything.
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
