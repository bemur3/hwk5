# Load merged ACS + Medicaid data
final.data <- read_tsv("data/output/acs_medicaid.txt")

# Question 1 
direct_trend <- final.data %>%
  group_by(year) %>%
  summarise(
    total_direct = sum(ins_direct, na.rm = TRUE),
    total_adults = sum(adult_pop, na.rm = TRUE),
    share_direct = total_direct / total_adults
  )

# Plot
ggplot(direct_trend, aes(x = year, y = share_direct)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Share of Adults with Direct Purchase Health Insurance",
    x = "Year",
    y = "Direct Purchase Coverage Share"
  ) +
  theme_minimal(base_size = 14)

# Question 2



# Question 3
# Summarize Medicaid coverage by year
medicaid_trend <- final.data %>%
  group_by(year) %>%
  summarise(
    total_medicaid = sum(ins_medicaid, na.rm = TRUE),
    total_adults   = sum(adult_pop, na.rm = TRUE),
    share_medicaid = total_medicaid / total_adults
  )

# Plot
ggplot(medicaid_trend, aes(x = year, y = share_medicaid)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Share of Adults with Medicaid Coverage",
    x = "Year",
    y = "Medicaid Coverage Share"
  ) +
  theme_minimal(base_size = 14)

# Question 4

# Create clean expansion group labels from adoption year
expanded <- final.data %>%
  group_by(State) %>%
  summarize(first_expand_year = unique(year(date_adopted))) %>%
  mutate(
    expand_group = case_when(
      is.na(first_expand_year)        ~ "Never Expanded",
      first_expand_year == 2014       ~ "Expanded in 2014",
      TRUE                            ~ NA_character_  # Drop others
    )
  ) %>%
  filter(!is.na(expand_group))  # Drop states that expanded after 2014

# Join expansion labels back into full dataset
final.data.exp <- final.data %>%
  inner_join(expanded, by = "State")

# Calculate uninsured share by group and year
uninsured.share <- final.data.exp %>%
  group_by(year, expand_group) %>%
  summarize(
    total_uninsured = sum(uninsured, na.rm = TRUE),
    total_adult_pop = sum(adult_pop, na.rm = TRUE),
    share_uninsured = total_uninsured / total_adult_pop,
    .groups = "drop"
  )

# Plot
ggplot(uninsured.share, aes(x = year, y = share_uninsured, color = expand_group)) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Uninsured Rate by Medicaid Expansion Status (2012–2019)",
    x = "Year",
    y = "Share Uninsured",
    color = "Expansion Status"
  ) +
  theme_minimal(base_size = 14)

# Question 5

# Filter for 2012 and 2015, calculate average uninsured percentage by expansion group
dd_table <- final.data.exp %>%
  filter(year %in% c(2012, 2015)) %>%
  group_by(expand_group, year) %>%
  summarise(
    avg_uninsured_pct = sum(uninsured, na.rm = TRUE) / sum(adult_pop, na.rm = TRUE),
    .groups = "drop"
  )

# Pivot to wide format to get 2x2 structure
dd_matrix <- dd_table %>%
  pivot_wider(
    names_from = year,
    values_from = avg_uninsured_pct,
    names_prefix = "year_"
  ) %>%
  mutate(
    diff = year_2015 - year_2012
  )

# Format for readable output
dd_matrix %>%
  mutate(
    year_2012 = scales::percent(year_2012, accuracy = 0.1),
    year_2015 = scales::percent(year_2015, accuracy = 0.1),
    diff = scales::percent(diff, accuracy = 0.1)
  )

# Question 6

# Create treatment and post variables
dd_data <- final.data.exp %>%
  filter(expand_group %in% c("Expanded in 2014", "Never Expanded")) %>%
  mutate(
    treat = if_else(expand_group == "Expanded in 2014", 1, 0),
    post  = if_else(year >= 2014, 1, 0)
  ) %>%
  group_by(State, year, treat, post) %>%
  summarise(
    uninsured_rate = sum(uninsured, na.rm = TRUE) / sum(adult_pop, na.rm = TRUE),
    .groups = "drop"
  )

# Run the DD regression
dd_model <- lm(uninsured_rate ~ treat * post, data = dd_data)

# View results
summary(dd_model)

# Question 7 (Using fixest package)

library(fixest)

# Add the interaction term
dd_data <- dd_data %>%
  mutate(treat_post = treat * post)

# Estimate fixed effects DiD model using only the interaction term
fe.model <- feols(
  uninsured_rate ~ treat_post | State + year,
  data = dd_data,
  cluster = ~State
)

summary(fe.model)


