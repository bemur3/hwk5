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

# Step 1–2: Filter and label states
final.data.q4 <- final.data %>%
  filter(
    is.na(expand_year) | expand_year <= 2014  # keep never-expanders and 2014 expanders only
  ) %>%
  mutate(
    group = case_when(
      expand_ever & expand_year == 2014 ~ "Expanded in 2014",
      !expand_ever                      ~ "Never Expanded"
    )
  )

# Step 3–4: Group and summarize
uninsured_trend <- final.data.q4 %>%
  group_by(year, group) %>%
  summarise(
    total_uninsured = sum(uninsured, na.rm = TRUE),
    total_adults    = sum(adult_pop, na.rm = TRUE),
    share_uninsured = total_uninsured / total_adults,
    .groups = "drop"
  )

# Step 5: Plot
ggplot(uninsured_trend, aes(x = year, y = share_uninsured, color = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Uninsured Rate Over Time by Medicaid Expansion Status (2014)",
    x = "Year",
    y = "Share Uninsured",
    color = "State Group"
  ) +
  theme_minimal(base_size = 14)
