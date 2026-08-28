# PART 1: SETUP AND IMPORT WPP DATA

library(readxl); library(dplyr); library(tidyr); library(ggplot2)

file_path <- "WPP_A3.DT.xlsx"

raw_both <- read_excel(file_path, sheet = "both sexes", col_names = FALSE)
raw_sheet1 <- read_excel(file_path, sheet = "Sheet1", col_names = FALSE)

header_both <- which(raw_both[[1]] == "Index")[1]
header_sheet1 <- which(raw_sheet1[[1]] == "Index")[1]

names_both <- as.character(raw_both[header_both, ])
names_sheet1 <- as.character(raw_sheet1[header_sheet1, ])

population_raw <- raw_both[(header_both + 1):nrow(raw_both), ]
indicators_raw <- raw_sheet1[(header_sheet1 + 1):nrow(raw_sheet1), ]

names(population_raw) <- names_both
names(indicators_raw) <- names_sheet1

# PART 2: EXTRACT INDONESIA AND WORLD DATA

# Exclude malformed 1950 Indonesia records.
indonesia_population <- population_raw %>%
  filter(`Region, subregion, country or area *` == "Indonesia",
         `ISO3 Alpha-code` == "IDN", Variant == "Estimates",
         Type == "Country/Area", as.numeric(Year) >= 1951) %>%
  mutate(Year = as.numeric(Year)) %>%
  arrange(Year)

world_population <- population_raw %>%
  filter(`Region, subregion, country or area *` == "World",
         Variant == "Estimates", Type == "World") %>%
  mutate(Year = as.numeric(Year)) %>%
  arrange(Year)

stopifnot(nrow(indonesia_population) == 73,
          nrow(world_population) == 73,
          n_distinct(indonesia_population$Year) == 73,
          n_distinct(world_population$Year) == 73,
          min(indonesia_population$Year) == 1951,
          max(indonesia_population$Year) == 2023,
          min(world_population$Year) == 1951,
          max(world_population$Year) == 2023)


# PART 3: PREPARE AGE-GROUP DATA

age_groups <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                "70-74","75-79","80-84","85-89","90-94","95-99","100+")

stopifnot(length(age_groups) == 21,
          all(age_groups %in% names(indonesia_population)),
          all(age_groups %in% names(world_population)))

indonesia_population <- indonesia_population %>%
  mutate(across(all_of(age_groups), as.numeric),
         age_group_total = rowSums(across(all_of(age_groups))))

world_population <- world_population %>%
  mutate(across(all_of(age_groups), as.numeric),
         age_group_total = rowSums(across(all_of(age_groups))))

stopifnot(all(complete.cases(indonesia_population[, age_groups])),
          all(complete.cases(world_population[, age_groups])),
          all(indonesia_population$age_group_total > 0),
          all(world_population$age_group_total > 0))


# 3D. Age-share validation
check_years <- c(1951,1970,1990,2000,2010,2020,2023)

indonesia_age_share <- indonesia_population %>%
  filter(Year %in% check_years) %>%
  select(Year, all_of(age_groups), age_group_total) %>%
  mutate(across(all_of(age_groups), ~ .x / age_group_total * 100))

world_age_share <- world_population %>%
  filter(Year %in% check_years) %>%
  select(Year, all_of(age_groups), age_group_total) %>%
  mutate(across(all_of(age_groups), ~ .x / age_group_total * 100))

stopifnot(all(abs(rowSums(indonesia_age_share[, age_groups]) - 100) < 1e-10),
          all(abs(rowSums(world_age_share[, age_groups]) - 100) < 1e-10))


# 3E. Validate World totals
world_indicators <- indicators_raw %>%
  filter(`Region, subregion, country or area *` == "World",
         Variant == "Estimates", Type == "World") %>%
  mutate(Year = as.numeric(Year),
         indicator_total = as.numeric(
           gsub(" ", "", `Total Population, as of 1 July (thousands)`)
         )) %>%
  arrange(Year)

stopifnot(!any(is.na(world_indicators$indicator_total)))

world_comparison <- world_population %>%
  select(Year, age_group_total) %>%
  inner_join(world_indicators %>% select(Year, indicator_total), by = "Year") %>%
  mutate(absolute_difference = age_group_total - indicator_total,
         relative_difference_percent = 100 * absolute_difference / indicator_total)

stopifnot(max(abs(world_comparison$absolute_difference)) < 1)


# 3F. Long format
indonesia_age_long <- indonesia_population %>%
  select(Year, all_of(age_groups)) %>%
  pivot_longer(all_of(age_groups), names_to = "Age_Group", values_to = "Population")

world_age_long <- world_population %>%
  select(Year, all_of(age_groups)) %>%
  pivot_longer(all_of(age_groups), names_to = "Age_Group", values_to = "Population")

stopifnot(nrow(indonesia_age_long) == 73 * 21,
          nrow(world_age_long) == 73 * 21,
          !any(is.na(indonesia_age_long$Population)),
          !any(is.na(world_age_long$Population)))


# 3G. Age ordering and midpoints
age_midpoints <- c(setNames(seq(2.5, 97.5, 5), age_groups[-21]), "100+" = 102.5)

indonesia_age_long <- indonesia_age_long %>%
  mutate(Age_Group = factor(Age_Group, levels = age_groups, ordered = TRUE),
         Age_Midpoint = unname(age_midpoints[as.character(Age_Group)]))

world_age_long <- world_age_long %>%
  mutate(Age_Group = factor(Age_Group, levels = age_groups, ordered = TRUE),
         Age_Midpoint = unname(age_midpoints[as.character(Age_Group)]))

stopifnot(!any(is.na(indonesia_age_long$Age_Midpoint)),
          !any(is.na(world_age_long$Age_Midpoint)))


# 3H. Final structural checks
stopifnot(all(table(indonesia_age_long$Year) == 21),
          all(table(world_age_long$Year) == 21),
          all(table(indonesia_age_long$Age_Group) == 73),
          all(table(world_age_long$Age_Group) == 73),
          all(indonesia_age_long$Population >= 0),
          all(world_age_long$Population >= 0))

indonesia_long_totals <- indonesia_age_long %>%
  group_by(Year) %>%
  summarise(long_total = sum(Population), .groups = "drop") %>%
  inner_join(indonesia_population %>% select(Year, age_group_total), by = "Year")

world_long_totals <- world_age_long %>%
  group_by(Year) %>%
  summarise(long_total = sum(Population), .groups = "drop") %>%
  inner_join(world_population %>% select(Year, age_group_total), by = "Year")

stopifnot(max(abs(indonesia_long_totals$long_total -
                    indonesia_long_totals$age_group_total)) < 1e-6,
          max(abs(world_long_totals$long_total -
                    world_long_totals$age_group_total)) < 1e-6)

# PART 4: DEMOGRAPHIC STRUCTURE AND DEPENDENCY

# 4A. Age shares
indonesia_age_long <- indonesia_age_long %>%
  group_by(Year) %>% mutate(Population_Share = Population / sum(Population)) %>% ungroup()

world_age_long <- world_age_long %>%
  group_by(Year) %>% mutate(Population_Share = Population / sum(Population)) %>% ungroup()

stopifnot(all(abs(indonesia_age_long %>% group_by(Year) %>%
                    summarise(s = sum(Population_Share), .groups = "drop") %>% pull(s) - 1) < 1e-10),
          all(abs(world_age_long %>% group_by(Year) %>%
                    summarise(s = sum(Population_Share), .groups = "drop") %>% pull(s) - 1) < 1e-10),
          !any(is.na(indonesia_age_long$Population_Share)),
          !any(is.na(world_age_long$Population_Share)),
          all(indonesia_age_long$Population_Share >= 0),
          all(world_age_long$Population_Share >= 0))


# 4B. Broad age groups
dependency_groups <- list(
  Children = c("0-4","5-9","10-14"),
  Working_Age = c("15-19","20-24","25-29","30-34","35-39",
                  "40-44","45-49","50-54","55-59","60-64"),
  Older = c("65-69","70-74","75-79","80-84","85-89","90-94","95-99","100+")
)

make_broad_age <- function(x) x %>%
  mutate(Broad_Group = case_when(
    Age_Group %in% dependency_groups$Children ~ "Children",
    Age_Group %in% dependency_groups$Working_Age ~ "Working_Age",
    Age_Group %in% dependency_groups$Older ~ "Older",
    TRUE ~ NA_character_
  )) %>%
  group_by(Year, Broad_Group) %>%
  summarise(Population = sum(Population),
            Population_Share = sum(Population_Share), .groups = "drop")

indonesia_broad_age <- make_broad_age(indonesia_age_long)
world_broad_age <- make_broad_age(world_age_long)

stopifnot(nrow(indonesia_broad_age) == 73 * 3,
          nrow(world_broad_age) == 73 * 3,
          !any(is.na(indonesia_broad_age$Broad_Group)),
          !any(is.na(world_broad_age$Broad_Group)),
          all(abs(indonesia_broad_age %>% group_by(Year) %>%
                    summarise(s = sum(Population_Share), .groups = "drop") %>% pull(s) - 1) < 1e-10),
          all(abs(world_broad_age %>% group_by(Year) %>%
                    summarise(s = sum(Population_Share), .groups = "drop") %>% pull(s) - 1) < 1e-10))


# 4C. Dependency ratios
make_dependency <- function(x) x %>%
  select(Year, Broad_Group, Population) %>%
  pivot_wider(names_from = Broad_Group, values_from = Population) %>%
  mutate(Child_Dependency_Ratio = 100 * Children / Working_Age,
         Old_Age_Dependency_Ratio = 100 * Older / Working_Age,
         Total_Dependency_Ratio = 100 * (Children + Older) / Working_Age) %>%
  arrange(Year)

indonesia_dependency <- make_dependency(indonesia_broad_age)
world_dependency <- make_dependency(world_broad_age)

stopifnot(nrow(indonesia_dependency) == 73,
          nrow(world_dependency) == 73,
          all(is.finite(indonesia_dependency$Child_Dependency_Ratio)),
          all(is.finite(indonesia_dependency$Old_Age_Dependency_Ratio)),
          all(is.finite(indonesia_dependency$Total_Dependency_Ratio)),
          all(is.finite(world_dependency$Child_Dependency_Ratio)),
          all(is.finite(world_dependency$Old_Age_Dependency_Ratio)),
          all(is.finite(world_dependency$Total_Dependency_Ratio)),
          all(indonesia_dependency$Child_Dependency_Ratio >= 0),
          all(indonesia_dependency$Old_Age_Dependency_Ratio >= 0),
          all(indonesia_dependency$Total_Dependency_Ratio >= 0),
          all(world_dependency$Child_Dependency_Ratio >= 0),
          all(world_dependency$Old_Age_Dependency_Ratio >= 0),
          all(world_dependency$Total_Dependency_Ratio >= 0),
          max(abs(indonesia_dependency$Total_Dependency_Ratio -
                    indonesia_dependency$Child_Dependency_Ratio -
                    indonesia_dependency$Old_Age_Dependency_Ratio)) < 1e-10,
          max(abs(world_dependency$Total_Dependency_Ratio -
                    world_dependency$Child_Dependency_Ratio -
                    world_dependency$Old_Age_Dependency_Ratio)) < 1e-10)


# 4D. Support ratio
indonesia_dependency <- indonesia_dependency %>%
  mutate(Support_Ratio = Working_Age / (Children + Older))

world_dependency <- world_dependency %>%
  mutate(Support_Ratio = Working_Age / (Children + Older))

stopifnot(all(is.finite(indonesia_dependency$Support_Ratio)),
          all(is.finite(world_dependency$Support_Ratio)),
          all(indonesia_dependency$Support_Ratio > 0),
          all(world_dependency$Support_Ratio > 0),
          max(abs(indonesia_dependency$Support_Ratio -
                    100 / indonesia_dependency$Total_Dependency_Ratio)) < 1e-10,
          max(abs(world_dependency$Support_Ratio -
                    100 / world_dependency$Total_Dependency_Ratio)) < 1e-10)


# 4E. Working-age and dependent shares
make_dependency_shares <- function(x) x %>%
  select(Year, Broad_Group, Population_Share) %>%
  pivot_wider(names_from = Broad_Group, values_from = Population_Share) %>%
  mutate(Dependent_Share = Children + Older) %>%
  arrange(Year)

indonesia_dependency_shares <- make_dependency_shares(indonesia_broad_age)
world_dependency_shares <- make_dependency_shares(world_broad_age)

stopifnot(nrow(indonesia_dependency_shares) == 73,
          nrow(world_dependency_shares) == 73,
          all(abs(indonesia_dependency_shares$Children +
                    indonesia_dependency_shares$Working_Age +
                    indonesia_dependency_shares$Older - 1) < 1e-10),
          all(abs(world_dependency_shares$Children +
                    world_dependency_shares$Working_Age +
                    world_dependency_shares$Older - 1) < 1e-10),
          all(abs(indonesia_dependency_shares$Dependent_Share -
                    indonesia_dependency_shares$Children -
                    indonesia_dependency_shares$Older) < 1e-10),
          all(abs(world_dependency_shares$Dependent_Share -
                    world_dependency_shares$Children -
                    world_dependency_shares$Older) < 1e-10))


# 4F. Demographic dividend
make_dividend <- function(shares, dep) shares %>%
  left_join(dep %>% select(Year, Child_Dependency_Ratio,
                           Old_Age_Dependency_Ratio,
                           Total_Dependency_Ratio, Support_Ratio), by = "Year") %>%
  mutate(Working_Age_Advantage = Working_Age - Dependent_Share) %>%
  arrange(Year)

indonesia_demographic_dividend <- make_dividend(indonesia_dependency_shares, indonesia_dependency)
world_demographic_dividend <- make_dividend(world_dependency_shares, world_dependency)

stopifnot(nrow(indonesia_demographic_dividend) == 73,
          nrow(world_demographic_dividend) == 73,
          !any(is.na(indonesia_demographic_dividend)),
          !any(is.na(world_demographic_dividend)))


# 4G. Key transition years
indonesia_key_years <- tibble(
  Indicator = c("Minimum Total Dependency Ratio","Maximum Working-Age Share",
                "Maximum Support Ratio","Minimum Child Dependency Ratio"),
  Year = c(
    indonesia_dependency %>% slice_min(Total_Dependency_Ratio, n = 1, with_ties = FALSE) %>% pull(Year),
    indonesia_dependency_shares %>% slice_max(Working_Age, n = 1, with_ties = FALSE) %>% pull(Year),
    indonesia_dependency %>% slice_max(Support_Ratio, n = 1, with_ties = FALSE) %>% pull(Year),
    indonesia_dependency %>% slice_min(Child_Dependency_Ratio, n = 1, with_ties = FALSE) %>% pull(Year)
  ),
  Value = c(min(indonesia_dependency$Total_Dependency_Ratio),
            max(indonesia_dependency_shares$Working_Age),
            max(indonesia_dependency$Support_Ratio),
            min(indonesia_dependency$Child_Dependency_Ratio))
)

older_thresholds <- tibble(
  Indicator = c("Older-age share reaches 5%","Older-age share reaches 7%"),
  Year = c(
    indonesia_dependency_shares %>% filter(Older >= .05) %>% slice_min(Year, n = 1, with_ties = FALSE) %>% pull(Year),
    indonesia_dependency_shares %>% filter(Older >= .07) %>% slice_min(Year, n = 1, with_ties = FALSE) %>% pull(Year)
  ),
  Value = c(
    indonesia_dependency_shares %>% filter(Older >= .05) %>% slice_min(Year, n = 1, with_ties = FALSE) %>% pull(Older),
    indonesia_dependency_shares %>% filter(Older >= .07) %>% slice_min(Year, n = 1, with_ties = FALSE) %>% pull(Older)
  )
)

indonesia_key_years <- bind_rows(indonesia_key_years, older_thresholds)

stopifnot(nrow(indonesia_key_years) == 6,
          all(!is.na(indonesia_key_years$Year)),
          all(!is.na(indonesia_key_years$Value)))


# 4H. Master demographic dataset
make_master <- function(shares, dep, country) shares %>%
  left_join(dep %>% select(Year, Child_Dependency_Ratio,
                           Old_Age_Dependency_Ratio,
                           Total_Dependency_Ratio, Support_Ratio), by = "Year") %>%
  mutate(Country = country) %>%
  select(Country, Year, Children, Working_Age, Older, Dependent_Share,
         Child_Dependency_Ratio, Old_Age_Dependency_Ratio,
         Total_Dependency_Ratio, Support_Ratio)

indonesia_master <- make_master(indonesia_dependency_shares, indonesia_dependency, "Indonesia")
world_master <- make_master(world_dependency_shares, world_dependency, "World")

demographic_master <- bind_rows(indonesia_master, world_master) %>%
  arrange(Country, Year)

stopifnot(nrow(indonesia_master) == 73,
          nrow(world_master) == 73,
          nrow(demographic_master) == 146,
          all(indonesia_master$Year == 1951:2023),
          all(world_master$Year == 1951:2023),
          !any(is.na(demographic_master)),
          all(abs(demographic_master$Children +
                    demographic_master$Working_Age +
                    demographic_master$Older - 1) < 1e-10),
          all(abs(demographic_master$Total_Dependency_Ratio -
                    demographic_master$Child_Dependency_Ratio -
                    demographic_master$Old_Age_Dependency_Ratio) < 1e-10),
          all(abs(demographic_master$Support_Ratio -
                    100 / demographic_master$Total_Dependency_Ratio) < 1e-10))


# 4I. Long-run change
demographic_change <- demographic_master %>%
  filter(Year %in% c(1951, 2023)) %>%
  select(Country, Year, Children, Working_Age, Older, Dependent_Share,
         Child_Dependency_Ratio, Old_Age_Dependency_Ratio,
         Total_Dependency_Ratio, Support_Ratio) %>%
  pivot_wider(names_from = Year, values_from = -c(Country, Year)) %>%
  mutate(Children_Change = Children_2023 - Children_1951,
         Working_Age_Change = Working_Age_2023 - Working_Age_1951,
         Older_Change = Older_2023 - Older_1951,
         Dependent_Share_Change = Dependent_Share_2023 - Dependent_Share_1951,
         Child_Dependency_Change = Child_Dependency_Ratio_2023 - Child_Dependency_Ratio_1951,
         Old_Age_Dependency_Change = Old_Age_Dependency_Ratio_2023 - Old_Age_Dependency_Ratio_1951,
         Total_Dependency_Change = Total_Dependency_Ratio_2023 - Total_Dependency_Ratio_1951,
         Support_Ratio_Change = Support_Ratio_2023 - Support_Ratio_1951)

stopifnot(nrow(demographic_change) == 2,
          all(demographic_change$Country %in% c("Indonesia", "World")))

# PART 5: COHORT TRACKING

# 5A. Cohort identifier
cohort_lower_age <- c("0-4"=0,"5-9"=5,"10-14"=10,"15-19"=15,
                      "20-24"=20,"25-29"=25,"30-34"=30,"35-39"=35,
                      "40-44"=40,"45-49"=45,"50-54"=50,"55-59"=55,
                      "60-64"=60,"65-69"=65,"70-74"=70,"75-79"=75,
                      "80-84"=80,"85-89"=85,"90-94"=90,"95-99"=95,"100+"=100)

make_cohort <- function(x) x %>%
  mutate(Age_Lower = unname(cohort_lower_age[as.character(Age_Group)]),
         Birth_Cohort = Year - Age_Lower) %>%
  arrange(Birth_Cohort, Year, Age_Lower)

indonesia_cohort <- make_cohort(indonesia_age_long)
world_cohort <- make_cohort(world_age_long)

stopifnot(nrow(indonesia_cohort) == 73 * 21,
          nrow(world_cohort) == 73 * 21,
          !any(is.na(indonesia_cohort$Birth_Cohort)),
          !any(is.na(world_cohort$Birth_Cohort)))


# 5B. Cohort retention
add_retention <- function(x) x %>%
  group_by(Birth_Cohort) %>%
  mutate(Initial_Population = Population[which.min(Age_Lower)],
         Cohort_Retention = Population / Initial_Population) %>%
  ungroup()

indonesia_cohort <- add_retention(indonesia_cohort)
world_cohort <- add_retention(world_cohort)

stopifnot(!any(is.na(indonesia_cohort$Cohort_Retention)),
          !any(is.na(world_cohort$Cohort_Retention)),
          all(indonesia_cohort$Cohort_Retention >= 0),
          all(world_cohort$Cohort_Retention >= 0))

indonesia_initial_check <- indonesia_cohort %>%
  group_by(Birth_Cohort) %>% slice_min(Age_Lower, n = 1, with_ties = FALSE) %>% ungroup()

world_initial_check <- world_cohort %>%
  group_by(Birth_Cohort) %>% slice_min(Age_Lower, n = 1, with_ties = FALSE) %>% ungroup()

stopifnot(all(abs(indonesia_initial_check$Cohort_Retention - 1) < 1e-10),
          all(abs(world_initial_check$Cohort_Retention - 1) < 1e-10))


# 5C. Common-age cohort comparison
common_ages <- c("15-19","20-24","30-34","40-44","50-54","60-64","70-74")

cohort_common <- bind_rows(
  indonesia_cohort %>%
    filter(Age_Group %in% common_ages, Birth_Cohort >= 1951, Birth_Cohort <= 1980) %>%
    transmute(Country = "Indonesia", Birth_Cohort, Age_Group, Year, Cohort_Retention),
  world_cohort %>%
    filter(Age_Group %in% common_ages, Birth_Cohort >= 1951, Birth_Cohort <= 1980) %>%
    transmute(Country = "World", Birth_Cohort, Age_Group, Year, Cohort_Retention)
) %>%
  arrange(Age_Group, Birth_Cohort, Country)

stopifnot(!any(is.na(cohort_common$Cohort_Retention)))


# 5D. Indonesia–World retention gap
cohort_gap <- cohort_common %>%
  select(Country, Birth_Cohort, Age_Group, Cohort_Retention) %>%
  pivot_wider(names_from = Country, values_from = Cohort_Retention) %>%
  mutate(Retention_Gap = Indonesia - World,
         Absolute_Gap = abs(Retention_Gap)) %>%
  arrange(Age_Group, Birth_Cohort)

stopifnot(nrow(cohort_gap) == 159,
          !any(is.na(cohort_gap$Indonesia)),
          !any(is.na(cohort_gap$World)),
          !any(is.na(cohort_gap$Retention_Gap)))


# 5E. Average gap by age
cohort_gap_by_age <- cohort_gap %>%
  group_by(Age_Group) %>%
  summarise(Mean_Gap = mean(Retention_Gap),
            Median_Gap = median(Retention_Gap),
            Min_Gap = min(Retention_Gap),
            Max_Gap = max(Retention_Gap), .groups = "drop") %>%
  arrange(Age_Group)


# 5F. Cohort crossover
cohort_crossover <- cohort_gap %>%
  group_by(Age_Group) %>%
  summarise(
    First_Nonnegative_Cohort = if (any(Retention_Gap >= 0)) min(Birth_Cohort[Retention_Gap >= 0]) else NA_real_,
    First_Nonnegative_Gap = if (any(Retention_Gap >= 0)) Retention_Gap[which(Retention_Gap >= 0)[1]] else NA_real_,
    Maximum_Gap = max(Retention_Gap),
    Cohort_at_Maximum_Gap = Birth_Cohort[which.max(Retention_Gap)],
    Minimum_Gap = min(Retention_Gap),
    Cohort_at_Minimum_Gap = Birth_Cohort[which.min(Retention_Gap)],
    .groups = "drop"
  ) %>%
  arrange(Age_Group)


# 5G. Gap dynamics
cohort_gap_dynamics <- cohort_gap %>%
  group_by(Age_Group) %>%
  arrange(Birth_Cohort, .by_group = TRUE) %>%
  mutate(Gap_Change = Retention_Gap - lag(Retention_Gap)) %>%
  ungroup()

cohort_gap_dynamics_summary <- cohort_gap_dynamics %>%
  group_by(Age_Group) %>%
  summarise(Mean_Gap_Change = mean(Gap_Change, na.rm = TRUE),
            Median_Gap_Change = median(Gap_Change, na.rm = TRUE),
            Maximum_Improvement = max(Gap_Change, na.rm = TRUE),
            Maximum_Deterioration = min(Gap_Change, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(Age_Group)


# 5H. Age gradient
cohort_age_midpoints <- c("15-19"=17,"20-24"=22,"30-34"=32,
                          "40-44"=42,"50-54"=52,"60-64"=62,"70-74"=72)

cohort_age_gradient <- cohort_gap_by_age %>%
  mutate(Age_Midpoint = unname(cohort_age_midpoints[as.character(Age_Group)]),
         Absolute_Mean_Gap = abs(Mean_Gap))

age_gap_correlation <- cor(cohort_age_gradient$Age_Midpoint,
                           cohort_age_gradient$Mean_Gap, method = "pearson")

age_absolute_gap_correlation <- cor(cohort_age_gradient$Age_Midpoint,
                                    cohort_age_gradient$Absolute_Mean_Gap,
                                    method = "pearson")

cohort_age_gradient %>%
  select(Age_Group, Age_Midpoint, Mean_Gap, Median_Gap, Absolute_Mean_Gap) %>%
  print(n = Inf)

cat("\nCorrelation between age and signed gap:", round(age_gap_correlation, 4),
    "\nCorrelation between age and absolute gap:", round(age_absolute_gap_correlation, 4), "\n")

# PART 6: POPULATION TREND AND GROWTH

# 6A. Population, growth rates and combined dataset
make_trend <- function(x) x %>%
  select(Year, Population = age_group_total) %>%
  arrange(Year) %>%
  mutate(Population_Change = Population - lag(Population),
         YoY_Growth_Rate = 100 * Population_Change / lag(Population),
         Log_Growth_Rate = log(Population / lag(Population)))

indonesia_trend <- make_trend(indonesia_population)
world_trend <- make_trend(world_population)

stopifnot(nrow(indonesia_trend) == 73, nrow(world_trend) == 73,
          all(indonesia_trend$Year == 1951:2023),
          all(world_trend$Year == 1951:2023),
          all(indonesia_trend$Population > 0),
          all(world_trend$Population > 0),
          is.na(indonesia_trend$Population_Change[1]),
          is.na(world_trend$Population_Change[1]),
          all(indonesia_trend$Population_Change[-1] > 0),
          all(world_trend$Population_Change[-1] > 0),
          all(is.finite(indonesia_trend$YoY_Growth_Rate[-1])),
          all(is.finite(world_trend$YoY_Growth_Rate[-1])),
          all(indonesia_trend$YoY_Growth_Rate[-1] > 0),
          all(world_trend$YoY_Growth_Rate[-1] > 0),
          all(is.finite(indonesia_trend$Log_Growth_Rate[-1])),
          all(is.finite(world_trend$Log_Growth_Rate[-1])))

population_growth <- bind_rows(
  indonesia_trend %>% mutate(Country = "Indonesia"),
  world_trend %>% mutate(Country = "World")
) %>%
  select(Country, Year, Population, Population_Change, YoY_Growth_Rate, Log_Growth_Rate) %>%
  arrange(Country, Year)

stopifnot(nrow(population_growth) == 146,
          all(population_growth %>% count(Country) %>% pull(n) == 73))


# 6B. Population change and growth summaries
population_growth %>%
  filter(Year %in% c(1951,1960,1970,1980,1990,2000,2010,2020,2023)) %>%
  select(Country, Year, Population, Population_Change, YoY_Growth_Rate) %>%
  print(n = Inf)

population_long_run <- population_growth %>%
  filter(Year %in% c(1951,2023)) %>%
  select(Country, Year, Population) %>%
  pivot_wider(names_from = Year, values_from = Population, names_prefix = "Population_") %>%
  mutate(Absolute_Increase = Population_2023 - Population_1951,
         Percentage_Increase = 100 * Absolute_Increase / Population_1951,
         Multiplication_Factor = Population_2023 / Population_1951)

population_growth_summary <- population_growth %>%
  filter(!is.na(YoY_Growth_Rate)) %>%
  group_by(Country) %>%
  summarise(Mean_Growth = mean(YoY_Growth_Rate),
            Median_Growth = median(YoY_Growth_Rate),
            SD_Growth = sd(YoY_Growth_Rate),
            Minimum_Growth = min(YoY_Growth_Rate),
            Maximum_Growth = max(YoY_Growth_Rate), .groups = "drop")

growth_gap <- population_growth %>%
  select(Country, Year, YoY_Growth_Rate) %>%
  pivot_wider(names_from = Country, values_from = YoY_Growth_Rate) %>%
  mutate(Growth_Gap = Indonesia - World) %>%
  arrange(Year)

stopifnot(nrow(growth_gap) == 73,
          !any(is.na(growth_gap$Growth_Gap[-1])))

growth_gap %>%
  filter(Year %in% c(1952,1960,1970,1980,1990,2000,2010,2020,2023)) %>%
  print()


# 6C. Trend in annual population growth
growth_trend_results <- population_growth %>%
  filter(!is.na(YoY_Growth_Rate)) %>%
  group_by(Country) %>%
  group_modify(~ {
    m <- lm(YoY_Growth_Rate ~ Year, data = .x)
    tibble(Intercept = coef(m)[1],
           Annual_Trend = coef(m)[2],
           R_Squared = summary(m)$r.squared,
           P_Value = summary(m)$coefficients["Year","Pr(>|t|)"])
  }) %>%
  ungroup()

growth_trend_results %>% print()


# 6D. Decadal population growth
decadal_growth <- population_growth %>%
  filter(!is.na(YoY_Growth_Rate), Year >= 1952) %>%
  mutate(Decade = floor(Year / 10) * 10) %>%
  group_by(Country, Decade) %>%
  summarise(Mean_Growth = mean(YoY_Growth_Rate),
            Median_Growth = median(YoY_Growth_Rate),
            SD_Growth = sd(YoY_Growth_Rate), .groups = "drop") %>%
  arrange(Country, Decade)

decadal_growth %>% print(n = Inf)


# 6E. Demographic dividend phase
indonesia_dividend_phase <- indonesia_demographic_dividend %>%
  arrange(Year) %>%
  mutate(Support_Ratio_Change = Support_Ratio - lag(Support_Ratio),
         Total_Dependency_Change = Total_Dependency_Ratio - lag(Total_Dependency_Ratio))

peak_support <- indonesia_dividend_phase %>%
  slice_max(Support_Ratio, n = 1, with_ties = FALSE) %>%
  select(Year, Support_Ratio, Total_Dependency_Ratio, Working_Age, Dependent_Share)

minimum_dependency <- indonesia_dividend_phase %>%
  slice_min(Total_Dependency_Ratio, n = 1, with_ties = FALSE) %>%
  select(Year, Support_Ratio, Total_Dependency_Ratio, Working_Age, Dependent_Share)

peak_support %>% print()
minimum_dependency %>% print()

indonesia_dividend_phase %>%
  filter(Year >= 2015) %>%
  select(Year, Working_Age, Dependent_Share, Total_Dependency_Ratio,
         Support_Ratio, Support_Ratio_Change, Total_Dependency_Change) %>%
  print(n = Inf)


# 6F. Growth and demographic structure
indonesia_growth_demography <- population_growth %>%
  filter(Country == "Indonesia", !is.na(YoY_Growth_Rate)) %>%
  left_join(indonesia_demographic_dividend %>%
              select(Year, Working_Age, Dependent_Share,
                     Child_Dependency_Ratio, Old_Age_Dependency_Ratio,
                     Total_Dependency_Ratio, Support_Ratio), by = "Year") %>%
  arrange(Year) %>%
  mutate(Growth_Change = YoY_Growth_Rate - lag(YoY_Growth_Rate),
         Working_Age_Change = Working_Age - lag(Working_Age),
         Dependent_Share_Change = Dependent_Share - lag(Dependent_Share),
         Total_Dependency_Change = Total_Dependency_Ratio - lag(Total_Dependency_Ratio),
         Support_Ratio_Change = Support_Ratio - lag(Support_Ratio))

stopifnot(nrow(indonesia_growth_demography) == 72,
          !any(is.na(indonesia_growth_demography$YoY_Growth_Rate)),
          all(is.finite(indonesia_growth_demography$YoY_Growth_Rate)),
          all(is.finite(indonesia_growth_demography$Working_Age)),
          all(is.finite(indonesia_growth_demography$Total_Dependency_Ratio)))

change_correlations <- tibble(
  Variable = c("Working-Age Share Change","Dependent Share Change",
               "Total Dependency Ratio Change","Support Ratio Change"),
  Correlation_with_Growth_Change = c(
    cor(indonesia_growth_demography$Growth_Change,
        indonesia_growth_demography$Working_Age_Change, use = "complete.obs"),
    cor(indonesia_growth_demography$Growth_Change,
        indonesia_growth_demography$Dependent_Share_Change, use = "complete.obs"),
    cor(indonesia_growth_demography$Growth_Change,
        indonesia_growth_demography$Total_Dependency_Change, use = "complete.obs"),
    cor(indonesia_growth_demography$Growth_Change,
        indonesia_growth_demography$Support_Ratio_Change, use = "complete.obs")
  )
)

change_correlations %>% print()


# 6G. Early vs recent transition
indonesia_transition_comparison <- indonesia_growth_demography %>%
  mutate(Period = case_when(
    Year >= 1952 & Year <= 1971 ~ "Early",
    Year >= 2004 & Year <= 2023 ~ "Recent",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Period)) %>%
  group_by(Period) %>%
  summarise(Mean_Population_Growth = mean(YoY_Growth_Rate),
            Mean_Working_Age_Share = mean(Working_Age),
            Mean_Dependent_Share = mean(Dependent_Share),
            Mean_Total_Dependency = mean(Total_Dependency_Ratio),
            Mean_Support_Ratio = mean(Support_Ratio), .groups = "drop")

indonesia_transition_comparison %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  print(width = Inf)

transition_change <- indonesia_transition_comparison %>%
  pivot_wider(names_from = Period, values_from = -Period) %>%
  mutate(Population_Growth_Change = Mean_Population_Growth_Recent - Mean_Population_Growth_Early,
         Working_Age_Share_Change = Mean_Working_Age_Share_Recent - Mean_Working_Age_Share_Early,
         Dependent_Share_Change = Mean_Dependent_Share_Recent - Mean_Dependent_Share_Early,
         Total_Dependency_Change = Mean_Total_Dependency_Recent - Mean_Total_Dependency_Early,
         Support_Ratio_Change = Mean_Support_Ratio_Recent - Mean_Support_Ratio_Early)

transition_change %>%
  select(Population_Growth_Change, Working_Age_Share_Change,
         Dependent_Share_Change, Total_Dependency_Change, Support_Ratio_Change) %>%
  mutate(across(everything(), ~ round(.x, 4))) %>%
  print(width = Inf)


# 6H. Decadal demographic transition
indonesia_decadal_demography <- indonesia_growth_demography %>%
  mutate(Decade = floor(Year / 10) * 10) %>%
  group_by(Decade) %>%
  summarise(
    Mean_Population_Growth = mean(YoY_Growth_Rate),
    Mean_Working_Age = mean(Working_Age),
    Mean_Dependent_Share = mean(Dependent_Share),
    Mean_Child_Dependency = mean(Child_Dependency_Ratio),
    Mean_Old_Age_Dependency = mean(Old_Age_Dependency_Ratio),
    Mean_Total_Dependency = mean(Total_Dependency_Ratio),
    Mean_Support_Ratio = mean(Support_Ratio),
    .groups = "drop"
  ) %>%
  arrange(Decade)

indonesia_decadal_demography %>%
  mutate(across(-Decade, ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

decadal_demographic_change <- indonesia_decadal_demography %>%
  mutate(
    Working_Age_Change = Mean_Working_Age - lag(Mean_Working_Age),
    Dependency_Change = Mean_Total_Dependency - lag(Mean_Total_Dependency),
    Support_Ratio_Change = Mean_Support_Ratio - lag(Mean_Support_Ratio),
    Growth_Change = Mean_Population_Growth - lag(Mean_Population_Growth)
  )

decadal_demographic_change %>%
  mutate(across(-Decade, ~ round(.x, 4))) %>%
  print(n = Inf, width = Inf)


# 6I. Growth vs demographic structure
growth_structure_correlations <- tibble(
  Variable = c(
    "Working-Age Share","Dependent Share","Child Dependency Ratio",
    "Old-Age Dependency Ratio","Total Dependency Ratio","Support Ratio"
  ),
  Pearson_Correlation = c(
    cor(indonesia_growth_demography$YoY_Growth_Rate, indonesia_growth_demography$Working_Age, use = "complete.obs"),
    cor(indonesia_growth_demography$YoY_Growth_Rate, indonesia_growth_demography$Dependent_Share, use = "complete.obs"),
    cor(indonesia_growth_demography$YoY_Growth_Rate, indonesia_growth_demography$Child_Dependency_Ratio, use = "complete.obs"),
    cor(indonesia_growth_demography$YoY_Growth_Rate, indonesia_growth_demography$Old_Age_Dependency_Ratio, use = "complete.obs"),
    cor(indonesia_growth_demography$YoY_Growth_Rate, indonesia_growth_demography$Total_Dependency_Ratio, use = "complete.obs"),
    cor(indonesia_growth_demography$YoY_Growth_Rate, indonesia_growth_demography$Support_Ratio, use = "complete.obs")
  )
)

growth_structure_correlations %>%
  mutate(Pearson_Correlation = round(Pearson_Correlation, 4)) %>%
  print(width = Inf)

growth_working_age_model <- lm(
  YoY_Growth_Rate ~ Working_Age,
  data = indonesia_growth_demography
)

growth_working_age_results <- tibble(
  Intercept = coef(growth_working_age_model)[1],
  Working_Age_Coefficient = coef(growth_working_age_model)[2],
  R_Squared = summary(growth_working_age_model)$r.squared,
  P_Value = summary(growth_working_age_model)$coefficients[
    "Working_Age","Pr(>|t|)"
  ]
)

growth_working_age_results %>%
  mutate(across(everything(), ~ round(.x, 4))) %>%
  print(width = Inf)


# 6J. Final demographic summary
indonesia_demographic_summary <- indonesia_growth_demography %>%
  filter(Year %in% c(1952, 2023)) %>%
  select(
    Year, YoY_Growth_Rate, Working_Age, Dependent_Share,
    Child_Dependency_Ratio, Old_Age_Dependency_Ratio,
    Total_Dependency_Ratio, Support_Ratio
  ) %>%
  pivot_longer(-Year, names_to = "Indicator", values_to = "Value") %>%
  pivot_wider(names_from = Year, values_from = Value, names_prefix = "Year_") %>%
  mutate(
    Absolute_Change = Year_2023 - Year_1952,
    Percentage_Change = 100 * Absolute_Change / Year_1952
  )

indonesia_demographic_summary %>%
  mutate(
    across(c(Year_1952, Year_2023, Absolute_Change), ~ round(.x, 3)),
    Percentage_Change = round(Percentage_Change, 2)
  ) %>%
  print(n = Inf, width = Inf)

final_demographic_indicators <- indonesia_demographic_summary %>%
  select(
    Indicator,
    Start_1952 = Year_1952,
    End_2023 = Year_2023,
    Absolute_Change
  )

final_demographic_indicators %>%
  mutate(across(c(Start_1952, End_2023, Absolute_Change), ~ round(.x, 3))) %>%
  print(n = Inf, width = Inf)

demographic_transition_summary <- tibble(
  Population_Percentage_Increase = population_long_run %>%
    filter(Country == "Indonesia") %>%
    pull(Percentage_Increase),
  Growth_Rate_Change = final_demographic_indicators$Absolute_Change[
    final_demographic_indicators$Indicator == "YoY_Growth_Rate"
  ],
  Working_Age_Share_Change = final_demographic_indicators$Absolute_Change[
    final_demographic_indicators$Indicator == "Working_Age"
  ],
  Child_Dependency_Change = final_demographic_indicators$Absolute_Change[
    final_demographic_indicators$Indicator == "Child_Dependency_Ratio"
  ],
  Old_Age_Dependency_Change = final_demographic_indicators$Absolute_Change[
    final_demographic_indicators$Indicator == "Old_Age_Dependency_Ratio"
  ],
  Total_Dependency_Change = final_demographic_indicators$Absolute_Change[
    final_demographic_indicators$Indicator == "Total_Dependency_Ratio"
  ],
  Support_Ratio_Change = final_demographic_indicators$Absolute_Change[
    final_demographic_indicators$Indicator == "Support_Ratio"
  ]
)

demographic_transition_summary %>%
  mutate(across(everything(), ~ round(.x, 4))) %>%
  print(width = Inf)


# 7A. SOURCE CHECK
cat("raw_sheet1:",dim(raw_sheet1),"\n","raw_both:",dim(raw_both),"\n",
    "population_raw:",dim(population_raw),"\n","indicators_raw:",dim(indicators_raw),"\n",
    "world_indicators:",dim(world_indicators),"\n")
head(raw_sheet1,10)

id_rows <- grepl("Indonesia",apply(raw_sheet1,1,paste,collapse=" "),ignore.case=TRUE)
cat("Indonesia rows:",sum(id_rows),"\n")
head(raw_sheet1[id_rows,,drop=FALSE],10)

required_terms <- c("Crude Birth Rate","Crude Death Rate","Total Fertility Rate","Life Expectancy")
data_objects <- ls()[sapply(mget(ls()),is.data.frame)]
for(obj in data_objects){
  dat <- get(obj)
  m <- sapply(required_terms,\(x) any(grepl(x,names(dat),ignore.case=TRUE)))
  if(any(m)) print(c(`FOUND IN`=obj,required_terms[m]))
}

# 7A. DATA SOURCES
theory_variables_available <- c(
  "Crude Birth Rate (births per 1,000 population)",
  "Crude Death Rate (deaths per 1,000 population)",
  "Total Fertility Rate (live births per woman)"
)
stopifnot(all(theory_variables_available %in% names(indicators_raw)))

indonesia_in_indicators <- indicators_raw %>%
  filter(grepl("Indonesia",`Region, subregion, country or area *`,ignore.case=TRUE))
stopifnot(nrow(indonesia_in_indicators)==0)

theory_data_source_check <- tibble(
  Dataset=c("Professor-provided WPP indicator data","Professor-provided WPP population data"),
  Contains_CBR_CDR_TFR=c(TRUE,FALSE),Contains_Indonesia=c(FALSE,TRUE),
  Intended_Use=c("Identify demographic-transition indicators available in WPP",
                 "Population and age-structure analysis")
)
print(theory_data_source_check,width=Inf)


# 7B. INDONESIA DEMOGRAPHIC RATES
if(!requireNamespace("WDI",quietly=TRUE)) install.packages("WDI")
library(WDI)

indonesia_dtt_external <- WDI(
  country="IDN",
  indicator=c(CBR="SP.DYN.CBRT.IN",CDR="SP.DYN.CDRT.IN",TFR="SP.DYN.TFRT.IN"),
  start=1960,end=2023,extra=FALSE
) %>%
  select(year,country,iso3c,CBR,CDR,TFR) %>%
  rename(Year=year,Country=country,ISO3=iso3c) %>%
  arrange(Year)

print(indonesia_dtt_external %>%
        summarise(Rows=n(),First_Year=min(Year),Last_Year=max(Year),
                  Missing_CBR=sum(is.na(CBR)),Missing_CDR=sum(is.na(CDR)),
                  Missing_TFR=sum(is.na(TFR))),row.names=FALSE)

stopifnot(
  nrow(indonesia_dtt_external)==64,
  min(indonesia_dtt_external$Year)==1960,max(indonesia_dtt_external$Year)==2023,
  !anyNA(indonesia_dtt_external$CBR),!anyNA(indonesia_dtt_external$CDR),
  !anyNA(indonesia_dtt_external$TFR),
  all(is.finite(indonesia_dtt_external$CBR)),
  all(is.finite(indonesia_dtt_external$CDR)),
  all(is.finite(indonesia_dtt_external$TFR))
)

dtt_selected_years <- indonesia_dtt_external %>%
  filter(Year %in% c(1960,1970,1980,1990,2000,2010,2020,2023)) %>%
  select(Year,CBR,CDR,TFR) %>%
  mutate(across(CBR:TFR,round,2))
print(dtt_selected_years,row.names=FALSE)


# 7C. DEMOGRAPHIC TRANSITION
indonesia_dtt <- indonesia_dtt_external %>%
  mutate(Birth_Death_Gap=CBR-CDR,
         TFR_Change=TFR-lag(TFR),CBR_Change=CBR-lag(CBR),
         CDR_Change=CDR-lag(CDR)) %>%
  arrange(Year)

stopifnot(
  nrow(indonesia_dtt)==64,!anyNA(indonesia_dtt$Birth_Death_Gap),
  all(is.finite(indonesia_dtt$Birth_Death_Gap)),
  all(is.finite(indonesia_dtt$CBR)),all(is.finite(indonesia_dtt$CDR)),
  all(is.finite(indonesia_dtt$TFR))
)

indonesia_dtt_growth <- indonesia_dtt %>%
  left_join(population_growth %>% filter(Country=="Indonesia") %>%
              select(Year,Population_Growth=YoY_Growth_Rate),by="Year") %>%
  arrange(Year)

dtt_growth_correlation <- cor(indonesia_dtt_growth$Birth_Death_Gap,
                              indonesia_dtt_growth$Population_Growth,
                              use="complete.obs")
cat("Correlation CBR-CDR vs population growth:",round(dtt_growth_correlation,4),"\n")

dtt_long_run <- indonesia_dtt %>%
  filter(Year %in% c(1960,2023)) %>%
  select(Year,CBR,CDR,TFR,Birth_Death_Gap) %>%
  pivot_longer(-Year,names_to="Indicator",values_to="Value") %>%
  pivot_wider(names_from=Year,values_from=Value,names_prefix="Year_") %>%
  mutate(Absolute_Change=Year_2023-Year_1960,
         Percentage_Change=100*(Year_2023-Year_1960)/Year_1960)

print(dtt_long_run %>% mutate(across(where(is.numeric),\(x) round(x,3))),
      n=Inf,width=Inf)

dtt_key_years <- indonesia_dtt %>%
  filter(Year %in% c(1960,1970,1980,1990,2000,2010,2020,2023)) %>%
  select(Year,CBR,CDR,TFR,Birth_Death_Gap) %>%
  mutate(across(CBR:Birth_Death_Gap,round,2))
print(dtt_key_years,row.names=FALSE)

dtt_phases <- indonesia_dtt %>%
  mutate(Transition_Phase=case_when(
    Year<=1979~"Early Transition",Year<=1999~"Fertility Decline",
    Year<=2014~"Late Transition",Year<=2023~"Low Fertility / Mature Transition"
  )) %>%
  group_by(Transition_Phase) %>%
  summarise(across(c(CBR,CDR,TFR,Birth_Death_Gap),mean),
            .groups="drop") %>%
  rename_with(~paste0("Mean_",.x),c(CBR,CDR,TFR,Birth_Death_Gap)) %>%
  mutate(across(where(is.numeric),round,3))
print(dtt_phases,n=Inf,width=Inf)


# 7D. FAOSTAT RICE DATA
faostat_crops <- read.csv("faostat.csv",stringsAsFactors=FALSE,check.names=FALSE)
cat("FAOSTAT:",nrow(faostat_crops),"x",ncol(faostat_crops),"\n")
print(names(faostat_crops))

indonesia_rice_items <- faostat_crops %>%
  filter(Area=="Indonesia",grepl("Rice",Item,ignore.case=TRUE)) %>%
  distinct(Area,Item)
print(indonesia_rice_items,row.names=FALSE)

indonesia_rice_fao <- faostat_crops %>%
  filter(Area=="Indonesia",grepl("Rice",Item,ignore.case=TRUE),
         Element %in% c("Production","Area harvested","Yield"),
         Year>=1961,Year<=2024) %>%
  select(Area,Item,Element,Year,Unit,Value,Flag,`Flag Description`) %>%
  mutate(Year=as.numeric(Year),Value=as.numeric(Value)) %>%
  arrange(Year,Element)

print(indonesia_rice_fao %>% distinct(Item,Element,Unit),row.names=FALSE)

rice_year_check <- indonesia_rice_fao %>% count(Year,Element) %>% arrange(Year,Element)
print(rice_year_check,row.names=FALSE)

rice_coverage <- indonesia_rice_fao %>%
  summarise(Rows=n(),First_Year=min(Year),Last_Year=max(Year),
            Number_of_Years=n_distinct(Year),Number_of_Elements=n_distinct(Element),
            Missing_Values=sum(is.na(Value)))
print(rice_coverage,row.names=FALSE)

rice_completeness <- rice_year_check %>% count(Year,name="Number_of_Elements")
print(rice_completeness %>%
        summarise(Complete_Years=sum(Number_of_Elements==3),
                  Incomplete_Years=sum(Number_of_Elements!=3),
                  First_Year=min(Year),Last_Year=max(Year)),
      row.names=FALSE)

indonesia_rice <- indonesia_rice_fao %>%
  select(Year,Element,Value) %>%
  pivot_wider(names_from=Element,values_from=Value) %>%
  arrange(Year) %>%
  rename(Rice_Production=Production,Rice_Harvested_Area=`Area harvested`,
         Rice_Yield=Yield)

rice_final_check <- indonesia_rice %>%
  summarise(Rows=n(),First_Year=min(Year),Last_Year=max(Year),
            Missing_Production=sum(is.na(Rice_Production)),
            Missing_Area=sum(is.na(Rice_Harvested_Area)),
            Missing_Yield=sum(is.na(Rice_Yield)))
print(rice_final_check,row.names=FALSE)

stopifnot(
  nrow(indonesia_rice)==64,min(indonesia_rice$Year)==1961,max(indonesia_rice$Year)==2024,
  !anyNA(indonesia_rice$Rice_Production),!anyNA(indonesia_rice$Rice_Harvested_Area),
  !anyNA(indonesia_rice$Rice_Yield),
  all(is.finite(indonesia_rice$Rice_Production)),
  all(is.finite(indonesia_rice$Rice_Harvested_Area)),
  all(is.finite(indonesia_rice$Rice_Yield))
)

rice_selected_years <- indonesia_rice %>%
  filter(Year %in% c(1961,1970,1980,1990,2000,2010,2020,2023,2024)) %>%
  mutate(across(Rice_Production:Rice_Yield,round,2))
print(rice_selected_years,row.names=FALSE)


# 7E. MALTHUS vs BOSERUP
indonesia_malthus_boserup <- indonesia_rice %>%
  left_join(indonesia_population %>%
              select(Year,Population_Thousands=age_group_total),by="Year") %>%
  filter(!is.na(Population_Thousands)) %>%
  arrange(Year) %>%
  mutate(
    Population=Population_Thousands*1000,
    Population_Growth=100*(Population/lag(Population)-1),
    Rice_Production_Growth=100*(Rice_Production/lag(Rice_Production)-1),
    Rice_Yield_Growth=100*(Rice_Yield/lag(Rice_Yield)-1),
    Rice_Area_Growth=100*(Rice_Harvested_Area/lag(Rice_Harvested_Area)-1),
    Rice_Production_Per_Capita=Rice_Production/Population
  )

malthus_boserup_coverage <- indonesia_malthus_boserup %>%
  summarise(Rows=n(),First_Year=min(Year),Last_Year=max(Year),
            Missing_Population=sum(is.na(Population)),
            Missing_Rice_Production=sum(is.na(Rice_Production)),
            Missing_Rice_Area=sum(is.na(Rice_Harvested_Area)),
            Missing_Rice_Yield=sum(is.na(Rice_Yield)),
            Missing_Production_Per_Capita=sum(is.na(Rice_Production_Per_Capita)))
print(malthus_boserup_coverage,row.names=FALSE)

malthus_boserup_long_run <- indonesia_malthus_boserup %>%
  filter(Year %in% c(1961,2023)) %>%
  select(Year,Population,Rice_Production,Rice_Harvested_Area,Rice_Yield,
         Rice_Production_Per_Capita)
print(malthus_boserup_long_run,row.names=FALSE)

malthus_boserup_change <- malthus_boserup_long_run %>%
  summarise(
    Population_Change_Percent=100*(Population[Year==2023]/Population[Year==1961]-1),
    Rice_Production_Change_Percent=100*(Rice_Production[Year==2023]/Rice_Production[Year==1961]-1),
    Rice_Area_Change_Percent=100*(Rice_Harvested_Area[Year==2023]/Rice_Harvested_Area[Year==1961]-1),
    Rice_Yield_Change_Percent=100*(Rice_Yield[Year==2023]/Rice_Yield[Year==1961]-1),
    Rice_Per_Capita_Change_Percent=100*(Rice_Production_Per_Capita[Year==2023]/
                                          Rice_Production_Per_Capita[Year==1961]-1)
  )
print(malthus_boserup_change %>% mutate(across(everything(),round,2)),row.names=FALSE)

malthus_indicator <- indonesia_malthus_boserup %>%
  select(Year,Population,Rice_Production,Rice_Production_Per_Capita) %>%
  mutate(Rice_Production_Per_Capita=round(Rice_Production_Per_Capita,8))

population_per_capita_correlation <- cor(
  indonesia_malthus_boserup$Population,
  indonesia_malthus_boserup$Rice_Production_Per_Capita,use="complete.obs"
)
cat("Correlation Population vs Rice Production/Capita:",
    round(population_per_capita_correlation,4),"\n")

boserup_data <- indonesia_malthus_boserup %>%
  select(Year,Population,Rice_Harvested_Area,Rice_Yield,Population_Growth,Rice_Yield_Growth)

population_yield_correlation <- cor(boserup_data$Population,boserup_data$Rice_Yield,use="complete.obs")
population_area_correlation <- cor(boserup_data$Population,boserup_data$Rice_Harvested_Area,use="complete.obs")
growth_yield_correlation <- cor(boserup_data$Population_Growth,boserup_data$Rice_Yield_Growth,use="complete.obs")

cat("Population vs Rice Yield:",round(population_yield_correlation,4),"\n",
    "Population vs Rice Area:",round(population_area_correlation,4),"\n",
    "Population Growth vs Yield Growth:",round(growth_yield_correlation,4),"\n")

growth_rate_correlations <- indonesia_malthus_boserup %>%
  select(Population_Growth,Rice_Production_Growth,Rice_Yield_Growth,Rice_Area_Growth) %>%
  cor(use="complete.obs")
print(round(growth_rate_correlations,4))

boserup_yield_model <- lm(log(Rice_Yield)~Year,data=indonesia_malthus_boserup)
boserup_yield_results <- tibble(
  Statistic=c("Annual log-yield trend","R-squared"),
  Estimate=c(coef(boserup_yield_model)[2],summary(boserup_yield_model)$r.squared)
)
print(boserup_yield_results %>% mutate(Estimate=round(Estimate,6)),row.names=FALSE)

malthus_boserup_key_years <- indonesia_malthus_boserup %>%
  filter(Year %in% c(1961,1970,1980,1990,2000,2010,2020,2023)) %>%
  select(Year,Population_Thousands,Population,Rice_Production,Rice_Harvested_Area,
         Rice_Yield,Rice_Production_Per_Capita) %>%
  mutate(
    Population_Thousands=round(Population_Thousands,2),
    Population=round(Population,0),
    Rice_Production=round(Rice_Production,2),
    Rice_Harvested_Area=round(Rice_Harvested_Area,2),
    Rice_Yield=round(Rice_Yield,2),
    Rice_Production_Per_Capita=round(Rice_Production_Per_Capita,6)
  )
print(malthus_boserup_key_years,row.names=FALSE)

# 7F. STATISTICAL ASSESSMENT

# 7F.1 GROWTH-RATE DATA
boserup_growth_data <- indonesia_malthus_boserup %>%
  select(Year,Population_Growth,Rice_Production_Growth,Rice_Yield_Growth,Rice_Area_Growth) %>%
  filter(complete.cases(.)) %>%
  arrange(Year)

stopifnot(nrow(boserup_growth_data)==62,min(boserup_growth_data$Year)==1962,
          max(boserup_growth_data$Year)==2023)

print(boserup_growth_data %>%
        summarise(across(-Year,list(Mean=mean,SD=sd))) %>%
        mutate(across(everything(),round,3)),row.names=FALSE)

growth_rate_correlation_matrix <- boserup_growth_data %>%
  select(-Year) %>% cor(use="complete.obs")
print(round(growth_rate_correlation_matrix,4))


# 7F.2 ASSOCIATIONAL MODELS
population_yield_growth_model <- lm(Rice_Yield_Growth~Population_Growth,data=boserup_growth_data)
population_yield_growth_summary <- summary(population_yield_growth_model)
population_yield_growth_results <- tibble(
  Statistic=c("Intercept","Population-growth coefficient","R-squared",
              "Adjusted R-squared","p-value for population-growth coefficient"),
  Estimate=c(coef(population_yield_growth_model),
             population_yield_growth_summary$r.squared,
             population_yield_growth_summary$adj.r.squared,
             coef(population_yield_growth_summary)[2,4])
)
print(population_yield_growth_results %>% mutate(Estimate=round(Estimate,6)),row.names=FALSE)

population_production_growth_model <- lm(Rice_Production_Growth~Population_Growth,data=boserup_growth_data)
population_production_growth_summary <- summary(population_production_growth_model)
population_production_growth_results <- tibble(
  Statistic=c("Intercept","Population-growth coefficient","R-squared",
              "Adjusted R-squared","p-value for population-growth coefficient"),
  Estimate=c(coef(population_production_growth_model),
             population_production_growth_summary$r.squared,
             population_production_growth_summary$adj.r.squared,
             coef(population_production_growth_summary)[2,4])
)
print(population_production_growth_results %>% mutate(Estimate=round(Estimate,6)),row.names=FALSE)


# 7F.3 DEMOGRAPHIC PHASES
boserup_phase_data <- boserup_growth_data %>%
  mutate(Transition_Phase=case_when(
    Year<=1979~"Early Transition",Year<=1999~"Fertility Decline",
    Year<=2014~"Late Transition",Year<=2023~"Low Fertility / Mature Transition"
  ))

boserup_phase_summary <- boserup_phase_data %>%
  group_by(Transition_Phase) %>%
  summarise(Years=n(),across(where(is.numeric),mean),.groups="drop") %>%
  mutate(across(where(is.numeric),round,3))

print(boserup_phase_summary,n=Inf,width=Inf,row.names=FALSE)

# 7F.4 STATISTICAL SUMMARY
malthus_boserup_statistical_summary <- tibble(
  Evidence=c("Population change, 1961–2023","Rice production change, 1961–2023",
             "Rice harvested-area change, 1961–2023","Rice yield change, 1961–2023",
             "Rice production per capita change, 1961–2023",
             "Correlation: population growth vs yield growth",
             "Correlation: population growth vs production growth",
             "Annual log-yield trend","Yield-trend R-squared"),
  Value=c(
    malthus_boserup_change$Population_Change_Percent,
    malthus_boserup_change$Rice_Production_Change_Percent,
    malthus_boserup_change$Rice_Area_Change_Percent,
    malthus_boserup_change$Rice_Yield_Change_Percent,
    malthus_boserup_change$Rice_Per_Capita_Change_Percent,
    cor(boserup_growth_data$Population_Growth,boserup_growth_data$Rice_Yield_Growth),
    cor(boserup_growth_data$Population_Growth,boserup_growth_data$Rice_Production_Growth),
    coef(boserup_yield_model)[2],summary(boserup_yield_model)$r.squared
  )
)
print(malthus_boserup_statistical_summary %>% mutate(Value=round(Value,4)),row.names=FALSE)


# 7F.5 INTERPRETIVE FLAGS
malthus_boserup_conclusion <- tibble(
  Criterion=c("Population increased substantially","Rice production increased faster than population",
              "Rice yield increased substantially","Harvested area increased less than production",
              "Rice production per capita increased"),
  Result=c(
    malthus_boserup_change$Population_Change_Percent>0,
    malthus_boserup_change$Rice_Production_Change_Percent>malthus_boserup_change$Population_Change_Percent,
    malthus_boserup_change$Rice_Yield_Change_Percent>0,
    malthus_boserup_change$Rice_Area_Change_Percent<malthus_boserup_change$Rice_Production_Change_Percent,
    malthus_boserup_change$Rice_Per_Capita_Change_Percent>0
  )
)
print(malthus_boserup_conclusion,row.names=FALSE)


# 7G. TIME-SERIES ROBUSTNESS
time_series_data <- boserup_growth_data %>% arrange(Year)
stopifnot(nrow(time_series_data)==62,min(time_series_data$Year)==1962,
          max(time_series_data$Year)==2023,
          all(is.finite(time_series_data$Population_Growth)),
          all(is.finite(time_series_data$Rice_Yield_Growth)))

plot(time_series_data$Year,time_series_data$Population_Growth,type="l",
     xlab="Year",ylab="Annual growth rate (%)",main="Indonesia: Population Growth")
plot(time_series_data$Year,time_series_data$Rice_Yield_Growth,type="l",
     xlab="Year",ylab="Annual growth rate (%)",main="Indonesia: Rice Yield Growth")

population_acf <- acf(time_series_data$Population_Growth,plot=FALSE)
yield_acf <- acf(time_series_data$Rice_Yield_Growth,plot=FALSE)
cat("Population-growth ACF:",round(population_acf$acf[1:6],4),"\n",
    "Rice-yield-growth ACF:",round(yield_acf$acf[1:6],4),"\n")
acf(time_series_data$Population_Growth,main="ACF: Indonesia Population Growth")
acf(time_series_data$Rice_Yield_Growth,main="ACF: Indonesia Rice Yield Growth")

if(!requireNamespace("lmtest",quietly=TRUE)) install.packages("lmtest")
if(!requireNamespace("sandwich",quietly=TRUE)) install.packages("sandwich")
if(!requireNamespace("zoo",quietly=TRUE)) install.packages("zoo")
library(lmtest);library(sandwich);library(zoo)

dw_test_yield <- dwtest(population_yield_growth_model)
bg_test_yield <- bgtest(population_yield_growth_model,order=2)
bp_test_yield <- bptest(population_yield_growth_model)
print(dw_test_yield);print(bg_test_yield);print(bp_test_yield)

robust_yield_coefficient <- lmtest::coeftest(
  population_yield_growth_model,
  vcov=sandwich::vcovHC(population_yield_growth_model,type="HC1")
)
print(robust_yield_coefficient)


# 7G.1 LAGGED ASSOCIATION
lagged_data <- time_series_data %>%
  mutate(Lagged_Population_Growth=lag(Population_Growth)) %>%
  filter(!is.na(Lagged_Population_Growth))

lagged_population_yield_model <- lm(Rice_Yield_Growth~Lagged_Population_Growth,data=lagged_data)
lagged_model_summary <- summary(lagged_population_yield_model)
lagged_population_yield_results <- tibble(
  Statistic=c("Intercept","Lagged population-growth coefficient","R-squared",
              "Adjusted R-squared","p-value for lagged population-growth coefficient"),
  Estimate=c(coef(lagged_population_yield_model),
             lagged_model_summary$r.squared,lagged_model_summary$adj.r.squared,
             coef(lagged_model_summary)[2,4])
)
print(lagged_population_yield_results %>% mutate(Estimate=round(Estimate,6)),row.names=FALSE)


# 7G.2 RELATIVE FOOD GROWTH
relative_food_growth <- time_series_data %>%
  mutate(Rice_Production_Growth_Minus_Population_Growth=
           Rice_Production_Growth-Population_Growth)

relative_food_growth_summary <- relative_food_growth %>%
  summarise(Mean_Excess_Rice_Production_Growth=mean(Rice_Production_Growth_Minus_Population_Growth),
            Median_Excess_Rice_Production_Growth=median(Rice_Production_Growth_Minus_Population_Growth),
            Years_Production_Growth_Exceeded_Population=
              sum(Rice_Production_Growth_Minus_Population_Growth>0),Total_Years=n())
print(relative_food_growth_summary %>% mutate(across(everything(),round,4)),row.names=FALSE)


# 7G.3 ROLLING ASSOCIATION
rolling_correlation <- zoo::rollapply(
  time_series_data[,c("Population_Growth","Rice_Yield_Growth")],
  width=10,FUN=\(x) cor(x[,1],x[,2],use="complete.obs"),
  by.column=FALSE,align="right",fill=NA
)
rolling_correlation_data <- tibble(
  Year=time_series_data$Year,
  Rolling_10Y_Correlation=as.numeric(rolling_correlation)
)
rolling_summary <- rolling_correlation_data %>%
  filter(!is.na(Rolling_10Y_Correlation)) %>%
  summarise(Minimum=min(Rolling_10Y_Correlation),
            Maximum=max(Rolling_10Y_Correlation),
            Mean=mean(Rolling_10Y_Correlation))
print(rolling_summary %>% mutate(across(everything(),round,4)),row.names=FALSE)

plot(rolling_correlation_data$Year,rolling_correlation_data$Rolling_10Y_Correlation,type="l",
     xlab="Year",ylab="10-year rolling correlation",
     main="Population Growth vs Rice Yield Growth")
abline(h=0,lty=2)


# 7G.4 FINAL ROBUSTNESS SUMMARY
time_series_diagnostics_summary <- tibble(
  Diagnostic=c("Annual population-growth vs yield-growth correlation",
               "Contemporaneous regression coefficient","Contemporaneous regression p-value",
               "Contemporaneous regression R-squared","Lagged population-growth coefficient",
               "Lagged regression p-value","Lagged regression R-squared",
               "Durbin-Watson statistic","Breusch-Godfrey p-value","Breusch-Pagan p-value",
               "Minimum 10-year rolling correlation","Maximum 10-year rolling correlation",
               "Mean 10-year rolling correlation"),
  Value=c(
    cor(time_series_data$Population_Growth,time_series_data$Rice_Yield_Growth),
    coef(population_yield_growth_model)[2],coef(population_yield_growth_summary)[2,4],
    population_yield_growth_summary$r.squared,coef(lagged_population_yield_model)[2],
    coef(lagged_model_summary)[2,4],lagged_model_summary$r.squared,
    as.numeric(dw_test_yield$statistic),bg_test_yield$p.value,bp_test_yield$p.value,
    min(rolling_correlation_data$Rolling_10Y_Correlation,na.rm=TRUE),
    max(rolling_correlation_data$Rolling_10Y_Correlation,na.rm=TRUE),
    mean(rolling_correlation_data$Rolling_10Y_Correlation,na.rm=TRUE)
  )
)
print(time_series_diagnostics_summary %>% mutate(Value=round(Value,6)),row.names=FALSE)

# 7H. GRAPHICAL SYNTHESIS
dtt_graph_data <- indonesia_malthus_boserup %>%
  arrange(Year) %>%
  mutate(Transition_Phase=case_when(
    Year<=1979~"Early Transition",Year<=1999~"Fertility Decline",
    Year<=2014~"Late Transition",Year<=2023~"Low Fertility / Mature Transition"
  ))
stopifnot(nrow(dtt_graph_data)==63,min(dtt_graph_data$Year)==1961,
          max(dtt_graph_data$Year)==2023,
          all(is.finite(dtt_graph_data$Population)),
          all(is.finite(dtt_graph_data$Rice_Production)),
          all(is.finite(dtt_graph_data$Rice_Yield)),
          all(is.finite(dtt_graph_data$Rice_Harvested_Area)),
          all(is.finite(dtt_graph_data$Rice_Production_Per_Capita)))

# 7H.1 INDEXED LEVELS
idx <- dtt_graph_data %>%
  transmute(Year,
            Population=100*Population/Population[Year==1961],
            Production=100*Rice_Production/Rice_Production[Year==1961],
            Area=100*Rice_Harvested_Area/Rice_Harvested_Area[Year==1961],
            Yield=100*Rice_Yield/Rice_Yield[Year==1961],
            Per_Capita=100*Rice_Production_Per_Capita/
              Rice_Production_Per_Capita[Year==1961])

plot(idx$Year,idx$Population,type="l",lwd=2,ylim=c(80,500),xlab="Year",
     ylab="Index (1961 = 100)",main="Indonesia: Population and Rice Production")
lines(idx$Year,idx$Production,lwd=2,lty=2)
legend("topleft",c("Population","Rice production"),lty=1:2,lwd=2,bty="n")

plot(idx$Year,idx$Population,type="l",lwd=2,xlab="Year",
     ylab="Index (1961 = 100)",main="Indonesia: Population and Rice Yield")
lines(idx$Year,idx$Yield,lwd=2,lty=2)
legend("topleft",c("Population","Rice yield"),lty=1:2,lwd=2,bty="n")

plot(idx$Year,idx$Area,type="l",lwd=2,ylim=c(80,500),xlab="Year",
     ylab="Index (1961 = 100)",main="Indonesia: Rice Area and Production")
lines(idx$Year,idx$Production,lwd=2,lty=2)
legend("topleft",c("Harvested area","Rice production"),lty=1:2,lwd=2,bty="n")

# 7H.2 PRODUCTION PER CAPITA
plot(dtt_graph_data$Year,dtt_graph_data$Rice_Production_Per_Capita,type="l",
     lwd=2,xlab="Year",ylab="Tonnes of rice per person",
     main="Indonesia: Rice Production per Capita")
abline(h=dtt_graph_data$Rice_Production_Per_Capita[
  dtt_graph_data$Year==1961],lty=2)

# 7H.3 GROWTH ASSOCIATIONS
plot(boserup_growth_data$Population_Growth,boserup_growth_data$Rice_Yield_Growth,
     pch=19,xlab="Population growth (%)",ylab="Rice yield growth (%)",
     main="Population Growth vs Rice Yield Growth")
abline(population_yield_growth_model,lwd=2);abline(h=0,v=0,lty=2)

plot(boserup_growth_data$Population_Growth,boserup_growth_data$Rice_Production_Growth,
     pch=19,xlab="Population growth (%)",ylab="Rice production growth (%)",
     main="Population Growth vs Rice Production Growth")
abline(population_production_growth_model,lwd=2);abline(h=0,v=0,lty=2)

# 7H.4 DEMOGRAPHIC PHASES
plot(dtt_graph_data$Year,dtt_graph_data$Rice_Yield,type="l",lwd=2,
     xlab="Year",ylab="Rice yield (kg/ha)",
     main="Rice Yield Across Demographic-Transition Phases")
abline(v=c(1979,1999,2014),lty=2)
text(c(1970,1989,2007,2019),max(dtt_graph_data$Rice_Yield)*.95,
     c("Early","Fertility Decline","Late Transition","Mature Transition"),cex=.75)

# 7H.5 LONG-RUN INDEX SUMMARY
dtt_index_summary <- idx %>%
  filter(Year==2023) %>%
  rename(Population_Index=Population,Rice_Production_Index=Production,
         Rice_Area_Index=Area,Rice_Yield_Index=Yield,
         Rice_Per_Capita_Index=Per_Capita)
print(dtt_index_summary %>% mutate(across(where(is.numeric),round,2)),
      row.names=FALSE)

# 7H.6 FINAL SYNTHESIS
dtt_final_synthesis <- tibble(
  Indicator=c("Population index, 2023 (1961 = 100)",
              "Rice production index, 2023 (1961 = 100)",
              "Rice harvested-area index, 2023 (1961 = 100)",
              "Rice yield index, 2023 (1961 = 100)",
              "Rice production-per-capita index, 2023 (1961 = 100)",
              "Population growth vs yield-growth correlation",
              "Lagged population-growth vs yield-growth p-value",
              "Minimum 10-year rolling correlation",
              "Maximum 10-year rolling correlation"),
  Value=c(idx$Population[idx$Year==2023],idx$Production[idx$Year==2023],
          idx$Area[idx$Year==2023],idx$Yield[idx$Year==2023],
          idx$Per_Capita[idx$Year==2023],
          cor(boserup_growth_data$Population_Growth,boserup_growth_data$Rice_Yield_Growth),
          lagged_model_summary$coefficients[2,4],
          min(rolling_correlation_data$Rolling_10Y_Correlation,na.rm=TRUE),
          max(rolling_correlation_data$Rolling_10Y_Correlation,na.rm=TRUE)))
print(dtt_final_synthesis %>% mutate(Value=round(Value,4)),row.names=FALSE)


# 7I. ROBUST INFERENCE AND SUB-PERIOD ANALYSIS
if (!requireNamespace("sandwich",quietly=TRUE)) install.packages("sandwich")
if (!requireNamespace("lmtest",quietly=TRUE)) install.packages("lmtest")

newey_west_yield <- lmtest::coeftest(
  population_yield_growth_model,
  vcov=sandwich::NeweyWest(population_yield_growth_model,lag=2,
                           prewhite=FALSE,adjust=TRUE))
newey_west_lagged_yield <- lmtest::coeftest(
  lagged_population_yield_model,
  vcov=sandwich::NeweyWest(lagged_population_yield_model,lag=2,
                           prewhite=FALSE,adjust=TRUE))
print(newey_west_yield);print(newey_west_lagged_yield)

# 7I.1 HAC RESULTS
hac_yield_results <- tibble(
  Statistic=c("Population-growth coefficient","HAC standard error",
              "HAC t-statistic","HAC p-value"),
  Estimate=unname(newey_west_yield["Population_Growth",
                                   c("Estimate","Std. Error","t value","Pr(>|t|)")]))
hac_lagged_yield_results <- tibble(
  Statistic=c("Lagged population-growth coefficient","HAC standard error",
              "HAC t-statistic","HAC p-value"),
  Estimate=unname(newey_west_lagged_yield["Lagged_Population_Growth",
                                          c("Estimate","Std. Error","t value","Pr(>|t|)")]))
print(hac_yield_results %>% mutate(Estimate=round(Estimate,6)),row.names=FALSE)
print(hac_lagged_yield_results %>% mutate(Estimate=round(Estimate,6)),row.names=FALSE)

# 7I.2 SUB-PERIOD CORRELATIONS
subperiod_correlations <- boserup_phase_data %>%
  group_by(Transition_Phase) %>%
  summarise(Years=n(),
            Population_Yield_Growth_Correlation=
              cor(Population_Growth,Rice_Yield_Growth,use="complete.obs"),
            Population_Production_Growth_Correlation=
              cor(Population_Growth,Rice_Production_Growth,use="complete.obs"),
            Mean_Population_Growth=mean(Population_Growth),
            Mean_Yield_Growth=mean(Rice_Yield_Growth),
            Mean_Production_Growth=mean(Rice_Production_Growth),.groups="drop")
print(subperiod_correlations %>% mutate(across(where(is.numeric),round,4)),
      row.names=FALSE)

# 7I.3 SUB-PERIOD REGRESSIONS
subperiod_models <- boserup_phase_data %>%
  group_by(Transition_Phase) %>%
  group_modify(~{
    m <- lm(Rice_Yield_Growth~Population_Growth,data=.x);s <- summary(m)
    tibble(Years=nrow(.x),Coefficient=coef(m)[2],
           R_squared=s$r.squared,P_value=coef(s)[2,4])
  }) %>% ungroup()
print(subperiod_models %>% mutate(across(where(is.numeric),round,6)),
      row.names=FALSE)

# 7I.4 PRODUCTION PER CAPITA PEAK
rice_per_capita_peak <- dtt_graph_data %>%
  slice_max(Rice_Production_Per_Capita,n=1,with_ties=FALSE) %>%
  select(Year,Rice_Production_Per_Capita)
print(rice_per_capita_peak,row.names=FALSE)

rice_per_capita_peak_comparison <- dtt_graph_data %>%
  summarise(Peak_Rice_Per_Capita=max(Rice_Production_Per_Capita),
            Peak_Year=Year[which.max(Rice_Production_Per_Capita)],
            Rice_Per_Capita_2023=Rice_Production_Per_Capita[Year==2023],
            Decline_From_Peak_To_2023_Percent=
              100*(Rice_Production_Per_Capita[Year==2023]/
                     max(Rice_Production_Per_Capita)-1))
print(rice_per_capita_peak_comparison %>%
        mutate(across(where(is.numeric),round,6)),row.names=FALSE)

# 7I.5 PRE- AND POST-PEAK COMPARISON
peak_phase_comparison <- dtt_graph_data %>%
  mutate(Period=if_else(Year<=1995,"1961–1995","1996–2023")) %>%
  group_by(Period) %>%
  summarise(Years=n(),
            Mean_Population_Growth=mean(Population_Growth,na.rm=TRUE),
            Mean_Rice_Production_Growth=mean(Rice_Production_Growth,na.rm=TRUE),
            Mean_Rice_Yield_Growth=mean(Rice_Yield_Growth,na.rm=TRUE),
            Mean_Rice_Area_Growth=mean(Rice_Area_Growth,na.rm=TRUE),
            Mean_Rice_Production_Per_Capita=
              mean(Rice_Production_Per_Capita,na.rm=TRUE),.groups="drop")
print(peak_phase_comparison %>% mutate(across(where(is.numeric),round,4)),
      row.names=FALSE)
# 7I.6 ANOVA: YIELD GROWTH ACROSS DEMOGRAPHIC PHASES

phase_yield_anova_model <- aov(
  Rice_Yield_Growth ~ Transition_Phase,
  data = boserup_phase_data
)

anova_table <- summary(phase_yield_anova_model)[[1]]

print(anova_table)


# 7I.7 ROBUSTNESS TABLE

robustness_summary <- tibble(
  Diagnostic=c(
    "Conventional population-growth → yield-growth p-value",
    "Newey-West HAC population-growth → yield-growth p-value",
    "Conventional lagged population-growth → yield-growth p-value",
    "Newey-West HAC lagged population-growth → yield-growth p-value",
    "Population-growth → yield-growth correlation",
    "Minimum 10-year rolling correlation",
    "Maximum 10-year rolling correlation",
    "Phase ANOVA p-value",
    "Rice production-per-capita change, 1961–2023",
    "Rice production-per-capita decline from peak to 2023"
  ),
  Value=c(
    population_yield_growth_summary$coefficients[2,4],
    newey_west_yield["Population_Growth","Pr(>|t|)"],
    lagged_model_summary$coefficients[2,4],
    newey_west_lagged_yield["Lagged_Population_Growth","Pr(>|t|)"],
    cor(
      boserup_growth_data$Population_Growth,
      boserup_growth_data$Rice_Yield_Growth,
      use="complete.obs"
    ),
    min(
      rolling_correlation_data$Rolling_10Y_Correlation,
      na.rm=TRUE
    ),
    max(
      rolling_correlation_data$Rolling_10Y_Correlation,
      na.rm=TRUE
    ),
    anova_table$`Pr(>F)`[1],
    malthus_boserup_change$Rice_Per_Capita_Change_Percent,
    rice_per_capita_peak_comparison$Decline_From_Peak_To_2023_Percent
  )
)

print(
  robustness_summary %>%
    mutate(Value=round(Value,6)),
  row.names=FALSE
)

# 7I.8 DESCRIPTIVE FLAGS

robustness_flags <- tibble(
  Criterion=c(
    "Population increased substantially",
    "Rice production increased faster than population",
    "Rice yield increased substantially",
    "Rice harvested area increased substantially less than production",
    "Rice production per capita increased over the full period",
    "Contemporaneous population-growth/yield-growth association remains significant under HAC SE",
    "Lagged population-growth/yield-growth association remains significant under HAC SE",
    "Population-growth/yield-growth correlation remains positive across all 10-year rolling windows",
    "Mean yield growth differs significantly across demographic phases"
  ),
  Result=c(
    malthus_boserup_change$Population_Change_Percent>0,
    
    malthus_boserup_change$Rice_Production_Change_Percent>
      malthus_boserup_change$Population_Change_Percent,
    
    malthus_boserup_change$Rice_Yield_Change_Percent>0,
    
    malthus_boserup_change$Rice_Area_Change_Percent<
      malthus_boserup_change$Rice_Production_Change_Percent,
    
    malthus_boserup_change$Rice_Per_Capita_Change_Percent>0,
    
    newey_west_yield["Population_Growth","Pr(>|t|)"]<.05,
    
    newey_west_lagged_yield["Lagged_Population_Growth","Pr(>|t|)"]<.05,
    
    min(
      rolling_correlation_data$Rolling_10Y_Correlation,
      na.rm=TRUE
    )>0,
    
    anova_table$`Pr(>F)`[1]<.05
  )
)

print(
  robustness_flags,
  row.names=FALSE
)


# 7J. FINAL EMPIRICAL SYNTHESIS
final_evidence_table <- tibble(
  Evidence=c("Population change, 1961–2023","Rice production change, 1961–2023",
             "Rice harvested-area change, 1961–2023","Rice yield change, 1961–2023",
             "Rice production-per-capita change, 1961–2023","Peak rice production per capita",
             "Decline from peak to 2023","Population-growth vs yield-growth correlation",
             "HAC p-value: population growth → yield growth",
             "HAC p-value: lagged population growth → yield growth",
             "Minimum 10-year rolling correlation","Maximum 10-year rolling correlation",
             "Phase ANOVA p-value"),
  Value=c(malthus_boserup_change$Population_Change_Percent,
          malthus_boserup_change$Rice_Production_Change_Percent,
          malthus_boserup_change$Rice_Area_Change_Percent,
          malthus_boserup_change$Rice_Yield_Change_Percent,
          malthus_boserup_change$Rice_Per_Capita_Change_Percent,
          rice_per_capita_peak_comparison$Peak_Rice_Per_Capita,
          rice_per_capita_peak_comparison$Decline_From_Peak_To_2023_Percent,
          cor(boserup_growth_data$Population_Growth,boserup_growth_data$Rice_Yield_Growth,
              use="complete.obs"),
          newey_west_yield["Population_Growth","Pr(>|t|)"],
          newey_west_lagged_yield["Lagged_Population_Growth","Pr(>|t|)"],
          min(rolling_correlation_data$Rolling_10Y_Correlation,na.rm=TRUE),
          max(rolling_correlation_data$Rolling_10Y_Correlation,na.rm=TRUE),
          anova_table$`Pr(>F)`[1]))
print(final_evidence_table %>% mutate(Value=round(Value,4)),row.names=FALSE)

malthusian_evidence <- tibble(
  Criterion=c("Population increased substantially",
              "Food production failed to keep pace with population",
              "Rice production per capita declined over the full period",
              "Rice production per capita remained below its 1961 level",
              "Rice production per capita declined after its historical peak"),
  Result=c(
    malthus_boserup_change$Population_Change_Percent>0,
    malthus_boserup_change$Rice_Production_Change_Percent<=
      malthus_boserup_change$Population_Change_Percent,
    malthus_boserup_change$Rice_Per_Capita_Change_Percent<0,
    dtt_graph_data$Rice_Production_Per_Capita[dtt_graph_data$Year==2023]<
      dtt_graph_data$Rice_Production_Per_Capita[dtt_graph_data$Year==1961],
    rice_per_capita_peak_comparison$Decline_From_Peak_To_2023_Percent<0))
print(malthusian_evidence,row.names=FALSE)

boserupian_evidence <- tibble(
  Criterion=c("Rice production increased substantially","Rice yield increased substantially",
              "Rice production increased faster than harvested area",
              "Rice production increased faster than population",
              "Population-growth / yield-growth association is positive",
              "Association survives HAC standard errors",
              "Lagged association survives HAC standard errors",
              "Yield productivity has a positive long-run trend"),
  Result=c(
    malthus_boserup_change$Rice_Production_Change_Percent>0,
    malthus_boserup_change$Rice_Yield_Change_Percent>0,
    malthus_boserup_change$Rice_Production_Change_Percent>
      malthus_boserup_change$Rice_Area_Change_Percent,
    malthus_boserup_change$Rice_Production_Change_Percent>
      malthus_boserup_change$Population_Change_Percent,
    cor(boserup_growth_data$Population_Growth,boserup_growth_data$Rice_Yield_Growth)>0,
    newey_west_yield["Population_Growth","Pr(>|t|)"]<.05,
    newey_west_lagged_yield["Lagged_Population_Growth","Pr(>|t|)"]<.05,
    coef(boserup_yield_model)[2]>0))
print(boserupian_evidence,row.names=FALSE)

robustness_evidence <- tibble(
  Criterion=c(
    "Rolling association remains positive in every window",
    "Yield growth differs significantly across demographic phases",
    "Population-growth / yield-growth coefficient is positive in every phase",
    "Population-growth / yield-growth association is temporally stable",
    "Evidence establishes causality"
  ),
  Result=c(
    min(
      rolling_correlation_data$Rolling_10Y_Correlation,
      na.rm=TRUE
    )>0,
    
    anova_table$`Pr(>F)`[1]<.05,
    
    all(subperiod_models$Coefficient>0),
    
    FALSE,
    
    FALSE
  )
)

print(
  robustness_evidence,
  row.names=FALSE
)

overall_classification <- tibble(
  Dimension=c("Long-run agricultural productivity",
              "Long-run food production relative to population",
              "Population-growth / productivity association",
              "Temporal stability of the association",
              "Evidence for simple Malthusian deterioration",
              "Evidence consistent with Boserupian intensification",
              "Causal identification"),
  Assessment=c("Strong positive evidence","Positive evidence",
               "Moderate positive association","Weak / unstable",
               "Not supported over the full period","Supported descriptively",
               "Not established"))
print(overall_classification,row.names=FALSE)

# 7K Dependency ratios
age_groups <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
                "75-79","80-84","85-89","90-94","95-99","100+")
stopifnot(all(age_groups %in% names(population_raw)))

world_dependency <- population_raw %>%
  filter(`Region, subregion, country or area *`=="World",Year %in% c(1952,2023)) %>%
  mutate(across(all_of(age_groups),as.numeric),
         Child_Dependency_Ratio=100*(`0-4`+`5-9`+`10-14`)/
           (`15-19`+`20-24`+`25-29`+`30-34`+`35-39`+`40-44`+`45-49`+`50-54`+`55-59`+`60-64`),
         Old_Age_Dependency_Ratio=100*(`65-69`+`70-74`+`75-79`+`80-84`+`85-89`+`90-94`+`95-99`+`100+`)/
           (`15-19`+`20-24`+`25-29`+`30-34`+`35-39`+`40-44`+`45-49`+`50-54`+`55-59`+`60-64`),
         Total_Dependency_Ratio=Child_Dependency_Ratio+Old_Age_Dependency_Ratio) %>%
  select(Year,Child_Dependency_Ratio,Old_Age_Dependency_Ratio,Total_Dependency_Ratio)

dependency_comparison <- bind_rows(
  indonesia_demographic_dividend %>% filter(Year %in% c(1952,2023)) %>%
    select(Year,Child_Dependency_Ratio,Old_Age_Dependency_Ratio,Total_Dependency_Ratio) %>%
    mutate(Country="Indonesia"),
  world_dependency %>% mutate(Year=as.numeric(Year),Country="World")) %>%
  select(Country,Year,everything())

dependency_final_summary <- dependency_comparison %>%
  pivot_longer(-c(Country,Year),names_to="Indicator",values_to="Value") %>%
  pivot_wider(names_from=Year,values_from=Value,names_prefix="Year_") %>%
  mutate(Change=Year_2023-Year_1952)

dependency_final_summary %>% mutate(across(where(is.numeric),~round(.x,2))) %>% print(n=Inf)
