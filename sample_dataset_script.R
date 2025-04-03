source("sample_dataset_driver.R")
library(dplyr)
library(haven)

# *************************************
# Design Variables
# *************************************

# Set seed to ensure reproducibility
set.seed(123)

# Define number of sampling points
# ... target n=8/sampling point for total n=1,200
n_points <- 150

# Stratification variables
# ... Create regional stratification variable. n initially set to 150 sampling points at n=8.
# ...  1 = Capital, 2 = North, 3 = South, 4 = East, 5 = West
strata <- c(
  rep(1, round(n_points*0.30)),
  rep(2, round(n_points*0.15)),
  rep(3, round(n_points*0.25)),
  rep(4, round(n_points*0.20)),
  rep(5, round(n_points*0.10))
)

sample_dataset <- data.frame(strata)

n_points = NULL

# Add Urbanicity variable
# ... Urban = 1, Rural = 2
sample_dataset <- sample_dataset %>%
  left_join(tibble(
    strata = 1:5,
    urban_p = c(1, 0.55, 0.65, 0.25, 0.15)
  ), by = "strata") %>%
  group_by(strata) %>%
  mutate(
    n = n(),
    n_urban = round(n * urban_p),
    n_rural = n - n_urban,
    urbanicity = sample(c(rep(1, n_urban[1]), rep(2, n_rural[1])))
  ) %>%
  ungroup() %>%
  select(-urban_p, -n, -n_urban, -n_rural)



table(sample_dataset$strata, sample_dataset$urbanicity) %>%
  prop.table(margin = 1)

# Add 'sampling_point' variable
sample_dataset <- sample_dataset %>%
  mutate(
    sampling_point = c(1:150)
  )


# Simulate enumerator ID variable
# ... This is a pretty simple distribution. 25 enumerators are allocated proportionally across the 5 regional strata, and each sampling point has 1 enumerator.
enumerator_table <- tibble(
  enumerator = 1:25,
  strata = c(rep(1,7), rep(2,4), rep(3,6), rep(4,5), rep(5,3))
)


sample_dataset <- sample_dataset %>%
  group_by(strata) %>%
  mutate(
    # Number of sampling points in this stratum
    n_points = n(),
    # Enumerators available for this stratum
    enumerators = list(enumerator_table$enumerator[enumerator_table$strata == unique(strata)]),
    # Assign enumerators fairly, ensuring no enumerator gets more than 8 sampling points
    enumerator_id = rep(sample(enumerators[[1]]), length.out = n_points) %>%
      sample()  # optional shuffle
  ) %>%
  ungroup() %>%
  select(-n_points, -enumerators)


enumerator_table = NULL


# Expand to n=1200. Each row is replicated 8 times to simulate 8 interviews per sampling point.
sample_dataset <- sample_dataset[rep(1:nrow(sample_dataset), each = 8),]


# Add interview ID variable
sample_dataset <- sample_dataset %>%
  mutate(
    interview_id = c(1:nrow(sample_dataset))
  )

sample_dataset <- as.data.frame(sample_dataset)

# *************************************
# Respondent Demographics
# *************************************

# Simulate gender
strata_means <- c(1.53,1.51,1.50,1.49,1.48)
urbanicity_means <- c(1.51,1.49)
strata_sd <- c(0.3,0.3,0.3,0.3,0.3)
urbanicity_sd <- c(0.3,0.3)

sample_dataset$gender <- generate_data_2factor(sample_dataset, "strata", "urbanicity", c(1,2), strata_means, urbanicity_means, strata_sd, urbanicity_sd)

table(sample_dataset$strata, sample_dataset$gender) %>%
  prop.table(margin = 1)


# Simulate age

# should be referencing nrow here? Or just 1?
sample_dataset$age <- ifelse(sample_dataset$urbanicity==1,
                         sample(c(18:90), nrow(sample_dataset[which(sample_dataset$urbanicity==1),]), replace = TRUE, prob = dnorm(c(18:90), mean = 39, sd = 14)),
                         sample(c(18:90), nrow(sample_dataset[which(sample_dataset$urbanicity==2),]), replace = TRUE, prob = dnorm(c(18:90), mean = 41, sd = 15))
                         )

sample_dataset$age_brackets <- case_when(
  sample_dataset$age >=18 & sample_dataset$age <= 24 ~ 1,
  sample_dataset$age >=25 & sample_dataset$age <= 34 ~ 2,
  sample_dataset$age >=35 & sample_dataset$age <= 44 ~ 3,
  sample_dataset$age >=45 & sample_dataset$age <= 54 ~ 4,
  sample_dataset$age >=55 ~ 5
)


table(sample_dataset$urbanicity, sample_dataset$age_brackets) %>%
  prop.table(margin = 1)


table(sample_dataset$strata, sample_dataset$age_brackets) %>%
  prop.table(margin = 1)

table(sample_dataset$gender, sample_dataset$age_brackets) %>%
  prop.table(margin = 1)


# Education
# ... 1 = Primary or less
# ... 2 = Secondary
# ... 3 = Vocational/technical
# ... 4 = University or higher

strata_means <- c(3.5,2.5,2.5,2.4,2.3)
urbanicity_means <- c(3,2)
strata_sd <- c(0.7,0.8,0.8,0.8,0.8)
urbanicity_sd <- c(0.7,0.7)

sample_dataset$education <- generate_data_2factor(sample_dataset, "strata","urbanicity",c(1:4),strata_means,urbanicity_means,strata_sd,urbanicity_sd)

sample_dataset$education <- generate_data_2factor(sample_dataset, "strata", "urbanicity", c(1,2,3,4),
                                                  c(3.6,3.2,3.0,2.7,2.6), c(3.4,2.3),
                                                  c(0.8,0.8,0.8,0.8,0.8), c(0.8,1))


table(sample_dataset$strata, sample_dataset$education) %>%
  prop.table(margin = 1)

table(sample_dataset$urbanicity, sample_dataset$education) %>%
  prop.table(margin = 1)

# Marital status
# ... 1 = Single/Never married
# ... 2 = Married
# ... 3 = Divorced/separated
# ... 4 = Widowed

sample_dataset$marital_status <- sapply(sample_dataset$age_brackets, function(age_brackets) {
  probs <- case_when(
    age_brackets == 1 ~ c(0.80,0.15,0.04,0.01),
    age_brackets == 2 ~ c(0.40,0.50,0.08,0.02),
    age_brackets == 3 ~ c(0.20,0.60,0.16,0.04),
    age_brackets == 4 ~ c(0.13,0.63,0.17,0.07),
    age_brackets == 5 ~ c(0.07,0.63,0.18,0.12)
  )
  sample(1:4, 1, prob = probs, replace = TRUE)
})

# Occupational status
# ... 1 = Employed in a paid job, full-time
# ... 2 = Employed in a paid job, part-time
# ... 3 = Self-employed, full-time
# ... 4 = Self-employed, part-time
# ... 5 = Homemaker
# ... 6 = Retired
# ... 7 = Student or pupil
# ... 8 = Unemployed and looking for a job
# ... 9 = Unemployed and not looking for a job
# ... 10 = OTHER
# ... 


# HH_Income
# ... 1 = 50k or less
# ... 2 = >50-75k
# ... 3 = >75-100k
# ... 4 = >100-150k
# ... 5 = >150-200k
# ... 6 = >200k 

sample_dataset$hh_income <- generate_data_3factor(sample_dataset, "strata", "urbanicity", "education", c(1,2,3,4,5,6),
                                                  c(5.0,4.8,4.3,4.2,3.0), c(4.0,2.0), c(1.0,2.5,3.7,4.5),
                                                  c(0.8,0.8,0.8,0.8,0.8), c(0.9,0.9), c(0.6,0.6,0.6,0.6))

table(sample_dataset$strata, sample_dataset$hh_income) %>%
  prop.table(margin = 1)

table(sample_dataset$urbanicity, sample_dataset$hh_income) %>%
  prop.table(margin = 1)

table(sample_dataset$education, sample_dataset$hh_income) %>%
  prop.table(margin = 1)


# *************************************
# Non-demographic variables
# *************************************

# q1_direction
sample_dataset$q1_direction <- generate_data_1factor(sample_dataset, "strata", c(1,2,3,4), 
                                                     c(1.0,1.5,4.0,3.2,3.2), c(0.8,0.8,0.7,1.0,1.0))

table(sample_dataset$strata, sample_dataset$q1_direction) %>%
  prop.table(margin = 1)



# q2_econ_hh 
sample_dataset$q2_econ_hh <- generate_data_1factor(sample_dataset, "strata", c(1,2,3,4),
                                                   c(2.5,2.7,3.3,2.8,2.8), c(0.9,0.9,0.8,1,1))

table(sample_dataset$strata, sample_dataset$q2_econ_hh) %>%
  prop.table(margin = 1)


# q3_econ_hh_past12
sample_dataset$q3_econ_hh_past12 <- generate_data_1factor(sample_dataset, "strata", c(1,2,3,4,5),
                                                          c(2.9,3.0,4,3.5,3.5), c(0.7,0.7,0.7,1,1))

table(sample_dataset$strata, sample_dataset$q3_econ_hh_past12) %>%
  prop.table(margin = 1)


# q4_prices_past12
sample_dataset$q4_prices_past12 <- generate_data_1factor(sample_dataset, "strata", c(1,2,3,4,5),
                                                         c(2.9,2.9,1.8,2.2,2.3), c(0.8,0.8,0.7,0.8,0.8))

table(sample_dataset$strata, sample_dataset$q4_prices_past12) %>%
  prop.table(margin = 1)

# q5_econ_next12
sample_dataset$q5_econ_next12 <- generate_data_1factor(sample_dataset, "strata", c(1,2,3,4,5),
                                                       c(1.9,2.2,4,3.7,3.8), c(1,1,0.7,0.8,0.8))

table(sample_dataset$strata, sample_dataset$q5_econ_next12) %>%
  prop.table(margin = 1)






# q7_debt
sample_dataset$q7_debt <- generate_data_1factor(sample_dataset, "strata", c(1,2,3),
                                                c(2.0,1.9,1.2,2,2), c(0.8,0.8,0.7,1,1))

table(sample_dataset$strata, sample_dataset$q7_debt) %>%
  prop.table(margin = 1)

# q7_economy
sample_dataset$q7_economy <- generate_data_1factor(sample_dataset, "q5_econ_next12", c(1,2,3),
                                                   c(1.9,1.8,1.5,1.2,1.0), c(0.7,0.7,0.7,0.4,0.4))


table(sample_dataset$q5_econ_next12, sample_dataset$q7_economy) %>%
  prop.table(margin = 1)

# q7_housing
sample_dataset$q7_housing <- generate_data_2factor(sample_dataset, "age_brackets", "urbanicity", c(1,2,3),
                                                   c(1.0,1.3,1.7,2.0,2.2), c(1.0,1.8),
                                                   c(0.3,0.5,0.7,1.0,1.0), c(0.5,0.8))

table(sample_dataset$age_brackets, sample_dataset$q7_housing) %>%
  prop.table(margin = 1)

table(sample_dataset$urbanicity, sample_dataset$q7_housing) %>%
  prop.table(margin = 1)


# q7_border
sample_dataset$q7_border <- generate_data_1factor(sample_dataset, "strata", c(1,2,3),
                                                  c(1.9,2.0,1.0,1.3,1.4), c(0.7,0.8,0.5,0.6,0.7))


table(sample_dataset$strata, sample_dataset$q7_border) %>%
  prop.table(margin = 1)


# q7_climate
sample_dataset$q7_climate <- generate_data_2factor(sample_dataset, "age_brackets","education",c(1,2,3),
                                                   c(1.4,1.5,1.8,1.9,2.2), c(2.0,1.8,1.7,1.0),
                                                   c(0.7,0.8,0.9,0.9,1.0), c(1.0,0.8,0.8,0.5))


table(sample_dataset$age_brackets, sample_dataset$q7_climate) %>%
  prop.table(margin = 1)

table(sample_dataset$education, sample_dataset$q7_climate) %>%
  prop.table(margin = 1)


# q7_skooma
sample_dataset$q7_skooma <- generate_data_2factor(sample_dataset, "strata", "urbanicity", c(1,2,3),
                                                  c(2.0,1.9,1.7,1.0,1.0), c(1.8,1.2),
                                                  c(1.0,0.8,0.8,0.5,0.5), c(0.8,0.4))

table(sample_dataset$strata, sample_dataset$q7_skooma) %>%
  prop.table(margin = 1)

table(sample_dataset$urbanicity, sample_dataset$q7_skooma) %>%
  prop.table(margin = 1)


# q7_university
sample_dataset$q7_university <- generate_data_2factor(sample_dataset, "education", "age_brackets", c(1,2,3),
                                                      c(2.6,2.3,1.7,1.0), c(1.0,1.3,1.6,2.0,2.1),
                                                      c(1.0,0.8,0.7,0.5), c(0.5,0.6,0.7,0.9,1.0))

table(sample_dataset$education, sample_dataset$q7_university) %>%
  prop.table(margin = 1)

table(sample_dataset$age_brackets, sample_dataset$q7_university) %>%
  prop.table(margin = 1)

# q8_army
sample_dataset$q8_army <- generate_data_2factor(sample_dataset, "strata", "education", c(1,2,3,4),
                                                c(1.5,1.5,1.0,1.2,1.2), c(1.0,1.2,1.2,1.5),
                                                c(0.7,0.8,0.6,0.7,0.6), c(0.7,0.7,0.7,0.8))

table(sample_dataset$strata, sample_dataset$q8_army) %>%
  prop.table(margin = 1)

table(sample_dataset$education, sample_dataset$q8_army) %>%
  prop.table(margin = 1)

# q8_police
sample_dataset$q8_police <- generate_data_2factor(sample_dataset, "strata", "education", c(1,2,3,4),
                                                c(1.7,1.6,1.1,1.3,1.3), c(1.2,1.3,1.3,1.7),
                                                c(0.7,0.8,0.6,0.7,0.6), c(0.7,0.7,0.7,0.8))

table(sample_dataset$strata, sample_dataset$q8_police) %>%
  prop.table(margin = 1)

table(sample_dataset$education, sample_dataset$q8_police) %>%
  prop.table(margin = 1)


# q8_universities
sample_dataset$q8_universities <- generate_data_2factor(sample_dataset, "strata", "education", c(1,2,3,4),
                                                  c(1.8,2.2,3.0,2.7,2.7), c(3.0,2.9,2.5,1.7),
                                                  c(0.7,0.8,0.7,1.0,1.0), c(1.0,1.0,0.8,0.7))

table(sample_dataset$strata, sample_dataset$q8_universities) %>%
  prop.table(margin = 1)

table(sample_dataset$education, sample_dataset$q8_universities) %>%
  prop.table(margin = 1)


# q8_healthcare
sample_dataset$q8_healthcare <- generate_data_2factor(sample_dataset, "strata", "education", c(1,2,3,4),
                                                      c(1.7,1.8,2.0,2.0,2.0), c(2.4,2.3,2.0,1.8),
                                                      c(0.8,0.8,0.8,1.0,1.0), c(0.8,0.8,0.7,0.7))

table(sample_dataset$strata, sample_dataset$q8_healthcare) %>%
  prop.table(margin = 1)

table(sample_dataset$education, sample_dataset$q8_healthcare) %>%
  prop.table(margin = 1)


# q8_reggovt
sample_dataset$q8_reggovt <- generate_data_2factor(sample_dataset, "strata", "education", c(1,2,3,4),
                                                   c(2.3,1.4,1.2,1.7,2.3), c(2.0,1.8,1.7,1.7),
                                                   c(0.8,0.8,0.5,0.7,1.0), c(1.0,0.8,0.7,0.7))

table(sample_dataset$strata, sample_dataset$q8_reggovt) %>%
  prop.table(margin = 1)

table(sample_dataset$education, sample_dataset$q8_reggovt) %>%
  prop.table(margin = 1)

# q8_council
sample_dataset$q8_council <- generate_data_2factor(sample_dataset, "strata", "education", c(1,2,3,4),
                                                   c(2.1,1.3,1.3,1.5,2.1), c(2.0,1.8,1.6,1.6),
                                                   c(0.8,0.8,0.5,0.7,1.0), c(1.0,0.8,0.7,0.7))

table(sample_dataset$strata, sample_dataset$q8_council) %>%
  prop.table(margin = 1)

table(sample_dataset$education, sample_dataset$q8_council) %>%
  prop.table(margin = 1)


# q8_parliament
sample_dataset$q8_parliament <- generate_data_2factor(sample_dataset, "strata", "education", c(1,2,3,4),
                                                      c(2.5,2.6,3.8,3.0,3.5), c(3.3,3.4,3.2,3.0),
                                                      c(0.8,0.8,0.6,0.8,0.7), c(0.9,0.8,0.8,0.8))

table(sample_dataset$strata, sample_dataset$q8_parliament) %>%
  prop.table(margin = 1)

table(sample_dataset$education, sample_dataset$q8_parliament) %>%
  prop.table(margin = 1)

# q8_president
sample_dataset$q8_president <- generate_data_2factor(sample_dataset, "strata", "education", c(1,2,3,4),
                                                     c(1.5,1.5,4.0,2.5,3.0), c(3.1,2.8,2.5,1.9),
                                                     c(0.8,0.8,0.5,1.0,1.0), c(1.0,0.8,0.8,0.8))

table(sample_dataset$strata, sample_dataset$q8_president) %>%
  prop.table(margin = 1)

table(sample_dataset$education, sample_dataset$q8_president) %>%
  prop.table(margin = 1)


prop.table(table(sample_dataset$q8_army))
prop.table(table(sample_dataset$q8_police))
prop.table(table(sample_dataset$q8_universities))
prop.table(table(sample_dataset$q8_healthcare))
prop.table(table(sample_dataset$q8_reggovt))
prop.table(table(sample_dataset$q8_council))
prop.table(table(sample_dataset$q8_parliament))
prop.table(table(sample_dataset$q8_president))

# q9_presapproval
sample_dataset$q9_presapproval <- generate_data_2factor(sample_dataset, "q8_president", "q1_direction", c(1,2,3,4),
                                                        c(1.0,1.5,3.0,4.0), c(1.5,2.0,3.0,4.0),
                                                        c(0.3,0.5,0.8,0.3), c(0.3,0.5,0.8,0.5))

table(sample_dataset$strata, sample_dataset$q9_presapproval) %>%
  prop.table(margin = 1)

table(sample_dataset$q8_president, sample_dataset$q9_presapproval) %>%
  prop.table(margin = 1)

table(sample_dataset$q8_president, sample_dataset$q1_direction) %>%
  prop.table(margin = 1)

prop.table(table(sample_dataset$q9_presapproval))


# q10_govnrapproval
sample_dataset$q10_govnrapproval <- generate_data_2factor(sample_dataset, "q8_reggovt", "strata", c(1,2,3,4),
                                                          c(1.0,2.0,3.0,4.0), c(1.5,1.7,1.0,2.0,2.5),
                                                          c(0.5,0.5,0.5,0.5), c(0.8,0.8,0.5,0.8,1.0))

table(sample_dataset$strata, sample_dataset$q10_govnrapproval) %>%
  prop.table(margin = 1)

table(sample_dataset$q8_reggovt, sample_dataset$q10_govnrapproval) %>%
  prop.table(margin = 1)

# q11_liberal
sample_dataset$q11_liberal <- generate_data_3factor(sample_dataset, "q9_presapproval", "strata", "education", c(1,2,3,4),
                                                    c(1.5,2.0,3.0,4.0), c(1.5,1.5,4.0,3.0,3.0), c(3.5,3.0,2.7,1.5),
                                                    c(0.3,0.8,0.8,0.3), c(0.5,0.5,0.5,0.8,0.8), c(0.8,0.8,0.7,0.5))


table(sample_dataset$q9_presapproval, sample_dataset$q11_liberal) %>%
  prop.table(margin = 1)

table(sample_dataset$strata, sample_dataset$q11_liberal) %>%
  prop.table(margin = 1)

table(sample_dataset$education, sample_dataset$q11_liberal) %>%
  prop.table(margin = 1)


# q11_conservative
sample_dataset$q11_conservative <- generate_data_3factor(sample_dataset, "q11_liberal", "strata", "q9_presapproval", c(1,2,3,4),
                                                         c(4.0,3.5,2.0,1.0), c(3.5,3.5,1.0,2.5,2.0), c(4.0,3.0,2.0,1.0),
                                                         c(0.3,0.8,0.8,0.3), c(0.5,0.5,0.5,0.8,0.8), c(0.5,0.8,0.8,0.5))

table(sample_dataset$q11_liberal, sample_dataset$q11_conservative) %>%
  prop.table(margin = 1)

table(sample_dataset$strata, sample_dataset$q11_conservative) %>%
  prop.table(margin = 1)

table(sample_dataset$education, sample_dataset$q11_conservative) %>%
  prop.table(margin = 1)


# q11_libertarian
sample_dataset$q11_libertarian <- generate_data_2factor(sample_dataset, "q11_liberal", "education", c(1,2,3,4),
                                                        c(3.5,3.0,2.5,2.0), c(3.5,3.0,2.0,2.0),
                                                        c(0.7,0.8,0.8,0.7), c(0.8,0.7,0.6,0.6))

table(sample_dataset$q11_liberal, sample_dataset$q11_libertarian) %>%
  prop.table(margin = 1)

table(sample_dataset$q11_conservative, sample_dataset$q11_libertarian) %>%
  prop.table(margin = 1)

table(sample_dataset$education, sample_dataset$q11_libertarian) %>%
  prop.table(margin = 1)

# q11_communist
sample_dataset$q11_communist <- generate_data_2factor(sample_dataset, "q11_conservative", "strata", c(1,2,3,4),
                                                      c(4.0,4.0,3.5,3.0), c(3.5,3.0,4.0,3.5,3.0),
                                                      c(0.5,0.5,0.8,0.8), c(0.8,0.8,0.5,0.8,0.8))

table(sample_dataset$q11_conservative, sample_dataset$q11_communist) %>%
  prop.table(margin = 1)

table(sample_dataset$q11_libertarian, sample_dataset$q11_communist) %>%
  prop.table(margin = 1)

table(sample_dataset$q11_liberal, sample_dataset$q11_communist) %>%
  prop.table(margin = 1)

table(sample_dataset$education, sample_dataset$q11_communist) %>%
  prop.table(margin = 1)

table(sample_dataset$strata, sample_dataset$q11_communist) %>%
  prop.table(margin = 1)

# q11_agrarian
sample_dataset$q11_agrarian <- generate_data_3factor(sample_dataset, "q11_liberal", "strata", "urbanicity",c(1,2,3,4),
                                                     c(3.5,3.0,2.5,2.0), c(3.5,3.5,3.5,2.5,1.5), c(3.5,1.5),
                                                     c(0.8,0.8,0.8,0.8), c(0.8,0.8,0.8,0.6,0.5), c(0.8,0.8))

table(sample_dataset$q11_liberal, sample_dataset$q11_agrarian) %>%
  prop.table(margin = 1)

table(sample_dataset$q11_conservative, sample_dataset$q11_agrarian) %>%
  prop.table(margin = 1)


table(sample_dataset$strata, sample_dataset$q11_agrarian) %>%
  prop.table(margin = 1)


table(sample_dataset$urbanicity, sample_dataset$q11_agrarian) %>%
  prop.table(margin = 1)


prop.table(table(sample_dataset$q11_liberal))
prop.table(table(sample_dataset$q11_conservative))
prop.table(table(sample_dataset$q11_libertarian))
prop.table(table(sample_dataset$q11_communist))
prop.table(table(sample_dataset$q11_agrarian))


# q12_likelihood
sample_dataset$q12_likelihood <- generate_data_3factor(sample_dataset, "q1_direction", "q11_conservative", "education", c(1,2,3,4),
                                                       c(2.0,2.5,2.0,1.0), c(1.0,2.5,3.0,1.3), c(2.7,2.5,1.7,1.5),
                                                       c(1.0,1.0,1.0,0.5), c(0.5,0.7,0.8,0.6), c(1.0,1.0,0.8,0.7))


table(sample_dataset$q11_conservative, sample_dataset$q12_likelihood) %>%
  prop.table(margin = 1)

table(sample_dataset$q11_liberal, sample_dataset$q12_likelihood) %>%
  prop.table(margin = 1)


table(sample_dataset$strata, sample_dataset$q12_likelihood) %>%
  prop.table(margin = 1)

table(sample_dataset$education, sample_dataset$q12_likelihood) %>%
  prop.table(margin = 1)


prop.table(table(sample_dataset$q12_likelihood))



# q15_roads
sample_dataset$q15_roads <- generate_data_3factor(sample_dataset, "strata", "urbanicity", "q10_govnrapproval", c(1,2,3,4),
                                                  c(2.0,1.0,2.0,2.5,3.0), c(2.0,3.5), c(1.0,2.0,3.0,4.0),
                                                  c(0.8,0.5,0.8,0.8,0.8), c(0.6,0.8), c(0.8,0.8,0.8,0.8))

table(sample_dataset$strata, sample_dataset$q15_roads) %>%
  prop.table(margin = 1)

table(sample_dataset$urbanicity, sample_dataset$q15_roads) %>%
  prop.table(margin = 1)

table(sample_dataset$q10_govnrapproval, sample_dataset$q15_roads) %>%
  prop.table(margin = 1)


# q15_transit
sample_dataset$q15_transit <- generate_data_2factor(sample_dataset, "strata", "urbanicity", c(1,2,3,4),
                                                    c(1.5,2.5,3.0,2.7,4.0), c(2.0,3.5),
                                                    c(0.8,1.0,1.0,0.8,0.6), c(0.6,0.8))

table(sample_dataset$strata, sample_dataset$q15_transit) %>%
  prop.table(margin = 1)

table(sample_dataset$urbanicity, sample_dataset$q15_transit) %>%
  prop.table(margin = 1)

# q15_electricity
sample_dataset$q15_electricity <- generate_data_2factor(sample_dataset, "strata", "urbanicity", c(1,2,3,4),
                                                        c(2.0,1.5,2.0,2.5,3.0), c(2.0,3.0),
                                                        c(1.0,0.8,0.8,0.8,0.8), c(0.8,0.8))

table(sample_dataset$strata, sample_dataset$q15_electricity) %>%
  prop.table(margin = 1)

table(sample_dataset$urbanicity, sample_dataset$q15_electricity) %>%
  prop.table(margin = 1)

# q15_water
sample_dataset$q15_water <- generate_data_2factor(sample_dataset, "strata", "urbanicity", c(1,2,3,4),
                                                  c(1.2,1.5,1.3,2.0,2.5), c(1.5,2.0),
                                                  c(0.5,0.5,0.5,0.5,0.5), c(0.5,0.7))

table(sample_dataset$strata, sample_dataset$q15_water) %>%
  prop.table(margin = 1)

table(sample_dataset$urbanicity, sample_dataset$q15_water) %>%
  prop.table(margin = 1)

# q15_police
sample_dataset$q15_police <- generate_data_2factor(sample_dataset, "strata", "q8_police", c(1,2,3,4),
                                                   c(2.5,1.5,1.3,2.0,2.5), c(1.0,2.0,3.0,4.0),
                                                   c(1.0,0.8,0.8,1.0,1.0), c(0.5,0.5,0.5,0.5))

table(sample_dataset$strata, sample_dataset$q15_police) %>%
  prop.table(margin = 1)

table(sample_dataset$urbanicity, sample_dataset$q15_police) %>%
  prop.table(margin = 1)

# q15_schools
sample_dataset$q15_schools <- generate_data_3factor(sample_dataset, "strata", "urbanicity", "education", c(1,2,3,4),
                                                    c(2.5,1.0,1.5,2.5,3.0), c(2.0,3.0), c(3.0,2.7,2.3,2.0),
                                                    c(1.0,0.8,1.0,1.0,0.8), c(1.0,0.8), c(0.8,0.8,0.8,0.8))

table(sample_dataset$strata, sample_dataset$q15_schools) %>%
  prop.table(margin = 1)

table(sample_dataset$urbanicity, sample_dataset$q15_schools) %>%
  prop.table(margin = 1)

table(sample_dataset$education, sample_dataset$q15_schools) %>%
  prop.table(margin = 1)

# q15_trash
sample_dataset$q15_trash <- generate_data_3factor(sample_dataset, "strata", "urbanicity", "hh_income", c(1,2,3,4),
                                                  c(2.5,1.5,1.5,2.5,2.8), c(2.0,3.0), c(4.0,3.5,3.0,2.0,1.5,1.0),
                                                  c(1.0,0.8,0.8,1.0,1.0), c(1.0,0.8), c(0.5,0.8,1.0,1.0,0.5,0.5))

table(sample_dataset$strata, sample_dataset$q15_trash) %>%
  prop.table(margin = 1)

table(sample_dataset$urbanicity, sample_dataset$q15_trash) %>%
  prop.table(margin = 1)

table(sample_dataset$hh_income, sample_dataset$q15_trash) %>%
  prop.table(margin = 1)


# q15_internet
sample_dataset$q15_internet <- generate_data_3factor(sample_dataset, "strata", "urbanicity", "age_brackets", c(1,2,3,4),
                                                     c(1.0,1.5,1.0,2.0,3.0), c(1.5,2.5), c(1.5,1.7,2.0,2.5,3.0),
                                                     c(0.8,0.8,0.8,1.0,1.0), c(1.0,1.0), c(0.8,0.8,0.8,1.0,1.0))

table(sample_dataset$strata, sample_dataset$q15_internet) %>%
  prop.table(margin = 1)

table(sample_dataset$urbanicity, sample_dataset$q15_internet) %>%
  prop.table(margin = 1)

table(sample_dataset$age_brackets, sample_dataset$q15_internet) %>%
  prop.table(margin = 1)


prop.table(table(sample_dataset$q15_roads))
prop.table(table(sample_dataset$q15_transit))
prop.table(table(sample_dataset$q15_electricity))
prop.table(table(sample_dataset$q15_water))
prop.table(table(sample_dataset$q15_police))
prop.table(table(sample_dataset$q15_schools))
prop.table(table(sample_dataset$q15_trash))
prop.table(table(sample_dataset$q15_internet))

# 16_comp
sample_dataset$q16_comp <- generate_data_2factor(sample_dataset, "strata", "q10_govnrapproval", c(1,2,3,4),
                                                 c(1.5,1.3,1.8,2.8,3.0), c(1.0,2.0,3.0,4.0),
                                                 c(0.8,0.8,0.8,1.0,1.0), c(0.5,0.5,0.5,0.5))

table(sample_dataset$strata, sample_dataset$q16_comp) %>%
  prop.table(margin = 1)

table(sample_dataset$urbanicity, sample_dataset$q16_comp) %>%
  prop.table(margin = 1)


# ... test
