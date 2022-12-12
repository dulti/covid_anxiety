# https://databank.worldbank.org/source/global-financial-inclusion
# this is the script for a report on Levels of financial anxiety related to global COVID-19 pandemic by different groups of population in 2021.
# for full information see the report

finincl <- read.csv("finincl.csv", stringsAsFactors = T, na.strings = '..')

library('dplyr')
library('stringr')
library('ggplot2')
library('tidyr')

str(finincl)


# cleaning and preparing
finincl <- finincl %>% 
  filter(if_all(.fns = ~ !is.na(.x))) %>% 
  mutate(Series.Short = factor(str_match(Series.Name, "(?<=: ).*$"))) %>% 
  mutate(Pct = X2021..YR2021.)
droplevels
str(finincl)

head(unique(finincl$Series.Short))

## sex

# helper function to choose variables
make_var_df <- function(dtf, pattern, col_name, levels){
  new_dft <- dtf %>% 
    filter(Series.Short <- str_detect(Series.Short, pattern)) %>% 
    droplevels
  new_dft$Anxiety <- factor(str_extract(new_dft$Series.Short, "^([a-z]+) w", 
                                        group = 1), 
                            levels = c("not", "somewhat", "very"))
  levels(new_dft$Anxiety) <- c("No", "Yes", "Yes")
  new_dft[[col_name]] <- factor(str_extract(new_dft$Series.Short, 
                                            paste0(c("(", pattern, ")"),
                                                   collapse = ''),
                                            group = 1))
  new_dft[c("Country.Name", "Pct", "Anxiety", col_name)]
  new_dft %>% 
    filter(Country.Name == "World") %>% 
    group_by(across(c(all_of(col_name), "Anxiety"))) %>% 
    summarize(Pct = sum(Pct)) %>% 
    ungroup()
}
finincl_sex <- make_var_df(finincl, "male|female", "Sex")
str(finincl_sex)


#chart
ggplot(finincl_sex, aes(x = Sex, y = Pct, fill = Anxiety)) +
  geom_col() +
  ggtitle(label = "Levels of anxiety by sex, %",
          subtitle = "Data source: World Bank, 2022\nhttps://databank.worldbank.org/source/global-financial-inclusion") +
  theme(plot.subtitle = element_text(size = 8)) +
  scale_fill_brewer(palette = "OrRd") +
  scale_y_continuous("% of respondents",
                     breaks = seq(0, 100, 25),
                     labels = paste0(seq(0, 100, 25), rep("%", 5)))

# test
sex_abs <- finincl_sex %>% 
  select(Anxiety, Sex, Pct) %>% 
  mutate(Pct = Pct * 128000 / 2) %>% 
  pivot_wider(names_from = Anxiety, values_from = Pct) %>% 
  select(2:3)
sex_abs <- as.data.frame(sex_abs)
rownames(sex_abs) <- c('Female', 'Male')
chisq.test(sex_abs)

## education
finincl_edu <- make_var_df(finincl, 
                           "primary education or less|secondary education or more", 
                           "Education")
str(finincl_edu)

#chart
ggplot(finincl_edu, aes(x = Education, y = Pct, fill = Anxiety)) +
  geom_col() +
  ggtitle(label = "Levels of anxiety by education, %",
          subtitle = "Data source: World Bank, 2022\nhttps://databank.worldbank.org/source/global-financial-inclusion") +
  theme(plot.subtitle = element_text(size = 8)) +
  scale_fill_brewer(palette = "OrRd") +
  scale_y_continuous("% of respondents",
                     breaks = seq(0, 100, 25),
                     labels = paste0(seq(0, 100, 25), rep("%", 5)))

# test
edu_abs <- finincl_edu %>% 
  select(Anxiety, Education, Pct) %>% 
  mutate(Pct = Pct * 128000 / 2) %>% 
  pivot_wider(names_from = Anxiety, values_from = Pct) %>% 
  select(2:3)
edu_abs <- as.data.frame(edu_abs)
rownames(edu_abs) <- c('Primary', 'Secondary')
chisq.test(edu_abs)

## employment
finincl_emp <- make_var_df(finincl, 
                           "in labor|out of labor", 
                           "Employment")
str(finincl_emp)

# chart
ggplot(finincl_emp, aes(x = Employment, y = Pct, fill = Anxiety)) +
  geom_col() +
  ggtitle(label = "Levels of anxiety by employment status, %",
          subtitle = "Data source: World Bank, 2022\nhttps://databank.worldbank.org/source/global-financial-inclusion") +
  theme(plot.subtitle = element_text(size = 8)) +
  scale_fill_brewer(palette = "OrRd") +
  scale_y_continuous("% of respondents",
                     breaks = seq(0, 100, 25),
                     labels = paste0(seq(0, 100, 25), rep("%", 5)))

# test
emp_abs <- finincl_emp %>% 
  select(Anxiety, Employment, Pct) %>% 
  mutate(Pct = Pct * 128000 / 2) %>% 
  pivot_wider(names_from = Anxiety, values_from = Pct) %>% 
  select(2:3)
emp_abs <- as.data.frame(emp_abs)
rownames(emp_abs) <- c('In labor', 'Out of labor')
chisq.test(emp_abs)

