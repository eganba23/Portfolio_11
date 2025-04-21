Commentary on Lab 12 - Smoking during pregnancy
================
Benjamin Egan

#### Created 4/14/2025

Link to the assignment page, at the time of creation:
<https://web.archive.org/web/20250414135227/https://datascience4psych.github.io/DataScience4Psych/lab12.html>

Link to assignment page as of April 21 2025:
<https://datascience4psych.github.io/DataScience4Psych/lab12.html>

## Commentary on Lab 12

I will be walking through lab 12, designed to practice data
manipulation, visualization, hypothesis testing, and calculating
confidence intervals. Questions and comments from the lab that I will be
addressing will be in bold. My commentary will always have “Commentary:”
infront. This can make it easier to quickly skim through the document by
searching for the word commentary.

Loading the data

``` r
data(ncbirths)
#view(ncbirths)
```

#### Before analyzing any new dataset, it’s important to get to know your data. Start by summarizing the variables and determining if their data type. Are they categorical? Are they numerical? For numerical variables, check for outliers. If you aren’t sure or want to take a closer look at the data, create a graph.

``` r
summary(ncbirths)
```

    ##       fage            mage            mature        weeks             premie   
    ##  Min.   :14.00   Min.   :13   mature mom :133   Min.   :20.00   full term:846  
    ##  1st Qu.:25.00   1st Qu.:22   younger mom:867   1st Qu.:37.00   premie   :152  
    ##  Median :30.00   Median :27                     Median :39.00   NA's     :  2  
    ##  Mean   :30.26   Mean   :27                     Mean   :38.33                  
    ##  3rd Qu.:35.00   3rd Qu.:32                     3rd Qu.:40.00                  
    ##  Max.   :55.00   Max.   :50                     Max.   :45.00                  
    ##  NA's   :171                                    NA's   :2                      
    ##      visits            marital        gained          weight      
    ##  Min.   : 0.0   not married:386   Min.   : 0.00   Min.   : 1.000  
    ##  1st Qu.:10.0   married    :613   1st Qu.:20.00   1st Qu.: 6.380  
    ##  Median :12.0   NA's       :  1   Median :30.00   Median : 7.310  
    ##  Mean   :12.1                     Mean   :30.33   Mean   : 7.101  
    ##  3rd Qu.:15.0                     3rd Qu.:38.00   3rd Qu.: 8.060  
    ##  Max.   :30.0                     Max.   :85.00   Max.   :11.750  
    ##  NA's   :9                        NA's   :27                      
    ##  lowbirthweight    gender          habit          whitemom  
    ##  low    :111    female:503   nonsmoker:873   not white:284  
    ##  not low:889    male  :497   smoker   :126   white    :714  
    ##                              NA's     :  1   NA's     :  2  
    ##                                                             
    ##                                                             
    ##                                                             
    ## 

By using summary(), I know there are 7 categorical variables and 6
numerical variables.

I way overengineered looking for outliers, getting help from Chat GPT.

``` r
base_outlier_check <- function(variable) {
  ncbirths %>%
  ggplot(aes(x = {{ variable }})) +
    geom_histogram(stat = "count") +
    labs(title = rlang::as_label(rlang::enquo(variable)), y = NULL)
}

map(c("fage", "mage","weeks","visits","gained","weight"), ~ base_outlier_check(!!sym(.x)))
```

    ## [[1]]

![](lab-12_files/figure-gfm/outlier%20check-1.png)<!-- -->

    ## 
    ## [[2]]

![](lab-12_files/figure-gfm/outlier%20check-2.png)<!-- -->

    ## 
    ## [[3]]

![](lab-12_files/figure-gfm/outlier%20check-3.png)<!-- -->

    ## 
    ## [[4]]

![](lab-12_files/figure-gfm/outlier%20check-4.png)<!-- -->

    ## 
    ## [[5]]

![](lab-12_files/figure-gfm/outlier%20check-5.png)<!-- -->

    ## 
    ## [[6]]

![](lab-12_files/figure-gfm/outlier%20check-6.png)<!-- -->

mage could have an outlier on the upper end. I don’t think weeks has an
outlier, even though it’s skewed left. visits may have an outlier on the
upper end. weight might have one or two on the upper end, but it’s
skewed left.

Commentary: This was pretty basic, and I made it way harder than it had
to be. This could, however, be a good moment to throw it back to lab 8
and show a way to use a function. I would just guide them on the
labeling bit, as it can be tricky. You can use do what I did and use
base R. This allows you to input all the variables at once using map().
If you use tidyverse for the label and use
deparse(substitute(variable)), you have to individually input each
function and cannot mass input.

#### What are the cases in this data set? How many cases are there in our sample?

``` r
ncbirths %>%
nrow()
```

    ## [1] 1000

There are 1000 cases, as each row represents an individual birth.

### Create a filtered data frame called ncbirths_white that contains data only from White mothers. Then, calculate the mean of the weights of their babies.

``` r
ncbirths_white <- ncbirths %>%
  filter(whitemom == "white")

mean(ncbirths_white$weight)
```

    ## [1] 7.250462

The mean weight of the white babies is 7.25 lbs. The hypothesis states
that there is no change in weight since 1995 (i.e. mu = 7.43). Since
white babies weigh less, we need to figure out if this is a meaningful
difference.

Commentary: You/someone else included a note in the code that says
“Mason make this clearer to why we need to actually do this.” I don’t
think you need to explain, if they watched the videos or viewed the
slides. What I would like more clarification on is question 3. My
imagination for an answer is to create a visual for the data and see if
mu falls within the CI in this sample.

### Are the criteria necessary for conducting simulation-based inference satisfied? Explain your reasoning.

``` r
ncbirths_white %>%
  ggplot(aes(
    x = weight
  ))+
  geom_histogram(stat = "count", color = "blue")+
  geom_vline(xintercept = 7.43, 
               color = "white", 
               linetype = "dashed")+
   geom_vline(xintercept = 7.25, 
               color = "red", 
               linetype = "dashed")+
  annotate("text", 
           x = 4, y = Inf, 
           label = "White = Mu, Red = Sample mean", 
           vjust = 2, 
           color = "black")+
  theme_bw()+
  labs(
    x = "Weight in lbs",
    y = NULL,
    title = "Distriubtion of weight",
    subtitle = "How does this shape up to the popluation mean?"
  )
```

    ## Warning in geom_histogram(stat = "count", color = "blue"): Ignoring unknown
    ## parameters: `binwidth`, `bins`, and `pad`

![](lab-12_files/figure-gfm/should%20we%20baby%20weight%20hypothesis%20test-1.png)<!-- -->
Based on this graph, I would guess that we probably would fail to
reject, considering the mean weight is so close to the population
weight. But this is only one sample, so it makes sense to use
bootstrapping to better estimate the difference.

## Steps for hypothesis testing

- take a bootstrap sample of from the original sample
- calculate this bootstrap sample’s mean
- repeat these two steps a large number of times to create a bootstrap
  distribution of means centered at the observed sample mean,
- shift this distribution to be centered at the null value by
  subtracting / adding X to all bootstrap mean (X = difference between
  mean of bootstrap distribution and null value)
- calculate the p-value as the proportion of bootstrap samples that
  yielded a sample mean at least as extreme as the observed sample mean.

``` r
seed
```

    ## NULL

``` r
white_weight_bootstrap <- ncbirths_white %>%
  specify(response = weight) %>% 
  generate(reps = 15000, type = "bootstrap") %>% 
  calculate(stat = "mean")

mean_white_weight_boostrap <- mean(white_weight_bootstrap$stat)

white_weight_bootstrap <-  white_weight_bootstrap %>%
  mutate(Cstat = (stat - 7.43))

white_weight_bootstrap %>%
  summarize(lower = quantile(Cstat, 0.025),
            upper = quantile(Cstat, 0.975))
```

    ## # A tibble: 1 × 2
    ##    lower   upper
    ##    <dbl>   <dbl>
    ## 1 -0.282 -0.0763

``` r
white_weight_bootstrap %>%
  ggplot(mapping = aes(x = Cstat)) +
  geom_histogram() +
  labs(title = "Bootstrap distribution of means for white baby weight", y = NULL, x = "Means")+
  geom_vline(xintercept = 0, 
               color = "black", 
               linetype = "solid")+
  annotate("text", 
           x = -.03, y = Inf, 
           label = "Pop mean", 
           vjust = 2, 
           color = "black")+
  geom_vline(xintercept = -0.2847633,                 #Lower CI
               color = "blue", 
               linetype = "dashed")+
  geom_vline(xintercept = -0.07682038,               #Upper CI
               color = "blue", 
               linetype = "dashed")+
  annotate("text", 
           x = -0.31, y = Inf, 
           label = "95% CI", 
           vjust = 2, 
           color = "black")+
  theme_bw()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](lab-12_files/figure-gfm/bootstrap%20weight-1.png)<!-- -->

``` r
# p value
pvalue_white_weight <- white_weight_bootstrap %>%
  mutate(sig = case_when(
    stat <= mean_white_weight_boostrap ~ 1,
    stat >= mean_white_weight_boostrap ~ 0,
  ))


white_weight_sig <- pvalue_white_weight %>%
  filter(sig == 1)

count(white_weight_sig)/15000
```

    ##           n
    ## 1 0.4981333

95% CI:(7.145237, 7.35318)

Based on this, it’s clear that the population mean falls outside the
confidence interval. We would reject the null and say that babies’
weight has decreased since 1995.

To calculate the p value, I created a variable that designated whether
the mean in the bootstrap was lower than the observed mean of 7.25. I
then divided the number of significant ones by the total number of
observations in the bootstrap to get a p of .50, indicating that the
sample means should not be significantly different from the popluation
mean.

Commentary: I would break out the step by step instructions. It was easy
to figure out how to make the graph based on the slides, so maybe add in
a tip to go back and reference the slides? I will say that I believe I
messed this up somewhere along the way, as my p value should be
reflective of the fact that the population mean of 7.43 is way outside
the confidence interval I estimated. If I had to guess, I either messed
up the calculation of the p value or my interpretation of the p value.

## Baby Weight vs Smoking

#### Make side-by-side box plots displaying the relationship between habit and weight. What does the plot highlight about the relationship between these two variables?

Commentary: I would specify here if you want us to continue to utilize
only White babies or all babies. I am assuming you want us to use all
babies here.

``` r
ncbirths %>%
  ggplot(aes(
    x = habit,
    y = weight
  ))+
    geom_boxplot()+
  labs(
    x = "Smoking Status",
    y = "Weight in lbs",
    title = "Relationship between smoking and weight"
  )
```

![](lab-12_files/figure-gfm/box%20plot%20habit%20weight-1.png)<!-- -->

It appears that there are a lot of underweight babies for non-smokers
compared to smokers. This could be due to sample differences, as
non-smokers could have more babies overall.

#### Before continuing, create a cleaned version of the dataset by removing any rows with missing values for habit or weight. Name this version ncbirths_clean.

``` r
ncbirths_clean <- ncbirths %>%
  filter(!habit == "NA")

ncbirths_clean %>%
  ggplot(aes(
    x = habit,
    y = weight
  ))+
    geom_boxplot()+
  labs(
    x = "Smoking Status",
    y = "Weight in lbs",
    title = "Relationship between smoking and weight"
  )
```

![](lab-12_files/figure-gfm/clean%20smoking%20data-1.png)<!-- -->

We can see that we cleaned the data to filter out the person who didn’t
answer the smoking question.

#### Calculate the observed difference in means between the baby weights of smoking and non-smoking mothers.

``` r
ncbirths_clean %>%
  group_by(habit) %>%
  summarize(mean_weight = mean(weight))
```

    ## # A tibble: 2 × 2
    ##   habit     mean_weight
    ##   <fct>           <dbl>
    ## 1 nonsmoker        7.14
    ## 2 smoker           6.83

We can see that non-smoking babies have a higher mean weight that
smoking babies.

#### Write the hypotheses for testing if the average weights of babies born to smoking and non-smoking mothers are different.

H0: Mu1 - Mu2 = 0 OR Mu1 = Mu2 (No difference between smokers and
non-smokers on baby weight)

H1: Mu1 - Mu2 ≠ 0 OR Mu1 ≠ Mu2 (There is a difference between smokers
and non-smokers on baby weigh)

Commentary: I TA for the 312 class and have hypothesis testing
essentially tattooed on my frontal lobe.

#### Run the appropriate hypothesis test, calculate the p-value, and interpret the results in context of the data and the hypothesis test.

``` r
#view(ncbirths_clean)

ci_vals <- ncbirths_clean %>%
  group_by(habit) %>%
  summarize(
    mean_weight = mean(weight),
    se = sd(weight) / sqrt(sum(!is.na(weight))),
    lower = mean_weight - 1.96 * se,
    upper = mean_weight + 1.96 * se
  )
ci_vals
```

    ## # A tibble: 2 × 5
    ##   habit     mean_weight     se lower upper
    ##   <fct>           <dbl>  <dbl> <dbl> <dbl>
    ## 1 nonsmoker        7.14 0.0514  7.04  7.25
    ## 2 smoker           6.83 0.123   6.59  7.07

CI for non-smokers (7.043530, 7.245016) CI for smokers (6.586688,
7.070772)

``` r
ncbirths_clean %>%
ggplot(aes(
  x = weight, 
  fill = habit, 
  color = habit)) +
  geom_density(alpha = 0.4) +
  
  # Add CI
  geom_vline(data = ci_vals, 
             aes(xintercept = lower, 
                 color = habit), 
             linetype = "dashed") +
  geom_vline(data = ci_vals, 
             aes(xintercept = upper, 
                 color = habit), 
             linetype = "dashed") +
  scale_color_manual(
    name = "Smoking Status",
    values = c("nonsmoker" = "blue", "smoker" = "red"))+
  scale_fill_manual(
    name = "Smoking Status",
    values = c("nonsmoker" = "skyblue", "smoker" = "pink"))+
  
  #I got some help from ChatGPT to make it better. I clearly could've used a string and wrote out the CI, but this looked cooler
  annotate("text", 
         x = 2, y = 0.33, hjust = 0, size = 3.5,
         label = paste0("Non-smoker 95% CI: [", 
                        round(ci_vals$lower[ci_vals$habit == "nonsmoker"], 2), ", ", 
                        round(ci_vals$upper[ci_vals$habit == "nonsmoker"], 2), "]"),
         color = "blue") +
annotate("text", 
         x = 2, y = 0.30, hjust = 0, size = 3.5,
         label = paste0("Smoker 95% CI: [", 
                        round(ci_vals$lower[ci_vals$habit == "smoker"], 2), ", ", 
                        round(ci_vals$upper[ci_vals$habit == "smoker"], 2), "]"),
         color = "red")+
  
  labs(
    x = "Baby weight in lbs",
    y = NULL,
    title = "Baby weight distribution based on smoking"
  ) +
  theme_bw()
```

![](lab-12_files/figure-gfm/visual%20CI%20for%20smoking-1.png)<!-- -->

Based on this graph, it looks like the two distributions heavily
overlap, and there appears to be some basic overlap for the CIs.

##### Commentary

I will admit that I was completely lost on how to get the p value. I
looked back through the class notes and here is what I came up with:

Idea 1: Create the null distribution <br/> I was thinking about somehow
creating the null distribution that assumes the mean difference is zero.
I could possibly do this by somehow bootstrapping the weight data but
telling the simluation that the resulting outcome should be two stats,
one for smokers and one for non-smokers

Idea 2: two boostraps and join <br/> For this, I would filter based on
smoking status. I would then bootstrap for each of the two datasets, and
then join them together. The downside to this is that I don’t know if I
would need to bootstrap the combined bootstraps to truly achieve what I
wanted to do.

Idea 3: cry <br/> No not actually, but I am thinking that I could
somehow create difference scores and bootstrap the difference scores. I
only think this would work if there is equal sample size, which I know
for a fact there isn’t based on the summary() look at the variables.

I’m thinking that I’ll need to use idea one, since I’m not confident
idea 2 would give me the best outcomes.

``` r
seed
```

    ## NULL

``` r
smoking_pvalue_test <- ncbirths_clean %>%
  group_by(habit) %>%
  specify(response = weight) %>% 
  generate(reps = 15000, type = "bootstrap") %>% 
  calculate(stat = "mean")

head(smoking_pvalue_test)
```

    ## Response: weight (numeric)
    ## # A tibble: 6 × 2
    ##   replicate  stat
    ##       <int> <dbl>
    ## 1         1  7.14
    ## 2         2  7.09
    ## 3         3  7.13
    ## 4         4  7.10
    ## 5         5  7.09
    ## 6         6  7.11

Ok so I fought with chat heavily to figure out how to do it this way via
idea 1, and I have zero clue why group_by isn’t doing it’s job. So now
I’ll try idea two, but using chat’s help

``` r
# Load necessary libraries
library(dplyr)
library(infer)


# Split the dataset by habit (smoker vs nonsmoker)
smoker_data <- ncbirths_clean %>% filter(habit == "smoker")
nonsmoker_data <- ncbirths_clean %>% filter(habit == "nonsmoker")

# Perform bootstrap for smokers
smoker_bootstrap <- specify(smoker_data, response = weight) %>%
  generate(reps = 15000, type = "bootstrap") %>%
  calculate(stat = "mean")

# Perform bootstrap for non-smokers
nonsmoker_bootstrap <- specify(nonsmoker_data, response = weight) %>%
  generate(reps = 15000, type = "bootstrap") %>%
  calculate(stat = "mean")

# Combine both bootstrap results into one data frame
white_weight_habit_bootstrap <- bind_rows(
  smoker_bootstrap %>% mutate(habit = "smoker"),
  nonsmoker_bootstrap %>% mutate(habit = "nonsmoker")
)
```

I went back and asked it to do it only using the libraries required thus
far.

``` r
# Split the dataset by 'habit' (smoker vs nonsmoker)
smoker_data <- ncbirths_clean %>% filter(habit == "smoker")
nonsmoker_data <- ncbirths_clean %>% filter(habit == "nonsmoker")

# Define a function to perform bootstrapping
bootstrap_mean <- function(data, reps = 15000) {
  # Create bootstrap samples and calculate means
  replicate(reps, {
    sample_data <- sample(data$weight, replace = TRUE)
    mean(sample_data)
  })
}

# Perform bootstrap for smokers
smoker_bootstrap_means <- bootstrap_mean(smoker_data, 15000)

# Perform bootstrap for non-smokers
nonsmoker_bootstrap_means <- bootstrap_mean(nonsmoker_data, 15000)

# Combine both bootstrap results into one data frame
white_weight_habit_bootstrap <- tibble(
  habit = c(rep("smoker", 15000), rep("nonsmoker", 15000)),
  bootstrap_mean = c(smoker_bootstrap_means, nonsmoker_bootstrap_means)
)
```

Another attempt using ChatGPT. Here is what I plugged into chat

What if i try creating a loop using for or if or while. The loop
specifies that I filter for only smoker data, simulate 1500, and apply a
categorical variable with the designation “smoker”. I then repeat the
sequence, except I now filter for only nonsmoker data pply a categorical
variable with the designation “nonsmoker”

It gave me a response I didn’t like, and asked “Is it possible to
restructure or recreate the code such that we won’t have to combine the
boostraps and that it will do it all in one go?”

``` r
# Set habit groups and number of reps
habits <- c("smoker", "nonsmoker")
n_reps <- 15000

# Initialize an empty tibble to collect all bootstrap results
white_weight_habit_bootstrap <- tibble()

# Loop through both habit groups and perform bootstrap
for (h in habits) {
  
  # Filter the data for the current group
  current_data <- ncbirths_clean %>% filter(habit == h)
  
  # Perform bootstrap in one go (faster than nested loop)
  boot_means <- map_dbl(1:n_reps, ~ mean(sample(current_data$weight, replace = TRUE)))
  
  # Append results directly to the main tibble
  white_weight_habit_bootstrap <- bind_rows(
    white_weight_habit_bootstrap,
    tibble(habit = h, bootstrap_mean = boot_means)
  )
}

# View result ---- view(white_weight_habit_bootstrap)
```

Commentary: To figure out what is happening with group_by, I simulated
data with extreme means.

``` r
seed
```

    ## NULL

``` r
small <- rnorm(100, mean = 10, sd = 5)

dfsmall <- data.frame(
    score = small,
    group = 1
  )

large <- rnorm(100, mean = 1000, sd = 5)

dflarge <- data.frame(
    score = large,
    group = 2
  )

simulation <- full_join(dfsmall,dflarge)
```

    ## Joining with `by = join_by(score, group)`

``` r
simluation_test1 <- simulation %>%
  group_by(group) %>%
  specify(response = score) %>% 
  generate(reps = 15000, type = "bootstrap") %>%
  calculate(stat = "mean") %>%
  ungroup()
```

Commentary: turns out it just ignores the group_by, as the bootstrapping
provides means that encompass the entire data.

Ok after talking to 4/16 Mason, we figured out she intended for us to
run a t-test.

``` r
t.test(weight ~ habit, data = ncbirths_clean)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  weight by habit
    ## t = 2.359, df = 171.32, p-value = 0.01945
    ## alternative hypothesis: true difference in means between group nonsmoker and group smoker is not equal to 0
    ## 95 percent confidence interval:
    ##  0.05151165 0.57957328
    ## sample estimates:
    ## mean in group nonsmoker    mean in group smoker 
    ##                7.144273                6.828730

Based on the T-test, we know that mothers who don’t smoke have babies
with a higher weight (7.144) than mothers who smoke (6.829).

#### Construct a 95% confidence interval for the difference between the average weights of babies born to smoking and non-smoking mothers.

``` r
mean_smoker_weight <- mean(smoker_data$weight)
mean_nonsmoker_weight <- mean(nonsmoker_data$weight)
sd_smoker_weight <- sd(smoker_data$weight)
sd_nonsmoker_weight <- sd(nonsmoker_data$weight)

mean_diff_smoker_weight <- mean_nonsmoker_weight - mean_smoker_weight

se_smoker_weight <- sqrt(sd_smoker_weight/nrow(smoker_data))+(sd_nonsmoker_weight/nrow(nonsmoker_data))


  
ci_diff <- data.frame(
    mean_weight = mean_diff_smoker_weight,
    se = se_smoker_weight,
    lower = mean_diff_smoker_weight - 1.96 * se_smoker_weight,
    upper = mean_diff_smoker_weight + 1.96 * se_smoker_weight
  )

ci_diff
```

    ##   mean_weight        se    lower    upper
    ## 1   0.3155425 0.1066273 0.106553 0.524532

The 95% CI for the mean difference between average weights is (.11 \< x
\> .52).

Commentary: I know to use the SE, but I’m not sure if other people do.
Might want to give them the equation as scaffolding?

#### First, a non-inference task: Determine the age cutoff for younger and mature mothers. Use a method of your choice, and explain how your method works.

``` r
test_mature_upper_cutoff <- ncbirths_clean %>%
  filter(mature == "mature mom")

min(test_mature_upper_cutoff$mage)
```

    ## [1] 35

``` r
test_mature_lower_cutoff <- ncbirths_clean %>%
  filter(mature == "younger mom")

min(test_mature_lower_cutoff$mage)
```

    ## [1] 13

I filtered based on mature mom’s and saw what the lowest age for mature
moms was. I did the same for young moms and looked at the maximum age.
Since they’re both 35, this will be the cutoff.

#### Conduct a hypothesis test evaluating whether the proportion of low birth weight babies is higher for mature mothers. Use alpha of .05

Commentary: I will note that this question assumes people got it right
originally.

Null: Mu1 - Mu2 = 0 OR Mu1 = Mu2 (No difference between mature mothers
on baby weight)

Alternate: Mu1 - Mu2 ≠ 0 OR Mu1 ≠ Mu2 (low birth weight babies is higher
for mature mothers)

``` r
t_test_LBW <- ncbirths %>%
  mutate(lowbirthweight_numeric = case_when(
    lowbirthweight == "not low" ~ 0,
    lowbirthweight == "low" ~ 1
  ))

t.test(lowbirthweight_numeric ~ mature, data = t_test_LBW)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  lowbirthweight_numeric by mature
    ## t = 0.889, df = 166.59, p-value = 0.3753
    ## alternative hypothesis: true difference in means between group mature mom and group younger mom is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.03427078  0.09041460
    ## sample estimates:
    ##  mean in group mature mom mean in group younger mom 
    ##                 0.1353383                 0.1072664

Assuming I did this correctly, there is no difference between younger
and mature mothers for babies with low birth weight (p = .37)

#### Calculate a confidence interval for the difference between the proportions of low birth weight babies between mature and younger mothers. Interpret the interval in the context of the data and explain what it means.

``` r
t_test_LBW <- ncbirths %>%
  mutate(lowbirthweight_numeric = case_when(
    lowbirthweight == "not low" ~ 0,
    lowbirthweight == "low" ~ 1
  ))

mature_low_weight <- t_test_LBW %>%
  filter(mature == "mature mom")
young_low_weight <- t_test_LBW %>%
  filter(mature == "younger mom")


prop_mature_low <- mean(mature_low_weight$lowbirthweight_numeric)
prop_young_low <- mean(young_low_weight$lowbirthweight_numeric)


diff_maturity_lowweight_mean <- prop_mature_low - prop_young_low

diff_maturity_lowweight_sd <- sqrt((1-prop_mature_low)/nrow(mature_low_weight)) + ((1-prop_young_low)/nrow(young_low_weight))


  
ci_lowweight_dif <- data.frame(
    mean_weight = diff_maturity_lowweight_mean,
    se = diff_maturity_lowweight_sd,
    lower = diff_maturity_lowweight_mean - 1.96 * diff_maturity_lowweight_sd,
    upper = diff_maturity_lowweight_mean + 1.96 * diff_maturity_lowweight_sd
  )

ci_lowweight_dif
```

    ##   mean_weight        se      lower     upper
    ## 1  0.02807191 0.0816598 -0.1319813 0.1881251

The CI is (-0.1319813, 0.1881251). Since the CI strattles zero, I know
it’s non-significant. If I had to guess what the CI represented, it
would represent how large the difference is between the probability of a
young mother giving birth to an underweight baby and an older mother
giving birth to an underweight baby.

Commentary: Learned SE when looking at proportions is (1-prop) for each
group instead of of the sd. Might be helpful to rewrite SE for
proportions or give students a hint to google it.
