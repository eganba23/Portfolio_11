Commentary on Lab 12 - Smoking during pregnancy
================
Benjamin Egan

#### Created 4/14/2025

Link to the assignment page, at the time of creation:
<https://web.archive.org/web/20250414135227/https://datascience4psych.github.io/DataScience4Psych/lab12.html>

Link to assignment page as of April 15 2025:
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

#### Construct a 95% confidence interval for the difference between the average weights of babies born to smoking and non-smoking mothers.

\*I did them out of order, this is Q10

``` r
view(ncbirths_clean)

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

#### Run the appropriate hypothesis test, calculate the p-value, and interpret the results in context of the data and the hypothesis test.

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
