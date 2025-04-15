Commentary on Lab 12 - Smoking during pregnancy
================
Benjamin Egan

#### Created 4/14/2025

Link to the assignment page, at the time of creation:
<https://web.archive.org/web/20250414135227/https://datascience4psych.github.io/DataScience4Psych/lab12.html>

Link to assignment page as of April 14 2025:
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

Commentary: You/someone else included a note that says
<!-- Mason make this clearer to why we need to actually do this -->.

- take a bootstrap sample of from the original sample
- calculate this bootstrap sample’s mean
- repeat these two steps a large number of times to create a bootstrap
  distribution of means centered at the observed sample mean,
- shift this distribution to be centered at the null value by
  subtracting / adding X to all bootstrap mean (X = difference between
  mean of bootstrap distribution and null value)
- calculate the p-value as the proportion of bootstrap samples that
  yielded a sample mean at least as extreme as the observed sample mean.

…

Add exercise headings as needed.
