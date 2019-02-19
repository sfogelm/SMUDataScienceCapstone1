Question 1

    breweries = read.csv('/Users/spencerfogelman/Downloads/CaseStudy1_2_2/Breweries.csv', stringsAsFactors = FALSE)
    beers = read.csv('/Users/spencerfogelman/Downloads/CaseStudy1_2_2/Beers.csv', stringsAsFactors = FALSE)
    library(dplyr)

    ## Warning: package 'dplyr' was built under R version 3.4.4

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    head(breweries)

    ##   Brew_ID                      Name          City State
    ## 1       1        NorthGate Brewing    Minneapolis    MN
    ## 2       2 Against the Grain Brewery    Louisville    KY
    ## 3       3  Jack's Abby Craft Lagers    Framingham    MA
    ## 4       4 Mike Hess Brewing Company     San Diego    CA
    ## 5       5   Fort Point Beer Company San Francisco    CA
    ## 6       6     COAST Brewing Company    Charleston    SC

    head(beers)

    ##                  Name Beer_ID   ABV IBU Brewery_id
    ## 1            Pub Beer    1436 0.050  NA        409
    ## 2         Devil's Cup    2265 0.066  NA        178
    ## 3 Rise of the Phoenix    2264 0.071  NA        178
    ## 4            Sinister    2263 0.090  NA        178
    ## 5       Sex and Candy    2262 0.075  NA        178
    ## 6        Black Exodus    2261 0.077  NA        178
    ##                            Style Ounces
    ## 1            American Pale Lager     12
    ## 2        American Pale Ale (APA)     12
    ## 3                   American IPA     12
    ## 4 American Double / Imperial IPA     12
    ## 5                   American IPA     12
    ## 6                  Oatmeal Stout     12

    breweries %>% group_by(State) %>% summarise(Total = n()) 

    ## # A tibble: 51 x 2
    ##    State Total
    ##    <chr> <int>
    ##  1 " AK"     7
    ##  2 " AL"     3
    ##  3 " AR"     2
    ##  4 " AZ"    11
    ##  5 " CA"    39
    ##  6 " CO"    47
    ##  7 " CT"     8
    ##  8 " DC"     1
    ##  9 " DE"     2
    ## 10 " FL"    15
    ## # ... with 41 more rows

Question 2

    merged = merge(x = breweries, y = beers, by.x = 'Brew_ID', by.y= 'Brewery_id', all = TRUE)
    head(merged, 6)

    ##   Brew_ID             Name.x        City State        Name.y Beer_ID   ABV
    ## 1       1 NorthGate Brewing  Minneapolis    MN       Pumpion    2689 0.060
    ## 2       1 NorthGate Brewing  Minneapolis    MN    Stronghold    2688 0.060
    ## 3       1 NorthGate Brewing  Minneapolis    MN   Parapet ESB    2687 0.056
    ## 4       1 NorthGate Brewing  Minneapolis    MN  Get Together    2692 0.045
    ## 5       1 NorthGate Brewing  Minneapolis    MN Maggie's Leap    2691 0.049
    ## 6       1 NorthGate Brewing  Minneapolis    MN    Wall's End    2690 0.048
    ##   IBU                               Style Ounces
    ## 1  38                         Pumpkin Ale     16
    ## 2  25                     American Porter     16
    ## 3  47 Extra Special / Strong Bitter (ESB)     16
    ## 4  50                        American IPA     16
    ## 5  26                  Milk / Sweet Stout     16
    ## 6  19                   English Brown Ale     16

    tail(merged, 6)

    ##      Brew_ID                        Name.x          City State
    ## 2405     556         Ukiah Brewing Company         Ukiah    CA
    ## 2406     557       Butternuts Beer and Ale Garrattsville    NY
    ## 2407     557       Butternuts Beer and Ale Garrattsville    NY
    ## 2408     557       Butternuts Beer and Ale Garrattsville    NY
    ## 2409     557       Butternuts Beer and Ale Garrattsville    NY
    ## 2410     558 Sleeping Lady Brewing Company     Anchorage    AK
    ##                         Name.y Beer_ID   ABV IBU                   Style
    ## 2405             Pilsner Ukiah      98 0.055  NA         German Pilsener
    ## 2406         Porkslap Pale Ale      49 0.043  NA American Pale Ale (APA)
    ## 2407           Snapperhead IPA      51 0.068  NA            American IPA
    ## 2408         Moo Thunder Stout      50 0.049  NA      Milk / Sweet Stout
    ## 2409  Heinnieweisse Weissebier      52 0.049  NA              Hefeweizen
    ## 2410 Urban Wilderness Pale Ale      30 0.049  NA        English Pale Ale
    ##      Ounces
    ## 2405     12
    ## 2406     12
    ## 2407     12
    ## 2408     12
    ## 2409     12
    ## 2410     12

Question 3

    for (i in 1:length(names(merged))){
      column = merged[,i]
      nas = sum(is.na(column))
      print(paste(names(merged)[i], ':', nas))
    }

    ## [1] "Brew_ID : 0"
    ## [1] "Name.x : 0"
    ## [1] "City : 0"
    ## [1] "State : 0"
    ## [1] "Name.y : 0"
    ## [1] "Beer_ID : 0"
    ## [1] "ABV : 62"
    ## [1] "IBU : 1005"
    ## [1] "Style : 0"
    ## [1] "Ounces : 0"

Question 4

    merged %>% group_by(State) %>% summarise(MedianAlcoholContent = median(ABV, na.rm=TRUE),
                                             MedianIBU = median(IBU, na.rm=TRUE))

    ## Warning: package 'bindrcpp' was built under R version 3.4.4

    ## # A tibble: 51 x 3
    ##    State MedianAlcoholContent MedianIBU
    ##    <chr>                <dbl>     <dbl>
    ##  1 " AK"               0.056       46  
    ##  2 " AL"               0.06        43  
    ##  3 " AR"               0.052       39  
    ##  4 " AZ"               0.055       20.5
    ##  5 " CA"               0.058       42  
    ##  6 " CO"               0.0605      40  
    ##  7 " CT"               0.06        29  
    ##  8 " DC"               0.0625      47.5
    ##  9 " DE"               0.055       52  
    ## 10 " FL"               0.057       55  
    ## # ... with 41 more rows

Question 5

    maxABV = max(merged$ABV, na.rm=TRUE)
    maxABV

    ## [1] 0.128

    maxIBU = max(merged$IBU, na.rm=TRUE)
    maxIBU

    ## [1] 138

    merged %>% filter(ABV == maxABV) %>% select(State)

    ##   State
    ## 1    CO

    merged %>% filter(IBU == maxIBU) %>% select(State)

    ##   State
    ## 1    OR

Question 6

    merged %>% select(ABV) %>% summary()

    ##       ABV         
    ##  Min.   :0.00100  
    ##  1st Qu.:0.05000  
    ##  Median :0.05600  
    ##  Mean   :0.05977  
    ##  3rd Qu.:0.06700  
    ##  Max.   :0.12800  
    ##  NA's   :62

Question 7

    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 3.4.4

    ggplot(merged, aes(x=ABV, y=IBU)) + geom_point() + labs(title='Alcohol Content vs Bitterness') +
      theme(plot.title = element_text(hjust = 0.5))

    ## Warning: Removed 1005 rows containing missing values (geom_point).

![](Capstone1_files/figure-markdown_strict/unnamed-chunk-7-1.png)
