Kyadav1\_Assignment2
================
Khushboo Yadav
10/4/2020

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](Kyadav1_Assignment2_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.

# 1.Importing libraries & data from csv file

``` r
library('lattice')
library('ggplot2')
library('FNN') 
library('caret') 
library('e1071') 
library('ISLR') 
library('dplyr') 
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library('fastDummies') 
library('gmodels') 

UniversalBank <- read.csv("~/Downloads/UniversalBank.csv")
summary(UniversalBank)
```

    ##        ID            Age          Experience       Income          ZIP.Code    
    ##  Min.   :   1   Min.   :23.00   Min.   :-3.0   Min.   :  8.00   Min.   : 9307  
    ##  1st Qu.:1251   1st Qu.:35.00   1st Qu.:10.0   1st Qu.: 39.00   1st Qu.:91911  
    ##  Median :2500   Median :45.00   Median :20.0   Median : 64.00   Median :93437  
    ##  Mean   :2500   Mean   :45.34   Mean   :20.1   Mean   : 73.77   Mean   :93152  
    ##  3rd Qu.:3750   3rd Qu.:55.00   3rd Qu.:30.0   3rd Qu.: 98.00   3rd Qu.:94608  
    ##  Max.   :5000   Max.   :67.00   Max.   :43.0   Max.   :224.00   Max.   :96651  
    ##      Family          CCAvg          Education        Mortgage    
    ##  Min.   :1.000   Min.   : 0.000   Min.   :1.000   Min.   :  0.0  
    ##  1st Qu.:1.000   1st Qu.: 0.700   1st Qu.:1.000   1st Qu.:  0.0  
    ##  Median :2.000   Median : 1.500   Median :2.000   Median :  0.0  
    ##  Mean   :2.396   Mean   : 1.938   Mean   :1.881   Mean   : 56.5  
    ##  3rd Qu.:3.000   3rd Qu.: 2.500   3rd Qu.:3.000   3rd Qu.:101.0  
    ##  Max.   :4.000   Max.   :10.000   Max.   :3.000   Max.   :635.0  
    ##  Personal.Loan   Securities.Account   CD.Account         Online      
    ##  Min.   :0.000   Min.   :0.0000     Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.000   1st Qu.:0.0000     1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median :0.000   Median :0.0000     Median :0.0000   Median :1.0000  
    ##  Mean   :0.096   Mean   :0.1044     Mean   :0.0604   Mean   :0.5968  
    ##  3rd Qu.:0.000   3rd Qu.:0.0000     3rd Qu.:0.0000   3rd Qu.:1.0000  
    ##  Max.   :1.000   Max.   :1.0000     Max.   :1.0000   Max.   :1.0000  
    ##    CreditCard   
    ##  Min.   :0.000  
    ##  1st Qu.:0.000  
    ##  Median :0.000  
    ##  Mean   :0.294  
    ##  3rd Qu.:1.000  
    ##  Max.   :1.000

``` r
str(UniversalBank)
```

    ## 'data.frame':    5000 obs. of  14 variables:
    ##  $ ID                : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Age               : int  25 45 39 35 35 37 53 50 35 34 ...
    ##  $ Experience        : int  1 19 15 9 8 13 27 24 10 9 ...
    ##  $ Income            : int  49 34 11 100 45 29 72 22 81 180 ...
    ##  $ ZIP.Code          : int  91107 90089 94720 94112 91330 92121 91711 93943 90089 93023 ...
    ##  $ Family            : int  4 3 1 1 4 4 2 1 3 1 ...
    ##  $ CCAvg             : num  1.6 1.5 1 2.7 1 0.4 1.5 0.3 0.6 8.9 ...
    ##  $ Education         : int  1 1 1 2 2 2 2 3 2 3 ...
    ##  $ Mortgage          : int  0 0 0 0 0 155 0 0 104 0 ...
    ##  $ Personal.Loan     : int  0 0 0 0 0 0 0 0 0 1 ...
    ##  $ Securities.Account: int  1 1 0 0 0 0 0 0 0 0 ...
    ##  $ CD.Account        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Online            : int  0 0 0 0 0 1 1 0 1 0 ...
    ##  $ CreditCard        : int  0 0 0 0 1 0 0 1 0 0 ...

# 2.Dropping ID and zipcode columns

``` r
UniversalBank<-UniversalBank[,c(-1,-5)] 
summary(UniversalBank)
```

    ##       Age          Experience       Income           Family     
    ##  Min.   :23.00   Min.   :-3.0   Min.   :  8.00   Min.   :1.000  
    ##  1st Qu.:35.00   1st Qu.:10.0   1st Qu.: 39.00   1st Qu.:1.000  
    ##  Median :45.00   Median :20.0   Median : 64.00   Median :2.000  
    ##  Mean   :45.34   Mean   :20.1   Mean   : 73.77   Mean   :2.396  
    ##  3rd Qu.:55.00   3rd Qu.:30.0   3rd Qu.: 98.00   3rd Qu.:3.000  
    ##  Max.   :67.00   Max.   :43.0   Max.   :224.00   Max.   :4.000  
    ##      CCAvg          Education        Mortgage     Personal.Loan  
    ##  Min.   : 0.000   Min.   :1.000   Min.   :  0.0   Min.   :0.000  
    ##  1st Qu.: 0.700   1st Qu.:1.000   1st Qu.:  0.0   1st Qu.:0.000  
    ##  Median : 1.500   Median :2.000   Median :  0.0   Median :0.000  
    ##  Mean   : 1.938   Mean   :1.881   Mean   : 56.5   Mean   :0.096  
    ##  3rd Qu.: 2.500   3rd Qu.:3.000   3rd Qu.:101.0   3rd Qu.:0.000  
    ##  Max.   :10.000   Max.   :3.000   Max.   :635.0   Max.   :1.000  
    ##  Securities.Account   CD.Account         Online         CreditCard   
    ##  Min.   :0.0000     Min.   :0.0000   Min.   :0.0000   Min.   :0.000  
    ##  1st Qu.:0.0000     1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.000  
    ##  Median :0.0000     Median :0.0000   Median :1.0000   Median :0.000  
    ##  Mean   :0.1044     Mean   :0.0604   Mean   :0.5968   Mean   :0.294  
    ##  3rd Qu.:0.0000     3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1.000  
    ##  Max.   :1.0000     Max.   :1.0000   Max.   :1.0000   Max.   :1.000

``` r
head(UniversalBank)
```

    ##   Age Experience Income Family CCAvg Education Mortgage Personal.Loan
    ## 1  25          1     49      4   1.6         1        0             0
    ## 2  45         19     34      3   1.5         1        0             0
    ## 3  39         15     11      1   1.0         1        0             0
    ## 4  35          9    100      1   2.7         2        0             0
    ## 5  35          8     45      4   1.0         2        0             0
    ## 6  37         13     29      4   0.4         2      155             0
    ##   Securities.Account CD.Account Online CreditCard
    ## 1                  1          0      0          0
    ## 2                  1          0      0          0
    ## 3                  0          0      0          0
    ## 4                  0          0      0          0
    ## 5                  0          0      0          1
    ## 6                  0          0      1          0

# 3.Identifying Numerical variable and categorical variable

All the variables in the dataset are of Integer/Num data types Numerical
<Data:->  
Age , Experience,Family ,Income,CCAvg,Mortgage

Categorical Variables :-  
1.Education, 2.Personal Loan , Securities Account , CD account , Online
and CreditCard are in Binary format hence not required to be converted
to dummy variable

``` r
UniversalBank_Numerical<-UniversalBank[c(1:3,5,7)]
UniversalBank_Categorical<-UniversalBank[,-c(1:3,5,7)]
head(UniversalBank_Categorical)
```

    ##   Family Education Personal.Loan Securities.Account CD.Account Online
    ## 1      4         1             0                  1          0      0
    ## 2      3         1             0                  1          0      0
    ## 3      1         1             0                  0          0      0
    ## 4      1         2             0                  0          0      0
    ## 5      4         2             0                  0          0      0
    ## 6      4         2             0                  0          0      1
    ##   CreditCard
    ## 1          0
    ## 2          0
    ## 3          0
    ## 4          0
    ## 5          1
    ## 6          0

# 4.Data Transformation

Steps followed:- 1.Dummy Variables creation for Education
(Level=1,2,3)  
2.After creating dummy variables , removed the extra education column  
3.factor conversion of Personal Loan

``` r
UniversalBank<-dummy_cols(UniversalBank,select_columns='Education')
UniversalBank<-UniversalBank[,-6] 
UniversalBank[,'Personal.Loan'] <- as.factor(UniversalBank[,'Personal.Loan'])


summary(UniversalBank)
```

    ##       Age          Experience       Income           Family     
    ##  Min.   :23.00   Min.   :-3.0   Min.   :  8.00   Min.   :1.000  
    ##  1st Qu.:35.00   1st Qu.:10.0   1st Qu.: 39.00   1st Qu.:1.000  
    ##  Median :45.00   Median :20.0   Median : 64.00   Median :2.000  
    ##  Mean   :45.34   Mean   :20.1   Mean   : 73.77   Mean   :2.396  
    ##  3rd Qu.:55.00   3rd Qu.:30.0   3rd Qu.: 98.00   3rd Qu.:3.000  
    ##  Max.   :67.00   Max.   :43.0   Max.   :224.00   Max.   :4.000  
    ##      CCAvg           Mortgage     Personal.Loan Securities.Account
    ##  Min.   : 0.000   Min.   :  0.0   0:4520        Min.   :0.0000    
    ##  1st Qu.: 0.700   1st Qu.:  0.0   1: 480        1st Qu.:0.0000    
    ##  Median : 1.500   Median :  0.0                 Median :0.0000    
    ##  Mean   : 1.938   Mean   : 56.5                 Mean   :0.1044    
    ##  3rd Qu.: 2.500   3rd Qu.:101.0                 3rd Qu.:0.0000    
    ##  Max.   :10.000   Max.   :635.0                 Max.   :1.0000    
    ##    CD.Account         Online         CreditCard     Education_1    
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.0000  
    ##  Median :0.0000   Median :1.0000   Median :0.000   Median :0.0000  
    ##  Mean   :0.0604   Mean   :0.5968   Mean   :0.294   Mean   :0.4192  
    ##  3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1.000   3rd Qu.:1.0000  
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.000   Max.   :1.0000  
    ##   Education_2      Education_3    
    ##  Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median :0.0000   Median :0.0000  
    ##  Mean   :0.2806   Mean   :0.3002  
    ##  3rd Qu.:1.0000   3rd Qu.:1.0000  
    ##  Max.   :1.0000   Max.   :1.0000

# 5.Splitting of data (60%,40%)

``` r
set.seed(15)

Train1_Index_UniBank = createDataPartition(UniversalBank$Personal.Loan,p=0.60, list=FALSE) 
Train1_Data_UniBank = UniversalBank[Train1_Index_UniBank,]
Validation_Data1_UniBank = UniversalBank[-Train1_Index_UniBank,] 
summary(Train1_Index_UniBank)
```

    ##    Resample1   
    ##  Min.   :   1  
    ##  1st Qu.:1211  
    ##  Median :2460  
    ##  Mean   :2466  
    ##  3rd Qu.:3702  
    ##  Max.   :4999

``` r
summary(Train1_Data_UniBank)
```

    ##       Age          Experience        Income           Family     
    ##  Min.   :23.00   Min.   :-3.00   Min.   :  8.00   Min.   :1.000  
    ##  1st Qu.:35.00   1st Qu.:10.00   1st Qu.: 39.00   1st Qu.:1.000  
    ##  Median :45.00   Median :20.00   Median : 63.00   Median :2.000  
    ##  Mean   :45.15   Mean   :19.92   Mean   : 73.65   Mean   :2.398  
    ##  3rd Qu.:55.00   3rd Qu.:30.00   3rd Qu.: 98.00   3rd Qu.:3.000  
    ##  Max.   :67.00   Max.   :43.00   Max.   :224.00   Max.   :4.000  
    ##      CCAvg           Mortgage     Personal.Loan Securities.Account
    ##  Min.   : 0.000   Min.   :  0.0   0:2712        Min.   :0.000     
    ##  1st Qu.: 0.700   1st Qu.:  0.0   1: 288        1st Qu.:0.000     
    ##  Median : 1.500   Median :  0.0                 Median :0.000     
    ##  Mean   : 1.932   Mean   : 57.1                 Mean   :0.103     
    ##  3rd Qu.: 2.500   3rd Qu.:101.2                 3rd Qu.:0.000     
    ##  Max.   :10.000   Max.   :635.0                 Max.   :1.000     
    ##    CD.Account          Online         CreditCard      Education_1    
    ##  Min.   :0.00000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median :0.00000   Median :1.0000   Median :0.0000   Median :0.0000  
    ##  Mean   :0.05467   Mean   :0.5873   Mean   :0.2907   Mean   :0.4163  
    ##  3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
    ##  Max.   :1.00000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
    ##   Education_2      Education_3    
    ##  Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median :0.0000   Median :0.0000  
    ##  Mean   :0.2873   Mean   :0.2963  
    ##  3rd Qu.:1.0000   3rd Qu.:1.0000  
    ##  Max.   :1.0000   Max.   :1.0000

``` r
summary(Validation_Data1_UniBank)
```

    ##       Age          Experience        Income           Family     
    ##  Min.   :23.00   Min.   :-3.00   Min.   :  8.00   Min.   :1.000  
    ##  1st Qu.:36.00   1st Qu.:11.00   1st Qu.: 38.00   1st Qu.:1.000  
    ##  Median :46.00   Median :20.00   Median : 64.00   Median :2.000  
    ##  Mean   :45.62   Mean   :20.38   Mean   : 73.96   Mean   :2.394  
    ##  3rd Qu.:56.00   3rd Qu.:30.00   3rd Qu.: 99.00   3rd Qu.:4.000  
    ##  Max.   :67.00   Max.   :43.00   Max.   :205.00   Max.   :4.000  
    ##      CCAvg           Mortgage      Personal.Loan Securities.Account
    ##  Min.   : 0.000   Min.   :  0.00   0:1808        Min.   :0.0000    
    ##  1st Qu.: 0.700   1st Qu.:  0.00   1: 192        1st Qu.:0.0000    
    ##  Median : 1.600   Median :  0.00                 Median :0.0000    
    ##  Mean   : 1.946   Mean   : 55.59                 Mean   :0.1065    
    ##  3rd Qu.: 2.600   3rd Qu.:100.00                 3rd Qu.:0.0000    
    ##  Max.   :10.000   Max.   :617.00                 Max.   :1.0000    
    ##    CD.Account        Online        CreditCard     Education_1    
    ##  Min.   :0.000   Min.   :0.000   Min.   :0.000   Min.   :0.0000  
    ##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.0000  
    ##  Median :0.000   Median :1.000   Median :0.000   Median :0.0000  
    ##  Mean   :0.069   Mean   :0.611   Mean   :0.299   Mean   :0.4235  
    ##  3rd Qu.:0.000   3rd Qu.:1.000   3rd Qu.:1.000   3rd Qu.:1.0000  
    ##  Max.   :1.000   Max.   :1.000   Max.   :1.000   Max.   :1.0000  
    ##   Education_2      Education_3   
    ##  Min.   :0.0000   Min.   :0.000  
    ##  1st Qu.:0.0000   1st Qu.:0.000  
    ##  Median :0.0000   Median :0.000  
    ##  Mean   :0.2705   Mean   :0.306  
    ##  3rd Qu.:1.0000   3rd Qu.:1.000  
    ##  Max.   :1.0000   Max.   :1.000

``` r
nrow(Train1_Data_UniBank)
```

    ## [1] 3000

``` r
nrow(Validation_Data1_UniBank)
```

    ## [1] 2000

# 6.Normalization of the data

    Normalization of Training and Validation data

``` r
# Copy the original data
train_norm_df <- Train1_Data_UniBank
valid_norm_df <- Validation_Data1_UniBank


# use preProcess() from the caret package to normalize 
norm_values <- preProcess(Train1_Data_UniBank, method=c("center", "scale"))

train_norm_df <- predict(norm_values, Train1_Data_UniBank) 
valid_norm_df <- predict(norm_values, Validation_Data1_UniBank) 

summary(train_norm_df)
```

    ##       Age            Experience            Income            Family       
    ##  Min.   :-1.9325   Min.   :-1.997167   Min.   :-1.4435   Min.   :-1.2237  
    ##  1st Qu.:-0.8857   1st Qu.:-0.864443   1st Qu.:-0.7619   1st Qu.:-1.2237  
    ##  Median :-0.0134   Median : 0.006883   Median :-0.2341   Median :-0.3482  
    ##  Mean   : 0.0000   Mean   : 0.000000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.8589   3rd Qu.: 0.878210   3rd Qu.: 0.5355   3rd Qu.: 0.5273  
    ##  Max.   : 1.9057   Max.   : 2.010934   Max.   : 3.3061   Max.   : 1.4028  
    ##      CCAvg            Mortgage       Personal.Loan Securities.Account
    ##  Min.   :-1.1014   Min.   :-0.5591   0:2712        Min.   :-0.3388   
    ##  1st Qu.:-0.7024   1st Qu.:-0.5591   1: 288        1st Qu.:-0.3388   
    ##  Median :-0.2465   Median :-0.5591                 Median :-0.3388   
    ##  Mean   : 0.0000   Mean   : 0.0000                 Mean   : 0.0000   
    ##  3rd Qu.: 0.3234   3rd Qu.: 0.4322                 3rd Qu.:-0.3388   
    ##  Max.   : 4.5978   Max.   : 5.6581                 Max.   : 2.9506   
    ##    CD.Account          Online          CreditCard      Education_1     
    ##  Min.   :-0.2404   Min.   :-1.1928   Min.   :-0.640   Min.   :-0.8444  
    ##  1st Qu.:-0.2404   1st Qu.:-1.1928   1st Qu.:-0.640   1st Qu.:-0.8444  
    ##  Median :-0.2404   Median : 0.8381   Median :-0.640   Median :-0.8444  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.000   Mean   : 0.0000  
    ##  3rd Qu.:-0.2404   3rd Qu.: 0.8381   3rd Qu.: 1.562   3rd Qu.: 1.1838  
    ##  Max.   : 4.1578   Max.   : 0.8381   Max.   : 1.562   Max.   : 1.1838  
    ##   Education_2       Education_3     
    ##  Min.   :-0.6349   Min.   :-0.6488  
    ##  1st Qu.:-0.6349   1st Qu.:-0.6488  
    ##  Median :-0.6349   Median :-0.6488  
    ##  Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 1.5746   3rd Qu.: 1.5407  
    ##  Max.   : 1.5746   Max.   : 1.5407

``` r
var(train_norm_df)
```

    ##                              Age   Experience       Income       Family
    ## Age                 1.000000e+00  0.994065572 -0.054288537 -0.060132228
    ## Experience          9.940656e-01  1.000000000 -0.045231432 -0.067273776
    ## Income             -5.428854e-02 -0.045231432  1.000000000 -0.162377043
    ## Family             -6.013223e-02 -0.067273776 -0.162377043  1.000000018
    ## CCAvg              -4.215627e-02 -0.039673641  0.653849958 -0.116418233
    ## Mortgage           -1.966273e-02 -0.016913289  0.187290732 -0.024805674
    ## Personal.Loan      -1.007146e-02 -0.010234174  0.149293235  0.013274679
    ## Securities.Account  4.946506e-05 -0.001011980 -0.007043089  0.035646129
    ## CD.Account          1.253534e-03  0.001400007  0.178745395  0.009992686
    ## Online              2.027579e-02  0.019129347  0.011992932 -0.002779808
    ## CreditCard          1.255342e-02  0.013491522  0.002623945  0.004650538
    ## Education_1        -1.032009e-02  0.016539653  0.208980842 -0.114091670
    ## Education_2        -2.310138e-02 -0.024451692 -0.118601665  0.113659355
    ## Education_3         3.403359e-02  0.006376175 -0.108067051  0.010530322
    ##                           CCAvg     Mortgage Personal.Loan Securities.Account
    ## Age                -0.042156270 -0.019662730  -0.010071463       4.946506e-05
    ## Experience         -0.039673641 -0.016913289  -0.010234174      -1.011980e-03
    ## Income              0.653849958  0.187290732   0.149293235      -7.043089e-03
    ## Family             -0.116418233 -0.024805674   0.013274679       3.564613e-02
    ## CCAvg               1.000000001  0.118215533   0.108956050       1.021513e-02
    ## Mortgage            0.118215533  0.999999974   0.034188246      -1.645213e-02
    ## Personal.Loan       0.108956050  0.034188246   0.086812938       6.949467e-03
    ## Securities.Account  0.010215134 -0.016452129   0.006949467       1.000000e+00
    ## CD.Account          0.135662916  0.095514032   0.095701292       3.140830e-01
    ## Online              0.001365652 -0.025504136   0.001251443       1.896512e-02
    ## CreditCard         -0.017440212 -0.014509508  -0.001991214      -2.129182e-02
    ## Education_1         0.161830070  0.051122531  -0.044571770       3.009946e-03
    ## Education_2        -0.085296368 -0.046145933   0.023758429      -1.904804e-03
    ## Education_3        -0.090171714 -0.009458175   0.024571966      -1.361674e-03
    ##                      CD.Account       Online   CreditCard  Education_1
    ## Age                 0.001253534  0.020275792  0.012553419 -0.010320085
    ## Experience          0.001400007  0.019129347  0.013491522  0.016539653
    ## Income              0.178745395  0.011992932  0.002623945  0.208980842
    ## Family              0.009992686 -0.002779808  0.004650538 -0.114091670
    ## CCAvg               0.135662916  0.001365652 -0.017440212  0.161830070
    ## Mortgage            0.095514032 -0.025504136 -0.014509508  0.051122531
    ## Personal.Loan       0.095701292  0.001251443 -0.001991214 -0.044571770
    ## Securities.Account  0.314083032  0.018965116 -0.021291815  0.003009946
    ## CD.Account          0.999999981  0.165829322  0.262637355 -0.030574454
    ## Online              0.165829322  0.999999815 -0.007686249  0.006071855
    ## CreditCard          0.262637355 -0.007686249  1.000000020  0.007382459
    ## Education_1        -0.030574454  0.006071855  0.007382459  1.000000000
    ## Education_2         0.009323499  0.011548957 -0.005766584 -0.536275484
    ## Education_3         0.023766279 -0.017999390 -0.002254949 -0.548081221
    ##                     Education_2  Education_3
    ## Age                -0.023101384  0.034033586
    ## Experience         -0.024451692  0.006376175
    ## Income             -0.118601665 -0.108067051
    ## Family              0.113659355  0.010530322
    ## CCAvg              -0.085296368 -0.090171714
    ## Mortgage           -0.046145933 -0.009458175
    ## Personal.Loan       0.023758429  0.024571966
    ## Securities.Account -0.001904804 -0.001361674
    ## CD.Account          0.009323499  0.023766279
    ## Online              0.011548957 -0.017999390
    ## CreditCard         -0.005766584 -0.002254949
    ## Education_1        -0.536275484 -0.548081221
    ## Education_2         1.000000068 -0.412056314
    ## Education_3        -0.412056314  1.000000056

``` r
summary(valid_norm_df)
```

    ##       Age             Experience            Income              Family         
    ##  Min.   :-1.93249   Min.   :-1.997167   Min.   :-1.443519   Min.   :-1.223658  
    ##  1st Qu.:-0.79849   1st Qu.:-0.777310   1st Qu.:-0.783849   1st Qu.:-1.223658  
    ##  Median : 0.07383   Median : 0.006883   Median :-0.212135   Median :-0.348157  
    ##  Mean   : 0.04029   Mean   : 0.039994   Mean   : 0.006974   Mean   :-0.002772  
    ##  3rd Qu.: 0.94614   3rd Qu.: 0.878210   3rd Qu.: 0.557480   3rd Qu.: 1.402844  
    ##  Max.   : 1.90568   Max.   : 2.010934   Max.   : 2.888313   Max.   : 1.402844  
    ##      CCAvg              Mortgage        Personal.Loan Securities.Account
    ##  Min.   :-1.101360   Min.   :-0.55909   0:1808        Min.   :-0.33881  
    ##  1st Qu.:-0.702417   1st Qu.:-0.55909   1: 192        1st Qu.:-0.33881  
    ##  Median :-0.189490   Median :-0.55909                 Median :-0.33881  
    ##  Mean   : 0.007767   Mean   :-0.01481                 Mean   : 0.01151  
    ##  3rd Qu.: 0.380428   3rd Qu.: 0.41999                 3rd Qu.:-0.33881  
    ##  Max.   : 4.597824   Max.   : 5.48183                 Max.   : 2.95057  
    ##    CD.Account           Online           CreditCard        Education_1      
    ##  Min.   :-0.24043   Min.   :-1.19281   Min.   :-0.64003   Min.   :-0.84443  
    ##  1st Qu.:-0.24043   1st Qu.:-1.19281   1st Qu.:-0.64003   1st Qu.:-0.84443  
    ##  Median :-0.24043   Median : 0.83808   Median :-0.64003   Median :-0.84443  
    ##  Mean   : 0.06304   Mean   : 0.04806   Mean   : 0.01835   Mean   : 0.01454  
    ##  3rd Qu.:-0.24043   3rd Qu.: 0.83808   3rd Qu.: 1.56191   3rd Qu.: 1.18383  
    ##  Max.   : 4.15775   Max.   : 0.83808   Max.   : 1.56191   Max.   : 1.18383  
    ##   Education_2        Education_3      
    ##  Min.   :-0.63486   Min.   :-0.64884  
    ##  1st Qu.:-0.63486   1st Qu.:-0.64884  
    ##  Median :-0.63486   Median :-0.64884  
    ##  Mean   :-0.03719   Mean   : 0.02117  
    ##  3rd Qu.: 1.57463   3rd Qu.: 1.54071  
    ##  Max.   : 1.57463   Max.   : 1.54071

``` r
var(valid_norm_df)
```

    ##                              Age    Experience       Income       Family
    ## Age                 9.992527e-01  0.9919256808 -0.058618748 -0.026346998
    ## Experience          9.919257e-01  0.9957042228 -0.050113981 -0.030980191
    ## Income             -5.861875e-02 -0.0501139809  1.062040956 -0.156977120
    ## Family             -2.634700e-02 -0.0309801915 -0.156977120  1.024457987
    ## CCAvg              -6.648800e-02 -0.0653012062  0.647735298 -0.098812999
    ## Mortgage           -1.367241e-03 -0.0006023911  0.240409449 -0.013969200
    ## Personal.Loan       9.417999e-03  0.0098962423  0.150752940  0.025514337
    ## Securities.Account -1.449965e-03 -0.0018553036  0.003855722 -0.002922344
    ## CD.Account          1.766886e-02  0.0234958150  0.181807378  0.022265771
    ## Online              2.557559e-03  0.0047483449  0.017634567  0.030171564
    ## CreditCard         -6.629966e-06  0.0018005517 -0.010070513  0.022266582
    ## Education_1        -5.437311e-02 -0.0324334295  0.238795124 -0.127155094
    ## Education_2        -4.819415e-03 -0.0075678298 -0.144508658  0.176675984
    ## Education_3         6.347259e-02  0.0425119506 -0.114578893 -0.037815368
    ##                           CCAvg      Mortgage Personal.Loan Securities.Account
    ## Age                -0.066487996 -0.0013672407   0.009417999       -0.001449965
    ## Experience         -0.065301206 -0.0006023911   0.009896242       -0.001855304
    ## Income              0.647735298  0.2404094487   0.150752940        0.003855722
    ## Family             -0.098812999 -0.0139692002   0.025514337       -0.002922344
    ## CCAvg               0.980612518  0.0953335957   0.105777285        0.022420583
    ## Mortgage            0.095333596  0.9796970318   0.052967393        0.011229410
    ## Personal.Loan       0.105777285  0.0529673925   0.086827414        0.005844844
    ## Securities.Account  0.022420583  0.0112294099   0.005844844        1.030117819
    ## CD.Account          0.152535871  0.0903321181   0.100663287        0.364055568
    ## Online             -0.011234242  0.0238148848   0.002730876        0.002863956
    ## CreditCard          0.009362167  0.0038639896   0.005058176       -0.006112514
    ## Education_1         0.148503735  0.0302195919  -0.042931427        0.012664239
    ## Education_2        -0.095325957 -0.0097395642   0.016650179        0.015937213
    ## Education_3        -0.065846647 -0.0229708899   0.029845284       -0.029464627
    ##                      CD.Account       Online    CreditCard  Education_1
    ## Age                 0.017668855  0.002557559 -6.629966e-06 -0.054373106
    ## Experience          0.023495815  0.004748345  1.800552e-03 -0.032433429
    ## Income              0.181807378  0.017634567 -1.007051e-02  0.238795124
    ## Family              0.022265771  0.030171564  2.226658e-02 -0.127155094
    ## CCAvg               0.152535871 -0.011234242  9.362167e-03  0.148503735
    ## Mortgage            0.090332118  0.023814885  3.863990e-03  0.030219592
    ## Personal.Loan       0.100663287  0.002730876  5.058176e-03 -0.042931427
    ## Securities.Account  0.364055568  0.002863956 -6.112514e-03  0.012664239
    ## CD.Account          1.243264723  0.208591220  3.378589e-01  0.006948230
    ## Online              0.208591220  0.980796946  2.152499e-02 -0.001065339
    ## CreditCard          0.337858868  0.021524988  1.016754e+00  0.026244817
    ## Education_1         0.006948230 -0.001065339  2.624482e-02  1.004891248
    ## Education_2         0.003261932  0.034678836 -2.131758e-02 -0.513634096
    ## Education_3        -0.010733221 -0.033215792 -7.206535e-03 -0.575798443
    ##                     Education_2  Education_3
    ## Age                -0.004819415  0.063472586
    ## Experience         -0.007567830  0.042511951
    ## Income             -0.144508658 -0.114578893
    ## Family              0.176675984 -0.037815368
    ## CCAvg              -0.095325957 -0.065846647
    ## Mortgage           -0.009739564 -0.022970890
    ## Personal.Loan       0.016650179  0.029845284
    ## Securities.Account  0.015937213 -0.029464627
    ## CD.Account          0.003261932 -0.010733221
    ## Online              0.034678836 -0.033215792
    ## CreditCard         -0.021317582 -0.007206535
    ## Education_1        -0.513634096 -0.575798443
    ## Education_2         0.963812491 -0.400637101
    ## Education_3        -0.400637101  1.018605107

``` r
class(train_norm_df)
```

    ## [1] "data.frame"

# 7.Performing knn clasification with all predictors

1.  performed knn clasification within the range of 1 to 14  
2.  plotted graph to better understand the result  
3.  performed confusion matrix for validation dataset

<!-- end list -->

``` r
set.seed(120)

accuracy_knn <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

# compute knn for different k on validation.
for(i in 1:14) {
  knn_pred <- knn(train_norm_df[,c(1:6,8:14)], valid_norm_df[,c(1:6,8:14)], 
                  cl = train_norm_df[,7], k = i)
  accuracy_knn[i, 2] <- confusionMatrix(knn_pred,valid_norm_df[,7])$overall[1] 
}

accuracy_knn
```

    ##     k accuracy
    ## 1   1   0.9510
    ## 2   2   0.9550
    ## 3   3   0.9595
    ## 4   4   0.9595
    ## 5   5   0.9605
    ## 6   6   0.9545
    ## 7   7   0.9570
    ## 8   8   0.9530
    ## 9   9   0.9535
    ## 10 10   0.9500
    ## 11 11   0.9515
    ## 12 12   0.9485
    ## 13 13   0.9505
    ## 14 14   0.9485

``` r
plot(accuracy_knn,type='o')
```

![](Kyadav1_Assignment2_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# 8.Best K value selection

With the help of graph and the table data , PFB the points I have
considered to analyze values for k and choose the best k=5.

1.  Higher the value of accuracy is essential part of choosing best k
2.  Small value of k will give more noise.
3.  Odd value of k also helps in minimizing the noise
4.  The graph goes down after K=5 The best choice of k which balances
    between overfitting is k=5.

\#Confusion Matrix for validation set using best K (k=5)

``` r
knn_predv5 <- knn(train_norm_df[,c(1:6,8:14)], valid_norm_df[,c(1:6,8:14)], 
                  cl = train_norm_df[,7], k = 5)
confusionMatrix(knn_predv5,valid_norm_df[,7],dnn=c("Actual","Prediction"))
```

    ## Confusion Matrix and Statistics
    ## 
    ##       Prediction
    ## Actual    0    1
    ##      0 1799   70
    ##      1    9  122
    ##                                          
    ##                Accuracy : 0.9605         
    ##                  95% CI : (0.951, 0.9686)
    ##     No Information Rate : 0.904          
    ##     P-Value [Acc > NIR] : < 2.2e-16      
    ##                                          
    ##                   Kappa : 0.7348         
    ##                                          
    ##  Mcnemar's Test P-Value : 1.473e-11      
    ##                                          
    ##             Sensitivity : 0.9950         
    ##             Specificity : 0.6354         
    ##          Pos Pred Value : 0.9625         
    ##          Neg Pred Value : 0.9313         
    ##              Prevalence : 0.9040         
    ##          Detection Rate : 0.8995         
    ##    Detection Prevalence : 0.9345         
    ##       Balanced Accuracy : 0.8152         
    ##                                          
    ##        'Positive' Class : 0              
    ## 

# Error Types

There are 2 types of error :- 1.False Positive: Type I Error  
2.False negative: Type II Error  
The confusion matrix has correctly predicted 1799 bank customer who does
not  
take the loan and 122 bank customer who will take the personal loan.  
However, it is observed that Type I Error is ‘9’ and  
Type II Error is ‘70’. It means that confusion matrix falsely predicted
9 customers accepting the personal loan and falsely predicted 70
customers to not accepted the Personal loan.

# 9.Merging the Train set and Validation set

Merging the Train and Validation dataset to perform KNN classification
with new customer dataset

``` r
UniversalBank_combined<-rbind.data.frame(train_norm_df,valid_norm_df)
nrow(UniversalBank_combined)
```

    ## [1] 5000

``` r
summary(UniversalBank_combined)
```

    ##       Age             Experience            Income             Family         
    ##  Min.   :-1.93249   Min.   :-1.997167   Min.   :-1.44352   Min.   :-1.223658  
    ##  1st Qu.:-0.88572   1st Qu.:-0.864443   1st Qu.:-0.76186   1st Qu.:-1.223658  
    ##  Median :-0.01340   Median : 0.006883   Median :-0.21213   Median :-0.348157  
    ##  Mean   : 0.01611   Mean   : 0.015998   Mean   : 0.00279   Mean   :-0.001109  
    ##  3rd Qu.: 0.85891   3rd Qu.: 0.878210   3rd Qu.: 0.53549   3rd Qu.: 0.527343  
    ##  Max.   : 1.90568   Max.   : 2.010934   Max.   : 3.30610   Max.   : 1.402844  
    ##      CCAvg              Mortgage         Personal.Loan Securities.Account 
    ##  Min.   :-1.101360   Min.   :-0.559090   0:4520        Min.   :-0.338805  
    ##  1st Qu.:-0.702417   1st Qu.:-0.559090   1: 480        1st Qu.:-0.338805  
    ##  Median :-0.246482   Median :-0.559090                 Median :-0.338805  
    ##  Mean   : 0.003107   Mean   :-0.005922                 Mean   : 0.004605  
    ##  3rd Qu.: 0.323436   3rd Qu.: 0.429780                 3rd Qu.:-0.338805  
    ##  Max.   : 4.597824   Max.   : 5.658061                 Max.   : 2.950565  
    ##    CD.Account           Online           CreditCard        Education_1       
    ##  Min.   :-0.24043   Min.   :-1.19281   Min.   :-0.64003   Min.   :-0.844434  
    ##  1st Qu.:-0.24043   1st Qu.:-1.19281   1st Qu.:-0.64003   1st Qu.:-0.844434  
    ##  Median :-0.24043   Median : 0.83808   Median :-0.64003   Median :-0.844434  
    ##  Mean   : 0.02522   Mean   : 0.01923   Mean   : 0.00734   Mean   : 0.005814  
    ##  3rd Qu.:-0.24043   3rd Qu.: 0.83808   3rd Qu.: 1.56191   3rd Qu.: 1.183830  
    ##  Max.   : 4.15775   Max.   : 0.83808   Max.   : 1.56191   Max.   : 1.183830  
    ##   Education_2        Education_3       
    ##  Min.   :-0.63486   Min.   :-0.648835  
    ##  1st Qu.:-0.63486   1st Qu.:-0.648835  
    ##  Median :-0.63486   Median :-0.648835  
    ##  Mean   :-0.01488   Mean   : 0.008466  
    ##  3rd Qu.: 1.57463   3rd Qu.: 1.540710  
    ##  Max.   : 1.57463   Max.   : 1.540710

# 10.New Customer dataframe creation

``` r
New_Customer<-data.frame(
'Age' = 40, 
'Experience' = 10,
'Income' = 84,
'Family' = 2, 
'CCAvg' = 2,
'Mortgage' = 0, 
'personal.loan'<-factor(c(0,1)),
'Securities.Account' = 0, 
'CD.Account' = 0,
'Online' = 1,
'CreditCard' = 1,
'Education_1' = 0,
'Education_2' = 1,
'Education_3' = 0
 )

summary(New_Customer)
```

    ##       Age       Experience     Income       Family      CCAvg      Mortgage
    ##  Min.   :40   Min.   :10   Min.   :84   Min.   :2   Min.   :2   Min.   :0  
    ##  1st Qu.:40   1st Qu.:10   1st Qu.:84   1st Qu.:2   1st Qu.:2   1st Qu.:0  
    ##  Median :40   Median :10   Median :84   Median :2   Median :2   Median :0  
    ##  Mean   :40   Mean   :10   Mean   :84   Mean   :2   Mean   :2   Mean   :0  
    ##  3rd Qu.:40   3rd Qu.:10   3rd Qu.:84   3rd Qu.:2   3rd Qu.:2   3rd Qu.:0  
    ##  Max.   :40   Max.   :10   Max.   :84   Max.   :2   Max.   :2   Max.   :0  
    ##  X.personal.loan.....factor.c.0..1.. Securities.Account   CD.Account
    ##  0:1                                 Min.   :0          Min.   :0   
    ##  1:1                                 1st Qu.:0          1st Qu.:0   
    ##                                      Median :0          Median :0   
    ##                                      Mean   :0          Mean   :0   
    ##                                      3rd Qu.:0          3rd Qu.:0   
    ##                                      Max.   :0          Max.   :0   
    ##      Online    CreditCard  Education_1  Education_2  Education_3
    ##  Min.   :1   Min.   :1    Min.   :0    Min.   :1    Min.   :0   
    ##  1st Qu.:1   1st Qu.:1    1st Qu.:0    1st Qu.:1    1st Qu.:0   
    ##  Median :1   Median :1    Median :0    Median :1    Median :0   
    ##  Mean   :1   Mean   :1    Mean   :0    Mean   :1    Mean   :0   
    ##  3rd Qu.:1   3rd Qu.:1    3rd Qu.:0    3rd Qu.:1    3rd Qu.:0   
    ##  Max.   :1   Max.   :1    Max.   :0    Max.   :1    Max.   :0

``` r
str(New_Customer)
```

    ## 'data.frame':    2 obs. of  14 variables:
    ##  $ Age                                : num  40 40
    ##  $ Experience                         : num  10 10
    ##  $ Income                             : num  84 84
    ##  $ Family                             : num  2 2
    ##  $ CCAvg                              : num  2 2
    ##  $ Mortgage                           : num  0 0
    ##  $ X.personal.loan.....factor.c.0..1..: Factor w/ 2 levels "0","1": 1 2
    ##  $ Securities.Account                 : num  0 0
    ##  $ CD.Account                         : num  0 0
    ##  $ Online                             : num  1 1
    ##  $ CreditCard                         : num  1 1
    ##  $ Education_1                        : num  0 0
    ##  $ Education_2                        : num  1 1
    ##  $ Education_3                        : num  0 0

# 11.Data Re- normalization

To perform KNN classification The combined data(Train+Validation) and
New customer data set will be normalized

``` r
Train_UniversalBank_combined_norm_df <- UniversalBank_combined
Test_New_Customer_norm_df <- New_Customer

norm2_values <- preProcess(UniversalBank_combined, method=c("center", "scale"))

Train_UniversalBank_combined_norm_df <- predict(norm2_values, UniversalBank_combined) 
Test_New_Customer_norm_df <- predict(norm2_values, New_Customer) 

summary(Train_UniversalBank_combined_norm_df)
```

    ##       Age             Experience            Income            Family       
    ##  Min.   :-1.94871   Min.   :-2.014710   Min.   :-1.4288   Min.   :-1.2167  
    ##  1st Qu.:-0.90188   1st Qu.:-0.881116   1st Qu.:-0.7554   1st Qu.:-1.2167  
    ##  Median :-0.02952   Median :-0.009121   Median :-0.2123   Median :-0.3454  
    ##  Mean   : 0.00000   Mean   : 0.000000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.84284   3rd Qu.: 0.862874   3rd Qu.: 0.5263   3rd Qu.: 0.5259  
    ##  Max.   : 1.88967   Max.   : 1.996468   Max.   : 3.2634   Max.   : 1.3973  
    ##      CCAvg            Mortgage       Personal.Loan Securities.Account
    ##  Min.   :-1.1089   Min.   :-0.5555   0:4520        Min.   :-0.3414   
    ##  1st Qu.:-0.7083   1st Qu.:-0.5555   1: 480        1st Qu.:-0.3414   
    ##  Median :-0.2506   Median :-0.5555                 Median :-0.3414   
    ##  Mean   : 0.0000   Mean   : 0.0000                 Mean   : 0.0000   
    ##  3rd Qu.: 0.3216   3rd Qu.: 0.4375                 3rd Qu.:-0.3414   
    ##  Max.   : 4.6131   Max.   : 5.6875                 Max.   : 2.9286   
    ##    CD.Account          Online          CreditCard       Education_1     
    ##  Min.   :-0.2535   Min.   :-1.2165   Min.   :-0.6452   Min.   :-0.8495  
    ##  1st Qu.:-0.2535   1st Qu.:-1.2165   1st Qu.:-0.6452   1st Qu.:-0.8495  
    ##  Median :-0.2535   Median : 0.8219   Median :-0.6452   Median :-0.8495  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.:-0.2535   3rd Qu.: 0.8219   3rd Qu.: 1.5495   3rd Qu.: 1.1770  
    ##  Max.   : 3.9438   Max.   : 0.8219   Max.   : 1.5495   Max.   : 1.1770  
    ##   Education_2       Education_3     
    ##  Min.   :-0.6245   Min.   :-0.6549  
    ##  1st Qu.:-0.6245   1st Qu.:-0.6549  
    ##  Median :-0.6245   Median :-0.6549  
    ##  Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 1.6010   3rd Qu.: 1.5266  
    ##  Max.   : 1.6010   Max.   : 1.5266

``` r
summary(Test_New_Customer_norm_df)
```

    ##       Age          Experience        Income          Family     
    ##  Min.   :39.99   Min.   :9.992   Min.   :82.98   Min.   :1.992  
    ##  1st Qu.:39.99   1st Qu.:9.992   1st Qu.:82.98   1st Qu.:1.992  
    ##  Median :39.99   Median :9.992   Median :82.98   Median :1.992  
    ##  Mean   :39.99   Mean   :9.992   Mean   :82.98   Mean   :1.992  
    ##  3rd Qu.:39.99   3rd Qu.:9.992   3rd Qu.:82.98   3rd Qu.:1.992  
    ##  Max.   :39.99   Max.   :9.992   Max.   :82.98   Max.   :1.992  
    ##      CCAvg          Mortgage        X.personal.loan.....factor.c.0..1..
    ##  Min.   :2.005   Min.   :0.005947   0:1                                
    ##  1st Qu.:2.005   1st Qu.:0.005947   1:1                                
    ##  Median :2.005   Median :0.005947                                      
    ##  Mean   :2.005   Mean   :0.005947                                      
    ##  3rd Qu.:2.005   3rd Qu.:0.005947                                      
    ##  Max.   :2.005   Max.   :0.005947                                      
    ##  Securities.Account    CD.Account           Online         CreditCard    
    ##  Min.   :-0.004578   Min.   :-0.02406   Min.   :0.9844   Min.   :0.9894  
    ##  1st Qu.:-0.004578   1st Qu.:-0.02406   1st Qu.:0.9844   1st Qu.:0.9894  
    ##  Median :-0.004578   Median :-0.02406   Median :0.9844   Median :0.9894  
    ##  Mean   :-0.004578   Mean   :-0.02406   Mean   :0.9844   Mean   :0.9894  
    ##  3rd Qu.:-0.004578   3rd Qu.:-0.02406   3rd Qu.:0.9844   3rd Qu.:0.9894  
    ##  Max.   :-0.004578   Max.   :-0.02406   Max.   :0.9844   Max.   :0.9894  
    ##   Education_1         Education_2     Education_3       
    ##  Min.   :-0.005809   Min.   :1.022   Min.   :-0.008435  
    ##  1st Qu.:-0.005809   1st Qu.:1.022   1st Qu.:-0.008435  
    ##  Median :-0.005809   Median :1.022   Median :-0.008435  
    ##  Mean   :-0.005809   Mean   :1.022   Mean   :-0.008435  
    ##  3rd Qu.:-0.005809   3rd Qu.:1.022   3rd Qu.:-0.008435  
    ##  Max.   :-0.005809   Max.   :1.022   Max.   :-0.008435

# 12.performing KNN classification for new Customer to Predict Personal Loan Acceptance

For K=1

``` r
knn_pred_cust_k1 <- knn(Train_UniversalBank_combined_norm_df[,-7], Test_New_Customer_norm_df[,-7], 
                cl = Train_UniversalBank_combined_norm_df[,7], k = 1)
print(knn_pred_cust_k1)
```

    ## [1] 1 1
    ## attr(,"nn.index")
    ##      [,1]
    ## [1,] 3356
    ## [2,] 3356
    ## attr(,"nn.dist")
    ##          [,1]
    ## [1,] 89.62198
    ## [2,] 89.62198
    ## Levels: 1

for K=5

``` r
knn_prediction_5 <- knn(Train_UniversalBank_combined_norm_df[,-7], Test_New_Customer_norm_df[,-7], 
                cl = Train_UniversalBank_combined_norm_df[,7], k = 5)
print(knn_prediction_5)
```

    ## [1] 1 1
    ## attr(,"nn.index")
    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,] 3356 3252 4693 4360  573
    ## [2,] 3356 3252 4693 4360  573
    ## attr(,"nn.dist")
    ##          [,1]     [,2]     [,3]     [,4]     [,5]
    ## [1,] 89.62198 89.65336 89.67676 89.70146 89.71947
    ## [2,] 89.62198 89.65336 89.67676 89.70146 89.71947
    ## Levels: 1

# Prediction for New Customer

As per the results for k=1 and k=5 , We can predict that new customer
will accept personal loan .

# 13.Re-partitioned the data(50%:30%:20%)

Train data , Validation data , Test data , Traval(Train+Validation)

``` r
set.seed(156)

Test3_Index_UniBank = createDataPartition(UniversalBank$Personal.Loan,p=0.20, list=FALSE) #test data
Test3_Data_UniBank = UniversalBank[Test3_Index_UniBank,]
Traval_Data_UniBank = UniversalBank[-Test3_Index_UniBank,] #left 80 prcnt of data
Train3_Index_UniBank = createDataPartition(Traval_Data_UniBank$Personal.Loan,p=0.625, list=FALSE) #50prcnt of the 80 prcnt data
Train3_Data_UniBank = Traval_Data_UniBank[Train3_Index_UniBank,]
#left 30 prcnt data
Validation3_Data_UniBank=Traval_Data_UniBank[-Train3_Index_UniBank,]

nrow(Train3_Data_UniBank)
```

    ## [1] 2500

``` r
nrow(Validation3_Data_UniBank)
```

    ## [1] 1500

``` r
nrow(Test3_Data_UniBank)
```

    ## [1] 1000

``` r
nrow(Traval_Data_UniBank)
```

    ## [1] 4000

# 14\. Normalization of the Repartitioned data

``` r
train3_norm_df <- Train3_Data_UniBank
valid3_norm_df <- Validation3_Data_UniBank
test3_norm_df<-Test3_Data_UniBank
traval_norm_df<-Traval_Data_UniBank

norm_values3 <- preProcess(Train3_Data_UniBank, method=c("center", "scale"))

train3_norm_df <- predict(norm_values3, Train3_Data_UniBank) 
valid3_norm_df <- predict(norm_values3, Validation3_Data_UniBank) 
test3_norm_df<- predict(norm_values3,Test3_Data_UniBank)
traval_norm_df<- predict(norm_values3,Traval_Data_UniBank)
summary(train3_norm_df)
```

    ##       Age             Experience           Income            Family       
    ##  Min.   :-1.94259   Min.   :-2.00485   Min.   :-1.4436   Min.   :-1.1868  
    ##  1st Qu.:-0.90407   1st Qu.:-0.88143   1st Qu.:-0.7657   1st Qu.:-1.1868  
    ##  Median : 0.04791   Median :-0.01725   Median :-0.1971   Median :-0.3200  
    ##  Mean   : 0.00000   Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.91335   3rd Qu.: 0.84693   3rd Qu.: 0.5245   3rd Qu.: 0.5468  
    ##  Max.   : 1.86533   Max.   : 1.97036   Max.   : 2.8424   Max.   : 1.4135  
    ##      CCAvg            Mortgage       Personal.Loan Securities.Account
    ##  Min.   :-1.1115   Min.   :-0.5495   0:2260        Min.   :-0.3377   
    ##  1st Qu.:-0.7133   1st Qu.:-0.5495   1: 240        1st Qu.:-0.3377   
    ##  Median :-0.2013   Median :-0.5495                 Median :-0.3377   
    ##  Mean   : 0.0000   Mean   : 0.0000                 Mean   : 0.0000   
    ##  3rd Qu.: 0.3675   3rd Qu.: 0.4293                 3rd Qu.:-0.3377   
    ##  Max.   : 4.1788   Max.   : 5.2632                 Max.   : 2.9601   
    ##    CD.Account          Online          CreditCard       Education_1     
    ##  Min.   :-0.2597   Min.   :-1.2578   Min.   :-0.6322   Min.   :-0.8627  
    ##  1st Qu.:-0.2597   1st Qu.:-1.2578   1st Qu.:-0.6322   1st Qu.:-0.8627  
    ##  Median :-0.2597   Median : 0.7947   Median :-0.6322   Median :-0.8627  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.:-0.2597   3rd Qu.: 0.7947   3rd Qu.: 1.5813   3rd Qu.: 1.1587  
    ##  Max.   : 3.8493   Max.   : 0.7947   Max.   : 1.5813   Max.   : 1.1587  
    ##   Education_2       Education_3     
    ##  Min.   :-0.6136   Min.   :-0.6539  
    ##  1st Qu.:-0.6136   1st Qu.:-0.6539  
    ##  Median :-0.6136   Median :-0.6539  
    ##  Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 1.6291   3rd Qu.: 1.5287  
    ##  Max.   : 1.6291   Max.   : 1.5287

``` r
summary(valid3_norm_df)
```

    ##       Age             Experience           Income            Family        
    ##  Min.   :-1.94259   Min.   :-2.00485   Min.   :-1.4436   Min.   :-1.18679  
    ##  1st Qu.:-0.90407   1st Qu.:-0.88143   1st Qu.:-0.7876   1st Qu.:-1.18679  
    ##  Median :-0.03863   Median :-0.01725   Median :-0.2627   Median :-0.32001  
    ##  Mean   :-0.04227   Mean   :-0.04075   Mean   :-0.0300   Mean   : 0.03652  
    ##  3rd Qu.: 0.74026   3rd Qu.: 0.76051   3rd Qu.: 0.4589   3rd Qu.: 0.54676  
    ##  Max.   : 1.86533   Max.   : 1.88394   Max.   : 3.2798   Max.   : 1.41354  
    ##      CCAvg             Mortgage        Personal.Loan Securities.Account 
    ##  Min.   :-1.11148   Min.   :-0.54951   0:1356        Min.   :-0.337693  
    ##  1st Qu.:-0.71329   1st Qu.:-0.54951   1: 144        1st Qu.:-0.337693  
    ##  Median :-0.25821   Median :-0.54951                 Median :-0.337693  
    ##  Mean   :-0.02154   Mean   : 0.01597                 Mean   :-0.005716  
    ##  3rd Qu.: 0.31064   3rd Qu.: 0.46172                 3rd Qu.:-0.337693  
    ##  Max.   : 4.57701   Max.   : 5.56279                 Max.   : 2.960088  
    ##    CD.Account           Online           CreditCard         Education_1      
    ##  Min.   :-0.25969   Min.   :-1.25778   Min.   :-0.632152   Min.   :-0.86272  
    ##  1st Qu.:-0.25969   1st Qu.:-1.25778   1st Qu.:-0.632152   1st Qu.:-0.86272  
    ##  Median :-0.25969   Median : 0.79473   Median :-0.632152   Median :-0.86272  
    ##  Mean   :-0.05698   Mean   :-0.07006   Mean   : 0.003837   Mean   :-0.04204  
    ##  3rd Qu.:-0.25969   3rd Qu.: 0.79473   3rd Qu.: 1.581265   3rd Qu.: 1.15865  
    ##  Max.   : 3.84927   Max.   : 0.79473   Max.   : 1.581265   Max.   : 1.15865  
    ##   Education_2        Education_3      
    ##  Min.   :-0.61360   Min.   :-0.65390  
    ##  1st Qu.:-0.61360   1st Qu.:-0.65390  
    ##  Median :-0.61360   Median :-0.65390  
    ##  Mean   : 0.02781   Mean   : 0.01833  
    ##  3rd Qu.: 1.62908   3rd Qu.: 1.52868  
    ##  Max.   : 1.62908   Max.   : 1.52868

``` r
summary(test3_norm_df)
```

    ##       Age             Experience           Income             Family       
    ##  Min.   :-1.94259   Min.   :-2.00485   Min.   :-1.44358   Min.   :-1.1868  
    ##  1st Qu.:-0.81753   1st Qu.:-0.79501   1st Qu.:-0.74382   1st Qu.:-1.1868  
    ##  Median : 0.04791   Median : 0.06917   Median :-0.21901   Median :-0.3200  
    ##  Mean   : 0.01667   Mean   : 0.02008   Mean   : 0.01865   Mean   : 0.0631  
    ##  3rd Qu.: 0.82680   3rd Qu.: 0.84693   3rd Qu.: 0.61195   3rd Qu.: 1.4135  
    ##  Max.   : 1.77878   Max.   : 1.88394   Max.   : 2.86429   Max.   : 1.4135  
    ##      CCAvg             Mortgage       Personal.Loan Securities.Account
    ##  Min.   :-1.11148   Min.   :-0.5495   0:904         Min.   :-0.33769  
    ##  1st Qu.:-0.71329   1st Qu.:-0.5495   1: 96         1st Qu.:-0.33769  
    ##  Median :-0.20132   Median :-0.5495                 Median :-0.33769  
    ##  Mean   :-0.01313   Mean   : 0.0499                 Mean   : 0.04155  
    ##  3rd Qu.: 0.36753   3rd Qu.: 0.5092                 3rd Qu.:-0.33769  
    ##  Max.   : 4.57701   Max.   : 5.7925                 Max.   : 2.96009  
    ##    CD.Account           Online           CreditCard        Education_1      
    ##  Min.   :-0.25969   Min.   :-1.25778   Min.   :-0.63215   Min.   :-0.86272  
    ##  1st Qu.:-0.25969   1st Qu.:-1.25778   1st Qu.:-0.63215   1st Qu.:-0.86272  
    ##  Median :-0.25969   Median : 0.79473   Median :-0.63215   Median :-0.86272  
    ##  Mean   : 0.02794   Mean   :-0.05911   Mean   : 0.08721   Mean   :-0.01375  
    ##  3rd Qu.:-0.25969   3rd Qu.: 0.79473   3rd Qu.: 1.58127   3rd Qu.: 1.15865  
    ##  Max.   : 3.84927   Max.   : 0.79473   Max.   : 1.58127   Max.   : 1.15865  
    ##   Education_2        Education_3      
    ##  Min.   :-0.61360   Min.   :-0.65390  
    ##  1st Qu.:-0.61360   1st Qu.:-0.65390  
    ##  Median :-0.61360   Median :-0.65390  
    ##  Mean   : 0.03678   Mean   :-0.02095  
    ##  3rd Qu.: 1.62908   3rd Qu.: 1.52868  
    ##  Max.   : 1.62908   Max.   : 1.52868

# 15.confusion matrix for the validation data set 30% and traning dataset 50%

``` r
knn_Train_Val <- knn(train3_norm_df[,-7], valid3_norm_df[,-7], 
                  cl = train3_norm_df[, 7], k = 5)
confusionMatrix(knn_Train_Val, valid3_norm_df[, 7])
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 1354   67
    ##          1    2   77
    ##                                          
    ##                Accuracy : 0.954          
    ##                  95% CI : (0.9421, 0.964)
    ##     No Information Rate : 0.904          
    ##     P-Value [Acc > NIR] : 3.461e-13      
    ##                                          
    ##                   Kappa : 0.668          
    ##                                          
    ##  Mcnemar's Test P-Value : 1.312e-14      
    ##                                          
    ##             Sensitivity : 0.9985         
    ##             Specificity : 0.5347         
    ##          Pos Pred Value : 0.9529         
    ##          Neg Pred Value : 0.9747         
    ##              Prevalence : 0.9040         
    ##          Detection Rate : 0.9027         
    ##    Detection Prevalence : 0.9473         
    ##       Balanced Accuracy : 0.7666         
    ##                                          
    ##        'Positive' Class : 0              
    ## 

# 16.Confusion Matrix for the training dataset and test dataset

``` r
knn_Train_Test <- knn(train3_norm_df[,-7], test3_norm_df[,-7], 
                  cl = train3_norm_df[, 7], k = 5)
confusionMatrix(knn_Train_Test, test3_norm_df[, 7])
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 901  41
    ##          1   3  55
    ##                                           
    ##                Accuracy : 0.956           
    ##                  95% CI : (0.9414, 0.9679)
    ##     No Information Rate : 0.904           
    ##     P-Value [Acc > NIR] : 5.202e-10       
    ##                                           
    ##                   Kappa : 0.692           
    ##                                           
    ##  Mcnemar's Test P-Value : 2.434e-08       
    ##                                           
    ##             Sensitivity : 0.9967          
    ##             Specificity : 0.5729          
    ##          Pos Pred Value : 0.9565          
    ##          Neg Pred Value : 0.9483          
    ##              Prevalence : 0.9040          
    ##          Detection Rate : 0.9010          
    ##    Detection Prevalence : 0.9420          
    ##       Balanced Accuracy : 0.7848          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

# 17.Confusion Matrix for the validation dataset and test dataset

``` r
knn_Val_Test <- knn(valid3_norm_df[,-7], test3_norm_df[,-7], 
                  cl = valid3_norm_df[, 7], k = 5)
confusionMatrix(knn_Val_Test, test3_norm_df[, 7])
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 895  41
    ##          1   9  55
    ##                                           
    ##                Accuracy : 0.95            
    ##                  95% CI : (0.9346, 0.9627)
    ##     No Information Rate : 0.904           
    ##     P-Value [Acc > NIR] : 5.473e-08       
    ##                                           
    ##                   Kappa : 0.6615          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.165e-05       
    ##                                           
    ##             Sensitivity : 0.9900          
    ##             Specificity : 0.5729          
    ##          Pos Pred Value : 0.9562          
    ##          Neg Pred Value : 0.8594          
    ##              Prevalence : 0.9040          
    ##          Detection Rate : 0.8950          
    ##    Detection Prevalence : 0.9360          
    ##       Balanced Accuracy : 0.7815          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

# 18.Confusion matrix for Testing and Traval data (Train+Validation)

``` r
knn_TraVal_Test <- knn(traval_norm_df[,-7], test3_norm_df[,-7], 
                  cl = traval_norm_df[, 7], k = 5)
confusionMatrix(knn_TraVal_Test, test3_norm_df[, 7])
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 899  34
    ##          1   5  62
    ##                                           
    ##                Accuracy : 0.961           
    ##                  95% CI : (0.9471, 0.9721)
    ##     No Information Rate : 0.904           
    ##     P-Value [Acc > NIR] : 5.695e-12       
    ##                                           
    ##                   Kappa : 0.7402          
    ##                                           
    ##  Mcnemar's Test P-Value : 7.340e-06       
    ##                                           
    ##             Sensitivity : 0.9945          
    ##             Specificity : 0.6458          
    ##          Pos Pred Value : 0.9636          
    ##          Neg Pred Value : 0.9254          
    ##              Prevalence : 0.9040          
    ##          Detection Rate : 0.8990          
    ##    Detection Prevalence : 0.9330          
    ##       Balanced Accuracy : 0.8202          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

Conclusion :- From the confusion matrices I can compute the sensitivity,
specificity, accuracy, precision, other performance metrics for each of
the classifiers. Accuracy is one most important performance metrics.

a.Confusion Matrix for Test with Training :-
Accuracy:0.956,Sensitivity:0.9967,Specificity:0.5729,Kappa:0.692  
b.Confusion Matrix for Test with Validation:-
Accuracy:0.95,Sensitivity:0.9900,Specificity:0.5729,Kappa:0.6615  
c.Confusion Matrix for Test with Traval :-
Accuracy:0.961,Sensitivity:0.9945,Specificity:0.6458,Kappa:0.7402
d.Confusion Matrix for Test with Training:-
Accuracy:0.954,Sensitivity:0.9985,Specificity:0.5347,Kappa:0.668

Based on the Accuracy,sensitivity,specificity and Kappa values
comparison between ‘a’ and ‘b’, confusion matrix for Test with Training
seems to be better than Confusion matrix test with Validation .

However, the confusion matrix Test with Traval(c) performs the best
among all other.
