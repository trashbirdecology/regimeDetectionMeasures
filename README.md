## About  
This package will calculate a various regime detection metrics that have been used to 'detect ecological regime shifts'. A 'new' metric, **distance travelled** is also calculated (Burnett and others, *in prep*). 

## Example
Run `example.R` to view an example run of all functions in the package.

## Measures/metrics calculated  

COMPOSITE:
1. Distance travelled -see also package [`distanceTravelled`](https://github.com/TrashBirdEcology/distanceTravelled). 
1. Fisher Information
1. Variance Index

UNIVARIABLE:
1. Skewness (mean and mode versions)
1. Kurtosis
1. Variance
1. Mean
1. Mode
1. Coefficient of variation, CV
1. Autocorrelation lag-1 (using `stats::acf`)

