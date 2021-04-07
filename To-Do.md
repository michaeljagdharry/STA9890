Import data with appropriate datatypes (last column - the response - is categorical)
  Anything not "Number of ..." is categorical, excluding response column
    Double-check if response column is strictly 0's and 1's

Fix 19's on 10-Fold CV plot ( regularize(a,plot=TRUE) ), add title to plot

Use regularize() to create dataframes for R2 values for test and train for each of ridge/lasso/elnet. From here,
  Produce two panels of boxplots of R2's from this combo dataframe (4 boxplots for train, 4 boxplots for test) (4(b))

For one split, produce side-by-side boxplots of train and test residuals.



Questions:
For #5(a): Are we expected to report the coefficients for the fitted models?
  Or, is #5(a) just repeating #3(b,c)?
