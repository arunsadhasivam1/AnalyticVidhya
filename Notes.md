NOTES:
=====

1.Analyse the dataset
2.first combined  test and train set
3.check whether data has NA ("") if so change to meaningfull.
4.if particular column is set as Factor datatype then even if it is "" it will be
   treated as blankspace('') by R.
5. Other columns (non - factor ) if have '' it will be treated as NA
   you can check only with is.nan(df$column)
6. To change null value in data frame(df) i.e if any column in data frame row is null
  if it is factor dataframe then  it will not allow to change 
  ==========================================================
  To change follow
  step 1 -> change to character , step2 change the value , and then revert back to factor.
    STEP 1: df$column<-as.character(df$column)
    STEP 2: df$column[which(df$column==''),'column']<-'Not specified'
    STEP 3: df$column<-as.factor(df$column)
    
  if it is non -factor you can change it easily
  =============================================
         df$column[which(is.nan(df$column)),'column']<-'Not specified'

example:
========
http://www.analyticsvidhya.com/blog/2015/06/solution-kaggle-competition-bike-sharing-demand/
