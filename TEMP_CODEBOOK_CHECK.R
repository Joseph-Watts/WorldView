colnames(indiv_data)

included_vars <- c("Q165", "Q166", "Q167", "Q168", "Q177",
                   "Q178", "Q179", "Q182", "Q183", "Q184",
                   "Q185", "Q186", "Q187", "Q188", "Q189",
                   "Q190", "Q191", "Q192", "Q193", "Q194",
                   "Q195", "Q260", "Q262", "Q263", "Q275",
                   "Q288", "Q289")

for(i_var in included_vars){
  print(i_var)
  print(class(indiv_ordinal[, i_var]))
  print(table(indiv_ordinal[, i_var]))
  print("--------------------------")
}

#' Issues

[1] "Q275"
[1] "numeric"

1     2     3     4     5     6     7     8     9 
4892 11268 14330 24709  8846  7818 16922  6231  1133 



[1] "Q289"
[1] "numeric"

0     1     2     3     4     5     6     7     8     9 
1898  2532  5602  1993 25398   255  7795  7886 19038 23460