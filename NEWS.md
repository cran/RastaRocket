# RastaRocket 1.1.2

## Improvements

- `desc_ei_per_grade()` and `desc_ei_per_pt()`
  - Made more flexible regarding variable names.
  - Users can now specify custom column names through function arguments.
 
- `desc_var()` and `base_table()`
  - Added the ability to specify which statistics to display for continuous variables.
  
- `digits` argument
  - Enhanced precision control with clearer and more flexible number rounding for categorical and continuous variables.


# RastaRocket 1.0.2

## Bug fixes
- Fix missing data handling (use missing data option instead of statistics computation, much better and less bug)

## Improvements
- Refactored `add_missing_info()` with cleaner and more elegant internal code.

# RastaRocket 1.0.1

## Bug fixes
- Fixed a bug in `freq_relevel()` where the *group* variable was unintentionally releveled. This behavior could alter group ordering in summary tables and cause issues when stacking multiple variables.

## Improvements
- Refactored `modify_ajouter_label_dm()` with cleaner and more elegant internal code.

## Documentation
- Updated the README file.

# RastaRocket 1.0.0

* Accepted on CRAN

# RastaRocket 0.9.0

* Initial CRAN submission.
