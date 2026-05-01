# RastaRocket 1.1.4

## Improvements

- Added a new `indent_table()` function to control text indentation in table cells.
- Added an `include_all_na_cat` argument to `desc_var()` and `prepare_table()` to better handle factors with missing levels.
- Added a new `write_rendercopy()` function to enable rendering the .qmd file programmatically without using the Render button, and to allow flexible definition of the output HTML file name and location.
- Updated `write_qmd()` to add components to facilitate switching between analysis phases.
- Added unit tests for `custom_round()`, `select_plus()`, and `riskdifference()`.
- Improved function documentation by using the @inheritParams roxygen tag.



# RastaRocket 1.1.3

## Major changes

- The `freq_relevel` argument in `desc_var()` and `base_table()` has been updated to provide more flexible options
  for reordering factor levels by frequency, using `sort` argument from `gtsummary`.  
-  The `freq_relevel` argument has been removed from the `prepare_table()` function, as it is now handled internally for greater consistency.

## New features

- A `language` argument has been added to the `desc_ei_per_grade()` function to support output translation.
- The `desc_ei_per_pt()` function has been revised to align with the new language translation feature.

## Documentation improvements

- Detailed descriptions and worked examples for `digits` and `freq_relevel` arguments have been added to the documentation of `desc_var()`.
- The vignette has been updated accordingly to illustrate these enhancements.

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
