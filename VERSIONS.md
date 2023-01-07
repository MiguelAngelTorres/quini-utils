
# Historical versions

## Future version
Features:
- Added new function to get the real and voted probability of any year and bet: **get_data_eduardolosilla()**.
- Added the **parameter allow_lower_fails in function signs_with_distance()**
- **Reduced time costs** in function filter_possible_results()
- **Reduced time costs** in function calculate_probability_of_reward()
- **Reduced time costs** in function get_voted_prob()

Changes:
- **get_rentability() has been renamed**. Now the function is called get_profitability().

Small bugfixes:
- Fixed a bug in signs_with_distance() that produce many warnings for no reason.
- Fixed a bug in get_em() when optimization is not applied, temp columns where not deleted from prob_voted_table.
- Fixed a bug in calculate_probability_of_roi(). Parenthesis where not paired.



## 1.0.0

Features:
- Added **random match generator**: get_random_matches().
- Added **two functions to fix matches probabilities**: normalize_voted() and increase_real_by_voted().
- Added **rentability function**, (get_rentability).
- Added **probability of reward function**, calculate_probability_of_reward().
- Added **probability of ROI function**, calculate_probability_of_roi().
- Added **filter possible results function**, filter_possible_results(). This was an existing code duplicated on some functions. Now
it is converted to a usable function.


Small bugfixes:
- Roxygen now reads imports to solve dependencies.

## 0.0.1
Features
- Added all code from local.

