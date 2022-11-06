
# Historical versions

## Future version
Features:
- Reduced time costs in function filter_possible_results()

Small bugfixes:
- Fixed a bug in signs_with_distance() that produce many warnings for no reason.



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

