### Linear Mixed Model Instructions (Group-level)

- Compare development trends across predefined groups (e.g. regions, hdi_group, development groups, or reference groups), assess between-group differences, and visualize model-based trajectories with uncertainty.

- The outcome variable is HDI, measured annually. Time (year) is included by default to model overall trends.

- Groups (such as regions, development groups or International reference groups) are modelled as random effects, allowing each group to follow its own trajectory around the overall global trend.

- You may optionally add group classifications as fixed effects to compare average differences between groups.

- For random effect: choose "random intercept" to allow groups to differ in their baseline, "random slope" to allow groups to differ in their trajectories, or "Random intercept and random slope" to allow groups to differ in both, their baseline and HDI changes over time.
