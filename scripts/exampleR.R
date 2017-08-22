library(tidyverse)
x = tibble(
  known=c(rep(T, 100), rep(F, 100)) %>% rep(2),
  observed= c(  c(runif(60, 1, 2), runif(40, 0, 1), runif(100, 0, 1)),   c(runif(100, 1, 2), runif(60, 0, 1), runif(40, 1, 2)) ),
  observation = c(rep(1, 200), rep(2, 200))
)
metrics = x %>% group_by(observation) %>% do(GENIE3::evaluate_ranking_direct(.$observed, .$known, sum(.$known), length(.$observed))$metrics)
metrics$diagonal = -(1-metrics$spec) + 1

split_metrics = x %>% group_by(observation) %>% do(splitauc::get_split_auroc(.$observed, .$known))

ggplot(metrics) + geom_ribbon(aes(1-spec, ymax=pmin(tpr, diagonal)), position="identity", fill="#BAACBD", ymin=0) + geom_ribbon(aes(1-spec, ymin= pmin(tpr, diagonal), ymax=tpr), position="identity", fill="#ABB7BC") + geom_line(aes(1-spec, tpr)) + geom_abline(slope=-1, intercept=1) + facet_wrap(~observation) +
  geom_label(aes(label=paste0("Sensitivity: ", round(auroc_sensitivity, 2))), x=0.15, y=0.25, hjust=0, data=split_metrics) +
  geom_label(aes(label=paste0("Specificity: ", round(auroc_specificity, 2))), x=0.65, y=0.65, hjust=0, data=split_metrics) +
  coord_equal()
