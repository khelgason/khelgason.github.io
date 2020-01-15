
source('scripts/1_load_and_clean_data.R')
source('scripts/2_munging_and_plots.R')
source('scripts/3_plot_maps.R')
source('scripts/4_tidy_modelling.R')
rmarkdown::render(input = 'report/rvk_bilastaeda_notkun.Rmd', envir = parent.frame())
