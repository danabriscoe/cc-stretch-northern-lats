## render rmd script to github docs/html

rmarkdown::render(
    # # "../code/02_plot_SST_ts.Rmd",
    # "rmd/northern_lats_EDA_20Sept2023.Rmd",
    "northern_lats_EDA_20Oct2023.qmd",
    # # "code/02_plot_SST_ts.Rmd",
    output_file = "../docs/index.html"
)