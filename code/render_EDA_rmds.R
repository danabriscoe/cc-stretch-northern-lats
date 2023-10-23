## render rmd script to github docs/html

rmarkdown::render(
    # "rmd/northern_lats_EDA_20Sept2023.Rmd",
    "northern_lats_EDA_20Oct2023.qmd",
    # output_file = "../docs/index.html"
    output_file = "../docs/index.qmd"
)