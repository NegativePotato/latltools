# Install
## Install (or load) the [devtools](https://github.com/hadley/devtools) package 
This can be done from [CRAN](https://cran.r-project.org/]). Enter the following commands in the R console : \
`install.packages("devtools")` \
`library(devtools)`

## Install and load [latltools](https://github.com/NegativePotato/latltools) from github
Enter the following commands in the R console : \
`install_github("NegativePotato/latltools")` \
`library(latltools)`

Todo : 
Include a test dataset for a working example

# Functions included 
## Better plot saving function 
save_plot(plot = my_plot, file_name_full_no_ext = file.path(“path/to/figure/directory”, “my_awesome_plot”), width = 7, height = 7, units = 'in')

## Categorize AVGPs
get_vgq_scores_latl(example_data.df, raw_values_qualtrics = T)

