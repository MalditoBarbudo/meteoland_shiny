##### Rmd to md script
# In order to show formatted and compiled Rmd documents in the shiny app we need
# to knit them to md files and use the includeMarkdown function in the shiny UI.
# This is due to the buggy includeHtml not working and that the includeMarkdown
# function does not process Rmd documents, only renders them without code execution

# libraries
library(knitr)

# files list
rmdfiles <- list.files('Docs/', '.Rmd', full.names = TRUE)
# loop (with sapply)
sapply(rmdfiles, knit, quiet = T)

## remember to move the files created in the root app directory to the
## Docs/ folder

print('Remember!!!!! Move the md files to Docs/ folder!')
