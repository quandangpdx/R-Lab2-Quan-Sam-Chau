require(knitr)
require(markdown)

knit('RLab02.Rmd','RLab02.md')
markdownToHTML('RLab02.md','RLab02.html')