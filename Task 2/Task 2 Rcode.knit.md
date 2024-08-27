# Solution for Task 2
*This file is a solution for the Task 2 of the Quantium Virtual Internship.*

# Open the libraries
<Container>

library(data.table)
library(tibble)
library(ggplot2)
library(tidyr)



# Load the data set
QVI_data <- read_csv("QVI_data.csv")

# Set themes for plots
<Container>
<Input>
<pre><code class="language-html"><backify-button></backify-button>
      
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))


