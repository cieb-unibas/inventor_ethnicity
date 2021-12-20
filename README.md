# Mind The Gap: Non-Western Origin Inventors in the USA and Europe
This repository contains code for an interactive report about migration patterns and the ethnic distribution of inventors in the USA and Western European countries. The full article can be accessed <a href = https://innoscape.ch/en/publications/immigration-of-inventors target = "_blank">*here*</a>.

The number of inventors with non-western ethnic backgrounds has increased tremendously in the USA and contributes significantly to its inventive capacity. This is different in continental European countries, which have been lagging behind in the global race for talents. Furthermore, the vast increase of non-western inventors in the USA has been strongly directed towards emerging high-technology fields. This pattern cannot be observed for European countries. We argue that the USA's extraordinary attractiveness for inventors from all around the world could be an important asset that is lacking in European countries and this should be a concern for European policymakers.

## Structure of the repository

### Data
The file `01_data_creation.R` contains the code to generate the data used in this analysis (the file `02_data_creation_funs.R` contains helper functions for data processing). Raw data on patents can be accessed at the USPTO and at the OECD.

### Report
This folder features the script `report_main.Rmd`, which produces the final report as a `.html` file. This report builds on packages and libraries such as `knitr`, `rmarkdown`, `ggplot2`, `viridis` and `plotly.js`. In addition to the main file, the folder contains the datasets used for the displayed plots (`plot1_df.csv`, `plot1_df.csv`, `plot1_df.csv` respectively) that were created with the `02_data_creation_funs.R` script and the file `plot2.html` which generates the interactive plot from the report.

### Example
![example_plot](https://raw.githubusercontent.com/cieb-unibas/inventor_ethnicity/main/data/example_plot.png)

The report is part of <a href = http://innoscape.ch/ target = "_blank">*Innoscape*</a>, an applied research project of the <a href = https://cieb.unibas.ch target = "_blank">Center for International Economics and Business | CIEB</a> at the University of Basel. With *Innoscape*, we analyze how well prepared the Swiss economy is for the transformations of the 21st century. We provide economic insights on how Switzerland can successfully master these challenges and discuss how the country could remain a global innovation leader in the future.

