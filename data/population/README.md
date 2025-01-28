
# Population count data

This folder contains three raw data files:
- `projections2018_simple.xls`
- `projections2018.xls`
- `projections2013.xls`

The first two were obtained from IBGE [here](https://www.ibge.gov.br/en/statistics/social/population/18176-population-projection.html?edicao=21933), while the latter was obtained from IBGE [here](https://www.ibge.gov.br/en/statistics/social/population/18176-population-projection.html?edicao=18180&t=resultados).


These raw data files are processed using two scripts:
- `processpopests_simple.R`: Which processes the first file into `popests_simple_2010_2020.csv`
- `processpopests.R`: Which processes the latter two files into `popests_2002_2020.csv`

This processing is minimal, and simply involves reading in the raw data, converting it to "long" format, checking for errors, and then saving it as a `.csv` file. Further details can be found in the scripts themselves or in the supplementary material of the paper.