
# Live births data

Raw live births data records for 2002 through 2020 were obtained from [here](https://opendatasus.saude.gov.br/dataset/sistema-de-informacao-sobre-nascidos-vivos-sinasc). These are not included in the GitHub repository due to their size.

The raw data are processed using the script `processlivebirths.R` which reads in the raw data, counts the number of births that occurred in each group, and then saves the processed data as `livebirths_processed_2002_2020.csv`.

The remaining file, `underreporting.csv`, contains the capture-recapture-derived estimates of live births underreporting obtained from [here](https://biblioteca.ibge.gov.br/index.php/b%20catalogo?view=detalhes&id=2101978.)