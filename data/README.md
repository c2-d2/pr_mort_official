# Organization

`cleaned_data` - contains all cleaned CSV for analysis

`rdata` - contains all intermediate RDS files used to generate figures and perform analysis

`data-dictionary.xlsx` - Data dictionary

`defunciones_2010_17.xlsx` - Official record of deaths from the Instituto de Estadisticas de Puerto Rico

`Mortalidad-RegDem-2015-17-NYT-part1.pdf` - Data used to generate figures for [NYT Article on deaths in Puerto Rico](https://www.nytimes.com/interactive/2017/12/08/us/puerto-rico-hurricane-maria-death-toll.html)

`mun_pop.csv` - population by municipality used in initial weighting

`official_deaths.csv` - `defunciones_2010_17.xlsx` reorganized and as a CSV

`raw-censored.xlsx` - Raw data as it was collected from our mobile data collection platform, dates, personal identifying information, location and data collector information have been censored to protect respondent privacy. In the raw data, numerous entries were "test" entries and are marked as such. The three sheets (`households`, `individuals`, `deaths`) are linked by `hh_id` in the `households` sheet and `hh_id_0` in `individuals` and `deaths`.

# Correspondence
For any issues with anonymization or major issues with the functionality of the script please create an issue

## License
The data collected and presented is licensed under the [Creative Commons Attribution 3.0 license](http://creativecommons.org/licenses/by/3.0/us/deed.en_US), and the underlying code used to format, analyze and display that content is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php).
