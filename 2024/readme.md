# how-to

- run R scripts (folder `R_scripts`) in the numbered order
- adjust html files in `d3` if necessary:
	- add color categories for additional data, e.g. new types of operations

# Repository contents

## `raw_data`

`deportation-union-data.xlsx` : Dataset from https://www.statewatch.org/deportation-union-rights-accountability-and-the-eu-s-push-to-increase-forced-removals/ **with small manual corrections**, e.g. from `deportations 2015, 2016, 2017 data - corrected.xlsx` - so **don't replace with original file**, rather add data or corrections manually into this version.

documents obtained by FOIA request to frontex are in folders with the date they were obtained.

## `R_scripts` & `clean_data`

the R scripts need to produce:

`OPTYPE_YEAR_new.csv` with YEAR,KEY,CRO,JRO,NRO
Note: When the same operation ID is used for multiple dates, they are counted as different operations. Only operations taking place the same day and sharing an ID are counted as the same operation.

`N_RETURNEES_YEARS_MSNAME_new.csv` with YEAR, DEST and one column for each MSNAME

`N_RETURNEES_MSNAME_DEST_ROUTES_MIN_15_new.csv` with DEST, MSNAME and N_RETURNEES for the 15 MS and DEST with most RETURNEES

`N_RETURNEES_MSNAME_DEST_new.csv` same as above but unfiltered (including all MS and DEST, not only top15)

`FX_CONTRIB_YEARS_MSNAME_new.csv` with YEAR, MSNAME and one column for each MSNAME

`CONTRIB_PP_BY_DEST_new.csv` with DEST, N_RETURNEES, FX_CONTRIB, FX_CONTRIB_PP, iso
Note: Only operations with exactly one member state and one destination were considered (all of which are from 2016 and later). The average is also based only on these operations.

##  issues / notes

**ISSUES:**

- Frontex contributions per person for Burundi are based on one single (very expensive) operation. Filter out?

- number of people deported by frontex is much lower in supplied data than in a report obtained by statewatch (https://www.statewatch.org/news/2023/june/frontex-aided-the-deportation-of-almost-25-000-people-in-2022/)

- data supplied by frontex on costs has figures with 4 digits after the decimal point for amounts in €

- dates differ for some operations between data supplied for number of deported persons and operation types.

**NOTES:**

- raw data includes more variables than included in graphics, e.g. categories of staff. check raw data
