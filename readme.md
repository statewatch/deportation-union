# how-to

`raw_data/frontex_docs_pdf` : convert files, e.g. with tabulizer, and save the converted files in `raw_data/frontex_docs_converted`.
run `clean_data_2006_18`, `clean_data_2019_21.R` and `export_data_d3.R`
adjust html files in `d3` if necessary.

# Repository contents

## `raw_data`

`deportation-union-data.xlsx` : Dataset from https://www.statewatch.org/deportation-union-rights-accountability-and-the-eu-s-push-to-increase-forced-removals/ **with small manual corrections**, e.g. from `deportations 2015, 2016, 2017 data - corrected.xlsx` - so **don't replace with original file**, rather add data or corrections manually into this version.

`raw_data/deportation-union-data.xlsx`: Dataset from original deportation union report **with small manual corrections**, e.g. from `deportations 2015, 2016, 2017 data - corrected.xlsx` - so **don't replace with original file**, rather add data or corrections manually into this version.

### `R_scripts/clean_data_2006_18.R` produces:

`clean_data/by_dest_2006_18.csv`:
- ROWID: unique identifier for operation per member state
- N_RETURNEES
- DEST
- N_ESC
- N_ESC_LEAD
- check_ESC_OBS_MED_by_dest: TRUE if sum of N_ESC and N_ESC_LEAD equals the sum across the ROWID. if not true, use `N_ESC_OBS_MED` from `by_ms_2006_18` instead.

`clean_data/by_ms_2006_18.csv`:
- DATE
- MSNAME
- N_RETURNEES
- N_FX_STAFF
- N_ESC_OBS_MED
- N_MEDS
- N_OBS
- N_MONITORS
- FX_CONTRIB
- ROID
- OPTYPE
- Notes/comments
- ID: unique identifier for operation. ROID where available, else a number created ourselves.
- MSISO
- ROWID: unique identifier for operation per member state

### `R_scripts/clean_data_2019_21.R` produces:

`clean_data/by_dest_2006_18.csv`:
- "ROID"
- "MSNAME"
- "DEST"
- "N_RETURNEES"
- "DATE"
- "OPTYPE"

`clean_data/by_ms_2019_21.csv`:
- "ROID"
- "DATE"
- "MSISO"
- "FX_CONTRIB"
- "MSNAME"
- "N_ESC_LEAD"
- "N_ESC"
- "FRESO_AP_CAT_1"
- "FRESO_AP_CAT_2"
- "FRESO_HQ_CAT_1"
- "FRESO_HQ_CAT_2"
- "FRESO_CAT_3"
- "N_MEDS"
- "N_OBS_MS"
- "N_OBS_TC"
- "N_MONITORS_NAT"
- "N_MONITORS_POOL_MS"
- "N_MONITORS_FX"
- "N_MONITORS"
- "N_OBS"
- N_ESC_OBS_MED

`clean_data/by_op_2019_21.csv`:
- ROID
- DATE
- N_FX_STAFF

### `R_scripts/export_data_d3.R` produces:

`OPTYPE_YEAR_new.csv` with YEAR,KEY,CRO,JRO,NRO
Note: When the same operation ID is used for multiple dates, they are counted as different operations. Only operations taking place the same day and sharing an ID are counted as the same operation.

`N_RETURNEES_YEARS_MSNAME_new.csv` with YEAR, DEST and one column for each MSNAME

`N_RETURNEES_MSNAME_DEST_ROUTES_MIN_15_new.csv` with DEST, MSNAME and N_RETURNEES for the 15 MS and DEST with most RETURNEES

`N_RETURNEES_MSNAME_DEST_new.csv` same as above but unfiltered (including all MS and DEST, not only top15)

`FX_CONTRIB_YEARS_MSNAME_new.csv` with YEAR, MSNAME and one column for each MSNAME

`CONTRIB_PP_BY_DEST_new.csv` with DEST, N_RETURNEES, FX_CONTRIB, FX_CONTRIB_PP, iso
Note: Only operations with exactly one member state and one destination were considered (all of which are from 2016 and later). The average is also based only on these operations.

##  issues / notes

**NOTES:**

*Abbreviations:*  
*MS = (EU) member state*  
*DEST = destination*  

- Missing dates are filled in as Dec 31st of the according year to make aggregating by year possible.
- Data was supplied in varying formats by Frontex for different years. Therefore the granularity of data varies for different years.

- RO ID's RO-2016-112 and -113 same MS, DEST and Date (2016-08-31) but different numbers. add together?
- RO-01516: date was 2020-10-13 in the files obtained in 12/21, date is 2020-10-14 in some files obtained 08/22 and still 2020-10-13 in other files from 08/22. set to 2020-10-13 everywhere.
- VRDS only included in data obtained in 08/22 (for years 2020 and 2021). not clear if VRD data is available for previous years upon request. excluded for now from datasets
