## Unresolved issues

as of Dec 5th 2021

`deportation-union-data.xlsx`:

- 2016 number 49: returnees numbers inconsistent: total is smaller than returnees (1) and (2) added // on 2016-05-11 -> operation with returnees (2) specified but no destination (2)
- 2017 nr. 110: escorts (1), (2) and (3) exist but only destinations and returnees (1) and (2).
- 2015 row 4: returnees (3) has number, but total, (1) and (2) not, only destinations (1) and (2)

August 19th: all above issues solved (see `deportations 2015, 2016, 2017 data - corrected.xlsx`)

as of Aug 20th 2022:

- RO-01516: date was 2020-10-13 in the files obtained in 12/21, date is 2020-10-14 in some files obtained 08/22 and still 2020-10-13 in other files from 08/22. set to 2020-10-13 everywhere.
- VRDS only included in data obtained in 08/22 (for years 2020 and 2021). not clear if VRD data is available for previous years upon request. excluded for now from datasets
- data is aggregated by ROID, when an operation includes multiple destinations, no data is provided per destination. Confirmatory application about this has been registered on Aug 20th (same procedure as 2021). **-> operations with multiple destinations are missing for 2021 in OPERATIONS_BY_DEST_MS.csv for now**
- "costs for AUT not available yet" in 2021 operations; no action taken so far, maybe exclude those operations?
- observers are supplied only per operation in data obtained 08/22, not disaggregated by member state **-> no oberservers in OPERATIONS_BY_MS.csv** foe 2020 / 2021
- "escorts from pool" no longer present in data obtained 08/22
- "medical staff" not supplied from 2019 onwards

# Repository contents

## `raw_data`

#### `deportation-union-data.xlsx` :

Dataset from https://www.statewatch.org/deportation-union-rights-accountability-and-the-eu-s-push-to-increase-forced-removals/ **with small manual corrections**, e.g. from `deportations 2015, 2016, 2017 data - corrected.xlsx` - so **don't replace with original file**, rather add data or corrections manually into this version.

#### `frontex_docs_pdf`:

Documents obtained from Frontex through request.

#### `frontex_docs_converted`:

raw_data/frontex_docs_pdf/ files converted with an online converter.

#### `country codes.xlsx`:

List of needes country codes, iso2 codes and iso3 codes.

## `R_scripts`

#### `clean_data.R`

takes
`raw_data/deportation-union-data.xlsx` (years 2006 - 2018)
and
`raw_data/frontex_docs_converted/` files
and generates:

- `OPERATIONS.csv`
- `OPERATIONS_BY_MS.csv`
- `OPERATIONS_BY_DEST_MS.csv`

(for details, see below)

#### `export_data_for_web.R`

takes exported data from `clean_data.R` and generates files used by `d3` code for interactive visualisations:

- `CONTRIB_PP_BY_DEST.csv`
- `FX_CONTRIB_YEARS_MSNAME.csv`
- `N_RETURNEES_MSNAME_DEST_ROUTES_MIN_15.csv`
- `N_RETURNEES_YEARS_MSNAME.csv`
- `OPTYPE_YEAR.csv`

(for details, see below)

## `clean_data`

**NOTES:**

- Missing dates are filled in as Dec 31st of the according year to make aggregating by year possible.
- Data was supplied in varying formats by Frontex for different years. Therefore the granularity of data varies for different years, and was sometimes only supplied as aggregates per operation, sometimes disaggregated by member states and/or destinations. Detailed data is included where possible (e.g. number of escorts per destinations), but because the data is not available in this detail for all years and operations, totals are higher than the sum of all disaggregated data points. For totals, please always use the data from the aggregated files.
- Data on contributions was only supplied until September 2020 and is missing for the last quarter of 2020.

*Abbreviations:*  
*MS = (EU) member state*  
*DEST = destination*  


#### `OPERATIONS.csv`:

all data aggregated by operation ID. FX_STAFF is only found in this file.

variables present in this data:

- "ID": unique ID per operation (unofficial, created for internal use). Until 2018 the "ID" is made up of the year and "Number" from the spreadsheet, for 2019 and 2020 ROID's were used.
- "OPTYPE": NRO, JRO etc.
- "N_RETURNEES" : total across MS and DESTs
- "N_ESC_OBS_MED" : total of escort (leader)s, observer, medical staff aggregated across MS. (Note: before 2018, data on escorts, observers, medical staff is only available as this aggregate, no disaggregated numbers on the different positions. For )
- "N_ESC": total escorts across MS and DESTs (mostly available only starting 2019)
- "N_ESC_LEAD": total escort leaders across MS and DESTs (mostly available only starting 2019)
- "N_FX_STAFF" : total of Frontex staff on operation
- "N_MEDS" : total number of medical staff across MS (mostly available only starting 2019)
- "N_OBS" : total number of observers across MS (mostly available only starting 2019)
- "N_MONITORS" : total number of (national) monitors across MS
- "N_MONITORS_POOL" : total number of monitors from pool
- "FX_CONTRIB" : total financial contribution in € across MS

variables **not** present in this data:

- "DATE" : because operations partly span across dates (multiple days). to find dates of the operation, check `OPERATIONS_BY_MS`
- "ROID": before 2017 instead of RO-IDs there are different codes for each member state. therefore impossible to match one ROID to each operation; check `OPERATIONS_BY_MS` for Codes and ROIDs involved in each Operation (`ID`)

#### `OPERATIONS_BY_MS.csv`:

all data (dis-)aggregated by operations (`ID`) and MS.

variables present in this data:

- "DATE"
- "ROID"
- "MSNAME"
- "MSISO"
- "N_RETURNEES": total across destinations
- "N_ESC_OBS_MED" : escort (leader)s, observer, medical staff aggregated (before 2018, only in this aggregate mostly)
- "N_ESC": total across destinations
- "N_ESC_LEAD": total across destinations
- "N_MEDS": medical staff from member state
- "N_OBS": observers from member state
- "N_MONITORS": monitors from member state
- "FX_CONTRIB": to member state
- "OPTYPE"
- "Notes/comments"
- "ID": internally used ID, until 2018 "Number" from Spreadsheet, 2019 + 2020 ROID.
- "ROWID": year and rownumber from spreadsheet (unique identifier for the combination of member state and operation)

#### `OPERATIONS_BY_DEST_MS.csv`:

all data (dis-)aggregated by operations (`ID`) MS and destination.

variables present in this data:

- "ROID"
- "MSNAME"
- "MSISO"
- "DEST"
- "N_RETURNEES"
- "N_ESC": **NOTE: incomplete data!** For many years and operations, only aggregated by MS and ESC/ESC_LEAD/MED/OBS is available. Do not add these to obtain totals per operations, instead use the above files for totals.
- "N_ESC_LEAD" : **NOTE: incomplete data!** For some years and operations, only aggregated by MS is available. Do not add these to obtain totals per operations, instead use the above files for totals.
- "ROWID": year and rownumber from spreadsheet (unique identifier for the combination of member state and operation)
- "ID": operation ID

#### `CONTRIB_PP_BY_DEST.csv`:

Frontex contributions for return operations per person by destination. To avoid distorted data, only destinations with at least 10 deported people (across the time period) were included. The 25 destinations with highest Frontex contribution rates per person as well as the overall average are shown. Used by `3_fx_contributions_pp_dest.html`

#### `FX_CONTRIB_YEARS_MSNAME.csv`:

Total of Frontex' financial contributions by years and receiving (member) states. Used by `2_fx_contributions_states.html`

#### `N_RETURNEES_MSNAME_DEST_ROUTES_MIN_15.csv`:

Number of returned people in the time frame between 2006 and 2020 (limited to 15 countries that most people were deported from and to), used by `0_deportations_sankey.html`

#### `N_RETURNEES_YEARS_MSNAME.csv`:

Number of returned people per year, member state (deporting country) and destination. Used by `1_deportations_barchart.html`

#### `OPTYPE_YEAR.csv`:

Number of operations and deported people per year and operation type (NRO, JRO, CRO)

## `d3`

Code for frontend visualisations.
