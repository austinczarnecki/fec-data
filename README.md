# fec-data
Scripted analysis of FEC election data for use in data visualization

### What this script does
This script takes the raw data files on [Independent Expenditures](http://www.fec.gov/data/IndependentExpenditure.do?format=html&election_yr=2016) and [Candidate Spending](http://www.fec.gov/data/CandidateSummary.do?format=html&election_yr=2016), performs some data cleaning before writing a number of JSON files containing information about the top seven candidates in the 2016 US Presidential election. 

By swapping in election data from previous years you could use the same script to see historical analysis of campaign finance, although some data-wrangling may be necessary if there are inconsistencies that are different from those that I found in the 2016 data.

#### Specific stuff I did
- Unify the `can_nam` field to have identical strings for identical candidates. `Donald Trump`, `Trump, Donald`, `Trump, Donald J.`, `TRUMP, DONALD`, and `TRUMP, DONALD J` all become `TRUMP, DONALD`. 
- Make sure that all entries have both `can_nam` and `can_id` fields. For some reason this isn't validated when submitting the data.
- Remove ammended database entries. This is explained more thoroughly in the code but basically amendments don't overwrite the previous row so ones with amendments submitted later need to be recursively removed and only the latest amendment saved.
- Convert strings like `"$1,000,000"` to numeric values like `1000000`.
- Categorize spending based on keywords in the `pur` field (stands for "purpose"). I chose 13 categories that covered most of the descriptions provided by filers.

### How to use
If you have an R environment either on the command line or using rStudio, you can simply download and run this script to generate the same files that I'm using to visualize FEC data in my blog post at [austinczarnecki.com](austinczarnecki.com).

### Visualization code that uses one of these JSON files
[https://bl.ocks.org/austinczarnecki/f8c71dd06c67ecbce840](https://bl.ocks.org/austinczarnecki/f8c71dd06c67ecbce840)

### References
Open data from the United States Federal Election Commission: [http://www.fec.gov/data/DataCatalog.do](http://www.fec.gov/data/DataCatalog.do)