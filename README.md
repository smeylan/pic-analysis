# pic-analysis
Analyses for "Word forms - not just their lengths - are optimized for efficient communication"

To download / clean / compute frequency and  in-context surprisal estimates, see [smeylan/ngrawk](https://github.com/smeylan/ngrawk/). 
To download these estimates instead, in the appropriate directory structure:

`wget cocosci.berkeley.edu/smeylan/pic/results.zip && unzip results.zip`  
`wget cocosci.berkeley.edu/smeylan/pic/token_results.zip && unzip token_results.zip`

The analysis requires other data sources including the Clearpond database, dates of first use from the Oxford English Dictionary, word lists from OPUS, and a list of plurals.
To download these:

`wget cocosci.berkeley.edu/smeylan/pic/data.zip && unzip data.zip`

To limit the analysis to morphologically simple words, you will need a copy of the CELEX2 corpus. Add a symlink with `ln -s` to the `data/` directory after decompressing.
