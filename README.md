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

To check against the Piantadosi et al. (2011) results, download and unzip publicly avalable data from the [Colala website](https://colala.bcs.rochester.edu/data/PiantadosiTilyGibson2011/Google10L-1T/) and place them in `data/Google1T_Piantadosi`. To filter the words, you will also need to request the OPUS wordlists from the Colala lab and place them in `data/OPUS_Piantadosi`.
