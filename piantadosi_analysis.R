aspell = function(strings, languageCode){   
    # Get a vector of booleans indicating whether strings are spelled correctly 
    # according to the dictionary for the language specified by languageCode
    # aspell must be on the path
    # install aspell for Mac with brew install aspell --with-lang-de --with-lang-en --with-lang-nl 
    # --with-lang-es --with-lang-fr; sudo apt-get install aspell-en for Ubuntu
    tf = tempfile()
    tf2 = paste0(tempfile(),'_out.txt')
    
    write.table(data.frame(strings), file=tf, col.names=F, row.names=F, sep=',', fileEncoding='UTF-8')  
    #keep declaring the encoding, or it reverts to Latin-1 in Jupyter because of...reasons.   
    aspell_command = paste0('LC_ALL=en_US.UTF-8 cat ', tf, '| LC_ALL=en_US.UTF-8 aspell -a -l ', languageCode, ' > ', tf2)
    Sys.setlocale('LC_ALL','en_US.UTF-8') 
    system(aspell_command, intern=T)    
    aspell_results = strsplit(readChar(tf2, file.info(tf2)$size), '\n')[[1]]
    aspell_results[aspell_results == ''] = ' '
    aspell_results = aspell_results[-1] 
    aspell_results[grep('&',aspell_results)]= '%' 
    aspell_results[grep('#',aspell_results)]= '%'
    aspell_results[grep('\\?',aspell_results)]= '%' #added for Swedish, check backwards compatibility
    aspell_results[grep('-',aspell_results)]= '*'
    aspell_results[grep('\\+',aspell_results)]= '*' 
    
    returnCandidate = strsplit(gsub(' ','%',gsub('% ','%', gsub('\\* ','*',paste(aspell_results, collapse='')))), '')[[1]]
    if (length(returnCandidate) != length(strings)){
        stop('return data is not the same length as strings')
    }
    file.remove(tf) 
    file.remove(tf2)    
    return(returnCandidate == '*')  
}

getCorrelations = function(dataset){
    # get correlations for each combination of lexical and sublexical variable that is available in the data
	lexVar = c('unigramSurprisal', 'surprisal')
	sublexVar = c('ortho.len','ss')		
	df = as.data.frame(matrix(nrow=length(lexVar), ncol = length(sublexVar)))
	rownames(df) = lexVar
	colnames(df) = sublexVar	
	for (lv in lexVar){ #iterate over lexical variable
		for (sv in sublexVar){ #iterate over sublexical variable
			if (!(lv %in% names(dataset)) | !(sv %in% names(dataset))){
				df[lv,sv] = NA #dataset is missing relevant variable
			} else {
				df[lv,sv] = cor(dataset[[lv]],  dataset[[sv]],method = 'spearman')
			}
		}
	}
	return(df)
}

getScoresForLanguage= function(dataPath, opusPath, language, languageCode, locale){	
    # Load the lexical surprisal, merge against the OPUS dataset, and check with words are in the relevant aspell dictionary
	sp = read.table(paste0(dataPath,'surprisal-',language,'-3.txt'), stringsAsFactors=F, header=T, encoding='utf-8')
	sp$unigramSurprisal = -1 * log(sp$frequency / sum(as.numeric(sp$frequency)))
    #sp1 = read.table(paste0(dataPath,'surprisal-',language,'-1.txt'), stringsAsFactors=F, header=T)
	#sp1$unigramLogFrequency = log(sp1$frequency)
	#sp1_selected = sp1[,c('word','unigramLogFrequency')]	
	
	opusFilePath = paste0(opusPath,'OpenSubtitles-',language,'.txt')
    sp_opus = read.table(opusFilePath, stringsAsFactors=F, header=T, encoding='utf-8')	#this is ordered by frequency    
	
	#throw out items with punctuation. Need to set the locale to do this properly for each language	
	Sys.setlocale('LC_ALL',locale) 
	sp_opus = sp_opus[grep('[^[:punct:]]',sp_opus$word),]				
	Sys.setlocale('LC_ALL','en_US.UTF-8') 
						
	sp_opus_subset = merge(sp, sp_opus[0:70000,]) #only merge items from the 70k most frequent items 
	sp_opus_subset = sp_opus_subset[order(sp_opus_subset$frequency, decreasing=T),][1:min(25000, nrow(sp_opus_subset)),]
	# take the numver of rows in the subset if there are fewer than 25000 words in merged dataset

    if (languageCode == 'de'){
        #German nouns are always capitalized, so they will always be out-of-dictionary
        lowerInDict = aspell(sp_opus_subset$word, languageCode)     
        upperInDict = aspell(sapply(sp_opus_subset$word, simpleCap), languageCode)   
        sp_opus_subset$inDictionary = lowerInDict | upperInDict
    } else {
         sp_opus_subset$inDictionary = aspell(sp_opus_subset$word, languageCode) #in this language? 
    }

    sp_opus_subset$englishLoanWord = aspell(sp_opus_subset$word, 'en') # in english?
    sp_opus_subset$group = 'in dictionary'
    sp_opus_subset$group[!sp_opus_subset$inDictionary] = 'not in dictionary'
    sp_opus_subset$group[sp_opus_subset$englishLoanWord & !sp_opus_subset$inDictionary] = 'english'
    sp_opus_subset$group = factor(sp_opus_subset$group, levels=c('english','not in dictionary','in dictionary'))
    
    rlist = list()
    rlist[['scores']] = getCorrelationsForSubsets(sp_opus_subset)
    rlist[['df']] = sp_opus_subset    
    
    rlist[['scores']]$language = simpleCap(language)
     rlist[['df']]$language = simpleCap(language)
	return(rlist)
}
getCorrelationsForSubsets = function(spr){
    #in dictionary and in English
    fc0 = cor(spr$unigramSurprisal, spr$ortho.len, method='spearman')
    sc0 = cor(spr$surprisal, spr$ortho.len, method='spearman')
    d0 = data.frame(analysis = 'All types (replication)', 'cor_type' = c('frequency', 'surprisal'),cor = c(fc0, sc0), n = nrow(spr))   
    
    inLang_and_eng = subset(spr, group %in%  c('in dictionary', 'english'))
    fc1 = cor(inLang_and_eng$unigramSurprisal, inLang_and_eng$ortho.len, method='spearman')
    sc1 = cor(inLang_and_eng$surprisal, inLang_and_eng$ortho.len, method='spearman')
    d1 = data.frame(analysis = 'Types in dictionary + English words', 'cor_type' = c('frequency', 'surprisal'),cor = c(fc1, sc1), n = nrow(inLang_and_eng))   
    
    #items in dictionary and not-in-english referring expressions
    inLang_and_notInLang = subset(spr, group %in%  c('in dictionary', 'not in dictionary'))
    fc2 = cor(inLang_and_notInLang$unigramSurprisal, inLang_and_notInLang$ortho.len, method='spearman')
    sc2 = cor(inLang_and_notInLang$surprisal, inLang_and_notInLang$ortho.len, method='spearman')
    d2 = data.frame(analysis = 'Types in dictionary + Types not in dictionary', 'cor_type' = c('frequency', 'surprisal'),cor = c(fc2, sc2), n = nrow(inLang_and_notInLang))   

    #in dictionary only
    inLang = subset(spr, group %in%  c('in dictionary'))    
    fc3 = cor(inLang$unigramSurprisal, inLang$ortho.len, method='spearman')
    sc3 = cor(inLang$surprisal, inLang$ortho.len, method='spearman')
    d3 = data.frame(analysis = 'Types in dictionary', 'cor_type' = c('frequency', 'surprisal'),cor = c(fc3, sc3), n = nrow(inLang))                 
                    
    #out of dictionary only 
    notInLang = subset(spr, group %in%  c('not in dictionary'))
    fc4 = cor(notInLang$unigramSurprisal, notInLang$ortho.len, method='spearman')
    sc4 = cor(notInLang$surprisal, notInLang$ortho.len, method='spearman')
    d4 = data.frame(analysis = 'Types not in dictionary', 'cor_type' = c('frequency', 'surprisal'),cor = c(fc4, sc4), n = nrow(notInLang))
    
    fromEnglish = subset(spr, group %in%  c('english'))
    fc5 = cor(fromEnglish$unigramSurprisal, fromEnglish$ortho.len, method='spearman')
    sc5 = cor(fromEnglish$surprisal,fromEnglish$ortho.len, method='spearman')
    d5 = data.frame(analysis = 'English types', 'cor_type' = c('frequency', 'surprisal'),cor = c(fc5, sc5), n = nrow(fromEnglish))
    return(rbind(d0, d1, d2, d3, d4, d5))                
}

getWordExamples = function(spr){
    print(paste('[in language]', paste(sample(subset(spr, group == 'in dictionary')$word, 50), collapse=' ')))
    print(paste('[not in language]', paste(sample(subset(spr, group == 'not in dictionary')$word, 50), collapse=' ')))    
    if (nrow(subset(spr, group == 'english')) > 0){
        print(paste('[English]', paste(sample(subset(spr, group == 'english')$word, 50), collapse=' ')))
    }    
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
      sep="", collapse=" ")
}


getLanguageReport = function(languageName, aspellLanguage, locale){
    results = getScoresForLanguage(dataPath, opusPath, languageName,aspellLanguage, locale)
    results$language = languageName
    results[['scores']]
    getWordExamples(results[['df']])
    table(results[['df']]$group)
    g1 = ggplot(results[['df']]) + geom_violin(aes(x=group, y=ortho.len)) + xlab('Group') + ylab('Orthographic Length')
    g2 = ggplot(results[['df']]) + geom_violin(aes(x=group, y=surprisal)) + xlab('Group') + ylab('Mean Trigram Surprisal')
    g3 = ggplot(results[['df']]) + geom_violin(aes(x=group, y=unigramSurprisal)) + xlab('Group') + ylab('Mean Unigram Surprisal')
    options(repr.plot.width=8, repr.plot.height=3)
    multiplot(g1,g2,g3, cols=3)
    return(results)
}