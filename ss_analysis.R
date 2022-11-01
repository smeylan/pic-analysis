library('rPython')
library('ggplot2')
library('reshape')
library('robust')

inf2NA <- function(x) { x[is.infinite(x)] <- NA; x }

getISOforLangString = function(langString){
    if (langString %in% c('en','fr','de','he','es','ru','it','en','fr','es','pt','sv','cs','pl','de','it','ro', 'nl')){
        return(langString)
    } else {
    	langDict = list() 
    	langDict[['eng-all']] = 'en'
    	langDict[['fre-all']] = 'fr'
    	langDict[['ger-all']] = 'de'
    	langDict[['heb-all']] = 'he'
    	langDict[['spa-all']] = 'es'
    	langDict[['rus-all']] = 'ru'
        langDict[['ita-all']] = 'it'
    	langDict[['english']] = 'en'
    	langDict[['french']] = 'fr'
    	langDict[['spanish']] = 'es'
    	langDict[['portuguese']] = 'pt'
    	langDict[['swedish']] = 'sv'
    	langDict[['czech']] = 'cs'
    	langDict[['polish']] = 'pl'
    	langDict[['german']] = 'de'
    	langDict[['italian']] = 'it'
    	langDict[['romanian']] = 'ro'
    	langDict[['dutch']] = 'nl'
    	return(langDict[[tolower(langString)]])
    }   
}

getLanguageForISO = function(isoString){
	isoDict = list()
	isoDict[['en']] = 'English'
	isoDict[['fr']] = 'French'
	isoDict[['de']] = 'German'
	isoDict[['he']] = 'Hebrew'
	isoDict[['es']] = 'Spanish'
	isoDict[['ru']] = 'Russian'
	isoDict[['pt']] = 'Portuguese'
	isoDict[['sv']] = 'Swedish'
	isoDict[['cs']] = 'Czech'
	isoDict[['pl']] = 'Polish'
	isoDict[['it']] = 'Italian'
	isoDict[['ro']] = 'Romanian'
	isoDict[['nl']] = 'Dutch'
    isoDict[['it']] = 'Italian'
	return(isoDict[[tolower(isoString)]])
}

gb12_langRemapper = list()
gb12_langRemapper[['Heb-all']] = 'Hebrew'
gb12_langRemapper[['Eng-all']] = 'English'
gb12_langRemapper[['Spa-all']] = 'Spanish'
gb12_langRemapper[['Ita-all']] = 'Italian'
gb12_langRemapper[['Ger-all']] = 'German'
gb12_langRemapper[['Fre-all']] = 'French'
gb12_langRemapper[['Rus-all']] = 'Russian'


longColNames = list()
longColNames[['ipa_ss']] = "PIC - Phonemes"
longColNames[["ipa_ss"]] = "PIC - Phonemes"
longColNames[["ipa_n"]] = "Number of Phonemes"
longColNames[["ipa_ss-ipa_n"]] = "Difference: PIC vs. No. Phonemes"
longColNames[["character_ss"]] = "PIC - Characters"
longColNames[["ortho_n"]] = "Number of Characters"
longColNames[["character_ss-ortho_n"]] = "Difference: PIC vs. No. Characters"
longColNames[["unigramSurprisal"]] = "Unigram Surprisal"
longColNames[["trigramSurprisal"]] = "Trigram Surprisal"
longColNames[["unigramSurprisal-trigramSurprisal"]] = "Difference: Unigram Surprisal - Trigram Surprisal"
longColNames[["frequency"]] = "Frequency"

expandColNames = function(X){   
    X = as.factor(X)
    levels(X) = sapply(levels(X), function(x){return(longColNames[[x]])})    
    return(X)
}


getScoresForLanguage= function(resultsPath, datasetName, sublexSize, language, endMarker, corMethod, numCor=25000, contextLimit=0, opusFilter=F, morphSimple=F, filename='opus_meanSurprisal.csv',languageCode=NA){	
	# Used in Piantadosi et al. (2011) Replication on Google 1T and Books 2012.ipynb	
	
    # Load the lexical surprisal, merge against the OPUS dataset, and check with words are in the relevant aspell dictionary
    lexPath = paste0(resultsPath, '/',datasetName,'/',language,'/00_lexicalSurprisal/', filename)
    lex = read.csv(lexPath, stringsAsFactors=F, header=T, encoding='utf-8')    
    lex$index = 1:nrow(lex) #retain the order of the retrieval wordlist    

    if (as.numeric(sublexSize) > 0){
        sublexPath = paste0(resultsPath,'/',datasetName,'/', language, '/01_sublexicalSurprisal/', sublexSize ,'_sublex.csv')       
        sublex = read.csv(sublexPath, stringsAsFactors=F, header=T, encoding='utf-8')
        sublex$word = sapply(sublex$word, tolower)    
        df =  merge(lex, sublex, by='word')	
    } else {
        df = lex
        df$character_n = sapply(df$word, nchar)
        df$ortho_n = sapply(df$word, nchar)
    }    


	df = df[order(df$index),]
	print(paste0('original number in ',language,': ',nrow(df)))
	if (nrow(df) > numCor){
		df = df[1:numCor,] # take the top 25000 words		
	}
    print('Limiting analysis')

	if (contextLimit > 0){
		df = subset(df, numContexts > contextLimit)
	}
	
    if (morphSimple){
        # load the relevant list of monomorphemic words

        # first get the CELEX code
        if (language %in% c('eng-all','en','eng','ENGLISH')){
            celex_code= 'E'
            celex_language = 'ENGLISH'
        } else if (language %in% c('ger-all','de','ger','GERMAN')){
            celex_code= 'G'
            celex_language = 'GERMAN'
        } else if (language %in% c('nl','DUTCH')){    
            celex_code= 'D'
            celex_language = 'DUTCH'
        }    

        raw = read.table(paste0('data/CELEX2/', celex_language,'/',celex_code,'ML/', celex_code,'ML.CD'),sep='\\', quote= "", encoding='utf-8', fill=T, stringsAsFactors=T, col.names = seq(0,44))
        morph = raw[,c(2,4)] 
        names(morph) = c('word','morph')
        print(dim(morph))
        morph_simp = unique(subset(morph, morph == 'M')$word)


        df = subset(df, word %in% morph_simp)
    } 


	if (opusFilter){
		#filter by frequency residuals from the relevant OPUS dataset. This removes words for which the dataset may not have a good estimate of lexical statistics
		
        print('Filtering...')
		opusPath = paste0('data/OPUS/noFilter_',getISOforLangString(language),'_2013.txt')
		opus = read.table(opusPath, header=T, encoding='utf-8')		
        

		names(opus)[names(opus) == 'count'] = 'opusCount'
		# df = merge(df, opus)		
		# df$opus_residuals = lm(frequency ~ opusCount, data=df)$residuals
		# print(dim(df))
		# P = ecdf(df$opus_residuals)
		# df$opus_residual_empirical = P(df$opus_residuals)	
		
		# df = subset(df, df$opus_residual_empirical > .1 & df$opus_residual_empirical < .9)			
        df = merge(opus, df)
        df = df[order(df$frequency, decreasing=T),][1:25000,]        
	}
	print(paste0('filtered number in ',language,': ',nrow(df)))
	
    
    #if endMarker is false, remove the end symbol from the sublexical surprisal array and recompute
    if (!endMarker){
        if('ipa_ss_array' %in% names(df)){
            df$ipa_ss_array = sapply(strsplit(gsub('\\[','',gsub('\\]','',df$ipa_ss_array)),','), function(x){as.numeric(x)[1:(length(x)-1)]})
            df$ipa_n = sapply(df$ipa_ss_array, length)
            df$ipa_ss = sapply(df$ipa_ss_array, sum)
        }
        # if('sampa_ss_array' %in% names(df)){
            # df$sampa_ss_array = sapply(strsplit(gsub('\\[','',gsub('\\]','',df$sampa_ss_array)),','), function(x){as.numeric(x)[1:(length(x)-1)]})
            # df$sampa_n = sapply(df$sampa_ss_array, length)
            # df$sampa_ss = sapply(df$sampa_ss_array, sum)
        # }
        if('character_ss_array' %in% names(df)){
            df$character_ss_array = sapply(strsplit(gsub('\\[','',gsub('\\]','',df$character_ss_array)),','), function(x){as.numeric(x)[1:(length(x)-1)]})
            df$character_n = sapply(df$character_ss_array, length)
            df$character_ss = sapply(df$character_ss_array, sum)
            
        }

        if('token_ipa_ss_array' %in% names(df)){
            df$token_ipa_ss_array = sapply(strsplit(gsub('\\[','',gsub('\\]','',df$token_ipa_ss_array)),','), function(x){as.numeric(x)[1:(length(x)-1)]})
            df$token_ipa_n = sapply(df$token_ipa_ss_array, length)
            df$token_ipa_ss = sapply(df$token_ipa_ss_array, sum)
        }


        if('character_ipa_ss_array' %in% names(df)){
            df$character_ipa_ss_array = sapply(strsplit(gsub('\\[','',gsub('\\]','',df$character_ipa_ss_array)),','), function(x){as.numeric(x)[1:(length(x)-1)]})
            df$character_ipa_n = sapply(df$character_ipa_ss_array, length)
            df$character_ipa_ss = sapply(df$character_ipa_ss_array, sum)
        }

        df$log_ortho_n = log(df$ortho_n)
        
    }
    
    df$endMarker = endMarker
    df$trigramSurprisal = df$mean_surprisal_weighted
    df$prob = df$frequency / sum(as.numeric(df$frequency), na.rm=T)
    df$unigramSurprisal = -1 *  log(df$prob)
   
   	if ('ipa_n' %in% names(df)){
		df$log_ipa_n = log(as.numeric(df$ipa_n))		
	} else {
		df$log_ipa_n = NA		
	}
	df$log_character_n = log(df$character_n)	
   
    results = list()    
	if (datasetName  == 'OPUS'){
    	df$language = getLanguageForISO(language)    	
    } else{
		df$language = simpleCap(language)
    }    
    
    print('Getting correlations')
    results[['score']] = getCorrelations2(df,corMethod)
    results[['score']]$endMarker = endMarker
    if (datasetName  == 'OPUS'){
    	results[['score']]$language = getLanguageForISO(language)    	
    } else{
		results[['score']]$language = simpleCap(language)    	
    }

    print('At the dictionary check')
    print(paste('languageCode:', languageCode))
    #dictionary check
    if (!is.na(languageCode)){
        print('In the dictionary check')
        if (languageCode == 'pt'){
            languageCode = 'pt-br'
        } else if (languageCode == 'en')  {
            languageCode = 'en-us'
        }   

        if (languageCode == 'de'){
            #German nouns are always capitalized, so they will always be out-of-dictionary
            lowerInDict = aspell(df$word, languageCode)     
            upperInDict = aspell(sapply(df$word, simpleCap), languageCode)   
            df$inDictionary = lowerInDict | upperInDict
        } else {
             df$inDictionary = aspell(df$word, languageCode) #in this language? 
        }

        df$englishLoanWord = aspell(df$word, 'en') # in english?
        df$group = 'in dictionary'
        df$group[!df$inDictionary] = 'not in dictionary'
        df$group[df$englishLoanWord & !df$inDictionary] = 'english'
        df$group = factor(df$group, levels=c('english','not in dictionary','in dictionary'))   
    }     

    results[['df']] = df
    
	return(results)
}

bootstrappedCI = function(df, param1, param2, numIter, method='spearman'){
    #corrs = mat.or.vec(1,numIter)
    #for (i in 1:numIter){
    corrs = unlist(mclapply(1:numIter, function(i){
        sampleIndices = sample(1:nrow(df), nrow(df), replace=T)
        ndf =df[sampleIndices,]         
        #corrs[i] = cor(ndf[[param1]], ndf[[param2]] , method, use='pairwise.complete.obs')
        return(cor(ndf[[param1]], ndf[[param2]] , method, use='pairwise.complete.obs'))
    }, mc.cores=detectCores()))        
    return(corrs)    
}

getCorrelations = function(dataset, corMethod){
    # get correlations for each combination of lexical and sublexical variable that is available in the data            
    
    lexVar = c('unigramSurprisal', 'trigramSurprisal', 'frequency')
    sublexVars = list(c('ipa_ss','ipa_n'),c('character_ss','ortho_n'))#'sampa_ss','sampa_n','log_ipa_ss','log_ipa_n','log_ortho_n','log_character_ss'   
    # sublexVars are provided in pairs for taking the partial correlations
    
    print(names(dataset))
    if ('token_ipa_ss' %in% names(dataset)){
        print('token_ipa_ss found')
        sublexVars = c(sublexVars, list(c('token_ipa_ss','ipa_n')))
    }    

    # these are added separately because of Hebrew
    if ('token_character_ss' %in% names(dataset)){
        print('token_character_ss found')
        sublexVars = c(sublexVars, list(c('token_character_ss', 'ortho_n')))
    }    

    #print(dataset$character_ipa_ss[1:100])
    #stop()

    df = do.call('rbind', lapply(lexVar, function(lv){ #iterate over lexical variables
        do.call('rbind', lapply(sublexVars, function(svs){ #iterate over sublexical variables             
            print(svs)
            if (!(lv %in% names(dataset)) | !(svs[1] %in% names(dataset)) | !(svs[2] %in% names(dataset))){
                print('Missing something!')
                 singleCor = NA #dataset is missing relevant variable
                 ci = c(NA, NA)                 
            } else {
                lexSurps = inf2NA(dataset[[lv]])
                if (any(lexSurps[!is.na(lexSurps)] < 0)){
                    stop('negative surprisal values')
                }                               
                dataset[[lv]] = inf2NA(dataset[[lv]])
                dataset[[svs[1]]] = inf2NA(dataset[[svs[1]]])
                dataset[[svs[2]]] = inf2NA(dataset[[svs[2]]])

                # So we can use a single inner loop for bootstrapping
                cors_df = percentile_bootstrap_cor_diff(dataset, lv, svs[1], svs[2], alpha=.01, residualize=F, corMethod=corMethod)
                partial_cors_df = percentile_bootstrap_cor_diff(dataset, lv, svs[1], svs[2], alpha=.01, residualize=T, corMethod=corMethod)

                return(rbind(cors_df, partial_cors_df))   
            }
        }))
    })) #, mc.cores=detectCores()
    
    return(df)
}

getCorrelations2 = function(dataset, corMethod){
    # Abstracted to handle xvars and yvars; yvars are compared directly
    # calls _getCor2 twice, b/c there is different bootstrapping going on

    sv_comparison = getCorHelper2(dataset,corMethod, xvars = c('unigramSurprisal', 'trigramSurprisal', 'frequency'),
        yvars = list(c('ipa_ss','ipa_n'),c('character_ss','ortho_n')))
    
    lv_comparison = getCorHelper2(dataset,corMethod, xvars = c('ipa_ss','ipa_n','character_ss','ortho_n'),
        yvars = list(c('unigramSurprisal', 'trigramSurprisal', 'frequency')))        

    both_comparisons = rbind.fill(sv_comparison, lv_comparison)
    return(both_comparisons)

}

 
 getCorHelper2 = function(dataset, corMethod, xvars, yvars){   
    # Abstracted to handle xvars and yvars; yvars are dyads for comparison
    
    #!!! can't remember the function of this
   
    # print(names(dataset))
    # if ('token_ipa_ss' %in% names(dataset)){
    #     print('token_ipa_ss found')
    #     sublexVars = c(sublexVars, list(c('token_ipa_ss','ipa_n')))
    # }    

    # # these are added separately because of Hebrew
    # if ('token_character_ss' %in% names(dataset)){
    #     print('token_character_ss found')
    #     sublexVars = c(sublexVars, list(c('token_character_ss', 'ortho_n')))
    # }    

    #print(dataset$character_ipa_ss[1:100])
    #stop()

    df = do.call('rbind.fill', lapply(xvars, function(xv){ #iterate over lexical variables
        do.call('rbind.fill', lapply(yvars, function(yvs){ #iterate over sublexical variables                         
            if (!(xv %in% names(dataset)) | !(yvs[1] %in% names(dataset)) | !(yvs[2] %in% names(dataset))){
                print('Missing something!')
                 singleCor = NA #dataset is missing relevant variable
                 ci = c(NA, NA) 
                 return(data.frame())                
            } else {
                X = inf2NA(dataset[[xv]])
                if (any(X[!is.na(X)] < 0)){
                    stop('negative surprisal values')
                }                               
                dataset[[xv]] = inf2NA(X)
                dataset[[yvs[1]]] = inf2NA(dataset[[yvs[1]]])
                dataset[[yvs[2]]] = inf2NA(dataset[[yvs[2]]])                


                # So we can use a single inner loop for bootstrapping
                cors_df = percentile_bootstrap_cor_diff(dataset, xv, yvs[1], yvs[2], alpha=.01, residualize=F, corMethod=corMethod)
                partial_cors_df = percentile_bootstrap_cor_diff(dataset, xv, yvs[1], yvs[2], alpha=.01, residualize=T, corMethod=corMethod)

                return(rbind.fill(cors_df, partial_cors_df))   
            }
        }))
    }))
    
    return(df)
}


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
      sep="", collapse=" ")
}

aspell = function(strings, languageCode, upperOK=NA){    
    if (languageCode == 'de'){        
        upperOK = T
    } else {
        upperOK =F
    }

    python.assign('strings', strings)
    python.exec( "import aspell" )
    python.exec(paste0("speller = aspell.Speller(('lang','",languageCode,"'),('encoding','utf-8'))"))
    python.exec("lower = [speller.check(x.encode('utf-8')) for x in strings]")
    lower = python.get('lower') == 1
    
    if(upperOK){
        python.exec("upper = [speller.check(x.title().encode('utf-8')) for x in strings]")   
        upper = python.get('upper') == 1
        rlist = upper | lower
    }        
    else {
        rlist = lower
    }    
    if (length(strings) != length(rlist)){
        stop('Dictionary tags are of different shape than input')
    }
    return(rlist)
}    

getInDictionaryCorrelations = function(scores,language, languageCode){
    if (language %in% c('GERMAN')){        
        scores[['df']]$aspell = aspell(scores[['df']]$word, languageCode, upperOK=T)
    } else{
        scores[['df']]$aspell = aspell(scores[['df']]$word, languageCode, upperOK=F)
    }
        
    scores[['df']] = subset(scores[['df']], !is.na(mean_surprisal_weighted) & !is.na(ortho_n))

    inDictionary = subset(scores[['df']], aspell)
    outOfDictionary = subset(scores[['df']], !aspell)

    print(paste(nrow(inDictionary) / nrow(scores[['df']]), ' % of items are in the dictionary' ))

    print('Correlation: Length ~ Lexical Surprisal')
    c1 = cor(inDictionary$ortho_n, -1*inDictionary$mean_surprisal_weighted, method='spearman')
    rdf = data.frame('inDict_surpCor'=c1)
    print(c1)
    print('Correlation: Length ~ Lexical Frequency')
    c2 = cor(inDictionary$ortho_n, -1*inDictionary$frequency, method='spearman')
    print(c2)
    rdf$inDict_freqCor = c2
    rdf$n = nrow(inDictionary)
    rdf$language = language
    return(rdf)
}   

getInfoContentDF = function(sp_lex_path, current_lex_path){
	#Used in Mean Information Content Comparison for Google 1T.ipynb
	
    sp_lex = read.table(sp_lex_path, stringsAsFactors=F, header=T, encoding='utf-8')    
    current_lex = read.csv(current_lex_path, encoding='utf-8')
    current_lex$mean_surprisal_weighted = -1 * log(exp(-1 * current_lex$mean_surprisal_weighted), base=2)	
    
    combined = merge(sp_lex, current_lex, by='word')            
    combined = combined[!is.na(combined$mean_surprisal_weighted) & !is.na(combined$surprisal),]
    print(names(combined))
    #combined[order(combined$frequency_x, decreasing=T),][1:25000,]
    
    
    # which items have the highest and lowest residuals?
    lm1 = lm(surprisal ~ mean_surprisal_weighted, combined)
    combined$residuals = lm1$residuals        
    return(combined)
}

getInfoContentCor = function(df){        
	#Used in Mean Information Content Comparison for Google 1T.ipynb
	
    correlations = list()    
    correlations['spearman'] = cor(df$surprisal, df$mean_surprisal_weighted, method='spearman')
    correlations['pearson'] = cor(df$surprisal, df$mean_surprisal_weighted, method='pearson')

    freq_correlations = list(0)
    freq_correlations['spearman'] = cor(log(df$frequency.x/ sum(as.numeric(df$frequency.x))), log(df$frequency.y/ sum(as.numeric(df$frequency.y))), method='spearman')
    freq_correlations['pearson'] = cor(log(df$frequency.x/sum(as.numeric(df$frequency.x))), log(df$frequency.y/ sum(as.numeric(df$frequency.y))), method='pearson')    
    return(list(correlations, freq_correlations))
}    

getInfoContentPlot = function(df){
	#Used in Mean Information Content Comparison for Google 1T.ipynb
	
    options(repr.plot.width=10, repr.plot.height=8, jupyter.plot_mimetypes = 'image/png')
    ggplot(df) + geom_point(aes(x=mean_surprisal_weighted, y=surprisal))+xlab(
        'Current Model')+ylab('Piantadosi et al. (2011)') + geom_abline(intercept = 0, slope = 1, colour='red')
}    

getBinnedLengthForPredictor = function(df, word_predictor, sentence_predictor, numBins){
    #sentence_predictor is unigram or trigram surprisal
    #word_predictor is ortho_n or ipa_ss 
    df$quantile = floor(ecdf(df[[sentence_predictor]])(df[[sentence_predictor]]) / (1/numBins)) 
    se <- function(x) sqrt(var(x)/length(x))
        
    length_gram_prob_mean = aggregate(get(word_predictor) ~ quantile, df, mean)
    names(length_gram_prob_mean) = c('quantile','mean')    
    length_gram_prob_se = aggregate(get(word_predictor) ~ quantile, df, se)
    names(length_gram_prob_se) =  c('quantile','se')
    length_gram_prob_1= merge(length_gram_prob_mean, length_gram_prob_se)
    length_gram_prob = aggregate(get(sentence_predictor) ~ quantile, df, mean)    
    names(length_gram_prob) =  c('quantile','mean_surprisal')    
    length_prob  = merge(length_gram_prob_1, length_gram_prob)              
    length_prob$se_high = length_prob$mean+length_prob$se
    length_prob$se_low = length_prob$mean-length_prob$se
    length_prob$sentence_predictor = sentence_predictor
    length_prob$word_predictor = word_predictor
    return(length_prob)
}

 getZippedVocabSize = function(strVec){
    # write to file    
    tf = tempfile()
    
    fileConn<-file(tf)
    writeLines(paste(strVec, collapse=' '), fileConn)
    close(fileConn)
        
    #zip it, gzip -c file.txt > file.txt.gz
    system(paste0('gzip -c ',tf, ' > ',tf,'.gz'))
    
    #get the size in bytes
    uncompressedSize = file.info(tf)$size
    compressedSize = file.info(paste0(tf,'.gz'))$size
    file.remove(tf)
    file.remove(paste0(tf,'.gz'))
    return(compressedSize / uncompressedSize)
}


queryZS = function(query,zs_file,finalColumnLabels){    
	command = paste0('zs dump ', zs_file,' --prefix="', query,'"')
	print(command)
    returnData  = system(command, intern=T)
    returnFrame  = do.call('rbind', lapply(strsplit(returnData,' |\t'), function(x){data.frame(t(x), stringsAsFactors=F)}))
    names(returnFrame) = paste0('w', 1:ncol(returnFrame))
	names(returnFrame)[(length(names(returnFrame))+1-length(finalColumnLabels)):length(names(returnFrame))] = finalColumnLabels 
    for (columnTitle in c('count', 'context')){
    	if (columnTitle %in% names(returnFrame))
		returnFrame[[columnTitle]] = as.numeric(returnFrame[[columnTitle]])
	}	

	return(returnFrame[order(returnFrame$count, decreasing=T),])
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
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

getCorsForDataset = function(dataset, robust=F,sentence_measures = c('ipa_n','ipa_ss'), word_measures = c('unigramSurprisal','mean_surprisal_weighted') ){
    rmat = mat.or.vec(length(sentence_measures), length(word_measures))    
    
    for (sm in c(1:length(sentence_measures))){
        for (wm in c(1:length(word_measures))){
            if (robust){
                lm = lmRob(as.formula(paste(sentence_measures[sm], '~' ,word_measures[wm])), dataset)                
            } else {
                lm = lm(as.formula(paste(sentence_measures[sm], '~' ,word_measures[wm])), dataset)                
            }                   
            rmat[sm, wm] = sqrt(summary(lm)$r.squared)
        }
    }
    
    rdf = data.frame(rmat)        
    names(rdf) = c('uniSurp','triSurp')
    rdf$sm = sentence_measures
    return(rdf)
}    

numformat <- function(val) { gsub("0\\.", ".", as.character(sprintf("%.2f", val))) }

get_spearman_cors = function(df, aggColumn){
  sp_ipa_ss_trigramSurprisal = cor(df$ipa_ss, df$mean_surprisal_weighted, method='spearman', use='pairwise.complete.obs')
  sp_ipa_ss_logFrequency = cor(df$ipa_ss, df$logFrequency, method='spearman', use='pairwise.complete.obs') 
  sp_ipa_n_trigramSurprisal = cor(df$ipa_n,df$mean_surprisal_weighted, method='spearman', use='pairwise.complete.obs')  
  sp_ipa_n_logFrequency = cor(df$ipa_n,df$logFrequency, method='spearman', use='pairwise.complete.obs')

  sp_character_ss_trigramSurprisal = cor(df$character_ss, df$mean_surprisal_weighted, method='spearman', use='pairwise.complete.obs')
  sp_character_ss_logFrequency = cor(df$character_ss, df$logFrequency, method='spearman', use='pairwise.complete.obs') 
  sp_character_n_trigramSurprisal = cor(df$character_n,df$mean_surprisal_weighted, method='spearman', use='pairwise.complete.obs')  
  sp_character_n_logFrequency = cor(df$character_n,df$logFrequency, method='spearman', use='pairwise.complete.obs')

  rdf = data.frame(              
      sp_ipa_ss_trigramSurprisal,
      sp_ipa_ss_logFrequency,
      sp_ipa_n_trigramSurprisal,
      sp_ipa_n_logFrequency,
      sp_character_ss_trigramSurprisal,
      sp_character_ss_logFrequency,
      sp_character_n_trigramSurprisal,
      sp_character_n_logFrequency,
       n=nrow(df))
  for (colname in c('sp_ipa_ss_trigramSurprisal','sp_ipa_ss_logFrequency','sp_ipa_n_trigramSurprisal','sp_ipa_n_logFrequency', 'sp_character_ss_trigramSurprisal','sp_character_ss_logFrequency','sp_character_n_trigramSurprisal','sp_character_n_logFrequency')){ 
      rdf[[paste0(colname,'_str')]] = numformat(rdf[[colname]])
  }    
  rdf[[aggColumn]] = unique(df[[aggColumn]])
  return(rdf)  
}

partialCor = function(language, voi, toPartial, method='spearman'){
    df = language[['df']]
    inf2NA <- function(x) { x[is.infinite(x)] <- NA; x }
    #force toPartial from ipa_ss to character_ss in the case of hebrew
    print(paste0('Processing language: ', df$language[1]))
    if ((df$language[1] %in% c('Hebrew','He','Heb-all', 'he')) & (toPartial %in% c('ipa_ss','ipa_n','token_ipa_ss'))){
        #newPartial = gsub('ipa','character',toPartial)
        #newVoi = gsub('ipa','character',voi)
        #print(paste('Hebrew: substititing', newPartial, 'for', toPartial))
        #toPartial = newPartial
        #voi = newVoi        
        rdf = rbind(data.frame(lv='Unigram Surprisal',singleCor=NA),
        data.frame(lv='Trigram Surprisal', singleCor=NA),
        data.frame(lv='Frequency', singleCor=NA))        
    
        rdf$language = df$language[1]
        return(rdf)        

    }        
    if (!(voi %in%  names(df)) | !(toPartial %in% names(df))){      
        print('voi or toPartial not in data frame')
        unigramCor = NA
        trigramCor = NA
        frequencyCor = NA
    } 
    else {
        print(voi)
        print(toPartial)
        print(nrow(df))   
        df = subset(df, !is.na(df[[voi]]) & !is.infinite(df[[voi]])& !is.na(df[[toPartial]]) & !is.infinite(df[[toPartial]]) )        
        print(paste0(nrow(df),' items after removing infinites'))
        df$residuals = lm(as.formula(paste(voi, '~', toPartial)), df)$residuals
        unigramCor = cor(inf2NA(df$unigramSurprisal), inf2NA(df$residuals), method=method, use = 'pairwise.complete.obs')    
        trigramCor = cor(inf2NA(df$trigramSurprisal), inf2NA(df$residuals), method=method, use = 'pairwise.complete.obs')    
        frequencyCor = cor(inf2NA(df$frequency), inf2NA(df$residuals), method=method, use = 'pairwise.complete.obs')    
    }    
    rdf = rbind(data.frame(lv='Unigram Surprisal',singleCor=unigramCor),
    data.frame(lv='Trigram Surprisal', singleCor=trigramCor),
    data.frame(lv='Frequency', singleCor=frequencyCor))        
    
    rdf$language = df$language[1]
    return(rdf)        
}

#partialCor is not generalized to handle frequency vs. surprisal partial correla


partialCor2 = function(language, voi, toPartial, method='spearman', 
    depvars=c('unigramSurprisal', 'trigramSurprisal','frequency'), 
    depvars_labels= c('Unigram Surprisal','Trigram Surprisal', 'Frequency'),
    depvars_axis='lv',
    indvar_axis= 'sv') {
    df = language[['df']]
    inf2NA <- function(x) { x[is.infinite(x)] <- NA; x }
    #force toPartial from ipa_ss to character_ss in the case of hebrew
    print(paste0('Processing language: ', df$language[1]))
    if ((df$language[1] %in% c('Hebrew','He','Heb-all', 'he')) & (toPartial %in% c('ipa_ss','ipa_n','token_ipa_ss'))){
        #newPartial = gsub('ipa','character',toPartial)
        #newVoi = gsub('ipa','character',voi)
        #print(paste('Hebrew: substititing', newPartial, 'for', toPartial))
        #toPartial = newPartial
        #voi = newVoi        
        rdf = rbind(data.frame(lv='Unigram Surprisal',singleCor=NA),
        data.frame(lv='Trigram Surprisal', singleCor=NA),
        data.frame(lv='Frequency', singleCor=NA))        
    
        rdf$language = df$language[1]
        return(rdf)        

    }        
    if (!(voi %in%  names(df)) | !(toPartial %in% names(df))){      
        print('voi or toPartial not in data frame')
        rdf = rbind(data.frame(lv='Unigram Surprisal',singleCor=NA),
        data.frame(lv='Trigram Surprisal', singleCor=NA),
        data.frame(lv='Frequency', singleCor=NA))        
    } 
    else {
        print(voi)
        print(toPartial)
        print(nrow(df))   
        df = subset(df, !is.na(df[[voi]]) & !is.infinite(df[[voi]])& !is.na(df[[toPartial]]) & !is.infinite(df[[toPartial]]) )        
        print(paste0(nrow(df),' items after removing infinites'))
        df$residuals = lm(as.formula(paste(voi, '~', toPartial)), df)$residuals

        getPartialScore = function(df, depvar, depvar_label, depvar_axis, method){
            this_cor = cor(inf2NA(df[[depvar]]), inf2NA(df$residuals), method=method, use = 'pairwise.complete.obs')    
            prdf = data.frame(singleCor=this_cor)
            prdf[[depvar_axis]] =  depvar_label
            return(prdf)
        }        

        
        rdf = do.call('rbind', lapply(1:length(depvars_labels), function(i){
            return(getPartialScore(df, depvars[i], depvars_labels[i], depvars_axis, method))
        }))    
           
        rdf$language = df$language[1]
        
    }       
    return(rdf)        
}

percentile_bootstrap_cor_diff = function(df_local, x, y1, y2, alpha=.05, residualize, R=2500, plot=F, corMethod){
    # Following Wilcox, 2016; https://garstats.wordpress.com/2017/03/01/comp2dcorr/
    
    df_local = df_local[!(is.na(df_local[[x]]) | is.na(df_local[[y1]]) | is.na(df_local[[y2]])),] #drop na values; infinite 

    N <- nrow(df_local)
    hi = floor((1-alpha/2)*R+.5)
    lo = floor((alpha/2)*R+.5)
    #print(paste('Hi:', hi))
    #print(paste('Lo:', lo))

    # bootstrapping
    samples = do.call('rbind',mclapply(1:R, function(r){
    #samples = do.call('rbind',lapply(1:R, function(r){
        idx <- sample.int(N, N, replace = TRUE)         
        df_copy1 = df_local
        df_copy2 = df_local
        
        if (residualize){
            y1_residualized = lm(reformulate(termlabels = y2, response = y1), data=df_copy1)$residuals
            y2_residualized = lm(reformulate(termlabels = y1, response = y2), data=df_copy2)$residuals

            # delayed assignment, else the second lm is fit AFTER a covariate is replaced
            df_copy1[[y1]] = y1_residualized
            df_copy2[[y2]] = y2_residualized
            #!!! both y1 and y2 residualized
        }
        
        corr1 <- cor(df_copy1[idx,x], df_copy1[idx,y1], method=corMethod)         
        corr2 <-  cor(df_copy2[idx,x], df_copy2[idx,y2], method=corMethod)        
        print('corr2:')
        if (is.na(corr2)){
            print(x)
            print(df_local[idx,x][1:100])
            print(y2)
            print(df_local[idx,y2][1:100])
            stop('corr2 is NA')
        }


        return(data.frame(corr1=corr1, corr2=corr2))
    #}))
    }, mc.cores=detectCores()))    
        
    corr1.boot <- samples$corr1
    corr2.boot <- samples$corr2                  
    
    #empirical
    if (residualize){        
        y1_residualized = lm(reformulate(termlabels = y2, response = y1), data=df_local)$residuals        
        y1_empirical_cor = cor(y1_residualized, df_local[[x]], method=corMethod)    
        y1_resid_var = y2

        y2_residualized = lm(reformulate(termlabels = y1, response = y2), data=df_local)$residuals        
        y2_empirical_cor = cor(y2_residualized, df_local[[x]], method=corMethod)    
        y2_resid_var = y1
        diff_resid_var = paste(y1,'-',y2)

    } else {
        y1_empirical_cor = cor(df_local[[y1]], df_local[[x]], method=corMethod)    
        y2_empirical_cor = cor(df_local[[y2]], df_local[[x]], method=corMethod)
        y1_resid_var = NA
        y2_resid_var = NA
        diff_resid_var = NA
    }

    #first variable, y1 ~ x
    corr1.boot.sort = sort(corr1.boot)
    y1_ci = c(corr1.boot.sort[lo], corr1.boot.sort[hi])
    y1_return = data.frame(yv=y1, xv = x, resid_var = y1_resid_var , meanBootstrap = mean(corr1.boot),
        singleCor=y1_empirical_cor, stringsAsFactors=F,lower=y1_ci[1], upper=y1_ci[2], pvalue=NA, direction = NA) 
    
    #second variable, y2 ~ x   
    corr2.boot.sort = sort(corr2.boot)
    y2_ci = c(corr2.boot.sort[lo], corr2.boot.sort[hi])
    y2_return = data.frame(yv=y2, xv = x, resid_var = y2_resid_var, meanBootstrap = mean(corr2.boot),
        singleCor=y2_empirical_cor, stringsAsFactors=F,lower=y2_ci[1], upper=y2_ci[2], pvalue=NA, direction = NA) 
    
    # difference between y ~ x1 and y ~ x2
    bootdiff = corr1.boot - corr2.boot
    
    bootdiffsort = sort(bootdiff);
    diffci = c(bootdiffsort[lo], bootdiffsort[hi])         
    

    if (plot){
        plot_df = data.frame(bootdiff)
        p1 = ggplot(plot_df) + geom_density(aes(x=bootdiff)) + theme_bw() + geom_vline(xintercept=diffci, colour='red')
        print(p1)        
    }
    

    if (diffci[1] < 0){
        direction = 'lower'        
        pvalue_lt0 = mean(bootdiffsort < 0)        
        pvalue = 2*min(pvalue_lt0,1-pvalue_lt0)
    } else {
        direction = 'higher'        
        pvalue_gt0 = mean(bootdiffsort > 0)
        pvalue = 2*min(pvalue_gt0,1-pvalue_gt0)
    }    
    
    
    # see https://stats.stackexchange.com/questions/20701/computing-p-value-using-bootstrap-with-r: "percentile bootstrap"
    
    difference = data.frame(yv= paste0(y1,'-',y2) , xv = x , resid_var = diff_resid_var, meanBootstrap = mean(bootdiff),
        singleCor=NA, stringsAsFactors=F,lower=diffci[1], upper=diffci[2], pvalue, direction = direction)
    
    combined_df = rbind(difference, y1_return, y2_return)
    combined_df$corMethod = corMethod
    combined_df$residualize = residualize
    
    return(combined_df)
}

sig_symbol = function(pvalue, direction){
    if (direction == 'lower'){
        if (abs(pvalue) < .001){
            return("***")####
        } else if (abs(pvalue) < .01){
            return("**")
        } else if (abs(pvalue) < .05){
            return("*")
        } else {
            return("ns")
        }
    } else {
        if (abs(pvalue) < .001){
            return("***")####
        } else if (abs(pvalue) < .01){
            return("**")
        } else if (abs(pvalue) < .05){
            return("*")
        } else {
            return("ns")
        }
    }

}