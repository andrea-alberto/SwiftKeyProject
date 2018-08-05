library(shiny)
library(tm)
library(data.table)
library(SnowballC)

## configurations
NGRAM_SEPARATOR <- " "

workingDir <- "data"

dirtyWordsFileName <- "dirtyWords_EN.txt"
dirtyWordsFilePath <- file.path(workingDir,dirtyWordsFileName)

currentSampledVer <- "80" # 005 01 02 80
vocFileNameWithPath <- file.path(workingDir,"vocabulary.Rds")

freqLimit <- 2146 #get 75% of coverage on twitter + news
#freqLimit <- 266 #get 90% of coverage on twitter + news
#freqLimit <- 50 #get 95% of coverage on twitter + news
ngramsFileNameWithPath <- file.path(workingDir, sprintf("ngrams-leftover-freq%d.Rds", freqLimit))

## model's data
ngramsModelDT <- readRDS(file=ngramsFileNameWithPath)	
vocabularyDT <- readRDS(file=vocFileNameWithPath)
vocabularyDT <- vocabularyDT[ freq > 1]
vocabularyDT[, index := .I]
dirtyWordsVector <- scan(dirtyWordsFilePath, what="character", sep="")

tokenizeCorpus <- function(corpus, addStartLinePlaceHolder=FALSE, dirtyWordsVector=NA) {
	if (addStartLinePlaceHolder)
		# remove non ASCII characters
		corpus <- tm_map(corpus, content_transformer(gsub), pattern = "^", replacement=SENTENCE_START_PLACEHOLDER)	
		
	# remove non ASCII characters
	corpus <- tm_map(corpus, content_transformer(gsub), pattern = "[^\x20-\x7E]", replacement=" ")	
	# strip white spaces
	#corpus <- tm_map(corpus, stripWhitespace)
	# remove punctuation
	corpus <- tm_map(corpus, removePunctuation, 
							preserve_intra_word_contractions = TRUE,
                          preserve_intra_word_dashes = TRUE)
	# remove numbers
	corpus <- tm_map(corpus, removeNumbers)
	# convert to non-capital characters
	corpus <- tm_map(corpus, content_transformer(tolower))
	# remove english stop-words
	corpus <- tm_map(corpus, removeWords, stopwords("english"))
	# remove dirty words
	if (is.null(dirtyWordsVector))
		dirtyWordsVector <- scan(dirtyWordsFilePath, what="character", sep="")
	corpus <- tm_map(corpus, removeWords, dirtyWordsVector)	
	# stem/tokenize the document
	corpus <- tm_map(corpus, stemDocument, "english")
	return(corpus)
}

predictOneWordFromHistoryIndexes <- function( historyIndexVector ){
	print("predictOneWordFromHistoryIndexes")
    result <- getNextWordFromEncoded4Gram(historyIndexVector,ngramsModelDT,vocabularyDT)
	word <- "Default"
	print("result")
	print(result)
	if (is.data.table(result) & (nrow(result)==1)) {
		wordCoded <- switch (result$n,
				-1,
				result$w2,
				result$w3,
				result$w4
		)
		if (wordCoded != -1)
			word <- vocabularyDT[wordCoded]$ngram
	}
	return(word)
}

predictOneWordFromSentence <- function( sentence){      
	print("predictOneWordFromSentence")
	historyIndexVector <- getHistoryIndexVector(sentence,dirtyWordsVector,vocabularyDT)
	print(historyIndexVector)
	return( predictOneWordFromHistoryIndexes(historyIndexVector))
}


getNextWordFromEncoded4Gram = function(inputArray, ngrams, vocabularyDT){
    if (length(inputArray) == 0) {
		print("ERROR - inputArray at least of length 3")
		return(-1)
    } else if (length(inputArray) > 3) {
		print("ERROR - inputArray at most of length 3")
		return(-1)
    } else if (length(inputArray) == 1) {
		inputW1 <- -1
		inputW2 <- -1
		inputW3 <- inputArray[1]
    } else if (length(inputArray) == 2) {
		inputW1 <- -1
		inputW2 <- inputArray[1]
		inputW3 <- inputArray[2]
    } else{
		inputW1 <- inputArray[1]
		inputW2 <- inputArray[2]
		inputW3 <- inputArray[3]
	}

    if (inputW3 <= 0) {
		print("ERROR - At least the last element should be greater than 0")
		return(-1)
	}
	emptyDT <- data.table(w1=numeric(), w2=numeric(), w3=numeric(), w4=numeric())
	print("Search in 4-grams")
	if (inputW1 > 0 & inputW2 > 0) {
		oneGroupIn4Gram <- ngrams[n==4 & w1 == inputW1 & w2 == inputW2 & w3 == inputW3]
		if (nrow(oneGroupIn4Gram) > 0) {
			oneGroupIn4Gram[, finalProb := getProbabilityFromEncoded4Gram3(w1, w2, w3, w4, ngrams, vocabularyDT,oneGroupIn4Gram,NULL,NULL)$finalProb, by = 1:nrow(oneGroupIn4Gram)]
			#return(oneGroupIn4Gram[which.max(finalProb),])
			return(oneGroupIn4Gram)
		}
	}
	print("Search in 3-grams")
	if (inputW2 > 0) {
		oneGroupIn3Gram <- ngrams[n==3 & w1 == inputW2 & w2 == inputW3]
		if (nrow(oneGroupIn3Gram) > 0) {
			#print(oneGroupIn3Gram)
			oneGroupIn3Gram[, finalProb := getProbabilityFromEncoded4Gram3(-1, w1, w2, w3, ngrams, vocabularyDT,emptyDT,oneGroupIn3Gram,NULL)$finalProb, by = 1:nrow(oneGroupIn3Gram)]
			#print(oneGroupIn3Gram)
			#return(oneGroupIn3Gram[which.max(finalProb),])
			return(oneGroupIn3Gram)
		}
	}
	print("Search in 2-grams")
	oneGroupIn2Gram <- ngrams[n==2 & w1 == inputW3]
	if (nrow(oneGroupIn2Gram) > 0) {
		#print(oneGroupIn2Gram)
		oneGroupIn2Gram[, finalProb := getProbabilityFromEncoded4Gram3(-1, -1, w1, w2, ngrams, vocabularyDT,emptyDT,emptyDT,oneGroupIn2Gram)$finalProb, by = 1:nrow(oneGroupIn2Gram)]
		#return(oneGroupIn2Gram[which.max(finalProb),])
		return(oneGroupIn2Gram)
	}
	print("No match found")
	return(-1)
}


getProbabilityFromEncoded4Gram3 = function(inputW1, inputW2, inputW3, inputW4, ngrams, vocabularyDT,
		oneGroupIn4Gram, oneGroupIn3Gram, oneGroupIn2Gram){
	#print("getProbabilityFromEncoded4Gram3")
    finalProb = -1
	#print(sprintf("inputW1=%d - inputW2=%d - inputW3=%d - inputW4=%d",inputW1,inputW2,inputW3,inputW4))
	if (is.null(oneGroupIn4Gram))
		oneGroupIn4Gram <- ngrams[n == 4 & w1 == inputW1 & w2 == inputW2 & w3 == inputW3]

	oneRecordIn4Gram <- oneGroupIn4Gram[w4 == inputW4]
	if (nrow(oneRecordIn4Gram) > 0) {
		#print("We found one in 4-gram")
		# We found one in 4-gram
		all_freq = sum(oneGroupIn4Gram$frequency)
		oneRecordIn4Gram[, finalProb := ((discount * freq) / all_freq)]
		### We're done!
		return(oneRecordIn4Gram)
	} else {
		# NOT found in 4-gram => check 3-gram & 2-gram & 1-gram
		if (nrow(oneGroupIn4Gram) > 0) {
			# Get the 4-grams left-over probability so that we can distribute it for lower-order grams.
			beta_leftoverprob = ngrams[w1 == inputW1 & w2 == inputW2 & w3 == inputW3 & w4 != -1][1]$leftoverprob
		}
		#print("Searching in 3-gram")
		if (is.null(oneGroupIn3Gram))
			oneGroupIn3Gram <- ngrams[n == 3 & w1 == inputW2 & w2 == inputW3]
		oneRecordIn3Gram <- oneGroupIn3Gram[w3 == inputW4]
		if (nrow(oneRecordIn3Gram) > 0) {
			#print("We found one in 3-gram")
			# We found one in 3-gram
			if (nrow(oneGroupIn4Gram) > 0) {
				# We only consider ones that do not appear in 4-grams...
				oneGroupIn3Gram = oneGroupIn3Gram[!(oneGroupIn3Gram$w3 %in% oneGroupIn4Gram$w4)]
				all_freq = sum(oneGroupIn3Gram$freq)
				alpha <- beta_leftoverprob / ( oneGroupIn3Gram[ ,sum(freq * discount)]  / all_freq ) 
				## TODO check if it's correct to use the same variable while assigning to it a value
				oneRecordIn3Gram[ , c("alpha","finalProb") := .(alpha, alpha * ((freq * discount ) / all_freq))]
			} else {
				all_freq = sum(oneGroupIn3Gram$freq)
				oneRecordIn3Gram[, finalProb := ((discount * freq) / all_freq)]
			}
			### We're done!
			#print(oneRecordIn3Gram)
			return(oneRecordIn3Gram)
		} else {
			# NOT found in 4-gram & 3-grams => check 2-gram & 1-gram
			if ( (nrow(oneGroupIn4Gram) == 0) & (nrow(oneGroupIn3Gram) > 0) ){
				# Get the 3-grams left-over probability so that we can distribute it for lower-order grams.
				beta_leftoverprob = ngrams[w1 == inputW2 & w2 == inputW3  & w3 != -1  & w4 == -1][1]$leftoverprob
			}
			if (is.null(oneGroupIn2Gram))
				oneGroupIn2Gram <- ngrams[n == 2 & w1 == inputW3]
			oneRecordIn2Gram <- oneGroupIn2Gram[w2 == inputW4]
			if (nrow(oneRecordIn2Gram) > 0) {
				#print("We found one in 2-gram")
				# We found one in 2-gram
				if (nrow(oneGroupIn4Gram) > 0) {
					# We only consider ones that do not appear in 4-grams...
					oneGroupIn2Gram = oneGroupIn2Gram[!(oneGroupIn2Gram$w2 %in% oneGroupIn4Gram$w4)]
					all_freq = sum(oneGroupIn2Gram$freq)
					alpha <- beta_leftoverprob / ( oneGroupIn2Gram[ ,sum(freq * discount)]  / all_freq ) 
					oneRecordIn2Gram[ , c("alpha","finalProb") := .(alpha, alpha * ((freq * discount ) / all_freq))]
				} else if (nrow(oneGroupIn3Gram) > 0) {
					# We only consider ones that do not appear in 3-grams...
					oneGroupIn2Gram = oneGroupIn2Gram[!(oneGroupIn2Gram$w2 %in% oneGroupIn3Gram$w3)]
					all_freq = sum(oneGroupIn2Gram$freq)
					alpha <- beta_leftoverprob / ( oneGroupIn2Gram[ ,sum(freq * discount)]  / all_freq ) 
					oneRecordIn2Gram[ , c("alpha","finalProb") := .(alpha, alpha * ((freq * discount ) / all_freq))]
				} else {
					all_freq = sum(oneGroupIn2Gram$freq)
					oneRecordIn2Gram[, finalProb := ((discount * freq) / all_freq)]
				}
				### We're done!
				return(oneRecordIn2Gram)
			} else {
				#	print("No match found")
				return(-1)
			}
		}
	}
}

check.if.formed  <- function(x){ 
	print("check.if.formed")
	print(x)
	gresResult <- grep("[?:!,.; ]$", x, value = TRUE)
	print(sprintf("gresResult = %s",gresResult))
	if( length(gresResult) > 0  ){
			return(TRUE)        
	}
	return(FALSE)        
}

getHistoryIndexVector <- function(line, dirtyWordsVector, vocabularyDT) {
	print("getHistoryIndexVector")
	corpus <- Corpus(VectorSource(line), readerControl = list(reader = readPlain, language = "en", load = TRUE))
	tokCorpus <- tokenizeCorpus(corpus, dirtyWordsVector=dirtyWordsVector)
	curHistory <- as.character(tokCorpus[[1]])
	print(sprintf("curHistory = %s",curHistory))
	ngramArray <- strsplit(curHistory, split = NGRAM_SEPARATOR)[[1]]
	if (length(ngramArray) < 2 ) {
		print("Line too short")
		return(c(-1, -1, -1))
	}
	ngramsDT <- data.table(word = ngramArray)
	ngramsDT[, num:=.I]
	ngramsDT <- merge (x=ngramsDT, y=vocabularyDT, by.x="word", by.y="ngram", all.x=TRUE, sort=FALSE)
	print(ngramsDT)
	ngramsDT[freq < freqLimit, index:=NA]
	ngramIndexArray <- ngramsDT$index
	print(sprintf("ngramIndexArray = %s",ngramIndexArray))
	historyIndexVector <- ngramIndexArray[(length(ngramIndexArray)-2):length(ngramIndexArray)]
	if (is.na(historyIndexVector[length(historyIndexVector)]))
		return(NA)
	historyIndexVector <- getHistoryWithoutNA(historyIndexVector)
}

getHistoryWithoutNA <- function(indexVector) {
	for (position in length(indexVector):1) {
		if (is.na(indexVector[position]))
			return(indexVector[(position+1):length(indexVector)])
	}
	return(indexVector)
}

shinyServer(
	function(input, output) {
		gc()
		#output$table <- renderTable({data.frame()})
		x <-  reactive({ 
			check.if.formed( input$sentence )
		})
	   
		output$value <- renderText({ 
			if(is.null(x() )){return(NULL) }
			if(x()){
				historyIndexVector <- getHistoryIndexVector( input$sentence, dirtyWordsVector, vocabularyDT)
				print("historyIndexVector")
				print(historyIndexVector)
				print(class(historyIndexVector))
				if ( all(is.na(historyIndexVector)) ) {
					output$alternatives <- renderText("")
					return("<b>Not able to get a prediction. The last word is unknown.</b>")
				} else if ( all(historyIndexVector == -1 ) ) {
					output$alternatives <- renderText("")
					return("<b>Too few significant words to get a prediction.</b>")
				} else {
					results <- getNextWordFromEncoded4Gram(historyIndexVector, ngramsModelDT, vocabularyDT)
					word <- "Default"
					print("results")
					print(results)
					if (is.data.table(results)) {
						firstResultIndex <- which.max(results$finalProb)
						firstResult <- results[firstResultIndex]
						wordCoded <- switch (firstResult$n, -1, firstResult$w2, firstResult$w3, firstResult$w4)
						if (wordCoded != -1)
							word <- vocabularyDT[wordCoded]$ngram
						alternativeResults <- results[-firstResultIndex]
						setkey(alternativeResults,finalProb)
						alternativeResults <- tail(alternativeResults,5)
						print("alternativeResults")
						print(alternativeResults)
						alternativeResultsCoded <- c()
						for (iAltRes in length(alternativeResults):1) {
							curRes <- alternativeResults[iAltRes]
							alternativeResultsCoded[iAltRes] <- switch (curRes$n, -1, curRes$w2, curRes$w3, curRes$w4)
						}
						output$alternatives <- renderText(paste("<font color=\"#006400\"><b>", vocabularyDT[alternativeResultsCoded]$ngram,"</b></font><br/>" ))
					}
					#res <- predictOneWordFromHistoryIndexes( historyIndexVector )
					return( paste(input$sentence,"<font color=\"#228B22\"><b>", word,"</b></font>" ))
				}
			} else {
				return("Terminate the sentence with space to get a prediction.."  )
			}
		})
		
		observeEvent(input$goButton, {
			if(is.null(x() )){return(NULL) }
			output$table <- renderTable({ predict.full( input$sentence,compose.gram )})   
		})
		
		output$table <- renderTable({ 
			if(is.null(x() )){return(NULL) }
			return( data.frame(predictor = c("predictor"), Pkn = c(0.0), outcome = c("no input")))
		})
	}
)