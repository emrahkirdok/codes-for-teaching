# a quick implementation of markov chains using an english dictionary
library(readr)
# first read the dataset
words <- read_csv("https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt", col_names = FALSE)

# create the word matrix
wordmatrix<-matrix(data = NA, ncol = 32, nrow = dim(words))


for (i in seq(1,dim(words)[1])){
  
  for ( j in seq(1, nchar( words[i,1] ) + 1 ) ){
    
    if (j != nchar( words[i,1] ) + 1){
      wordmatrix[i,j] <- substr(x = words[i,1],start = j,stop = j )
    }else{
      wordmatrix[i,j] <- "_"
      }
    
    
    
  }
}

# create first order markov chain
gen_word_1st <- function(size, probab_tbl){
  word <- vector(mode = "character", length = as.integer(size))
  for (i in seq(1,as.integer(size))){
    word[i] <- sample(x = names(probab_tbl), prob = probab_tbl, size = 1)
  }
  return(paste0(word,collapse = ""))
}

# get the probabilities for the first character
freq1 <- table(wordmatrix[,1])/sum(table(wordmatrix[,1]))

freq2 <- c()

for (i in seq(1,length(freq1))){
  chr1 <- names(freq1[i])
  chr2_db <- data.frame(table(wordmatrix[wordmatrix[,1]==chr1,2])/sum(table(wordmatrix[wordmatrix[,1]==chr1,2])), stringsAsFactors = F)
  layer <- cbind(rep(chr1, nrow(chr2_db)), rep(freq1[i], nrow(chr2_db)))
  
  freq2<-rbind(freq2,cbind(layer,chr2_db))
}

colnames(freq2) <- c("first", "first_probab", "second", "second_probab")
freq2$first<-as.character(freq2$first)
freq2$first_probab<-as.numeric(as.character(freq2$first_probab))
freq2$second<-as.character(freq2$second)
freq2$second_probab<-as.numeric(as.character(freq2$second_probab))


firstprobab<-unique(freq2[,1:2])
i=0
while (i<100){
  word1<-sample(x = firstprobab$first, prob = firstprobab$first_probab, size = 1)
  word2<-sample(x = freq2[freq2$first==word1,][,3], size = 1, prob = freq2[freq2$first==word1,][,4])
  word3<-sample(x = freq2[freq2$first==word2,][,3], size = 1, prob = freq2[freq2$first==word2,][,4])
  word4<-sample(x = freq2[freq2$first==word3,][,3], size = 1, prob = freq2[freq2$first==word3,][,4])
  word<-paste0(word1,word2,word3,word4, " ", collapse = "")
  i=i+1
  cat(word)
}


# we could add another markov chain
# now we will calculate the conditional probability of the third character given the previous character
freq3 <- c()

for (i in seq(1,length(freq1))){
  second_layer <- c()
  chr1 <- names(freq1[i])
  chr2_df <- data.frame(table(wordmatrix[wordmatrix[,1]==chr1,2])/sum(table(wordmatrix[wordmatrix[,1]==chr1,2])), stringsAsFactors = F)
  chr2_df[,1]<-as.character(chr2_df[,1])
  for (j in seq(1,nrow(chr2_df))){
    cat(i,j,"\n")
    chr2<-chr2_df[j,1]
    chr2_probab<-chr2_df[j,2]
    chr3_df<-data.frame(table(wordmatrix[wordmatrix[,2]==chr2,3])/sum(table(wordmatrix[wordmatrix[,2]==chr2,3])), stringsAsFactors = F)
    layer <- cbind(rep(chr2,nrow(chr3_df)), rep(chr2_probab,nrow(chr3_df)))
    layer <- cbind(layer,chr3_df)
    second_layer <- rbind(second_layer,layer)
  }
  
  layer <- cbind(rep(chr1, nrow(second_layer)), rep(freq1[i], nrow(second_layer)))
  
  freq3<-rbind(freq3,cbind(layer,second_layer))
}

colnames(freq3) <- c("first","firstP","second", "secondP", "third", "thirdP")

freq3$first<-as.character(freq3$first)
freq3$firstP<-as.numeric(as.character(freq3$firstP))
freq3$second<-as.character(freq3$second)
freq3$secondP<-as.numeric(as.character(freq3$secondP))
freq3$third<-as.character(freq3$third)
freq3$thirdP<-as.numeric(as.character(freq3$thirdP))

# generate words
# it seems it is working
i=0
while (i<50){
  word1<-sample(x = firstprobab$first, prob = firstprobab$first_probab, size = 1)
  probabs<-unique(data.frame(freq3[freq3$first==word1,][,3], freq3[freq3$first==word1,][,4],stringsAsFactors = F))
  word2<-sample(x = probabs[,1], size = 1,prob = probabs[,2])
  probabs<-data.frame(freq3[freq3[freq3$second==word2,][,3]==word2,][,5], freq3[freq3[freq3$second==word2,][,3]==word2,][,6], stringsAsFactors = F)
  word3<-sample(x = probabs[,1], size = 1,prob = probabs[,2])

  word4<-sample(x = firstprobab$first, prob = firstprobab$first_probab, size = 1)
  cat(paste0(word1,word2, word3, word4, collapse = ""), "\n")
  Sys.sleep(0.5)
  i=i+1
}


# I tried to increase to forth chain, but I think it is not working correctly because of the NA
# however it is too much work to generate the whole probability tree
freq4 <- c()

for (i in seq(1,length(freq1))){
  second_layer <- c()
  chr1 <- names(freq1[i])
  chr2_df <- data.frame(table(wordmatrix[wordmatrix[,1]==chr1,2])/sum(table(wordmatrix[wordmatrix[,1]==chr1,2])), stringsAsFactors = F)
  chr2_df[,1]<-as.character(chr2_df[,1])
  for (j in seq(1,nrow(chr2_df))){
    third_layer <- c()
    #cat(i,j,"\n")
    chr2<-chr2_df[j,1]
    chr2_probab<-chr2_df[j,2]
    if (chr2!=""){
      
      chr3_df<-data.frame(table(wordmatrix[wordmatrix[,2]==chr2,3])/sum(table(wordmatrix[wordmatrix[,2]==chr2,3])), stringsAsFactors = F)
      chr3_df[,1]<-as.character(chr3_df[,1])
      chr3<-chr3_df[j,1]
      chr3_probab<-chr3_df[j,2]
      for (z in seq(1,nrow(chr3_df))){
        cat(i,j,z,"\n")
        chr4_df<-data.frame(table(wordmatrix[wordmatrix[,3]==chr2,4])/sum(table(wordmatrix[wordmatrix[,3]==chr2,4])), stringsAsFactors = F)
        layer <- cbind(rep(chr3,nrow(chr4_df)), rep(chr3_probab,nrow(chr4_df)))
        layer <- cbind(layer,chr4_df)
        third_layer <- rbind(third_layer,layer)
      }
    }
  
    layer <- cbind(rep(chr3,nrow(chr4_df)), rep(chr3_probab,nrow(chr4_df)))
    layer <- cbind(layer,third_layer)
    second_layer <- rbind(second_layer,layer)
  }
  
  layer <- cbind(rep(chr1, nrow(second_layer)), rep(freq1[i], nrow(second_layer)))
  
  freq4<-rbind(freq4,cbind(layer,second_layer))
}

colnames(freq4) <- c("first","firstP","second", "secondP", "third", "thirdP", "fourth", "fourthP")

freq4$first<-as.character(freq4$first)
freq4$firstP<-as.numeric(as.character(freq4$firstP))
freq4$second<-as.character(freq4$second)
freq4$secondP<-as.numeric(as.character(freq4$secondP))
freq4$third<-as.character(freq4$third)
freq4$thirdP<-as.numeric(as.character(freq4$thirdP))
freq4$fourth<-as.character(freq4$fourth)
freq4$fourth<-as.numeric(as.character(freq4$fourth))


i=0
while (i<50){
  word1<-sample(x = firstprobab$first, prob = firstprobab$first_probab, size = 1)
  probabs<-unique(data.frame(freq4[freq4$first==word1,][,3], freq4[freq4$first==word1,][,4],stringsAsFactors = F))
  word2<-sample(x = probabs[,1], size = 1,prob = probabs[,2])
  probabs<-data.frame(freq4[freq4[freq4$second==word2,][,3]==word2,][,5], freq4[freq4[freq4$second==word2,][,3]==word2,][,6], stringsAsFactors = F)
  word3<-sample(x = probabs[,1], size = 1,prob = probabs[,2])
  
  probabs<-data.frame(freq4[freq4[freq4$third==word3,][,3]==word3,][,5], freq4[freq4[freq4$third==word3,][,3]==word3,][,6], stringsAsFactors = F)
  word4<-sample(x = firstprobab$first, prob = firstprobab$first_probab, size = 1)
  cat(paste0(word1,word2, word3, word4, collapse = ""), "\n")
  Sys.sleep(0.5)
  i=i+1
}


#-----------------------------------

# I tried to write a much simpler code for the first order chain
# but really, not working enough
#first order
i=1
chances<-c()
while (i+1<=ncol(wordmatrix)){
  chances<-rbind(chances,cbind(wordmatrix[,i], wordmatrix[,i+1]))
  i=i+1
}


getchr<-function(word_in, chances=chances){
  nextchr<-chances[chances[,1]==word_in,2]
  nextchr<-nextchr[!is.na(nextchr)]
  nextchr<-sample(x = nextchr,size = 1)
  return(nextchr)
}



makeword <- function(init=freq1, probMAT=chances){
  word<-sample(x = names(init), size = 1, prob = init)
  cat(word)
  while(word != "_"){
    word<-getchr(word_in=word, chances=probMAT)
      cat(word)
  }
  
  cat(" ")
  
}
init<-freq1
i=1
while(i<10000){
  word<-sample(x = names(init), size = 1, prob = init)
  cat(word)
  word<-getchr(word_in=word, chances=chances)
  if (word=="_"){cat(" ");word<-sample(x = names(init), size = 1, prob = init)}else{cat(word)}
  i=i+1
  
}

#then I tried to model second order markov chain as like this:
#the conditional probability of the third character, given the first two characters

i=1
chances_2nd<-c()
while (i+2<=ncol(wordmatrix)){
  first_syl<-apply(X = wordmatrix[,i:(i+1)],MARGIN = 1, function(x){paste0(x,collapse = "")})
  
  chances_2nd<-rbind(chances_2nd, cbind(first_syl , wordmatrix[,i+2]))
  i=i+1
}


getchr<-function(word_in, chances=chances){
  nextchr<-chances[chances[,1]==word_in,2]
  nextchr<-nextchr[!is.na(nextchr)]
  nextchr<-sample(x = nextchr,size = 1)
  return(nextchr)
}



makeword <- function(init=freq1, probMAT=chances){
  word<-sample(x = names(init), size = 1, prob = init)
  cat(word)
  while(word != "_"){
    word<-getchr(word_in=word, chances=probMAT)
    cat(word)
  }
  
  cat(" ")
  
}

start_mat <- chances_2nd[!is.na(chances_2nd[,1]),1]
start_mat<-start_mat[start_mat!="NANA"]
start_mat<-start_mat[start_mat!="_NA"]

start_mat_prob <- table(start_mat)/sum(table(start_mat))

i=1
while(i<10000){
  length_word<-nchar(sample(x = names(start_mat_prob),1, prob = start_mat_prob))
  j=1
  while (j<=length_word){
    SY<-sample(x = start_mat, size = 1)
    
    if (substr(SY,start = 2,stop = 2)=="_"){
      SY<-gsub(pattern = "_",replacement = " ", x = SY)
      cat(SY)
      SY<-sample(x = start_mat, size = 1)
    }
    
    if (substr(SY,start = 2,stop = 2)!="_"){
      cat(SY)
      SY_N<-getchr(word_in=SY, chances=chances_2nd)
      if (SY_N=="_"){
        cat(" ")
        
      }else{
        cat(SY_N)
        SY<-paste0(substr(SY,start = 2,stop = 2),SY_N,collapse = "")
      }
      
    }
    
    if (substr(SY,start = 1,stop = 1)=="_"){
      #SY<-gsub(pattern = "_",replacement = " ", x = SY)
      SY<-sample(x = start_mat, size = 1)
      cat(SY)
    }
    
    #SY_N<-getchr(word_in=SY, chances=chances)
    #if (word=="_"){cat(" ");word<-sample(x = names(init), size = 1, prob = init)}else{cat(word)}
    i=i+1
    
  }
  cat(" ")
}
