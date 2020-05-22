#Fun Riddler for the week
#https://fivethirtyeight.com/features/somethings-fishy-in-the-state-of-the-riddler/

#What words share a letter with every single state but one. Find the longest word.

dictionary= read.table("https://norvig.com/ngrams/word.list")$V1

longest = 0
states= tolower(state.name)
state_letters<-strsplit(states, "")
wordList=c()
state_list=c()
for (i in 1:length(dictionary)){
  word = dictionary[i]
  if (nchar(word)<longest)next
  word_letters<-strsplit(word, split="")[[1]]
  counts=sapply(state_letters, function(x)length(intersect(word_letters, x)))
  state_flag = which(counts==0)
  if (length(state_flag)==1){
    if(length(word_letters)==longest){
      wordList=c(wordList, word)
      state_list = c(state_list, states[state_flag])
      print(word)
      print(states[state_flag])
    }else{
      wordList=word
      state_list=states[state_flag]
      longest=length(word_letters)
      print(word)
      print(states[state_flag])
    }
  }
}
