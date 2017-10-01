####I have discovered recently that many probability puzzles are easily solved if they can be written in 
#terms of absorbing Markov Chains. If you can specify the transition probabilities from going from one state to 
#another it is really easy to calculate the amount of time to get to an end or "absorbing state" 

#The following function takes a transition matrix and calculates the expected number of steps before absorbtion 
#from the first row of the matrix
#I'm sure this exists in a much more efficient and easy to use form in R, but I'm not sure where and it's good practice
AbsMarkovEV<-function(mat)
{
  stopifnot(apply(mat, 1, sum)==1) ##all rows must sum to 1 to be valid probabilities
  
  if (!mat[nrow(mat), ncol(mat)]==1)
  {
    print("Not an absorbing matrix")
    return(NULL)
  }
  sized<-TRUE
  sub_mat<-mat
  while(sized){
    sub_mat<-sub_mat[-nrow(sub_mat), -ncol(sub_mat)]
    sized<-sub_mat[nrow(sub_mat), ncol(sub_mat)]==1
  }
  EV<-solve(diag(nrow(sub_mat)) - sub_mat)
  return(sum(EV[1,]))
}

##To demonstrate: a simple problem involving the expected number of coin flips to get two heads
##3 states:
#1. Last flip (if exists) not a head
#2. Last flip a head, flip before (if exists) not a head
#3. Last two flips heads
transition_matrix<-matrix(c(1/2, 1/2,0, 1/2,0,1/2,0,0,1), byrow=T, nrow=3)
AbsMarkovEV(transition_matrix)
#6 coin flips is the Expected value before flipping two heads

#one more example involving an urn and 4 different colored balls. Pull two balls out and paint the first the color
#of the second. How long until all balls are the same color
AbsMarkovEV(matrix(c(0,0,0,0,0,1,1/2,0,0,0,0,1/6,1/3,1/4,0,0,1/3
                     ,2/3,1/2,0,0,0,0,1/4,1), nrow=5))
#100% of my knowledge of Markov Chains comes from this really clear and fascinating chapter 
# http://www.dartmouth.edu/~chance/teaching_aids/books_articles/probability_book/Chapter11.pdf
#I hope to add more functions to compute other interesting properties such as "Probability of being at B given started at A after 3 steps"


bathroom<-matrix(c(0,1/3,1/3,1/3,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,0,1/3,1/3,0,1/3,0), nrow=6, byrow=T)
trans_states<-((bathroom%^%100+bathroom%^%101)/2)[1,]
