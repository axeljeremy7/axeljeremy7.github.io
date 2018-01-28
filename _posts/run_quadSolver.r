#!/usr/bin/Rscript
# quadSolver solves the quadratic equation
#     a * X^2 + b*x + c = 0
# for real roots.
# Input: real values: a, b and c
# Return: a list ret
#           ret$flag  0 real roots
#                     1 no real roots
#                     2 not a real quadratic
#           ret$x1    first real root, or 
#                     NULL if no real roots
#           ret$x2    second real root, or
#                     NULL if no real roots
# Reference: https://en.wikipedia.org/wiki/Quadratic_equation
# Note a couple more checks could be added below. The reference
# also indicate some things that cold be done to improve accuracy.
quadSolver <- function(a, b, c) {
# check for a real quadratic (not checking if a, b and c are scalars)
  if((a == 0.0) || !(is.numeric(a) && is.numeric(b) && is.numeric(c)) ) { 
    return(list(flag=2, x1=NULL, x2=NULL))
  }
  discriminate = b*b - 4.0*a*c
# not checking it b^2 - 4ac overflowed
  if(discriminate < 0.0) {
    return(list(flag=1, x1=NULL, x2=NULL))
  }
  sqrtDiscriminate = sqrt(discriminate)
  x1 = (-b + sqrtDiscriminate) / (2.0 * a)
  x2 = (-b - sqrtDiscriminate) / (2.0 * a)
  return(list(flag=0, x1=x1, x2=x2))
}

fin <- file('stdin')
cat('Input a b c: ')

line <- readLines(fin, n=1)
class(line)
length(line)
line
cat('\n')

args_list <- strsplit(line, ' ')
class(args_list)
length(args_list)
args_list
cat('\n')

args_vec <- unlist(args_list)
class(args_vec)
length(args_vec)
args_vec
cat('\n')

args_num <- as.numeric(args_vec)
class(args_num)
length(args_num)
args_num
cat('\n')

result <- quadSolver(args_num[1], args_num[2], args_num[3])
class(result)
length(result)
result
cat('\n')
cat('flag= ', result$flag, ', x1= ', result$x1, ', x2= ', result$x2, '\n')

