# ***** MARCH 2022: 31 DAYS OF CODING CHALLENGE ON DATASCIENTISTDIARY.COM *****

# Day 1: Make a function lowercase that, given a string, returns the string, except in all lowercase letters.

lowercase <- function(string){
  answer <- tolower(string)
  return(answer)
}

# Day 2: Make a function switch_case that, given a string, returns the string with uppercase letters in lowercase and vice-versa. Include punction and other non-cased characters unchanged.

switch_case <- function(string){
  new_string = ''
  for (letter in strsplit(string, "")[[1]]){
    if(str_detect(letter,"[[:lower:]]")){
      letter = toupper(letter)
      new_string = str_c(c(new_string, letter), collapse = "")
    }
    else if(str_detect(letter,"[[:upper:]]")){
      letter = tolower(letter)
      new_string = str_c(c(new_string, letter), collapse = "")
    }
    else {
      new_string = str_c(c(new_string, letter), collapse = "")
    }
  }
  print(new_string)
}

# Day 3: Make a function reverse_string that, given a string returns that string in reverse.

library(stringi)

reverse_string <- function(string){
  reverse = stri_reverse(string)
  print(reverse)
}

reverse_string2 <- function(string){
  reversed = ""
  for (i in strsplit(string, split = "")[[1]]){
    reversed = paste(i, reversed, sep = "")
  }
  print(reversed)
}

# Day 4: Create gerund_infinitive function which will return the infinitive form of the input verb (e.g. doing -> to do). If the word doesn't end with "ing", return "This is not a gerund!".

library(DescTools)

gerund_infinitive <- function(string){
  core_word <- substring(string,1, nchar(string)-3)
  if(StrRight(string, 3) == "ing"){
    print(paste0("to ", core_word))
  }
  else{
    print("That's not a gerund!")
  }
}


# Day 5: REST (Saturday)

# Day 6: REST (Sunday)

# Day 7: Write a function named capital_indexes. The function takes a single parameter, which is a string. Your function should return a list of all the indexes in the string that have capital letters.

capital_indexes <- function(string){
  indexes <- str_locate_all(pattern = "[:upper:]", string)
  return(indexes)
}

# Day 8: Create a womensday function which will ask user for his/her name. As the majority of Polish female names are ended with "a", if it's the last letter of user's name, function should return the Women's Day wishes. Otherwise, the function should proceed with another output (up to your creativity :) ).

womensday <- function(){
  name = readline(prompt = "Enter your name: ")
  len = nchar(name)
  if (substr(name, len, len) == 'a'){
    print(paste("Happy Women's Day, ", name, "!", sep = ""))
  } else {
    print("OK.")
  }
}

womensday()

# Day 9: Create a function named mid_letter that takes a string as its parameter. Your function should extract and return the middle letter. If there is no middle letter, your function should return an empty string. For example mid("abc") should return "b" and mid ("aaaa") should return "".

mid_letter <- function(x){
  if (as.integer(nchar(x)) %% 2 == 0){
    print("")
  }
  else{
    print(str_sub(x, ceiling(nchar(x)/2), nchar(x)/2 + 1))
  }
}

# Day 10: Create your own creative funny male names (name + surname) generator.

random_name <- function(){
  vovels = c("a", "o", "e", "u", "y", "i")
  endings = c("szard", "tr", "slaw", "drzej", "mierz", "liusz", "teusz", "uel")
  print(paste(sample(LETTERS, 1), sample(vovels, 1), sample(endings, 1), sep=""))
}


# Day 11: Two strings are anagrams if you can make one from another by just rearranging the letters. Write a function named is_anagram that takes two strings as its parameters. Your function should return True if the strings are anagrams and False otherwise.

str_arrange <- function(x){
  x %>%
    stringr::str_split("") %>% # Split string into letters
    purrr::map(~sort(.) %>% paste(collapse = "")) %>% # Sort and re-combine
    as_vector() # Convert list into vector
}

is_anagram <- function(string1, string2){
  sorted_string1 = str_arrange(string1)
  sorted_string2 = str_arrange(string2)
  
  print(sorted_string1 == sorted_string2)
}

# Day 12: REST (Saturday)

# Day 13: REST (Sunday)

# Day 14: Define a function named largest_difference that takes a list of numbers as its only parameter. Your function should compute and return the difference between the largest and smallest number in the list.

largest_difference <- function(numbers){
  print(max(numbers) - min(numbers))
}

#largest_difference(c(1,100,34))

# Day 15: Write a function named add_dots that takes a string and adds "." in between each letter.

add_dots <- function(word){
  print(paste(strsplit(word, split = "")[[1]], collapse = "."))
}

#add_dots("Hello")

# Day 16: Write a function named is_palindrome that takes a single string as its parameter. Your function should return True if the string is a palindrome, and False otherwise.

is_palindrome = function(word){ # using solution from day 3
  return(reverse_string(word) == word)
}

# Day 17: Define a calculate_factorial function that will return the factorial for the given number.

num = as.integer(readline(prompt="Enter a number: "))
factorial = 1
# check is the number is negative, positive or zero
if(num < 0) {
  print("Sorry, factorial does not exist for negative numbers")
} else if(num == 0) {
  print("The factorial of 0 is 1")
} else {
  for(i in 1:num) {
    factorial = factorial * i
  }
  print(paste("The factorial of", num ,"is",factorial))
}

# Day 18: Define a function named all_equal that takes a list and checks whether all elements in the list are the same.

all_equal <- function(list_of_numbers){
  return (length(unique(list_of_numbers)) == 1)
}

all_equal2 <- function(list_of_numbers){
  first = list_of_numbers[1]
    for(i in 2:length(list_of_numbers)){
      if (list_of_numbers[i]!= first){
        return(FALSE)
      }
      return(TRUE)
    }
}

# Day 19: REST (Saturday)

# Day 20: REST (Sunday)

# Day 21: Write a function move_zero to push all zeros to the end of a list.

move_zero <- function(x){
  return(c(x[x!=0], x[x==0]))  
}

# move_zero(c(0,1,0,3))

# Day 22: Write a perfectly_balanced function which, having a string containing only the characters x and y, check that the number of characters x and y are the same.

perfectly_balanced <- function(word){
  w = as.list(strsplit(word, "")[[1]])
  return(length(w[w=="x"]) == length(w[w=="y"]))
}

# perfectly_balanced("xxyy")

# Day 23: Write a function is_ugly to check whether a given number is an ugly number.

is_ugly <- function(x){
  for(i in c(2,3,5)){
    while(x%%i == 0){x=x/i}
  }
  return (x == 1)
}

# is_ugly(15)

# Day 24: Write a function is_geometric to check if a sequence of numbers is a geometric progression or not.

is_geometric <- function(x){
  if(length(x)<2){return(TRUE)}
  divided = c()
  for (i in 2:length(x)){
    divided = c(divided, x[i]/x[i-1])
  }
  return(length(unique(divided)) == 1)
}

# is_geometric(c(2,3,5))

# Day 25: Write a function to display the Fibonacci sequence up to n-th term.

# Day 26: REST (Saturday)

# Day 27: REST (Sunday)

# Day 28: Write a function digit_adder which will take a number from the user, transform each of its digits to one digit larger, and eventually return a new number. For example, 998 becomes 10109.

digit_adder <- function(n){
  ans = ""
  for(i in as.numeric(strsplit(as.character(n), "")[[1]])){
    ans = paste(ans, i+1, sep="")
  }
  print(as.numeric(ans))
}

# digit_adder(998)

# Day 29: Write an additive_persistence function that returns the number of loops it needs to do to sum the digits of a user-specified number to a single digit number. Examples: 13 -> 1, 1234 -> 2, 199 -> 3.

additive_persistence <- function(n){
  ans = 0
  while(nchar(n) > 1){
    n = sum(as.numeric(strsplit(format(n, scientific = FALSE), "")[[1]]))
    ans = ans + 1 
  }
  return(ans)
}

# additive_persistence(1234)

# Day 30: Given two Roman numerals, return whether the first one is less than the second one.

# Day 31: Write the function count_ones which, after receiving the number n from the user, will determine how many times "1" will appear if you print all the numbers from 1 to n inclusive.
