##regex

## substitute $ with !
sub(pattern = "\\$", "\\!", "I love girls$")

p = "I love girls$$$$"
## substitute all $ with .
gsub(pattern = "\\$", "\\.", p)

sub(pattern = "pussy", "dick", "All men have pussy")

vack = "\\\\"
space = " "
gsub(pattern = vack, space, "I\\NEED\\SPACE")

dated <- "12/2/2014"
replaced <- "14"
sub(pattern = "2014", replaced, dated)

####### sequences
## substitute any digit with !
gsub(pattern = "\\d", replacement = "!", "I am working! to be 18years old in 30years time")
## substitute any non-digit with __
gsub(pattern = "\\D", replacement = "__", "So here we go t stage 6 in just1year")
wordrp <- "I am working! to be 18years old in 30years time.xxx hyls"

## substitute any word with %
gsub("\\w", "%", wordrp)

## substitute any non-word with -
gsub(pattern = "\\W", "-", wordrp)

## substitute whitespace with ^
gsub(pattern = "\\s", "^", wordrp)

#### character classess
x <- c("RStudio", "v.0.99.484", "2015", "09-22-2015", "grep vs. grepl")
## find any strings with numeric values b/t 0-9
grep(pattern = "[0-9]", x, value = TRUE )

# FIND any string with numeric values between 6-8
grep(pattern = "[6-8]", x, value = TRUE)

# find any string with characters R or r
grep(pattern = "[Rr]", x, value = T)

# find any string that have non-alphanumeric characters
grep(pattern = "[^a-z0-9A-Z]", x, value = T)

############### POSIX characters
y <- "I like beer! #beer, @wheres_my_beer, I like R (v3.2.2) #rrrrrrr2015"

# remove space or tabs
gsub(pattern = "[[:blank:]]", replacement = "", y)

# replace punctuation with whitespace
gsub(pattern = "[[:punct:]]", replacement = " ", y)

# remove alphanumeric characters
gsub(pattern = "[[:alnum:]]", replacement = "", y)


############# qunatifiers
View(state.name)
# match states that contain z
grep(pattern = "z+", state.name, value = T)

#match states with two s
grep(pattern = "s{2}", state.name, value = T)

# match states with one or two s
grep(pattern = "s{1,2}", state.name, value = T) 


# matching states in North or South (|) in the pattern
grep(pattern = "North | South", state.division, value = T)

## use grepl to obtain logical output
grepl(pattern = "North | South", state.division)

## count number of states in North or South
sum(grepl(pattern = "North | South", state.division))

#### use regexpr() to fnd position of the match
z <- c("v.111", "0v.11", "11v.", "000")
regexpr("v.",z)


##### splitting charcater vectors
std <- paste(state.name[1:10], collapse = " ")
std
strsplit(std, " ")

# output as vector and not list
unlist(strsplit(std, " "))
