
# Importing libraries
library(arules)
library(arulesViz)
library(shinythemes)

# 1.) book
book <- read.csv("D:/EXCELR/ASSIGNMENTS/R/Association_rules/book.csv")
View(book)
class(book)
# since the data is in list we are converting it into transactions, so before
# that need to convert into matrix format
book_trans <- as(as.matrix(book), "transactions")
View(book_trans)
# in case of list format directly can be called in transactions.
inspect(book_trans)

# perform apriori algorithm
book_rules <- apriori(book_trans,parameter = list(support = 0.002, confidence=0.5, minlen=1))
inspect(book_rules[1:100])
length(book_rules)

# removing redundant
#book_rules <- book_rules[!is.redundant(book_rules)]

gi <- generatingItemsets(book_rules)
inspect(gi)
?duplicated
d <- which(duplicated(gi))
length(d)
final_book_rules <- book_rules[-d]
inspect(final_book_rules)
length(final_book_rules)

# sorting
final_book_rules.sorted = sort(final_book_rules,by = "lift")
arules::inspect(final_book_rules.sorted[1:100])

#apriori plots
plot(final_book_rules.sorted) #Visualize Association Rules and Itemsets

plotly_arules(final_book_rules) # Interactive Scatter Plot for Association Rules using plotly

ruleExplorer(final_book_rules)  # Explore Association Rules Interactively

inspectDT(final_book_rules)     # Inspect Associations Interactively Using datatable
saveAsGraph(final_book_rules, "book_rules", type="items", format="graphml")  # Save rules or itemsets as a graph description


# whenever we have data containing item names, then load the data using 
# read.transactions(file="path", format ="busket", sep= ",")

#2.) groceries

a <- read.csv("D:/EXCELR/ASSIGNMENTS/R/Association_rules/groceries.csv")
View(a)
groceries <- read.csv("D:/EXCELR/ASSIGNMENTS/R/Association_rules/groceries.csv")
View(groceries)
length(groceries)
str(groceries)
inspect(groceries[1:10])
class(groceries)
groceries_rules <- apriori(groceries,parameter = list(support = 0.002, confidence=0.5,minlen = 1))
inspect(groceries_rules)
length(groceries_rules)

# removing redundant
#book_rules <- book_rules[!is.redundant(book_rules)]

gi <- generatingItemsets(groceries_rules)
inspect(gi)
?duplicated
d <- which(duplicated(gi))
length(d)
final_groceries_rules <- groceries_rules[-d]
inspect(final_groceries_rules)
length(final_groceries_rules)



# sorting
final_groceries_rules.sorted = sort(final_groceries_rules,by = "lift")
arules::inspect(final_groceries_rules.sorted[1:100])


#apriori plots
plot(final_groceries_rules.sorted) #Visualize Association Rules and Itemsets

plotly_arules(final_groceries_rules.sorted) # Interactive Scatter Plot for Association Rules using plotly

#3.) my movies
my_movies <- read.csv("D:/EXCELR/ASSIGNMENTS/R/Association_rules/my_movies.csv")
View(my_movies)
str(my_movies)
colnames(my_movies)
my_movies_data <- my_movies[1:5]
View(my_movies_data)

# perform apriori algorithm

my_movies_rules = apriori(my_movies_data, parameter = list (minlen=1, supp=0.2, conf=0.5))
arules::inspect(my_movies_rules)
length(my_movies_rules)

# removing redundant
#book_rules <- book_rules[!is.redundant(book_rules)]
?generatingItemsets
gi <- generatingItemsets(my_movies_rules)
inspect(gi)
?duplicated
d <- which(duplicated(gi))
length(d)
final_groceries_rules <- my_movies_rules[-d]
inspect(final_groceries_rules)
length(final_groceries_rules)

# sorting
final_groceries_rules.sorted = sort(final_groceries_rules,by = "lift")
arules::inspect(final_groceries_rules.sorted)

#apriori plots
plot(final_groceries_rules.sorted) #Visualize Association Rules and Itemsets

plotly_arules(final_groceries_rules.sorted) # Interactive Scatter Plot for Association Rules using plotly
