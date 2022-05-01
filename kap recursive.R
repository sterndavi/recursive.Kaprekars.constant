# How many tries till you get 6174

# Trying to find the Kaprekar's constant in a recursive manner

# https://en.wikipedia.org/wiki/6174_(number)

library(stringr)


kap_fun_recursive <- function(x) {
    check_condition_repeated_digits <-
        function(x) {
            repetitions <- x |>
                as.character() |>
                str_split("") |>
                unlist() |>
                table() |>
                as.vector()
            return(ifelse(any(c(3, 4) %in% repetitions),
                          TRUE,
                          FALSE))
        }
    
    
    if (is.numeric(x) &
        length(unlist(str_split(as.character(x), ""))) == 4 &
        check_condition_repeated_digits(x) == FALSE) {
        splitednum <- x |>
            as.character() |>
            str_split("") |>
            unlist() |>
            as.integer()
        
        sorted_splitednum <- splitednum |>
            sort() |>
            rev()
        
        rev_sorted_splitednum <- sorted_splitednum |>
            rev() |>
            as.character() |>
            paste(collapse = "") |>
            as.integer()
        
        sorted_splitednum <- sorted_splitednum |>
            as.character() |>
            paste(collapse = "") |>
            as.integer()
        
        x <- sorted_splitednum - rev_sorted_splitednum
        
        
        if (x != 6174L) {
            kap_fun_recursive(x)
            print(glue::glue(
                "{sorted_splitednum}-{rev_sorted_splitednum} = {x}"
            ))
        } else{
            return(print(
                glue::glue(
                    "{sorted_splitednum}-{rev_sorted_splitednum} = {x} - Finished"
                )
            ))
        }
        
        
    } else {
        return(message("the input doesn't match the prerequisites"))
    }
    
    
}


# I was not able to find a way to count iterations without making the function too impure, or making not functional at all,
# so, the output messages were far from what I wanted them to be

#Testing the is.numeric condition:
kap_fun_recursive("1453")

#Testing the length condition:
kap_fun_recursive(24664)

#Testing the using at least two different digits condition
kap_fun_recursive(4445)
kap_fun_recursive(1111)

#Testing the results
kap_fun_recursive(1567)
kap_fun_recursive(5465)
kap_fun_recursive(9373)
kap_fun_recursive(1100)
kap_fun_recursive(3123)
kap_fun_recursive(4543)
