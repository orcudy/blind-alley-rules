This repository contains a mechansim for filtering blind-alley rules from formal grammars. 

###Contents
**utils.ml**: a library containing general utilites  
**points.ml**: methods for calculating fixed and periodic points of arbitrary functions  
**rules.ml**: a mechanism for filtering out blind-alley rules in formal grammars  


###Filtering Mechanism
1. Generate initial white list. (A list containing nonterminals that have a
derivation where all elements on the right hand side of the rule are terminal.)  
2. Iteratively generate the final white list. (A list containing nonterminals
that can reach a terminal state after an arbitrary number of iterations.)  
3. Remove any rule from the grammar that contains (either on the left or right
hand side) a symbol that is not present in the white list.  

###Running
1. Download the zip and navigate to the root directory
2. Run `make`
3. Run `./main`
4. Clean the project with `make clean`
