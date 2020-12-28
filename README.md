# Parallel Greedy Tetris Solver
### By Trey Gilliland and Derek Zhang

## Dependencies

This solver relies on Data.Matrix, System.Random, and Control.Monad.Par which can be installed using stack as follows:

```
stack install random
stack install matrix
stack install monad-par
```

## How to use

To compile (and output an eventlog):
```
stack ghc -- -O2 -Wall Tetris.hs -threaded -rtsopts -eventlog
```

To run compiled executable:

* Note: input.txt can be replaced with any desired single line file of space seperated integers 
* The -N# flag can be used to set the desired number of cores to run on
```
./Tetris input.txt +RTS -N4 -s -lsf
```


## File list

1. Tetris.hs - main file containing search methods and main implementations. This file should be the target for compilation
    * Note: an alternate main method is provided that allows the user to pass in an N value to place n random Tetrominos onto an empty board
    * Printing can be removed for performance evaluation by commenting out line 73 in gameloop
    ```
    -- putStrLn $ show (BoardMove board piece score)
    ```

2. AI.hs - AI module contains methods involving scoring a board based off heuristics and weights

3. Helpers.hs - Helpers module contains helper methods used for printing and calculations

4. Types.hs - Types module is used in all other files and contains the types and instances needed across our code

5. input.txt - a list of 500 random integers that can be used with the executable to run our program
