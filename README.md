# Introduction #

`MatrixTree.hs` defines a recursive data structure `MatrixTree` for
representing matrices, with an emphasis on efficient representation of
sparse matrices.  It also provides functions for reading/writing
`MatrixTrees` to files in `MatrixMarket` formats.

`MatrixMarket.hs` contains functions to read/write between
`MatrixMarket` format files and a generic matrix data structure.

`SpAMM.hs` contains functions for performing matrix algebra on
`MatrixTrees` using the SpAMM algorithm.

The folder `Testing` contains a script `tests.hs` that loads
`MatrixMarket` files from the folder Matrices, performs SpAMM
operations on them, and reports on the correctness of the
results. Compile tests.hs with -i.:.. so it can find the modules it
needs.

# Usage #

To use SpAMM in a code, import `MatrixTree.hs` (for reading/writing
files) and `SpAMM.hs`.
