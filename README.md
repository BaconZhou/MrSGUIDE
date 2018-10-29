# Mr.S

**M**ultiplre **R**esponse **S**ubgroup identification

The current package is still under heavily devolpment.

For subgroup identification use [GUIDE](https://www.stat.wisc.edu/~loh/guide.html) algorithm.

Here are the links and paper for reference:

Loh, W.-Y., Man, M. and Wang, S. (2019), [Subgroups from regression trees with adjustment for prognostic effects and post-selection inference](http://www.stat.wisc.edu/~loh/treeprogs/guide/sm18.pdf), Statistics in Medicine

Loh, W.-Y., Fu, H., Man, M., Champion, V. and Yu, M. (2016), [Identification of subgroups with differential treatment effects for longitudinal and multiresponse variables](http://www.stat.wisc.edu/~loh/treeprogs/guide/LFMCY16.pdf), Statistics in Medicine

# Package install

- OpenMP support is required.

## MacOS

Please refer to [R Compiler Tools for Rcpp on macOS](https://thecoatlessprofessor.com/programming/r-compiler-tools-for-rcpp-on-macos/) if you have problem with compliering the package.

I used homebrew for libomp

```
brew update
brew install llvm libomp gcc
```

In the `~/.R/Makevars` file

```
CC=/usr/local/opt/llvm/bin/clang
CXX=/usr/local/opt/llvm/bin/clang++
CXX1X=/usr/local/opt/llvm/bin/clang++
CXX11=/usr/local/opt/llvm/bin/clang++

FLIBS=-L/usr/local/Cellar/gcc/8.2.0 -L/usr/local/Cellar/gcc/8.2.0/lib/gcc/8 -lgfortran -lquadmath -lm
```
