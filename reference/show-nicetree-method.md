# summarise nicetree object

Prints to console the number of nodes and levels and a cross-table,
excluding the root node. Also prints the root node and names of the
first layer in the tree (children of the root node).

## Usage

``` r
# S4 method for class 'nicetree'
show(object)
```

## Arguments

- object:

  of class nicetree

## Value

prints summary to console

## Examples

``` r
h <- nicetree()
(h)
#> 
#>  total number of nodes:  4871
#>  total number of levels (excluding 0):  
#>  number of nodes by level :
#>    0    1    2    3 
#>    1   16  176 4679 
#> 
#>  Root node :  root ( root )
#>  First layer nodes : BLOODA CANCER CARDIO DIABET DIGEST FERTIL GYNAEC INFECT KIDNEY LIVERC MENTAL MUSCUL NEUROL RESPIR SKINCO UROLOG
```
