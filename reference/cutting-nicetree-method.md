# take a cutting from a tree (object of class nicetree)

A subtree is formed with the specified node at the root. This is a
'cutting': all nodes that are not descendents of the specified node are
eliminated. The function returns a (smaller) tree (object of class
nicetree), with the specified node as the root.

## Usage

``` r
# S4 method for class 'nicetree'
cutting(h, nodelabel = NULL)
```

## Arguments

- h:

  an object of class nicetree

- nodelabel:

  node of interest

## Value

object of class nicetree

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
subh <- cutting(h, "CANCER")
(subh)
#> 
#>  total number of nodes:  879
#>  total number of levels (excluding 0):  
#>  number of nodes by level :
#>   1   2   3 
#>   1  27 852 
#> 
#>  Root node :  CANCER ( Cancer )
#>  First layer nodes : Bltc Blddc Babmc Brnc Brsc Crvc Clrc Cmoc Endc Gbct Hanc Lvrcnc Lngc Mtst Nrnt Ospc Ovrc Pncc Patc Prtc Prsc Rnlc Srcm Skncnc Stmc Thyc Uatc
```
