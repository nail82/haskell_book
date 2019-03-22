module RewriteLetToWhere where

g = x * 3 + y
    where x = 3
          y = 1000

g1 = x * 5
     where y = 10
           x = 10 * 5 + y

g2 = z / x + y
     where x = 7
           y = negate x
           z = y * 10
