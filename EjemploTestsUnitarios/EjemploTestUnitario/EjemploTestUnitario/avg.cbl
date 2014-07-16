       identification division.
       program-id. avg.

       data division.
       working-storage section.
       01 a pic 99.
       01 b pic 99.
       
       linkage section.
       01 res pic 99.
       
       procedure division using a b res.
           compute res = ( a + b ) / 2.
           
       end program avg.