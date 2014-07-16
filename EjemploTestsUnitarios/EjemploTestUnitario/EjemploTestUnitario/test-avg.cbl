       identification division.
       program-id. test-avg.

       data division.
       working-storage section.
           copy CBUC0002.
           01 a pic 99.
           01 b pic 99.
           01 res pic 99.
           01 expected pic 99.
           01 assert-name pic x(20).
           
           linkage section.
               copy CBUC0001.
                      
       procedure division using cbu-ctx.
*arrange
          move 10 to a.
          move 8 to b.
          move 9 to expected.
       
*act
          call "avg" using a b res.

*assert
          move "avg(10,8)=9" to assert-name.
          call cbu-assert-nb3-equals
               using cbu-ctx assert-name expected res.
           
       end program test-avg.