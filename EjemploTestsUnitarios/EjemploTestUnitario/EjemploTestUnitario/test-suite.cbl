       identification division.
       program-id. test-suite.

       data division.
       working-storage section.
         copy CBUC0001.
         copy CBUC0002.
         01 str1 pic x(20).
         01 str2 pic x(100).
         01 wish pic x(1) value 'n'.

       procedure division.
       
	   call CBU-initialize using CBU-ctx.
	   call CBU-add-suite using CBU-ctx str1 str2.
		      
	   initialize str1 str2.
	   move "test-avg" to str1.
	   move "test-avg description" to str2.
	   call CBU-add-test-next using CBU-ctx str1 str2.
		      
       call CBU-run using cbu-ctx.
		      
	   display "Press enter to exit."
	   accept wish.
           
       end program test-suite.