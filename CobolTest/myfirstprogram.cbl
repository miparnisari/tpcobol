       IDENTIFICATION division.
       PROGRAM-ID. myfirstprogram.

       ENVIRONMENT division.

       CONFIGURATION section.
   
       INPUT-OUTPUT section.
         FILE-CONTROL.
       
           SELECT file-write ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA division.
        FILE section.
         FD file-write
           VALUE OF FILE-ID IS "filedemo.txt".
       01 filerec.
        02 everything PIC X(32).
  
       WORKING-STORAGE section.
       01 rec.
        02 myname PIC X(20).
        02 onenum PIC 999.
        02 filler PIC X VALUE '+'.
        02 secnum PIC 999.
        02 filler PIC X VALUE '='.
        02 total PIC 9999.
        77 wish PIC X VALUE 'Y'.
       01 calledprog     pic x(20) value "calledprogram".

       PROCEDURE division.
       begin.

           OPEN OUTPUT file-write.

           PERFORM UNTIL wish = 'N' OR 'n'
            DISPLAY "Enter your name (up to 20 chars):"
            ACCEPT myname

            DISPLAY "Enter one number (up to 3 digits):"
            ACCEPT onenum

            DISPLAY "Enter another number (up to 3 digits):"
            ACCEPT secnum
            
            COMPUTE total = onenum + secnum
            DISPLAY "The sum is:"
            DISPLAY total

            MOVE rec TO filerec
            WRITE filerec

            DISPLAY "Would you like to continue? (Y/N): "
            ACCEPT wish
       
           call calledprog.
      
           CLOSE file-write.
           STOP RUN.