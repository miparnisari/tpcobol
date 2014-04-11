       IDENTIFICATION division.
         PROGRAM-ID. tpcobolparte1.

       ENVIRONMENT division.

       CONFIGURATION section.
   
       INPUT-OUTPUT section.
         FILE-CONTROL.
           SELECT alquileresmae ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA division.
        FILE section.
         FD alquileresmae
           VALUE OF FILE-ID IS "alquileresmaestro.dat".
       01 alquileresmae-rec.
        02 alq-patente PIC X(6).  
        02 alq-fecha PIC 9(8).
        02 alq-tipo-doc PIC X. *> D=DNI, C=CEDULA IDENTIDAD, R=LIBRETA, P=PASAPORTE, L=LICENCIA
        02 alq-nro-doc PIC X(20).
        02 alq-importe PIC 9(4)V99. *> 1234.99
  
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
       actualizamaestro.

           OPEN OUTPUT alquileresmae.

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

            DISPLAY "Would you like to continue? (Y/N): "
            ACCEPT wish
       
           call calledprog.
      
           CLOSE alquileresmae.
           STOP RUN.