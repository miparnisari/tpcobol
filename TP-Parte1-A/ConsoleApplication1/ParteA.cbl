       IDENTIFICATION DIVISION.
       PROGRAM-ID. Program1.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       
       FILE-CONTROL.
           SELECT ALQUILERES ASSIGN TO INPUT
           "..\..\..\Files\ALQUILERES.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS ALQUILERES-STATUS.
           
           SELECT SOLICITUDES1 ASSIGN TO INPUT
           "..\..\..\Files\SOLICITUDES1.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS SOLICITUDES1-STATUS.
           
           SELECT SOLICITUDES2 ASSIGN TO INPUT
           "..\..\..\Files\SOLICITUDES2.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS SOLICITUDES2-STATUS.
          
           SELECT SOLICITUDES3 ASSIGN TO INPUT
           "..\..\..\Files\SOLICITUDES3.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS SOLICITUDES3-STATUS.
           
           SELECT AUTOS ASSIGN TO INPUT
           "..\..\..\Files\AUTOS.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS AUTOS-STATUS.
           
           SELECT RECHAZADOS ASSIGN TO OUTPUT
           "..\..\..\Files\RECHAZADOS.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS RECHAZADOS-STATUS.
           
           SELECT NUEVOALQUILERES ASSIGN TO OUTPUT
            "..\..\..\Files\NUEVOALQUILERES.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS NUEVOALQUILERES-STATUS.
           
           SELECT APROBADOS ASSIGN TO PRINTER.
           
       DATA DIVISION.
       FILE SECTION.
       
        FD    ALQUILERES LABEL RECORD STANDARD.
        01    REG-ALQUILERES.
               03 CLAVE-ALQ.
                   05  ALQ-PATENTE   PIC X(6).
                   05  ALQ-FECHA. 
                        07  ALQ-FECHA-DD   pic 9(2).
                        07  ALQ-FECHA-MM    pic 9(2).
                        07  ALQ-FECHA-AAAA   pic 9(4).
               03  ALQ-TIPODOC   PIC X.
               03  ALQ-NRODOC    PIC X(20).
               03  ALQ-IMPORTE   PIC 9(4)V99.
               
        FD    NUEVOALQUILERES  LABEL RECORD STANDARD.
        01    REG-NUEVOALQUILERES.
               03 CLAVE-NVALQ.
                   05  NUEVOALQ-PATENTE   PIC X(6).
                   05  NUEVOALQ-FECHA.
                        07  NUEVOALQ-FECHA-DD   PIC 9(2).
                        07  NUEVOALQ-FECHA-MM    PIC 9(2).
                        07  NUEVOALQ-FECHA-AAAA   PIC 9(4).
               03  NUEVOALQ-TIPODOC   PIC X.
               03  NUEVOALQ-NRODOC    PIC X(20).
               03  NUEVOALQ-IMPORTE   PIC 9(4)V99.
               
        FD    SOLICITUDES1  LABEL RECORD STANDARD.
        01    REG-SOLICITUDES1.
               03 CLAVE-SOL1.
                   05  SOL1-PATENTE   PIC X(6).
                   05  SOL1-FECHA.
                        07  ALQ-FECHA-DD   PIC 9(2).
                        07  ALQ-FECHA-MM    PIC 9(2).
                        07  ALQ-FECHA-AAAA   PIC 9(4).
               03  SOL1-TIPODOC   PIC X.
               03  SOL1-NRODOC    PIC X(20).
               
        FD    SOLICITUDES2  LABEL RECORD STANDARD.
        01    REG-SOLICITUDES2.
               03 CLAVE-SOL2.
                   05  SOL2-PATENTE   PIC X(6).
                   05  SOL2-FECHA.
                        07  ALQ-FECHA-DD   PIC 9(2).
                        07  ALQ-FECHA-MM    PIC 9(2).
                        07  ALQ-FECHA-AAAA   PIC 9(4).
               03  SOL2-TIPODOC   PIC X.
               03  SOL2-NRODOC    PIC X(20).
               
        FD    SOLICITUDES3  LABEL RECORD STANDARD.
        01    REG-SOLICITUDES3.
               03 CLAVE-SOL3.
                   05  SOL3-PATENTE   PIC X(6).
                   05  SOL3-FECHA.
                        07   SOL3-FECHA-DD   PIC 9(2).
                        07   SOL3-FECHA-MM    PIC 9(2).
                        07   SOL3-FECHA-AAAA   PIC 9(4).
               03  SOL3-TIPODOC   PIC X.
               03  SOL3-NRODOC    PIC X(20).

        FD    AUTOS  LABEL RECORD STANDARD.
        01    REG-AUTOS.
               03  AUT-PATENTE   PIC X(6).
               03  AUT-DESC      PIC X(30).
               03  AUT-MARCA     PIC X(20).
               03  AUT-COLOR     PIC X(10).
               03  AUT-TAMAÑO    PIC X.
               03  AUT-IMPORTE   PIC 9(4)V99.
               
        FD    RECHAZADOS  LABEL RECORD STANDARD.
        01    REG-RECHAZADOS.
               03 CLAVE-RECH.
                   05  RECH-PATENTE       PIC X(6).
                   05  RECH-FECHA.
                        07   RECH-FECHA-DD   PIC 9(2).
                        07   RECH-FECHA-MM    PIC 9(2).
                        07   RECH-FECHA-AAAA   PIC 9(4).
               03  RECH-TIPODOC   PIC X.
               03  RECH-NRODOC        PIC X(20).
               03  RECH-MOTIVO        PIC 9.
               03  RECH-AGENCIA       PIC 9.
               
        FD    APROBADOS  LABEL RECORD OMMITED.
        01    LINEA-APROBADOS PIC X(80).

       WORKING-STORAGE SECTION.
       
       01 VECAUTOS.
           03 VECAUTOS-ELEMENT OCCURS 300 TIMES
               ASCENDING KEY IS VECAUT-PATENTE 
               INDEXED BY INDICE.
                05  VECAUT-PATENTE   PIC X(6).
                05  VECAUT-DESC      PIC X(30).
                05  VECAUT-MARCA     PIC X(20).
                05  VECAUT-COLOR     PIC X(10).
                05  VECAUT-TAMAÑO    PIC X.
                05  VECAUT-IMPORTE   PIC 9(4)V99.
       
       01 CLAVE-ANTERIOR.
           03  CLVANTERIOR-PATENTE   PIC X(6).
           03  CLVANTERIOR-FECHA     PIC 9(8). 
           
       01 CLAVE-MENOR.
           03  CLVMENOR-PATENTE   PIC X(6).
           03  CLVMENOR-FECHA     PIC 9(8).
           
       77 ALQUILERES-STATUS PIC X(2).
       77 SOLICITUDES1-STATUS PIC X(2).
       77 SOLICITUDES2-STATUS PIC X(2).
       77 SOLICITUDES3-STATUS PIC X(2).
       77 AUTOS-STATUS PIC X(2).
       77 RECHAZADOS-STATUS PIC X(2).
       77 NUEVOALQUILERES-STATUS PIC X(2).
       77 TOTPATENTE PIC 99999V99 VALUE 0.
       77 TOTGRAL PIC 9999999V99 VALUE 0.
       77 TOTDIASPATENTE PIC 9999 VALUE 0.
       77 FLAG-CLAVE-APROB PIC X(2).
       77 FLAG-CLAVE-RECH PIC  X(2).
       77 PATENTE-VALIDA PIC X(2).
       77 AGENCIA PIC X.
       77 NUMERO-HOJA PIC 999 VALUE 1.
       77 CANT-LINEAS PIC 99 VALUE 0.
       
       01 ENCABEZADO1.
           03  FILLER      PIC X(9)    VALUE "Fecha: ".
           03  E1-FECHA-DD    PIC 9(2).
           03  FILLER      PIC X    VALUE "/".
           03  E1-FECHA-MM    PIC 9(2).
           03  FILLER      PIC X    VALUE "/".
           03  E1-FECHA-AAAA  PIC 9(4).
           03  FILLER      PIC X(50)   VALUE SPACES.
           03  FILLER      PIC X(6)    VALUE "Hoja: ".
           03  E1-HOJA      PIC 9(3).
           
       01 ENCABEZADO2.
           03 FILLER PIC x(26) VALUE SPACES.
           03 FILLER PIC X(38) VALUE 
               "Listado de autos alquilados aprobados".
           03 FILLER PIC x(26) VALUE SPACES.   
           
       01 ENCABEZADO3.
           03 FILLER PIC X(9) VALUE "Patente: ".
           03 E3-PATENTE-LETRAS   PIC X(3).
           03 FILLER PIC X VALUE SPACES.
           03 E3-PATENTE-NUMEROS  PIC X(3).
           03 FILLER PIC X(24) VALUE 
           "           Descripcion: ".
           03 E3-DESCRIPCION  PIC X(30).  
           03 FILLER PIC X(10) VALUE   SPACES. 
           03 FILLER PIC X(16) VALUE   SPACES. 
           03 FILLER PIC X(18) VALUE 
           "           Marca: ".  
           03 E3-MARCA     PIC X(20).
           03 FILLER PIC X(16) VALUE   SPACES.   
           03 FILLER PIC X(10) VALUE   SPACES. 
           03 FILLER PIC X(16) VALUE   SPACES. 
           03 FILLER PIC X(18) VALUE 
           "           Color: ".  
           03 E3-COLOR     PIC X(10).
           03 FILLER PIC X(36) VALUE   SPACES.   
           03 FILLER PIC X(16) VALUE   SPACES. 
           03 FILLER PIC X(19) VALUE 
           "           Tamaño: ".  
           03 E3-TAMAÑO     PIC X.
           03 FILLER PIC X(44) VALUE   SPACES.
           
       01 ENCABEZADO4.
           03 FILLER PIC X(14) VALUE
           "        Fecha".
           03 FILLER PIC X(17) VALUE 
           "         Tipo Doc".
           03 FILLER PIC X(21) VALUE 
           "       Nro. Documento".
           03 FILLER PIC X(15) VALUE 
           "       Agencia".  
           03 FILLER PIC X(13) VALUE SPACES.                                                                                                                                                                                                                                          ".
       
       01 ENCABEZADO5.
           03 FILLER PIC X(80) VALUE ALL "-".
           
       01 ENCABEZADO6.
           03  FILLER PIC X(6) VALUE SPACES.
           03  E6-FECHA-DD    PIC 9(2).
           03  FILLER      PIC X    VALUE "/".
           03  E6-FECHA-MM    PIC 9(2).
           03  FILLER      PIC X    VALUE "/".
           03  E6-FECHA-AAAA    PIC 9(4).
           03  FILLER PIC X(10) VALUE SPACES.
           03  E6-TIPODOC    PIC X.
           03  FILLER PIC X(8) VALUE SPACES.
           03  E6-NRODOC    PIC X(20).
           03  FILLER PIC X(7) VALUE SPACES.
           03  E6-AGENCIA    PIC 9.
           03 FILLER PIC X(28) VALUE SPACES. 
           
       01 ENCABEZADO7.
           03  FILLER PIC X(35) VALUE 
           "Totales por patente                ".
           03  FILLER    PIC X(17) VALUE
          "Cantidad de días ".
           03  E7-TOTDIASPATENTE      PIC 9999 .
           03  FILLER PIC X(16) VALUE
           "      Importe   ".
           03 E7-TOTPATENTE-ENTEROS      PIC 99999.
           03 FILLER PIC X VALUE ",".
           03  E7-TOTPATENTE-DECIMALES PIC 99.
           
       01 ENCABEZADO8.
           03  FILLER PIC X(35) VALUE 
           "Totales general                ".
           03  FILLER    PIC X(21) VALUE SPACES.
           03  FILLER PIC X(14) VALUE
           "      Importe ".
           03  E8-TOTGRALPATENTE-ENTEROS      PIC 9999999.
           03  FILLER PIC X VALUE ",".
           03  E8-TOTGRALPATENTE-DECIMALES PIC 99.
           
       01 ENCABEZADO9.
           03 FILLER PIC X(80) VALUE SPACES.
       
       PROCEDURE DIVISION.
           PERFORM INICIALIZAR.
           PERFORM LEER-ALQ.
           PERFORM LEER-SOL1.
           PERFORM LEER-SOL2.
           PERFORM LEER-SOL3.
           PERFORM LEER-AUTOS.
           PERFORM GUARDAR-AUTOS-EN-VECAUTOS.
           PERFORM IMPRIMIR-ENCABEZADO-1-Y-2.
           PERFORM PROCESO1 UNTIL ALQUILERES-STATUS IS EQUAL TO 10 AND 
               SOLICITUDES1-STATUS IS EQUAL TO 10 AND   
               SOLICITUDES2-STATUS IS EQUAL TO 10 AND
               SOLICITUDES3-STATUS IS EQUAL TO 10.                 
           PERFORM IMPRIMIR-TOT-GRAL.
           PERFORM CERRAR-ARCHIVOS.
           STOP RUN.
           
       INICIALIZAR.
           OPEN INPUT ALQUILERES.
           IF ALQUILERES-STATUS IS NOT EQUAL TO 00
               DISPLAY "ERROR ABRIR ALQUILERES FS: " ALQUILERES-STATUS
               STOP RUN
           END-IF.
           
           OPEN INPUT SOLICITUDES1.
           IF SOLICITUDES1-STATUS IS NOT EQUAL TO 00
               DISPLAY "ERROR ABRIR SOLICITUDES1 FS: " 
                    SOLICITUDES1-STATUS                                 
               STOP RUN
           END-IF.
           
           OPEN INPUT SOLICITUDES2.
           IF SOLICITUDES2-STATUS IS NOT EQUAL TO 00
               DISPLAY "ERROR ABRIR SOLICITUDES2 FS: " 
                   SOLICITUDES2-STATUS                                  
               STOP RUN
           END-IF.
           
           OPEN INPUT SOLICITUDES3.
           IF SOLICITUDES3-STATUS IS NOT EQUAL TO 00
               DISPLAY "ERROR ABRIR SOLICITUDES3 FS: " 
                   SOLICITUDES3-STATUS                                  
               STOP RUN
           END-IF.
           
           OPEN INPUT AUTOS.
           IF AUTOS-STATUS IS NOT EQUAL TO 00
               DISPLAY "ERROR ABRIR AUTOS FS: " 
                   AUTOS-STATUS                                  
               STOP RUN
           END-IF
           
           OPEN OUTPUT RECHAZADOS.
           IF RECHAZADOS-STATUS IS NOT EQUAL TO 00
               DISPLAY "ERROR ABRIR RECHAZADOS FS: " 
                   RECHAZADOS-STATUS                                  
               STOP RUN
           END-IF.
           
           OPEN OUTPUT NUEVOALQUILERES.
           IF NUEVOALQUILERES-STATUS IS NOT EQUAL TO 00
               DISPLAY "ERROR ABRIR NUEVOALQUILERES FS: " 
                  NUEVOALQUILERES-STATUS                                
               STOP RUN
           END-IF.
           
       CERRAR-ARCHIVOS.
           CLOSE ALQUILERES.
           CLOSE SOLICITUDES1.
           CLOSE SOLICITUDES2.
           CLOSE SOLICITUDES3.
           CLOSE NUEVOALQUILERES.
           CLOSE AUTOS.
           CLOSE RECHAZADOS.
           
       LEER-ALQ.
           READ ALQUILERES RECORD AT END MOVE HIGH-VALUE TO CLAVE-ALQ.
           IF ALQUILERES-STATUS IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR LEER ALQUILERES FS: " ALQUILERES-STATUS
           END-IF.
          
           
       LEER-SOL1.
           READ SOLICITUDES1 RECORD AT END MOVE HIGH-VALUE TO 
           CLAVE-SOL1.                                                  
           IF SOLICITUDES1-STATUS IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR LEER SOLICITUDES1 FS: " 
               SOLICITUDES1-STATUS                                      
           END-IF.
           
       LEER-SOL2.
           READ SOLICITUDES2 RECORD AT END MOVE HIGH-VALUE TO 
           CLAVE-SOL2.                                                  
           IF SOLICITUDES2-STATUS IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR LEER SOLICITUDES2 FS: " 
               SOLICITUDES2-STATUS                                      
           END-IF.

       LEER-SOL3.
           READ SOLICITUDES3 RECORD AT END MOVE HIGH-VALUE TO 
           CLAVE-SOL3.                                                  
           IF SOLICITUDES3-STATUS IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR LEER SOLICITUDES3 FS: " 
               SOLICITUDES3-STATUS                                      
           END-IF.

       LEER-AUTOS.
           READ AUTOS RECORD.
           IF AUTOS-STATUS IS NOT EQUAL TO 00 AND 10
               DISPLAY "ERROR LEER AUTOS FS: " AUTOS-STATUS
           END-IF.
          
       GUARDAR-AUTOS-EN-VECAUTOS.
           PERFORM GUARDO-AUTO VARYING INDICE FROM 1 BY 1 UNTIL INDICE
           > 300 OR AUTOS-STATUS IS EQUAL TO 10.
           
       GUARDO-AUTO.
           MOVE AUT-PATENTE TO VECAUT-PATENTE(INDICE).
           MOVE AUT-DESC TO VECAUT-DESC(INDICE).
           MOVE AUT-COLOR TO VECAUT-COLOR(INDICE ).
           MOVE AUT-MARCA TO VECAUT-MARCA(INDICE).
           MOVE AUT-TAMAÑO TO VECAUT-TAMAÑO(INDICE).
           MOVE AUT-IMPORTE TO VECAUT-IMPORTE(INDICE).
           PERFORM LEER-AUTOS.
           
       IMPRIMIR-ENCABEZADO-1-Y-2.
           MOVE FUNCTION CURRENT-DATE(7:2) TO E1-FECHA-DD.
           MOVE FUNCTION CURRENT-DATE(5:2) TO E1-FECHA-MM.
           MOVE FUNCTION CURRENT-DATE(1:4) TO E1-FECHA-AAAA.
           MOVE NUMERO-HOJA TO E1-HOJA.
           DISPLAY ENCABEZADO1.
           DISPLAY ENCABEZADO2.
           ADD 2 TO CANT-LINEAS.
           
           
       PROCESO1.
           MOVE 0 TO TOTPATENTE.
           MOVE 0 TO TOTDIASPATENTE.
           PERFORM DETERMINAR-CLAVE-MENOR.
           MOVE CLAVE-MENOR TO CLAVE-ANTERIOR.
           SET INDICE TO 1.
           PERFORM BUSCAR-PATENTE-EN-VEC-AUTOS.
           PERFORM PROCESO2 UNTIL CLVMENOR-PATENTE IS NOT EQUAL TO 
               CLVANTERIOR-PATENTE.
           IF TOTPATENTE IS NOT EQUAL TO ZERO
               ADD TOTPATENTE TO TOTGRAL
               PERFORM IMPRIMIR-TOTALES-POR-PATENTE
           END-IF.
           
       DETERMINAR-CLAVE-MENOR.
           IF CLAVE-ALQ  < CLAVE-SOL1
               MOVE CLAVE-ALQ TO CLAVE-MENOR 
           ELSE 
               MOVE CLAVE-SOL1 TO CLAVE-MENOR
           END-IF.
           
           IF CLAVE-SOL2  < CLAVE-MENOR
               MOVE CLAVE-SOL2 TO CLAVE-MENOR 
           END-IF.
           
           IF CLAVE-SOL3  < CLAVE-MENOR
               MOVE CLAVE-SOL3 TO CLAVE-MENOR
           END-IF.
           IF CLVMENOR-FECHA IS NOT EQUAL TO CLVANTERIOR-FECHA
              PERFORM INICIALIZAR-FLAGS
              MOVE CLVMENOR-FECHA TO CLVANTERIOR-FECHA
           END-IF.
           
       BUSCAR-PATENTE-EN-VEC-AUTOS.
           SEARCH VECAUTOS-ELEMENT 
               AT END PERFORM AUTO-NO-ENCONTRADO
           WHEN VECAUT-PATENTE(INDICE) IS EQUAL TO CLVMENOR-PATENTE 
           PERFORM AUTO-ENCONTRADO.
       
       AUTO-NO-ENCONTRADO.
           MOVE 'NO' TO PATENTE-VALIDA.
           MOVE 'SI' TO  FLAG-CLAVE-RECH.
           MOVE CLVMENOR-PATENTE TO RECH-PATENTE.
           MOVE CLVMENOR-FECHA TO RECH-FECHA.
           MOVE  2 TO RECH-MOTIVO.
           
       AUTO-ENCONTRADO.
           MOVE 'SI' TO PATENTE-VALIDA.
           IF TOTGRAL IS NOT EQUAL TO ZERO
               PERFORM IMPRIMIR-SALTO-DE-LINEA UNTIL CANT-LINEAS
                    IS EQUAL TO 0
               PERFORM IMPRIMIR-ENCABEZADO-1-Y-2
           END-IF.
           PERFORM IMPRIMIR-ENCABEZADO-PATENTE.
           
           
           
       IMPRIMIR-ENCABEZADO-PATENTE.
           ADD 9 TO CANT-LINEAS.
           IF CANT-LINEAS IS GREATER THAN 60
               ADD 1 TO NUMERO-HOJA
               MOVE 0 TO CANT-LINEAS
               PERFORM IMPRIMIR-ENCABEZADO-1-Y-2
               ADD 9 TO CANT-LINEAS
           END-IF.  
           MOVE VECAUT-PATENTE(INDICE)(1:3) TO E3-PATENTE-LETRAS.
           MOVE VECAUT-PATENTE(INDICE)(4:3) TO E3-PATENTE-NUMEROS.
           MOVE VECAUT-DESC(INDICE) TO E3-DESCRIPCION.
           MOVE VECAUT-MARCA(INDICE) TO E3-MARCA.
           MOVE VECAUT-COLOR(INDICE) TO E3-COLOR.
           MOVE VECAUT-TAMAÑO(INDICE) TO E3-TAMAÑO.
           DISPLAY ENCABEZADO3.
           DISPLAY ENCABEZADO4.
           DISPLAY ENCABEZADO5.

           
       PROCESO2.
           PERFORM INICIALIZAR-FLAGS.
           PERFORM POS-ALQUILERES.
           PERFORM POS-SOLICITUDES1.
           PERFORM POS-SOLICITUDES3.
           PERFORM POS-SOLICITUDES2.
           PERFORM DETERMINAR-CLAVE-MENOR.

       INICIALIZAR-FLAGS.
          MOVE 'NO' TO  FLAG-CLAVE-APROB.
          IF PATENTE-VALIDA IS EQUAL TO 'SI'
            MOVE 'NO' TO  FLAG-CLAVE-RECH
          END-IF.
          
       POS-ALQUILERES.
           MOVE 0 TO AGENCIA.
           IF CLAVE-MENOR IS EQUAL TO CLAVE-ALQ
               PERFORM PROCESAR-ALQUILERES
           END-IF.
           
       POS-SOLICITUDES1.
           MOVE 1 TO AGENCIA.
           IF CLAVE-MENOR IS EQUAL TO CLAVE-SOL1
               PERFORM PROCESAR-SOLICITUDES1
           END-IF.
           
       POS-SOLICITUDES3.
           MOVE 3 TO AGENCIA.
           IF CLAVE-MENOR IS EQUAL TO CLAVE-SOL3
               PERFORM PROCESAR-SOLICITUDES3
           END-IF.
           
       POS-SOLICITUDES2.
           MOVE 2 TO AGENCIA.
           IF CLAVE-MENOR IS EQUAL TO CLAVE-SOL2
               PERFORM PROCESAR-SOLICITUDES2
           END-IF.
           
       PROCESAR-ALQUILERES.
           IF PATENTE-VALIDA IS EQUAL TO 'NO'
                MOVE 'SI' TO  FLAG-CLAVE-RECH
                MOVE 2 TO RECH-MOTIVO 
                MOVE AGENCIA TO RECH-AGENCIA
                MOVE ALQ-TIPODOC TO RECH-TIPODOC
                MOVE ALQ-NRODOC TO RECH-NRODOC
                WRITE REG-RECHAZADOS
           ELSE
               MOVE 'SI' TO FLAG-CLAVE-APROB
               WRITE REG-NUEVOALQUILERES FROM REG-ALQUILERES
               ADD ALQ-IMPORTE TO TOTPATENTE
           END-IF.
           PERFORM LEER-ALQ.
           
           
       PROCESAR-SOLICITUDES1.
           IF FLAG-CLAVE-APROB IS EQUAL TO 'SI'
                MOVE 'SI' TO  FLAG-CLAVE-RECH
                MOVE 1 TO RECH-MOTIVO
                MOVE AGENCIA TO RECH-AGENCIA
                MOVE CLVMENOR-PATENTE TO RECH-PATENTE
                MOVE CLVMENOR-FECHA TO RECH-FECHA
                MOVE SOL1-TIPODOC TO RECH-TIPODOC
                MOVE SOL2-NRODOC TO RECH-NRODOC
                WRITE REG-RECHAZADOS
           ELSE IF PATENTE-VALIDA IS EQUAL TO 'SI'
                MOVE 'SI' TO FLAG-CLAVE-APROB
                MOVE REG-SOLICITUDES1 TO REG-NUEVOALQUILERES
                MOVE AUT-IMPORTE TO NUEVOALQ-IMPORTE
                WRITE REG-NUEVOALQUILERES
                ADD AUT-IMPORTE TO TOTPATENTE
                ADD 1 TO TOTDIASPATENTE
                PERFORM IMPRIMO-APROBADO
           ELSE IF PATENTE-VALIDA IS EQUAL TO 'NO'
                MOVE 'SI' TO  FLAG-CLAVE-RECH
                MOVE 2 TO RECH-MOTIVO
                MOVE AGENCIA TO RECH-AGENCIA
                MOVE SOL1-TIPODOC TO RECH-TIPODOC
                MOVE SOL1-NRODOC TO RECH-NRODOC
                WRITE REG-RECHAZADOS 
           END-IF.
           PERFORM LEER-SOL1.
           
       PROCESAR-SOLICITUDES3.
           IF FLAG-CLAVE-APROB IS EQUAL TO 'SI'
                MOVE 'SI' TO  FLAG-CLAVE-RECH
                MOVE 1 TO RECH-MOTIVO
                MOVE CLVMENOR-PATENTE TO RECH-PATENTE
                MOVE CLVMENOR-FECHA TO RECH-FECHA
                MOVE AGENCIA TO RECH-AGENCIA
                MOVE SOL3-TIPODOC TO RECH-TIPODOC
                MOVE SOL3-NRODOC TO RECH-NRODOC
                WRITE REG-RECHAZADOS
          ELSE IF PATENTE-VALIDA IS EQUAL TO 'SI'
                MOVE 'SI' TO FLAG-CLAVE-APROB
                MOVE REG-SOLICITUDES3 TO REG-NUEVOALQUILERES
                MOVE AUT-IMPORTE TO NUEVOALQ-IMPORTE
                WRITE REG-NUEVOALQUILERES
                ADD AUT-IMPORTE TO TOTPATENTE
                ADD 1 TO TOTDIASPATENTE
                PERFORM IMPRIMO-APROBADO
           ELSE IF PATENTE-VALIDA IS EQUAL TO 'NO'
                MOVE 'SI' TO  FLAG-CLAVE-RECH
                MOVE 2 TO RECH-MOTIVO
                MOVE AGENCIA TO RECH-AGENCIA
                MOVE SOL3-TIPODOC TO RECH-TIPODOC
                MOVE SOL3-NRODOC TO RECH-NRODOC
                WRITE REG-RECHAZADOS
           END-IF.
           PERFORM LEER-SOL3.
           
       PROCESAR-SOLICITUDES2.
           IF FLAG-CLAVE-APROB IS EQUAL TO 'SI'
                MOVE 'SI' TO  FLAG-CLAVE-RECH
                MOVE 1 TO RECH-MOTIVO
                MOVE AGENCIA TO RECH-AGENCIA
                MOVE CLVMENOR-PATENTE TO RECH-PATENTE
                MOVE CLVMENOR-FECHA TO RECH-FECHA
                MOVE SOL2-TIPODOC TO RECH-TIPODOC
                MOVE SOL2-NRODOC TO RECH-NRODOC
                WRITE REG-RECHAZADOS
          ELSE IF PATENTE-VALIDA IS EQUAL TO 'SI'
                MOVE 'SI' TO FLAG-CLAVE-APROB
                MOVE REG-SOLICITUDES2 TO REG-NUEVOALQUILERES
                MOVE AUT-IMPORTE TO NUEVOALQ-IMPORTE
                WRITE REG-NUEVOALQUILERES
                ADD AUT-IMPORTE TO TOTPATENTE
                ADD 1 TO TOTDIASPATENTE
                PERFORM IMPRIMO-APROBADO
           ELSE IF PATENTE-VALIDA IS EQUAL TO 'NO'
                MOVE 'SI' TO  FLAG-CLAVE-RECH
                MOVE 2 TO RECH-MOTIVO
                MOVE AGENCIA TO RECH-AGENCIA
                MOVE SOL2-TIPODOC TO RECH-TIPODOC
                MOVE SOL2-NRODOC TO RECH-NRODOC
                WRITE REG-RECHAZADOS
           END-IF.
           PERFORM LEER-SOL2.
           
       IMPRIMO-APROBADO.  
           ADD 1 TO CANT-LINEAS.
           IF CANT-LINEAS IS GREATER THAN 60
               ADD 1 TO NUMERO-HOJA
               MOVE 0 TO CANT-LINEAS
               PERFORM IMPRIMIR-ENCABEZADO-1-Y-2
               PERFORM IMPRIMIR-ENCABEZADO-PATENTE
               ADD 1 TO CANT-LINEAS
           END-IF. 
           MOVE NUEVOALQ-FECHA(1:2) TO E6-FECHA-DD.
           MOVE NUEVOALQ-FECHA(3:2) TO E6-FECHA-MM.
           MOVE NUEVOALQ-FECHA(5:4) TO E6-FECHA-AAAA.
           MOVE NUEVOALQ-TIPODOC TO E6-TIPODOC.
           MOVE NUEVOALQ-NRODOC TO E6-NRODOC.
           MOVE AGENCIA TO E6-AGENCIA.
           DISPLAY ENCABEZADO6.
           
       
       IMPRIMIR-SALTO-DE-LINEA.
           ADD 1 TO CANT-LINEAS.
           DISPLAY ENCABEZADO9.
           IF CANT-LINEAS IS GREATER THAN 60
               MOVE 0 TO CANT-LINEAS
               ADD 1 TO NUMERO-HOJA
           END-IF. 
           
       IMPRIMIR-TOTALES-POR-PATENTE.
           ADD 3 TO CANT-LINEAS.
           IF CANT-LINEAS IS GREATER THAN 60
               ADD 1 TO NUMERO-HOJA
               MOVE 0 TO CANT-LINEAS
               PERFORM IMPRIMIR-ENCABEZADO-1-Y-2
               ADD 3 TO CANT-LINEAS
           END-IF.    
           MOVE TOTDIASPATENTE TO E7-TOTDIASPATENTE.
           MOVE TOTPATENTE(1:5) TO  E7-TOTPATENTE-ENTEROS.
           MOVE TOTPATENTE(6:2) TO E7-TOTPATENTE-DECIMALES.
           DISPLAY ENCABEZADO7.
           DISPLAY ENCABEZADO9.
           DISPLAY ENCABEZADO9.
       
           
       IMPRIMIR-TOT-GRAL.
         ADD 1 TO CANT-LINEAS.
         IF CANT-LINEAS IS GREATER THAN 60
               ADD 1 TO NUMERO-HOJA
               MOVE 0 TO CANT-LINEAS
               PERFORM IMPRIMIR-ENCABEZADO-1-Y-2
           END-IF.    
           MOVE TOTGRAL(1:7) TO  E8-TOTGRALPATENTE-ENTEROS.
           MOVE TOTGRAL(8:2) TO E8-TOTGRALPATENTE-DECIMALES.
           DISPLAY ENCABEZADO8.
           
       END PROGRAM Program1.