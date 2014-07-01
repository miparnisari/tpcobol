       identification division.
       program-id. Principal.
       author. "Maria Ines Parnisari - Ignacio Mazzara".
       date-written. "1er cuatrimestre 2014".

       ENVIRONMENT DIVISION.
       configuration section.
       input-output section.
       file-control.
       
       select alquileresmae
           assign to disk "..\..\..\Entrada\alquileres.dat"
           organization is indexed
           access mode is sequential
           record key is alq-clave
           file status is fs-alquileresmae.
           
       select choferes
           assign to disk "..\..\..\Entrada\choferes.dat"
           organization is indexed
           access mode is dynamic
           record key is cho-clave
           file status is fs-choferes.
       
       select rechazos
           assign to disk "..\..\..\Salida\rechazos.txt"
           organization is indexed
           record key is rech-clave
           file status is fs-rechazados.
       
       select listado
           assign to disk "..\..\..\Salida\listado.txt"
           organization is line sequential
           file status is fs-listado.
           
       select temporal
           assign to disk "..\..\..\Salida\listado-temporal.tmp".
           
       
       DATA DIVISION.
       file section.
       
       fd alquileresmae
           label record is standard.
       01 rec-alquileresmae.
           03 alq-clave.
               05  alq-patente pic x(6).
               05  alq-fecha   pic 9(8).
           03 alq-tipo-doc     pic x.
           03 alq-nro-doc      pic x(20).
           03 alq-importe      pic 9(4)v99.
           03 alq-chofer       pic x(7).   *> nro de legajo del chofer
           03 alq-estado       pic x.
               
               
       fd choferes
           label record is standard.
       01 rec-choferes.
           03  cho-clave.
               05  cho-nro-legajo  pic x(7).
               05  cho-fecha-desde pic 9(8).
           03  cho-fecha-hasta     pic 9(8).
           03  cho-turno           pic x.
        
        
       fd rechazos
           label record is standard.
       01  rec-rechazos.
           03  rech-clave.
               05  rech-patente    pic x(6).
               05  rech-fecha      pic 9(8).
           03  rech-tipo-doc       pic x.
           03  rech-nro-doc        pic x(20).
           03  rech-importe        pic 9(4)v99.
           
           
       fd listado
           label record is standard.
       01  reg-listado         pic x(80).
       
       
       sd temporal
           data record is reg-temporal.
       01  reg-temporal.
           03 temp-clave.
               05  temp-fecha.
                   07  temp-fecha-dd    pic     99.
                   07  temp-fecha-mm    pic     99.
                   07  temp-fecha-aaaa  pic     9999.
               05  temp-cho-nro-legajo  pic     x(7).
               05  temp-cho-turno       pic     x.
           03  temp-cli-numero      pic     x(8).
           03  temp-cli-tipo-doc    pic     x.
           03  temp-cli-nro-doc     pic     x(20).
           03  temp-cli-direccion   pic     x(30).
           03  temp-importe         pic     9(4)v99.
           
           
       working-storage section.
       
       01 cli-codigo-estado   pic x(2).   
       01 cli-numero          pic x(8).  
       01 cli-direccion       pic x(30).
       01 fs-alquileresmae    pic xx.
           88 ok-alq                   value "00".
           88 no-alq                   value "23".
           88 eof-alq                  value "10".
           
       01 fs-choferes         pic xx.
           88 ok-chof                  value "00".
           88 no-chof                  value "23".
           88 eof-chof                 value "10".
           
       01 fs-listado          pic xx.
           88 ok-list                  value "00".
           88 no-list                  value "23".
           88 eof-list                 value "10".
       
       01 fs-rechazados       pic xx.
           88 ok-rech                  value "00".
           88 no-rech                  value "23".
           88 eof-rech                 value "10".
           
       01 fecha-actual.
            03  fecha-actual-aaaa      pic     9999.
            03  fecha-actual-mm        pic     99.
            03  fecha-actual-dd        pic     99.
           
       01 clave-indice-chofer.
           03 chofer-actual        pic  x(7).
           03 fecha-desde          pic  9(8).
           
       01 ENCABEZADO1.
           03  FILLER               PIC X(7)    VALUE "Fecha: ".
           03  fecha-actual-dd      PIC 9(2).
           03  FILLER               PIC X       VALUE "/".
           03  fecha-actual-mm      PIC 9(2).
           03  FILLER               PIC X       VALUE "/".
           03  fecha-actual-aaaa    PIC 9(4).
           03  FILLER               PIC X(50)   VALUE SPACES.
           03  FILLER               PIC X(6)    VALUE "Hoja: ".
           03  E1-nro-hoja          PIC 9(3).
           
       01 ENCABEZADO2.
           03 FILLER PIC x(20) VALUE SPACES.
           03 FILLER PIC X(40) VALUE "Listado de alquileres aprobados".
           03 FILLER PIC x(20) VALUE SPACES.   
           
       01 ENCABEZADO3.
           03  FILLER              PIC X(7)    VALUE "Fecha: ".
           03  temp-fecha-dd       PIC 9(2).
           03  FILLER              PIC X       VALUE "/".
           03  temp-fecha-mm       PIC 9(2).
           03  FILLER              PIC X       VALUE "/".
           03  temp-fecha-aaaa     PIC 9(4).
           
       01 ENCABEZADO4.
           03  FILLER              PIC X(8)    VALUE "Chofer: ".
           03  E4-CHOFER           PIC x(7).
           03  FILLER              PIC X(10)   VALUE "   Turno: ".
           03  E4-TURNO            PIC x.
           03  FILLER              PIC X(57)   VALUE SPACES.
           
       01 ENCABEZADO5.
           03 FILLER PIC X(13)     VALUE     "      Cliente".
           03 FILLER PIC X(15)     VALUE     "       Tipo Doc".
           03 FILLER PIC X(17)     VALUE     "   Nro. Documento".
           03 FILLER PIC X(21)     VALUE     "            Direccion".                                                                                                                                                                                                                      ".
       
       01 ENCABEZADO6.
           03 FILLER PIC X(80)     VALUE ALL "-".
           
       01 ENCABEZADO7.
           03  FILLER              PIC X(6)   VALUE SPACES.
           03  temp-cli-numero     PIC X(8).
           03  FILLER              PIC X(10)  VALUE SPACES.
           03  temp-cli-tipo-doc   PIC X.
           03  FILLER              PIC X(8)   VALUE SPACES.
           03  temp-cli-nro-doc    PIC X(20).
           03  FILLER              PIC X(3)   VALUE SPACES.
           03  temp-cli-direccion  PIC X(30).
                    
       01 ENCABEZADO8.
           03  FILLER PIC X(23)    VALUE  "Totales por Chofer: ".
           03  E8-TOT-CHOFER       PIC 9(5)v99.
           
       01 ENCABEZADO9.
           03  FILLER PIC X(23)    VALUE  "Totales por Fecha: ".
           03  E9-TOT-FECHAS       PIC 9(7)v99.
           
       01 ENCABEZADO10.
           03  FILLER PIC X(26)     VALUE  "Total general: ".
           03  E10-TOT-GRAL         PIC 9(9)v99.
       
       01 chof-estado               pic xx.
           88 chof-estado-activo        value 'si'.
           88 chof-estado-inactivo      value 'no'.
       77 tot-gral                  pic 9(9)v99     value zeroes.
       77 tot-fechas                pic 9(7)v99     value zeroes.
       77 tot-chof                  pic 9(5)v99     value zeroes.
       
       77 nro-hoja                  pic 9(3)       value 1.
       77 nro-linea                 pic 99         value zeroes.
       77 lineas-por-hoja           pic 99         value 60.
       
       77 op                        pic x.
       77 contador                  pic 99.
       77 EndOfFile                 pic 9.

       PROCEDURE DIVISION.
           perform abrir-clientes.
           perform abrir-choferes.
           perform sort-section.
           perform cerrar-choferes.
           perform cerrar-clientes.
           accept op.
           stop run.
       
       abrir-clientes. 
           move "A" to op.
           call "BuscarDatosCliente" using op, alq-nro-doc, 
           cli-codigo-estado, cli-numero, cli-direccion.
           
       abrir-choferes.
           open input choferes.
           if is not ok-chof
               display "Error al abrir archivo choferes fs: "
                 fs-choferes
               accept op
               stop run
           end-if.
           
       cerrar-choferes.
           close choferes.
           
       cerrar-clientes.
           move "C" to op.
           call "BuscarDatosCliente" using op, alq-nro-doc, 
           cli-codigo-estado, cli-numero, cli-direccion.

       
       sort-section.
           sort temporal ascending key temp-clave of reg-temporal
               input procedure entrada
               output procedure salida.
           
       *> ENTRADA SECTION
       entrada.
           perform abrir-alquileres.
           perform abrir-rechazados.
           perform leer-alquileres.
           perform procesar-alquileres until eof-alq. 
           perform cerrar-alquileres. 
           perform cerrar-rechazados.
               
       abrir-alquileres.
           open input alquileresmae.
           if is not ok-alq
               display "Error al abrir archivo alquileres fs: "
                 fs-alquileresmae
               accept op
               stop run
           end-if.
           
       abrir-rechazados.
           open output rechazos.
           if is not ok-rech
               display "Error al abrir archivo rechazados fs: "
                 fs-rechazados
               accept op
               stop run
           end-if.
           
       leer-alquileres.
           read alquileresmae.
           if fs-alquileresmae is not equal to 00 and 10
               display "Error al leer alquileres fs:" fs-alquileresmae
           end-if.
           
       procesar-alquileres.
           if alq-estado = "P"
               perform posicionar-choferes.
           perform leer-alquileres.
           
       posicionar-choferes.
           move alq-chofer to cho-nro-legajo.
           move "no" to chof-estado.
           move 00000000 to cho-fecha-desde.
           
           start choferes key >= cho-clave.
           if ok-chof
               perform leer-choferes
               perform procesar-choferes until eof-chof or 
                   cho-fecha-desde > alq-fecha or chof-estado-activo
           end-if.
           if chof-estado-inactivo
                perform rechazar-alquiler
           end-if.
       
       leer-choferes.
           read choferes next record.
           
       procesar-choferes.
           if cho-fecha-hasta > alq-fecha
               move "si" to chof-estado
               move "T" to alq-estado
               perform actualizar-alquileres
               move "P" to op
               call "BuscarDatosCliente" using op, alq-nro-doc,
                cli-codigo-estado, cli-numero, cli-direccion
               perform escribir-arch-temporal.
           perform leer-choferes.
           
       actualizar-alquileres.  
           rewrite rec-alquileresmae.
           
       escribir-arch-temporal.
           move alq-fecha to temp-fecha.
           move cho-nro-legajo to temp-cho-nro-legajo of reg-temporal.
           move cho-turno to temp-cho-turno of reg-temporal.
           move cli-numero to temp-cli-numero of reg-temporal.
           move alq-tipo-doc to temp-cli-tipo-doc of reg-temporal.
           move alq-nro-doc to temp-cli-nro-doc of reg-temporal.
           move cli-direccion to temp-cli-direccion of reg-temporal.
           move alq-importe to temp-importe of reg-temporal.
           release reg-temporal.
           
       rechazar-alquiler.
            move alq-clave to rech-clave.
            move alq-tipo-doc to rech-tipo-doc.
            move alq-nro-doc to rech-nro-doc.
            move alq-importe to rech-importe.
            write rec-rechazos.
            display "Rechazo: " rec-rechazos.
           
       cerrar-alquileres.
           close alquileresmae.
           
       cerrar-rechazados.
           close rechazos.
       
       *> SALIDA SECTION
       salida.
           perform abrir-listado.
           perform leer-temporal.
           perform escribir-fecha-actual-y-hoja.
           perform procesar-fecha until EndOfFile = 1. 
           perform escribir-tot-gral.
           perform cerrar-listado.  
           
       abrir-listado.
           open output listado.
            if is not ok-list
               display "Error al abrir archivo listado fs: "
                 fs-listado
               accept op
               stop run
           end-if.
           
       leer-temporal.
           return temporal record 
               at end set EndOfFile to 1.
       
       escribir-fecha-actual-y-hoja.
           move function current-date to fecha-actual.
           move corresponding fecha-actual to ENCABEZADO1.
           move nro-hoja to E1-nro-hoja.
           display ENCABEZADO1.
           move ENCABEZADO1 to reg-listado.
           write reg-listado.
           display ENCABEZADO2.
           move ENCABEZADO2 to reg-listado.
           write reg-listado.
       
       procesar-fecha.
           perform inicializar-procesar-fecha.
           perform escribir-fecha-alquiler.
           perform procesar-chofer until 
               EndOfFile = 1 or
               fecha-actual <> temp-fecha.                           
           perform escribir-tot-fecha.
           perform hoja-nueva.
       
       inicializar-procesar-fecha.
          move 0 to tot-fechas.
          move temp-fecha to fecha-actual.
       
       escribir-fecha-alquiler.
           move corresponding temp-fecha to ENCABEZADO3.
           display ENCABEZADO3.
           move ENCABEZADO3 to reg-listado.
           write reg-listado.
       
       procesar-chofer.
           perform inicializar-procesar-chofer.
           perform escribir-chofer-turno.
           perform escribir-encabezado-cliente.
           perform procesar-cliente until 
               EndOfFile = 1 or 
               fecha-actual <> temp-fecha of reg-temporal                             
               or chofer-actual <> temp-cho-nro-legajo of reg-temporal.
           perform escribir-tot-chof.
           perform chequear-hoja-nueva.
       
       inicializar-procesar-chofer.
           move 0 to tot-chof.
           move temp-cho-nro-legajo of reg-temporal to chofer-actual.
       
       escribir-chofer-turno.
           move temp-cho-nro-legajo of reg-temporal to E4-CHOFER.
           move temp-cho-turno of reg-temporal to E4-TURNO.
           display ENCABEZADO4.
           move ENCABEZADO4 to reg-listado.
           write reg-listado.
       
       escribir-encabezado-cliente.
           display ENCABEZADO5.
           move ENCABEZADO5 to reg-listado.
           write reg-listado.
           display ENCABEZADO6.
           move ENCABEZADO6 to reg-listado.
           write reg-listado.
           add 6 to nro-linea.
       
       procesar-cliente.
           perform escribir-cliente.
           perform sumar-totales.
           perform chequear-hoja-nueva.
           perform leer-temporal.
           
       escribir-cliente.
           move corresponding reg-temporal to ENCABEZADO7.
           display ENCABEZADO7.
           move ENCABEZADO7 to reg-listado.
           write reg-listado.
           
       sumar-totales.
           add temp-importe to tot-chof.
           add temp-importe to tot-fechas.
           add temp-importe to tot-gral.
           add 1 to nro-linea.
           
       chequear-hoja-nueva.  
           
           if (nro-linea > lineas-por-hoja)               
               perform hoja-nueva.
       
       escribir-tot-chof.
           move tot-chof to E8-TOT-CHOFER.
           display ENCABEZADO8.
           move ENCABEZADO8 to reg-listado.
           write reg-listado.
           add 1 to nro-linea.
           
       escribir-tot-fecha.
           move tot-fechas to E9-TOT-FECHAS.
           display ENCABEZADO9.
           move ENCABEZADO9 to reg-listado.
           write reg-listado.
           add 1 to nro-linea.
           
       hoja-nueva.
           move nro-linea to contador.
           perform escribir-lineas-en-blanco until contador >=             
               lineas-por-hoja.
           add 1 to nro-hoja.
           perform escribir-fecha-actual-y-hoja.
           move 0 to nro-linea.
           
       escribir-lineas-en-blanco.
           move spaces to reg-listado.
           write reg-listado.
           display reg-listado.
           add 1 to contador.
       
       escribir-tot-gral.
           move tot-gral to E10-TOT-GRAL.
           display ENCABEZADO10.
           move ENCABEZADO10 to reg-listado.
           write reg-listado.
           
       cerrar-listado.
           close listado.