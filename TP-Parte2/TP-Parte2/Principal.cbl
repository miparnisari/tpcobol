       identification division.
       program-id. Principal.
       author. "Maria Ines Parnisari - Ignacio Mazzara".
       date-written. "1er cuatrimestre 2014".

       ENVIRONMENT DIVISION.
       configuration section.
       input-output section.
       file-control.
       
       select alquileresmae
           assign to disk "..\..\Files\alquileres.dat"
           organization is indexed
           record key is alq-clave
           file status is fs-alquileresmae.
           
       select choferes
           assign to disk "..\..\Files\choferes.dat"
           organization is indexed
           record key is cho-clave
           file status is fs-choferes.
       
       select rechazos
           assign to disk "..\..\Files\rechazos.txt"
           organization is indexed
           record key is rech-clave
           file status is fs-rechazos.
       
       select listado
           assign to disk "..\..\Files\listado.txt"
           organization is line sequential
           file status is fs-listado.
           
       select temporal
           assign to disk "..\..\Files\listado-temporal.tmp".
           
       
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
               88  alq-estado-terminado    value "T".
               88  alq-estado-rechazado    value "R".
               88  alq-estado-presentado   value "P".
               
               
       fd choferes
           label record is standard.
       01 rec-choferes.
           03  cho-clave.
               05  cho-nro-legajo  pic x(7).
               05  cho-fecha-desde pic 9(8).
           03  cho-fecha-hasta pic 9(8).
           03  cho-turno       pic x.
               88  cho-turno-maniana       value "M".
               88  cho-turno-tarde         value "T".
               88  cho-turno-noche         value "N".
               
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
           03  temp-fecha.
               05  temp-fecha-dd    pic     99.
               05  temp-fecha-mm    pic     99.
               05  temp-fecha-aaaa  pic     9999.
           03  temp-cho-nro-legajo  pic     x(7).
           03  temp-cho-turno       pic     x(6). *> "manana", "tarde" etc
           03  temp-cli-numero      pic     x(8).
           03  temp-cli-tipo-doc    pic     x.
           03  temp-cli-nro-doc     pic     x(20).
           03  temp-cli-direccion   pic     x(30).
           
           
       working-storage section.
       
       01 chof-estado pic xx.
       01  cli-codigo-estado   pic x(2).   
       01  cli-numero          pic x(8).  
       01  cli-direccion       pic x(30).
       01 fs-alquileresmae     pic xx.
           88 ok-alq                    value "00".
           88 no-alq                    value "23".
           88 eof-alq                   value "10".
           
       01 fs-choferes          pic xx.
           88 ok-chof                   value "00".
           88 no-chof                   value "23".
           88 eof-chof                  value "10".
           
       01 fs-listado          pic xx.
           88 ok-list                  value "00".
           88 no-list                   value "23".
           88 eof-list                  value "10".
       
       01 fs-rechazados       pic xx.
           88 ok-rech                   value "00".
           88 no-rech                  value "23".
           88 eof-rech                  value "10".
           
       01 fecha-actual.
               05  fecha-actual-dd    pic     99.
               05  fecha-actual-mm    pic     99.
               05  fecha-actual-aaaa  pic     9999.
               
       01 clave-indice-chofer.
           03 chofer-actual pic x(7).
           03 fecha-desde  pic  9(8).
           
       01 encabezado1.
           03  FILLER      PIC X(9)    VALUE "Fecha: ".
           03  E1-FECHA-DD    PIC 9(2).
           03  FILLER      PIC X    VALUE "/".
           03  E1-FECHA-MM    PIC 9(2).
           03  FILLER      PIC X    VALUE "/".
           03  E1-FECHA-AAAA  PIC 9(4).
           03  FILLER      PIC X(50)   VALUE SPACES.
           03  FILLER      PIC X(6)    VALUE "Hoja: ".
           03  E1-HOJA      PIC 9(3).
           
       01 encabezado2.
           03 FILLER PIC x(26) VALUE SPACES.
           03 FILLER PIC X(38) VALUE 
               "Listado de alquieres aprobados".
           03 FILLER PIC x(26) VALUE SPACES.   
           
       01 ENCABEZADO3.
           03  FILLER      PIC X(9)    VALUE "Fecha: ".
           03  E3-FECHA-DD    PIC 9(2).
           03  FILLER      PIC X    VALUE "/".
           03  E3-FECHA-MM    PIC 9(2).
           03  FILLER      PIC X    VALUE "/".
           03  E3-FECHA-AAAA  PIC 9(4).
           03  FILLER      PIC X(59)   VALUE SPACES.
           03  FILLER      PIC X(8)    VALUE "Chofer: ".
           03  E3-CHOFER    PIC x(7).
           03  FILLER      PIC X(8)    VALUE " Turno: ".
           03  E3-FECHA-MM    PIC x.
           03  FILLER      PIC X(59)   VALUE SPACES.
           
       01 ENCABEZADO4.
           03 FILLER PIC X(14) VALUE
           "       Cliente".
           03 FILLER PIC X(17) VALUE 
           "       Tipo Doc".
           03 FILLER PIC X(21) VALUE 
           "       Nro. Documento".
           03 FILLER PIC X(17) VALUE 
           "       Direccion".  
           03 FILLER PIC X(13) VALUE SPACES.                                                                                                                                                                                                                                          ".
       
       01 ENCABEZADO5.
           03 FILLER PIC X(80) VALUE ALL "-".
           
       01 ENCABEZADO6.
           03  FILLER PIC X(6) VALUE SPACES.
           03  E6-CLIENTE    PIC X(8).
           03  FILLER PIC X(10) VALUE SPACES.
           03  E6-TIPODOC    PIC X.
           03  FILLER PIC X(8) VALUE SPACES.
           03  E6-NRODOC    PIC X(20).
           03  FILLER PIC X(6) VALUE SPACES.
           03  E6-DIRECCION   PIC X(30).
           
       01 ENCABEZADO7.
           03  FILLER PIC X(23) VALUE 
           "Totales por Chofer: ".
           03  E7-TOT-CHOFER PIC 9999.
           
       01 ENCABEZADO8.
           03  FILLER PIC X(24) VALUE 
           " Totales por Fecha: ".
           03  E8-TOT-CHOFER PIC 9999.
           
       01 ENCABEZADO9.
           03  FILLER PIC X(26) VALUE 
           "Totales general: ".
           03  E9-TOT-GRAL PIC 999999.
       
       77 fs-rechazos          pic xx.
       77 fs-temporal          pic xx.
       77 chof-estado-activo pic xx value 'si'.
       77 chof-estado-inactivo pic xx value 'no'.
       77 tot-gral pic 999999  value 0.
       77 tot-fechas pic 9999 value 0.
       77 tot-chof pic 9999 value 0.
       77 nro-hoja pic  9(3) value 0.
       77 nro-linea pic  99 value 0.
       77 lineas-por-hoja pic 99 value 60.
       77 estado-alquiler pic x.
       77 op pic x.

       PROCEDURE DIVISION.
           perform inicializar.
           perform abrir-clientes.
           perform abrir-choferes.
      *    perform sort-section.
           perform cerrar-choferes.
           perform cerrar-clientes.
           stop run.
           
       inicializar.
       
       abrir-clientes. 
           move "A" to op.
           call "BuscarDatosCliente" using op, alq-nro-doc, 
           cli-codigo-estado,                                           
              cli-numero, cli-direccion .

       abrir-choferes.
           open input choferes.
           if is not ok-chof
               display "Error al abrir archivo choferes fs: "
                 fs-choferes
               stop run
               
           end-if.
           
       cerrar-choferes.
           close choferes.
           
       cerrar-clientes.
           move "C" to op.
           call "BuscarDatosCliente" using op , -1 .

       
       sort-section.
           sort temporal ascending key temp-fecha, temp-cho-nro-legajo,
               temp-cli-numero
               input procedure entrada
               output procedure salida.
           
       *> ENTRADA SECTION
       entrada.
           perform inicializar-entrada.
           perform abrir-alquileres.
           perform abrir-rechazados.
           perform leer-alquileres.
           perform procesar-alquileres until  eof-alq. 
           perform cerrar-alquileres. 
           perform cerrar-rechazados.
       
       inicializar-entrada.
          
       abrir-alquileres.
           open input alquileresmae.
           if is not ok-alq
               display "Error al abrir archivo alquileres fs: "
                 fs-alquileresmae
               stop run
           end-if.
           
       abrir-rechazados.
           open input rechazos
           if is not ok-rech
               display "Error al abrir archivo rechazados fs: "
                 fs-rechazados
               stop run
           end-if.
           
       leer-alquileres.
           read alquileresmae record at end move high-value to 
           alq-chofer.
           if fs-alquileresmae is not equal to 00 and 10
               display "Error al leer alquileres fs:" fs-alquileresmae
           end-if.
           move alq-chofer to cho-nro-legajo.
           move alq-fecha  to cho-fecha-desde.
           
           
       procesar-alquileres.
           perform inicializar-procesar-alquileres.
           if alq-estado is equal to "P"
               perform posicionar-choferes
           end-if.
           perform leer-alquileres.
           
       inicializar-procesar-alquileres.   
       
       posicionar-choferes.
           move chof-estado-inactivo to chof-estado.
           start choferes key is less than cho-clave.
           if ok-chof
               perform leer-choferes
               perform procesar-choferes until eof-chof or 
                   cho-fecha-desde > alq-clave or chof-estado is
                   equal to chof-estado-activo
           else if no-chof
                   display "no record aply"
           else
               display "choferes fs: " fs-choferes
           end-if.
           if chof-estado is equal to chof-estado-inactivo
                perform rechazar-alquiler
           end-if.
       
       leer-choferes.
           read choferes next record at end move high-value to 
           cho-clave.
           if fs-choferes is not equal to 00 and 10
               display "Error al leer choferes fs:" fs-choferes
           end-if.
           
          
           
       inicializar-proceso-choferes.
       procesar-choferes.
           if cho-fecha-hasta > alq-fecha
               move "T" to alq-estado
           end-if.
           perform actualizar-alquileres.
           move "P" to op.
           call "BuscarDatosCliente" using op, alq-nro-doc,
           cli-codigo-estado, cli-numero, cli-direccion .               
           perform escribir-arch-temporal.
           
       actualizar-alquileres.  
           rewrite rec-alquileresmae.
           
       escribir-arch-temporal.
           move alq-fecha to temp-fecha.
           move cho-nro-legajo to temp-cho-nro-legajo.
           move cho-turno to temp-cho-turno.
           move cli-numero to temp-cli-numero.
           move alq-tipo-doc to temp-cli-tipo-doc.
           move alq-nro-doc to temp-cli-nro-doc.
           move cli-direccion to temp-cli-direccion.
           release reg-temporal.
           
       rechazar-alquiler.
            move alq-clave to rech-clave.
            move alq-tipo-doc to rech-tipo-doc.
            move alq-nro-doc to rech-nro-doc.
            move alq-importe to rech-importe.
            write rec-rechazos.
           
       cerrar-alquileres.
           close alquileresmae.
           
       cerrar-rechazados.
           close rechazos.
       
       *> SALIDA SECTION
       salida.
           perform abrir-listado.
           perform leer-temporal.
           perform escribir-fecha-actual-y-hoja.
           perform procesar-fecha until  fs-temporal is equal to 10. 
           perform escribir-tot-gral.
           perform cerrar-listado.  
           
       abrir-listado.
           open output listado.
            if is not ok-list
               display "Error al abrir archivo listado fs: "
                 fs-listado
               stop run
           end-if.
           
       leer-temporal.
           return  temporal record at end move high-value to 
               temp-fecha.
       
       escribir-fecha-actual-y-hoja.
       
       procesar-fecha.
           perform inicializar-procesar-fecha.
           perform escribir-fecha-alquiler.
           perform procesar-chofer until  fs-temporal is equal to 10 or 
               fecha-actual is not equal to temp-fecha.
           perform escribir-tot-fecha.
           perform hoja-nueva.
       
       inicializar-procesar-fecha.
          move 0 to tot-fechas.
          move temp-fecha to fecha-actual.
          
       
       escribir-fecha-alquiler.
       
       procesar-chofer.
           perform inicializar-procesar-chofer.
           perform escribir-chofer-turno.
           perform escribir-encabezado-cliente.
           perform procesar-cliente until  fs-temporal is equal to 10 
           or fecha-actual is not equal to temp-fecha or chofer-actual 
           is                                                           
           not equal to temp-cho-nro-legajo.     
           perform escribir-tot-chof.
           perform chequear-hoja-nueva.
       
       inicializar-procesar-chofer.
           move 0 to tot-chof.
           move temp-cho-nro-legajo to chofer-actual.
       
       escribir-chofer-turno.
       
       escribir-encabezado-cliente.
       
       procesar-cliente.
           perform escribir-cliente.
           perform sumar-totales.
           perform chequear-hoja-nueva.
           perform leer-temporal.
           
       escribir-cliente.
       sumar-totales.
           add 1 to tot-chof.
           add 1 to tot-fechas.
           add 1 to tot-gral.
           
       chequear-hoja-nueva.  
       escribir-tot-chof.
       escribir-tot-fecha.
       hoja-nueva.
       escribir-tot-gral.
       cerrar-listado.
           close listado.
