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
           
       77 fs-rechazos          pic xx.
       77 fs-listado           pic xx.
       77 fs-temporal          pic xx.
       77 chof-estado-activo pic xx value 'si'.
       77 chof-estado-inactivo pic xx value 'no'.
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
           move "a" to op.
           call "BuscarDatosCliente" using op, -1, cli-codigo-estado ,
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
           move "c" to op.
           call "BuscarDatosCliente" using op , -1 .

       
       sort-section.
           sort temporal ascending key temp-fecha
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
           perform posicionar-choferes.
           perform rechazar-alquiler.
           
       inicializar-procesar-alquileres.   
       
       posicionar-choferes.
           start choferes key is less than cho-clave.
            if ok-chof
               perform leer-choferes
               perform procesar-choferes until eof-chof or 
                   cho-fecha-desde > alq-clave or chof-estado is
                   equal to chof-estado-activo
           else if no-chof
                   display "no record aplica"
           else
               display "choferes fs: " fs-choferes
           end-if.
       
       leer-choferes.
           read choferes next record.
          
           
       inicializar-proceso-choferes.
       procesar-choferes.
           perform actualizar-alquileres.
           move "p" to op.
           call "BuscarDatosCliente" using op, alq-nro-doc .
           perform escribir-arch-temporal.
           
       actualizar-alquileres.    
       escribir-arch-temporal.
       rechazar-alquiler.
           
       cerrar-alquileres.
           close alquileresmae.
           
       cerrar-rechazados.
           close rechazos.
       
       *> SALIDA SECTION
       salida.
           perform abrir-listado.
           perform leer-temporal.
           perform procesar-fecha until  fs-temporal is equal to 10. 
           perform cerrar-listado.  
           perform cerrar-temporal.
           
       abrir-listado.
       leer-temporal.
           return  temporal record at end move high-value to 
               temp-fecha.
           
       procesar-fecha.
           perform procesar-chofer until  fs-temporal is equal to 10 or 
               fecha-actual is not equal to temp-fecha.
               
       procesar-chofer.
           perform procesar-cliente until  fs-temporal is equal to 10 
           or fecha-actual is not equal to temp-fecha or chofer-actual 
           is                                                           
           not equal to temp-cho-nro-legajo.                            
         
       procesar-cliente.
           
       cerrar-listado.
       cerrar-temporal.