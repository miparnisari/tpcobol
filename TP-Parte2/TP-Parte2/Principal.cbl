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
       
       01 fs-alquileresmae     pic xx.
           88 ok-alq                    value "00".
           88 no-alq                    value "23".
           88 eof-alq                   value "10".
           
       01 fs-choferes          pic xx.
           88 ok-chof                   value "00".
           88 no-chof                   value "23".
           88 eof-chof                  value "10".
           
       01 fecha-actual.
               05  fecha-actual-dd    pic     99.
               05  fecha-actual-mm    pic     99.
               05  fecha-actual-aaaa  pic     9999.
               
       77 fs-rechazos          pic xx.
       77 fs-listado           pic xx.
       77 fs-temporal          pic xx.
       77 chof-estado-activo pic xx value 'si'.
       77 chof-estado-inactivo pic xx value 'no'.
       77 chofer-actual pic     x(7).

       PROCEDURE DIVISION.
           perform abrir-clientes.
           perform abrir-choferes.
           perform abrir-temporal.
           perform sort-section.
           perform cerrar-choferes.
           perform cerrar-clientes.
           stop run.
           
       abrir-clientes. 
           call "BuscarDatosCliente" using alq-nro-doc, .

       abrir-choferes.
       abrir-temporal.
       cerrar-choferes.
       cerrar-clientes.
           call "BuscarDatosCliente" using alq-nro-doc, .

       
       sort-section.
           sort temporal ascending key temp-fecha
               input procedure entrada
               output procedure salida.
           
       abrir-archivos.
           open input alquileresmae
                       choferes.
                       
                       
       cerrar-archivos.
           close alquileresmae
                   choferes.
                   
                   
       *> ENTRADA SECTION
       entrada.
           perform abrir-alquileres.
           perform abrir-rechazados.
           perform leer-alquileres.
           perform procesar-alquileres until  eof-alq. 
           perform cerrar-alquileres. 
           perform cerrar-rechazados.
       
       abrir-alquileres.
       abrir-rechazados.
       leer-alquileres.
       procesar-alquileres.
           perform leer-choferes.
           perform inicializar-proceso-choferes.
           perform procesar-choferes until eof-chof or cho-fecha-desde 
           > alq-clave or chof-estado is equal to chof-estado-activo.   
           perform rechazar-alquiler.
           
       leer-choferes.
       inicializar-proceso-choferes.
       procesar-choferes.
           perform actualizar-alquileres.
           call "BuscarDatosCliente" using alq-nro-doc, .
           perform escribir-arch-temporal.
           
       actualizar-alquileres.    
       escribir-arch-temporal.
       rechazar-alquiler.
       cerrar-alquileres.
       cerrar-rechazados.
       
       *> SALIDA SECTION
       salida.
           perform abrir-listado.
           perform leer-temporal.
           perform procesar-fecha until  fs-temporal is equal to 10. 
           perform cerrar-listado.  
           perform cerrar-temporal.
           
       abrir-listado.
       leer-temporal.
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