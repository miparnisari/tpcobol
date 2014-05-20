       IDENTIFICATION DIVISION.
       program-id. "TP_Parte1_B".
       author. "Maria Ines Parnisari - Ignacio Mazzara".
       date-written. "1er cuatrimestre 2014".
       
       ENVIRONMENT DIVISION.
       configuration section.
       input-output section.
       file-control.
       
       select alquileresmae
           assign to disk "..\..\alquileresmae.dat"
           organization is line sequential
           file status is fs-alquileresmae.
           
       select autos
           assign to disk "..\..\autos.dat"
           organization is line sequential
           file status is fs-autos.

       DATA DIVISION.
       file section.
       
       fd alquileresmae.
       01 alquileresmae-rec.
           03 alq-patente      pic X(6).  
           03 alq-fech         pic 9(8).
           03 alq-tipo-doc     pic X.      *> D=DNI, C=CEDULA IDENTIDAD, R=LIBRETA, P=PASAPORTE, L=LICENCIA
           03 alq-nro-doc      pic X(20).
           03 alq-importe      pic 9(4)V99. *> 1234.99
           
           
       fd autos.
       01 autos-rec.
           03 aut-patente      pic x(6).
           03 aut-desc         pic x(30).
           03 aut-marca        pic x(20).
           03 aut-color        pic x(10).
           03 aut-tamano       pic x. *> C=Chico, M=Mediano, G=grande
           03 aut-importe      pic 9(4)v99.
           03 filler           pic x.
       
       working-storage section.
       
       77 fs-alquileresmae     pic xx.
       77 alquileresmae-eof    pic xx   value "NO".
           88 eofmae                    value "SI".
           
       77 fs-autos             pic xx.
       77 autos-eof            pic xx   value "NO".
           88  eofautos                 value "SI".
           
       01 fecha.
           03  fecha-aaaa       pic 9(4).
           03  fecha-mm        pic 9(2).
           03  fecha-dd        pic 9(2).
           
       01 ws-hoja                 pic 9(3)    value 001.
             
       01 encabezado1.
           03  filler      pic x(9)    value "Fecha: ".
           03  fecha-dd    pic 9(2).
           03  filler      pic x(1)    value "/".
           03  fecha-mm    pic 9(2).
           03  filer       pic x(1)    value "/".
           03  fecha-aaaa  pic 9(4).
           03  filler      pic x(20)   value spaces.
           03  filler      pic x(6)    value "Hoja: ".
           03  hoja        pic 9(3).
           
       01 encabezado2      pic x(80)   value "Listado Estadistico de Alquileres por Mes".
       01 encabezado3      pic x(80)   value all spaces.
       01 encabezado4      pic x(80)   value "Marca         Ene Feb Mar Abr May Jun Jul Ago Sep Oct Nov Dic        Total".
       01 encabezado5      pic x(80)   value all "-".

       01 matrizmarcaxmes.
           03  matriz-marca    occurs 300 times.
               05  matriz-totalmensual occurs  12 times.
                   07  elem    pic 9(3).
       
       01 vecmarcas.
           03  vecmarcas-elem  occurs 300 times
               ascending key is marca
               indexed by ind.
               05  marca       pic x(20).
               
       01 vectotalmensual.
           03  vectotalmensual-elem  occurs 12 times pic 9(4).
       
       
       01 vectotalmarca.
           03  vectotalmarca-elem  occurs 300 times pic 9(4).
           
       
       01 ws-sub pic 9(3).
       
       
       procedure division.
        
           perform abrir-archivos.
           perform cargar-marcas.
           perform imprimir-encabezado-estadisticas.
           perform calcular-estadisticas.
           perform imprimir-matriz-marca-mes.
           perform imprimir-totales-mensuales-y-general.
           perform cerrar-archivos.
           accept ws-sub.
           stop run.
           
       abrir-archivos.
           open input alquileresmae
                       autos.
       
       cargar-marcas.
           perform leer-autos.
           
           move 1 to ws-sub.
           perform cargar-vector-marcas until eofautos OR ws-sub > 300.
           
       
       leer-autos.
           read autos record
               at end move "SI" to autos-eof.
           display "MARCA:" aut-marca.
           
       
       cargar-vector-marcas.
           set ind to 1.
           search vecmarcas-elem
               at end 
                   move aut-marca to marca(ws-sub)
                   add 1 to ws-sub
               when aut-marca = marca(ind)
                   *> no hacer nada
           end-search.
           perform leer-autos.
           
       imprimir-encabezado-estadisticas.
           move function current-date to fecha.
           move corresponding fecha to encabezado1.
           move ws-hoja to encabezado1.
           display encabezado1.
           display encabezado2.
           display encabezado3.
           display encabezado4.
           display encabezado5.
       
       
       calcular-estadisticas.
       
       
       imprimir-matriz-marca-mes.
       
       
       imprimir-totales-mensuales-y-general.
       
       
       cerrar-archivos.
           close alquileresmae
                 autos.
