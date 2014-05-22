       IDENTIFICATION DIVISION.
       program-id. "TP_Parte1_B".
       author. "Maria Ines Parnisari - Ignacio Mazzara".
       date-written. "1er cuatrimestre 2014".
       
       ENVIRONMENT DIVISION.
       configuration section.
       input-output section.
       file-control.
       
       select alquileresmae
           assign to disk "..\..\Archivos\alquileres.dat"
           organization is line sequential
           file status is fs-alquileresmae.
           
       select autos
           assign to disk "..\..\Archivos\autos.dat"
           organization is line sequential
           file status is fs-autos.

       DATA DIVISION.
       file section.
       
       fd alquileresmae.
       01 alquileresmae-rec.
           03 alq-patente      pic X(6).  
           03 alq-fech.
               05  fecha-dd    pic 9(2).
               05  fecha-mm    pic 9(2).
               05  fecha-aa    pic 9(4).
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
       
       working-storage section.
       
       77 fs-alquileresmae     pic xx.
       77 alquileresmae-eof    pic xx   value "NO".
           88 eofalquileres             value "SI".
           
       77 fs-autos             pic xx.
       77 autos-eof            pic xx   value "NO".
           88  eofautos                 value "SI".
           
       01 fecha.
           03  fecha-aaaa      pic 9(4).
           03  fecha-mm        pic 9(2).
           03  fecha-dd        pic 9(2).
           
       01 ws-hoja                 pic 9(3)    value 001.
       01 ws-sub                  pic 9(3).
       01 ws-total-general        pic 9(5)    value 00000.
       01 ws-indice-marca         pic 9(3).
       01 ws-i                    pic 9(3).
       
       
       01 detalle.
           03 marca            pic x(20).
           03 filler           pic x(4) value spaces.
           03 det-ene          pic 9(3).
           03 filler           pic x(1) value spaces.
           03 det-feb          pic 9(3).
           03 filler           pic x(1) value spaces.
           03 det-mar          pic 9(3).
           03 filler           pic x(1) value spaces.
           03 det-abr          pic 9(3).
           03 filler           pic x(1) value spaces.
           03 det-may          pic 9(3).
           03 filler           pic x(1) value spaces.
           03 det-jun          pic 9(3).
           03 filler           pic x(1) value spaces.
           03 det-jul          pic 9(3).
           03 filler           pic x(1) value spaces.
           03 det-ago          pic 9(3).
           03 filler           pic x(1) value spaces.
           03 det-sep          pic 9(3).
           03 filler           pic x(1) value spaces.
           03 det-oct          pic 9(3).
           03 filler           pic x(1) value spaces.
           03 det-nov          pic 9(3).
           03 filler           pic x(1) value spaces.
           03 det-dec          pic 9(3).
           03 filler           pic x(1) value spaces.
           03 det-total        pic 9(4).
       
       01 encabezado1.
           03  filler      pic x(9)    value "Fecha: ".
           03  fecha-dd    pic 9(2).
           03  filler      pic x(1)    value "/".
           03  fecha-mm    pic 9(2).
           03  filer       pic x(1)    value "/".
           03  fecha-aaaa  pic 9(4).
           03  filler      pic x(50)   value spaces.
           03  filler      pic x(6)    value "Hoja: ".
           03  e1hoja        pic 9(3).
           
       01 encabezado2      pic x(80)   value "                Listado Estadistico de Alquileres por Mes                 ".
       01 encabezado3      pic x(80)   value all spaces.
       01 encabezado4      pic x(80)   value "Marca                   Ene Feb Mar Abr May Jun Jul Ago Sep Oct Nov Dic TOTAL".
       01 encabezado5      pic x(80)   value all "-".

       01 matrizmarcaxmes.
           03  matrizmarcaxmes-fila    occurs 300 times.
               05  matrizmarcaxmes-col     occurs  12 times.
                   07  matrizmarcaxmes-elem    pic 9(3) value 000.
       
       01 vecmarcas.
           03  vecmarcas-elem  occurs 300 times
               ascending key is marca
               indexed by ind.
               05  marca       pic x(20).
               05  patente     pic x(6).
               
       01 vectotalmensual.
           03  vectotalmensual-elem    occurs 12 times pic 9(4).
       
       
       01 vectotalmarca.
           03  vectotalmarca-elem      occurs 300 times pic 9(4).
           
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
           
       leer-alquileresmae.
           read alquileresmae record
               at end move "SI" to alquileresmae-eof.
               
       cargar-vector-marcas.
           set ind to 1.
           search vecmarcas-elem
               at end 
                   move aut-marca to marca of vecmarcas(ws-sub)
                   move aut-patente to patente of vecmarcas(ws-sub)
                   add 1 to ws-sub
               when aut-marca = marca of vecmarcas(ind) and aut-patente = patente(ind)
                   *> no hacer nada
           end-search.
           perform leer-autos.
           
           
       imprimir-encabezado-estadisticas.
           move function current-date to fecha.
           move corresponding fecha to encabezado1.
           move ws-hoja to e1hoja.
           display encabezado1.
           display encabezado2.
           display encabezado3.
           display encabezado4.
           display encabezado5.
       
       
       calcular-estadisticas.
           perform leer-alquileresmae.
           perform proceso until eofalquileres.
           
       
       proceso.
           set ind to 1.
           move corresponding alq-fech to fecha.
           search vecmarcas-elem
               when alq-patente = patente of vecmarcas(ind)
                   set ws-indice-marca to ind
           end-search.
           
           add 1 to matrizmarcaxmes-elem(ws-indice-marca, fecha-mm of fecha).
           add 1 to vectotalmensual-elem(fecha-mm of fecha).
           add 1 to vectotalmarca-elem(ws-indice-marca).
           add 1 to ws-total-general.
           
           perform leer-alquileresmae.
       
       imprimir-matriz-marca-mes.
           move 1 to ws-i.
           perform imprimir-fila-marca until ws-i > 300 or vecmarcas-elem(ws-i) = "".
           
       imprimir-fila-marca.
           perform imprimir-col-mes until ws-i > 300 or vecmarcas-elem(ws-i) = "".
           
       imprimir-col-mes.
           move vecmarcas-elem(ws-i) to (marca of detalle).
           move matrizmarcaxmes-col(ws-i, 1) to det-ene.
           move matrizmarcaxmes-col(ws-i, 2) to det-feb.
           move matrizmarcaxmes-col(ws-i, 3) to det-mar.
           move matrizmarcaxmes-col(ws-i, 4) to det-abr.
           move matrizmarcaxmes-col(ws-i, 5) to det-may.
           move matrizmarcaxmes-col(ws-i, 6) to det-jun.
           move matrizmarcaxmes-col(ws-i, 7) to det-jul.
           move matrizmarcaxmes-col(ws-i, 8) to det-ago.
           move matrizmarcaxmes-col(ws-i, 9) to det-sep.
           move matrizmarcaxmes-col(ws-i, 10) to det-oct.
           move matrizmarcaxmes-col(ws-i, 11) to det-nov.
           move matrizmarcaxmes-col(ws-i, 12) to det-dec.
           move vectotalmarca-elem(ws-i) to det-total.
           display detalle.
           add 1 to ws-i.
       
       imprimir-totales-mensuales-y-general.
           display encabezado3.
           move "Totales" to (marca of detalle).
           move vectotalmensual-elem(1) to det-ene.
           move vectotalmensual-elem(2) to det-feb.
           move vectotalmensual-elem(3) to det-mar.
           move vectotalmensual-elem(4) to det-abr.
           move vectotalmensual-elem(5) to det-may.
           move vectotalmensual-elem(6) to det-jun.
           move vectotalmensual-elem(7) to det-jul.
           move vectotalmensual-elem(8) to det-ago.
           move vectotalmensual-elem(9) to det-sep.
           move vectotalmensual-elem(10) to det-oct.
           move vectotalmensual-elem(11) to det-nov.
           move vectotalmensual-elem(12) to det-dec.
           move ws-total-general to det-total.
           display detalle.
       
       cerrar-archivos.
           close alquileresmae
                 autos.
