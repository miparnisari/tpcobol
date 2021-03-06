       IDENTIFICATION DIVISION.
       program-id. "TP_Parte1_B".
       author. "Maria Ines Parnisari - Ignacio Mazzara".
       date-written. "1er cuatrimestre 2014".
       
       ENVIRONMENT DIVISION.
       configuration section.
       input-output section.
       file-control.
       
       select alquileresmae
           assign to disk "..\..\Files\alquileres.dat"
           organization is line sequential
           file status is fs-alquileresmae.
           
       select autos
           assign to disk "..\..\Files\autos.dat"
           organization is line sequential
           file status is fs-autos.
       
       select estadisticas
           assign to disk "..\..\Files\estadisticas.txt"
           organization is line sequential
           file status is fs-estadisticas.

       DATA DIVISION.
       file section.
       
       fd alquileresmae
           label record is standard.
       01 alquileresmae-rec.
           03 alq-patente      pic X(6).  
           03 alq-fech.
               05  fecha-dd    pic 9(2).
               05  fecha-mm    pic 9(2).
               05  fecha-aa    pic 9(4).
           03 filler           pic x(28).
           
           
       fd autos
           label record is standard.
       01 autos-rec.
           03 aut-patente      pic x(6).
           03 filler           pic x(30).
           03 aut-marca        pic x(20).
           03 filler           pic x(18).
           
       
       fd estadisticas
           label record is standard.
       01 estadisticas-rec.
           03 filler           pic x(80).
       
       working-storage section.
       
       01 fs-alquileresmae     pic xx.
           88 eofalquileres             value "10".
           
       01 fs-autos             pic xx.
           88  eofautos                 value "10".    
       
       01 fs-estadisticas      pic xx.
           
       01 fecha-de-hoy.
           03  fecha-aaaa      pic 9(4).
           03  fecha-mm        pic 9(2).
           03  fecha-dd        pic 9(2).
       
       01 cant-lineas-por-pag     pic 9(2)    value 10.
       
       01 ws-hoja                 pic 9(3)    value 001.
       01 ws-nro-linea            pic 9(2)    value 00.
       01 ws-indice-vecmarcas                  pic 9(3).
       01 ws-total-general        pic 9(5)    value 00000.
       01 ws-indice-marca         pic 9(3).
       01 ws-i                    pic 9(3).
       01 ws-maxautos             pic 9(3)     value 300.
       
       
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
           03  filler      pic x(1)   value "/".
           03  fecha-aaaa  pic 9(4).
           03  filler      pic x(50)   value spaces.
           03  filler      pic x(6)    value "Hoja: ".
           03  e1hoja      pic 9(3).
           
       01 encabezado2      pic x(80)   value "                Listado Estadistico de Alquileres por Mes                 ".
       01 encabezado3      pic x(80)   value all spaces.
       01 encabezado4      pic x(80)   value "Marca                   Ene Feb Mar Abr May Jun Jul Ago Sep Oct Nov Dic TOTAL".
       01 encabezado5      pic x(80)   value all "-".

       01 matrizmarcaxmes occurs 300 times.
            03  matrizmarcaxmes-col     occurs  12 times.
                05  matrizmarcaxmes-elem    pic 9(3) value 000.
       
       01 vecmarcas occurs 300 times
               ascending key is vec-marca
               indexed by ind.
               03  vec-marca       pic x(20).
               03  vec-patente     pic x(6).       *> se usa para luego saber la marca de un auto por su patente
               
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
           accept ws-indice-vecmarcas.
           stop run.
           
       abrir-archivos.
           open input alquileresmae.
           if (fs-alquileresmae <> 00)
               display "Error al abrir archivo de alquileres: " fs-alquileresmae
               accept ws-indice-vecmarcas
               stop run
           end-if.
           open input autos.
           if (fs-autos <> 00)
               close alquileresmae
               display "Error al abrir archivo de autos: " fs-autos
               accept ws-indice-vecmarcas
               stop run
           end-if.
           open output estadisticas.
       
       cargar-marcas.
           perform leer-autos.
           move 1 to ws-indice-vecmarcas.
           perform cargar-vector-marcas 
               until eofautos or ws-indice-vecmarcas > ws-maxautos.
           
       
       leer-autos.
           read autos record.
           
       leer-alquileresmae.
           read alquileresmae record.
             
       cargar-vector-marcas.
           set ind to 1.
           search vecmarcas
               at end 
                   move aut-marca to vec-marca(ws-indice-vecmarcas)
                   move aut-patente to vec-patente(ws-indice-vecmarcas)
                   add 1 to ws-indice-vecmarcas
               when aut-marca = vec-marca(ind)
                   *> display "marca repetida: " marca of vecmarcas(ind)
           end-search.
           perform leer-autos.
           
           
       imprimir-encabezado-estadisticas.
           move function current-date to fecha-de-hoy.
           move corresponding fecha-de-hoy to encabezado1.
           move ws-hoja to e1hoja.
           display encabezado1.
           move encabezado1 to estadisticas-rec.
           write estadisticas-rec.
           
           display encabezado2.
           move encabezado2 to estadisticas-rec.
           write estadisticas-rec.
           
           display encabezado3.
           move encabezado3 to estadisticas-rec.
           write estadisticas-rec.
           
           display encabezado4.
           move encabezado4 to estadisticas-rec.
           write estadisticas-rec.
           
           display encabezado5.
           move encabezado5 to estadisticas-rec.
           write estadisticas-rec.
           
           add 5 to ws-nro-linea.
       
       calcular-estadisticas.
           perform leer-alquileresmae.
           perform proceso until eofalquileres.
       
       proceso.
           set ind to 1.
           move corresponding alq-fech to fecha-de-hoy.
           search vecmarcas
               when alq-patente = vec-patente(ind)
                   set ws-indice-marca to ind
           end-search.
           
           add 1 to matrizmarcaxmes-elem(ws-indice-marca, fecha-mm of fecha-de-hoy).
           add 1 to vectotalmensual-elem(fecha-mm of fecha-de-hoy).
           add 1 to vectotalmarca-elem(ws-indice-marca).
           add 1 to ws-total-general.
           
           perform leer-alquileresmae.
       
       imprimir-matriz-marca-mes.
           move 1 to ws-i.
           perform imprimir-fila-marca 
               until ws-i > ws-maxautos or vecmarcas(ws-i) = "".
           
       imprimir-fila-marca.
           perform imprimir-col-mes 
               until ws-i > ws-maxautos or vecmarcas(ws-i) = "".
           
       imprimir-col-mes.
           move vecmarcas(ws-i) to (marca of detalle).
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
           move detalle to estadisticas-rec.
           write estadisticas-rec.
           add 1 to ws-i.
           
           perform chequeo-cambio-pagina.

       
       imprimir-totales-mensuales-y-general.
           display encabezado3.
           move encabezado3 to estadisticas-rec.
           write estadisticas-rec.
           
           perform chequeo-cambio-pagina.
           
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
           move detalle to estadisticas-rec.
           write estadisticas-rec.
       
       chequeo-cambio-pagina.
           add 1 to ws-nro-linea.
           
           if (ws-nro-linea >= cant-lineas-por-pag)
               move 0 to ws-nro-linea
               add 1 to ws-hoja
               perform imprimir-encabezado-estadisticas
           end-if.
       
       cerrar-archivos.
           close alquileresmae
                 autos
                 estadisticas.