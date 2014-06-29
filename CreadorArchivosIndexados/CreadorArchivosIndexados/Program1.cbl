       IDENTIFICATION DIVISION.
       program-id. Program1.
       author. "Maria Ines Parnisari - Ignacio Mazzara".
       date-written. "1er cuatrimestre 2014".

       ENVIRONMENT DIVISION.
       configuration section.
       input-output section.
       file-control.
       
       select in-clientes
           assign to disk "..\..\..\ArchivosTextoPlano\clientes.txt"
           organization is line sequential
           file status is fs-in-clientes.
           
       select out-clientes-seq
           assign to disk "..\..\..\ArchivosIndexados\clientes.dat"
           organization is indexed
           access mode is sequential
           record key is cli-numero of reg-out-clientes-seq
           alternate record key is cli-documento of reg-out-clientes-seq
           file status is fs-out-clientes.
       
       select out-clientes-rnd
           assign to disk "..\..\..\ArchivosIndexados\clientes.dat"
           organization is indexed
           access mode is random
           record key is cli-numero of reg-out-clientes-rnd
           alternate record key is cli-documento of reg-out-clientes-rnd
           file status is fs-out-clientes.
       
       select in-choferes
           assign to disk "..\..\..\ArchivosTextoPlano\choferes.txt"
           organization is line sequential
           file status is fs-in-choferes.
           
       select out-choferes-seq
           assign to disk "..\..\..\ArchivosIndexados\choferes.dat"
           organization is indexed
           access mode is sequential
           record key is clave of reg-out-choferes-seq
           file status is fs-out-choferes.
       
       select out-choferes-rnd
           assign to disk "..\..\..\ArchivosIndexados\choferes.dat"
           organization is indexed
           access mode is random
           record key is clave of reg-out-choferes-rnd
           file status is fs-out-choferes.
       
       select in-alquileres
           assign to disk "..\..\..\ArchivosTextoPlano\alquileres.txt"
           organization is line sequential
           file status is fs-in-alquileres.
       
       select out-alquileres-seq
           assign to disk "..\..\..\ArchivosIndexados\alquileres.dat"
           organization is indexed
           access mode is sequential
           record key is clave of reg-out-alquileres-seq
           file status is fs-out-alquileres.
       
       select out-alquileres-rnd
           assign to disk "..\..\..\ArchivosIndexados\alquileres.dat"
           organization is indexed
           access mode is random
           record key is clave of reg-out-alquileres-rnd
           file status is fs-out-alquileres.

       DATA DIVISION.
       file section.
       
       fd in-clientes
           label record is standard.
       01 reg-in-clientes.
           03  cli-numero      pic x(8).
           03  cli-fec-alta    pic 9(8).
           03  cli-telefono    pic x(20).
           03  cli-direccion   pic x(30).
           03  cli-documento   pic x(20).
       
       fd out-clientes-rnd
           label record is standard.
       01 reg-out-clientes-rnd.
           03  cli-numero      pic x(8).
           03  cli-fec-alta    pic 9(8).
           03  cli-telefono    pic x(20).
           03  cli-direccion   pic x(30).
           03  cli-documento   pic x(20).
       
       fd out-clientes-seq
           label record is standard.
       01 reg-out-clientes-seq.
           03  cli-numero      pic x(8).
           03  cli-fec-alta    pic 9(8).
           03  cli-telefono    pic x(20).
           03  cli-direccion   pic x(30).
           03  cli-documento   pic x(20).
       
       fd in-choferes
           label record is standard.
       01 reg-in-choferes.
           03  cho-nro-legajo  pic x(7).
           03  cho-fecha-desde pic 9(8).
           03  cho-fecha-hasta pic 9(8).
           03  cho-turno       pic x.
       
       fd out-choferes-rnd
           label record is standard.
       01 reg-out-choferes-rnd.
           03 clave.
               05  cho-nro-legajo  pic x(7).
               05  cho-fecha-desde pic 9(8).
           03  cho-fecha-hasta     pic 9(8).
           03  cho-turno           pic x.
           
       fd out-choferes-seq
           label record is standard.
       01 reg-out-choferes-seq.
           03 clave.
               05  cho-nro-legajo  pic x(7).
               05  cho-fecha-desde pic 9(8).
           03  cho-fecha-hasta     pic 9(8).
           03  cho-turno           pic x.
           
       fd in-alquileres
           label record is standard.
       01 reg-in-alquileres.
           03  alq-patente         pic x(6).
           03  alq-fecha           pic 9(8).
           03  alq-tipo-doc        pic x.
           03  alq-nro-doc         pic x(20).
           03  alq-importe         pic 9(4)v99.
           03  alq-chofer          pic x(7).
           03  alq-estado          pic x.
       
       fd out-alquileres-rnd
           label record is standard.
       01 reg-out-alquileres-rnd.
           03  clave.
               05  alq-patente         pic x(6).
               05  alq-fecha           pic 9(8).
           03  alq-tipo-doc        pic x.
           03  alq-nro-doc         pic x(20).
           03  alq-importe         pic 9(4)v99.
           03  alq-chofer          pic x(7).
           03  alq-estado          pic x.
           
       fd out-alquileres-seq
           label record is standard.
       01 reg-out-alquileres-seq.
           03  clave.
               05  alq-patente         pic x(6).
               05  alq-fecha           pic 9(8).
           03  alq-tipo-doc        pic x.
           03  alq-nro-doc         pic x(20).
           03  alq-importe         pic 9(4)v99.
           03  alq-chofer          pic x(7).
           03  alq-estado          pic x.
           
       working-storage section.
       01 fs-in-clientes pic xx.
       01 fs-out-clientes pic xx.
       
       01 fs-in-choferes pic xx.
       01 fs-out-choferes pic xx.
       
       01 fs-in-alquileres pic xx.
       01 fs-out-alquileres pic xx.
       
       01 ws-exit                  pic x.
       01 cant-clientes            pic 9(10) value zeroes.
       01 cant-choferes            pic 9(10) value zeroes.
       01 cant-alquileres          pic 9(10) value zeroes.
           

       PROCEDURE DIVISION.
           perform abrir-archivos.
           perform crear-clientes.
           perform imprimir-clientes.
           perform crear-alquileres.
           perform imprimir-alquileres.
           perform crear-choferes.
           perform imprimir-choferes.
           perform cerrar-archivos.
           accept ws-exit.
           stop run.
                    
       abrir-archivos.
           open input in-clientes.
           open input in-choferes.
           open input in-alquileres.
           
           open output out-clientes-rnd.
           open output out-choferes-rnd.
           open output out-alquileres-rnd.
           
       *> *
       *> ARCHIVO DE CLIENTES
       *>
       
       crear-clientes.
           read in-clientes.
           perform cargar-clientes until fs-in-clientes <> 00.
           close out-clientes-rnd.
           
       cargar-clientes.
           move corresponding reg-in-clientes to reg-out-clientes-rnd.
           write reg-out-clientes-rnd.
           if (fs-out-clientes <> 00)
               display "Error al escribir el archivo de clientes: "
               fs-out-clientes.
           read in-clientes.
       
       imprimir-clientes.
           open input out-clientes-seq.
           read out-clientes-seq.
           perform chequeo-clientes until fs-out-clientes <> 00.
           display "--------------------".
           display "TOTAL REGISTROS CLIENTES: " cant-clientes.
           close out-clientes-seq.
           
       chequeo-clientes.
           display "------CLIENTE-------".
           display "NUMERO: " cli-numero of reg-out-clientes-seq.
           display "FECHA DE ALTA: " cli-fec-alta of 
           reg-out-clientes-seq.                                        
           display "TEL: " cli-telefono of reg-out-clientes-seq.
           display "DIRECCION: " cli-direccion of reg-out-clientes-seq.
           display "DOCUMENTO: " cli-documento of reg-out-clientes-seq.
           add 1 to cant-clientes.
           read out-clientes-seq.
           
       *> *
       *> ARCHIVO DE ALQUILERES
       *>
       
       crear-alquileres.
           read in-alquileres.
           perform cargar-alquileres until fs-in-alquileres <> 00.
           close out-alquileres-rnd.
           
       cargar-alquileres.
           move corresponding reg-in-alquileres to 
           reg-out-alquileres-rnd.                                      
           move corresponding reg-in-alquileres to 
           clave of reg-out-alquileres-rnd.                                                                  
           write reg-out-alquileres-rnd.
           if (fs-out-alquileres <> 00)
               display "Error al escribir el archivo de alquileres: "
               fs-out-alquileres.
           read in-alquileres.
       
       imprimir-alquileres.
           open input out-alquileres-seq.
           read out-alquileres-seq.
           perform chequeo-alquileres until fs-out-alquileres <> 00.
           display "--------------------".
           display "TOTAL REGISTROS ALQUILERES: " cant-alquileres.
           close out-alquileres-seq.
           
       chequeo-alquileres.
           display "------ALQUILER-------".
           display "PATENTE: " alq-patente of reg-out-alquileres-seq.
           display "FECHA: " alq-fecha of reg-out-alquileres-seq.
           display "TIPO DOC: " alq-tipo-doc of reg-out-alquileres-seq.
           display "DOCUMENTO: " alq-nro-doc of reg-out-alquileres-seq.
           display "IMPORTE: " alq-importe of reg-out-alquileres-seq.
           display "LEGAJO CHOFER: " alq-chofer of 
           reg-out-alquileres-seq.                                      
           display "ESTADO: " alq-estado of reg-out-alquileres-seq.
           add 1 to cant-alquileres.
           read out-alquileres-seq.
      
       *> *
       *> ARCHIVO DE CHOFERES
       *>
       
       crear-choferes.
           read in-choferes.
           perform cargar-choferes until fs-in-choferes <> 00.
           close out-choferes-rnd.
           
       cargar-choferes.
           move corresponding reg-in-choferes to reg-out-choferes-rnd.
           move corresponding reg-in-choferes to clave of 
           reg-out-choferes-rnd.
           write reg-out-choferes-rnd.
           if (fs-out-choferes <> 00)
               display "Error al escribir el archivo de choferes: "
               fs-out-choferes.
           read in-choferes.
       
       imprimir-choferes.
           open input out-choferes-seq.
           read out-choferes-seq.
           perform chequeo-choferes until fs-out-choferes <> 00.
           display "--------------------".
           display "TOTAL REGISTROS CHOFERES: " cant-choferes.
           close out-choferes-seq.
           
       chequeo-choferes.
           display "------CHOFER-------".
           display "LEGAJO CHOFER: " cho-nro-legajo of 
           reg-out-choferes-seq.                                        
           display "FECHA DESDE: " cho-fecha-desde of 
           reg-out-choferes-seq.                                        
           display "FECHA HASTA: " cho-fecha-hasta of 
           reg-out-choferes-seq.                                        
           display "TURNO: " cho-turno of reg-out-choferes-seq.
           add 1 to cant-choferes.
           read out-choferes-seq. 
       
       cerrar-archivos.
           close in-clientes.
           close in-choferes.
           close in-alquileres.