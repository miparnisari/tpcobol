       IDENTIFICATION DIVISION.
       program-id. Program1.
       author. "Maria Ines Parnisari - Ignacio Mazzara".
       date-written. "1er cuatrimestre 2014".

       ENVIRONMENT DIVISION.
       configuration section.
       input-output section.
       file-control.
       
       select in-clientes
           assign to disk "..\..\ArchivosTextoPlano\clientes.txt"
           organization is sequential
           file status is fs-in-clientes.
       
       select out-clientes
           assign to disk "..\..\ArchivosIndexados\clientes.dat"
           organization is indexed
           access mode is random       *> se busca por numero de doc
           record key is cli-numero of reg-out-clientes
           alternate record key is cli-documento of reg-out-clientes
           file status is fs-out-clientes.

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
       
       fd out-clientes
           label record is standard.
       01 reg-out-clientes.
           03  cli-numero      pic x(8).
           03  cli-fec-alta    pic 9(8).
           03  cli-telefono    pic x(20).
           03  cli-direccion   pic x(30).
           03  cli-documento   pic x(20).
           
       working-storage section.
       01 fs-in-clientes          pic xx.
           88 ok-cli                   value "00".
           88 no-cli                   value "23".
           88 eof-cli                  value "10".
           
       01 fs-out-clientes          pic xx.
           88 ok-cli                   value "00".
           88 no-cli                   value "23".
           88 eof-cli                  value "10".
           

       PROCEDURE DIVISION.
           perform abrir-archivos.
           perform crear-clientes.
           perform crear-alquileres.
           perform crear-choferes.
           perform cerrar-archivos.
           stop run.
                    
       abrir-archivos.
           open input in-clientes.
           if is not ok-cli of fs-in-clientes
               display "Error al abrir archivo clientes fs: "
                 fs-in-clientes
               stop run
           end-if.
           
       crear-alquileres.
       
       crear-choferes.
       
       crear-clientes.

       cerrar-archivos.
           close in-clientes.