       IDENTIFICATION DIVISION.
       program-id. ChoferEstaActivo.
       author. "Maria Ines Parnisari - Ignacio Mazzara".
       date-written. "1er cuatrimestre 2014".

       ENVIRONMENT DIVISION.
       configuration section.
       input-output section.
       file-control.
       
       select choferes
           assign to disk "..\..\Files\choferes.dat"
           organization is indexed
           access mode is sequential       *> se busca por numero de doc
           record key is reg-clave
           file status is fs-choferes.

       DATA DIVISION.
       file section.
       
       fd choferes
           label record is standard.
       01  reg-choferes.
           03  reg-clave.
               05  cho-nro-legajo      pic x(7).
               05  cho-fecha-desde     pic 9(8).
           03  cho-fecha-hasta         pic 9(8).
           03  cho-turno               pic x.
           
       working-storage section.
       01 fs-choferes          pic xx.
           88 ok-cho                   value "00".
           88 no-cho                   value "23".
           88 eof-cho                  value "10".
           
           
       linkage section.
       01  out-codigo-estado       pic x(2).   *> resultado
       01  in-cli-nro-doc          pic x(8).   *> parametro
       01  out-cli-numero          pic x(8).   *> resultado
       01  out-cli-direccion       pic x(30).  *> resultado

       PROCEDURE DIVISION using in-cli-nro-doc, out-codigo-estado,
           out-cli-numero, out-cli-direccion.
       
           move in-cli-nro-doc to cli-nro-doc.
           
           open input clientes.
       
           read clientes record
               key is cli-nro-doc.
               
           if (ok-cli)
               display "Cliente " cli-nro-doc " encontrado!"
               move cli-numero to out-cli-numero
               move cli-direccion to out-cli-direccion
           else if (no-cli)
               display "Cliente " cli-nro-doc " NO encontrado."
           else if (eof-cli)
               display "Fin de archivo de clientes."
               
           end-if.
           
           move fs-clientes to out-codigo-estado.
           
           close clientes.
           
           stop run.