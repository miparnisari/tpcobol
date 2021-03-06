       IDENTIFICATION DIVISION.
       program-id. BuscarDatosCliente.
       author. "Maria Ines Parnisari - Ignacio Mazzara".
       date-written. "1er cuatrimestre 2014".

       ENVIRONMENT DIVISION.
       configuration section.
       input-output section.
       file-control.
       
       select clientes
           assign to disk "..\..\..\Entrada\clientes.dat"
           organization is indexed
           access mode is random       *> se busca por numero de doc
           record key is cli-numero
           alternate record key is cli-nro-doc
           file status is fs-clientes.

       DATA DIVISION.
       file section.
       
       fd clientes
           label record is standard.
       01  reg-clientes.
           03  cli-numero      pic x(8).
           03  filler          pic x(28).
           03  cli-direccion   pic x(30).
           03  cli-nro-doc     pic x(20).
           
       working-storage section.
       01 fs-clientes          pic xx.
           88 ok-cli                   value "00".
           88 no-cli                   value "23".
           88 eof-cli                  value "10".
           
           
       linkage section.
       01  in-op                   pic x.      *> parametro
       01  in-cli-nro-doc          pic x(8).   *> parametro
       01  out-codigo-estado       pic x(2).   *> resultado
       01  out-cli-numero          pic x(8).   *> resultado
       01  out-cli-direccion       pic x(30).  *> resultado

       PROCEDURE DIVISION using in-op, in-cli-nro-doc, 
           out-codigo-estado, out-cli-numero, out-cli-direccion.
           
           if (in-op is equal to "A")
               perform abrir-cliente.
           if (in-op is equal to "C")
               perform cerrar-cliente.
           if (in-op is equal to "P")
               perform procesar-cliente.
               
           goback. *> retorna control a Principal
               
       abrir-cliente.
           open input clientes.
           if (is not ok-cli)
               display "Error al abrir archivo clientes fs: "
                 fs-clientes.
           
       cerrar-cliente.
           close clientes.
           
       procesar-cliente.
           move in-cli-nro-doc to cli-nro-doc.
           
           read clientes record
               key is cli-nro-doc.
               
           if (ok-cli)
               move cli-numero to out-cli-numero
               move cli-direccion to out-cli-direccion
           else if (no-cli)
               display "Cliente " cli-nro-doc " NO encontrado."
           else if (eof-cli)
               display "Fin de archivo de clientes."
               
           end-if.
           
           move fs-clientes to out-codigo-estado.
