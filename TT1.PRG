*****************************************************************************
* Modulo : Transacciones Administrativas                                    *
* Autor  : Richard Jim‚nez Velasco                                          *
* Fecha  : Septiembre 20 de 1994                                            *
* Version : 1.00                                                            *
*                                                                           *
*****************************************************************************

#include "inkey.ch"
#include "dbedit.ch"
#include "setcurs.ch"
#include "common.ch"

SET DATE FRENCH

FUNCTION man_transa
      IF cerr_abi="A"
        area12:=SELECT()
        pan0=OPEN_WINDOW(0,0,23,79,1,'Transacciones Administrativas ',"B/W+",5,.T.)
        MESSBAR_ON(' <F2> Hrs. Trabajadas  <F3> Permiso  <F4> Suplencias  <F5> Vacaci¢n  <F6> Bajas  <F7> Mas..','N/W','R/W')

        IF mes!="@"
           datos_sal()
           @5,12 SAY SPACE(12) COLOR "B/W"
           @5,12 SAY mes COLOR "B/W"
           bus_cont(7,5,20,65)
        ENDIF
        CLOSE_WINDOW(pan0)
        SELECT(area12)
       ELSE
         MENSAJE("Ya no es Posible Realizar Transacciones en este mes,todas las operaciones fueron CERRADAS",12,17,'W/R+',.T.,.F.)
       ENDIF

RETURN ''

*  -------------------------------------------------------
* Funci¢n que hace la busqueda por contexto con 2 indices
* ---------------------------------------------------------

FUNCTION Bus_Cont(fila1,columna1,fila2,columna2)

DECLARE vec_campos := {"nitem","nombre"}
DECLARE vec_pict   := {"@S10", "@S40"}
DECLARE vec_nombre := {"No. Item","Nombre del empleado"}

PRIVATE  codigo_aux,a,formato
PRIVATE  nro,pantalla, pantalla1, antrec[2],antcol,hostkey[2],f1,f2,c1,c2,it1[3],IT2[3]

  SET SOFTSEEK ON
  antcol = 1
  f1  = fila1
  f2  = fila2
  c1  = columna1
  c2  = columna2
 
  a:=SELECT()
  SELECT 1
  USE dat_empl index nitem,nombre,codigo,eventual,ci

  codigo_aux = SPACE(5)
  formato := codigo_aux :='     '   && modificar en la configuracion

  SETCOLOR("W+/B+,B/W+")
  antrec[1]:= antrec [2] := 0
  hostkey[1]:= ''     && inicializo
  hostkey[2]:=SPACE(30)
  hostkey[2]:=''
  SET COLOR TO "N/N"
  @ f1+1,c2+1 CLEAR TO f2+1,c2+2

  @ f2+1,c1+2 CLEAR TO f2+1,c2+2

  SETCOLOR("W+/B+,B/W+")

  DBEDIT (f1,c1,f2,c2, vec_campos, "contex",vec_pict, vec_nombre)

  SELECT(a)

RETURN codigo_aux

//-------------------------------------------------------------------------

FUNCTION contex(modo, poscol)
LOCAL nkey,cadena,caracter,ncampo,rr

  nkey =LASTKEY()

   IF poscol <> antcol
      IF poscol=1
             @ f2 + 2,21  SAY SPACE(30) COLOR "R/W"
             hostkey[2]=""
      ELSE
             @ f2 + 2,7  SAY  SPACE(10)  COLOR "R/W"
             hostkey[1]=""
      ENDIF
      SET ORDER TO poscol
      IF hostkey[poscol] <> NIL
         SEEK hostkey[poscol]
         desplegar(codigo,nitem,tip_empl,nombre)
      ENDIF
      antcol = poscol
      RETURN 2
   ENDIF

   DO CASE
      CASE modo == DE_HITTOP
           TONE (1000,0)

      CASE modo == DE_HITBOTTOM
           TONE (1000,0)
                       
      CASE nkey == 27
           codigo_aux = " "
           RETURN DE_ABORT
      CASE nkey == 13

      CASE nkey = 8 //del

           IF poscol = 1
             hostkey[1]=left(hostkey[1],len(hostkey[1])-1)
             @ f2 + 2, c1 +2 SAY  SPACE(30)      COLOR "B/W"   && borro lo  anterior
             @ f2 + 2, c1 +2 SAY  hostkey[1]     COLOR "B/W"
             SEEK VAL(hostkey[1])
             desplegar(codigo,nitem,tip_empl,nombre)
           ELSE
              hostkey[poscol] = LEFT(hostkey[poscol],LEN(hostkey[poscol])-1)
              @ f2 + 2, c1 + 16 SAY LEFT(hostkey[2]+SPACE(25),25) COLOR "B/W"
              SEEK hostkey[poscol]             && sigo buscando
              desplegar(codigo,nitem,tip_empl,nombre)
          ENDIF
          antrec[poscol] = RECNO()
          RETURN 1

      // Busqueda

       CASE poscol=2 .AND. ( (nkey >= 65 .AND. nkey <= 90) )  .OR. nkey==32 .OR. (nkey>=97 .AND. nkey<=122) //nombres y apellidos

           hostkey[poscol]=hostkey[poscol] + UPPER(CHR(nkey))

           SEEK hostkey[poscol]

           desplegar(codigo,nitem,tip_empl,nombre)

              @ f2 + 2, c1 + 16 SAY hostkey[2] COLOR "B/W"
           antrec[poscol] = RECNO()
           RETURN 1

       CASE poscol=1 .AND. (nkey>=48 .AND. nkey<=57) .OR. nkey=45 .OR. (nkey>=65 .AND. nkey<=90) .OR. (nkey>=97 .AND. nkey<=122)

             hostkey[poscol] = hostkey[poscol]+CHR(nkey)

             SEEK hostkey[poscol]

             desplegar(codigo,nitem,tip_empl,nombre)

              @ f2 + 2, c1+2 SAY  hostkey[1]     COLOR "B/W"
           antrec[poscol] = RECNO()
           RETURN 1

      CASE RECNO() <> antrec[poscol]
           antrec[poscol] = RECNO()
            desplegar(codigo,nitem,tip_empl,nombre)
           DO CASE
              CASE poscol = 1
                     hostkey[poscol] = ''
              CASE poscol = 2
                   @ f2 + 2, c1 + 18 SAY SPACE(25) COLOR "B/W"
                   hostkey[poscol] = ''
           ENDCASE
           RETURN 1

      CASE nkey == -1             && F2  Horas trabajadas
         TONE (1000,0)
         marcar(nitem,nombre)
         IF es(nombre);registro(nitem,nombre,RECNO());ENDIF
         KEYBOARD(CHR(24))
      CASE nkey == -2             && F3
         TONE (1000,0)
         marcar(nitem,nombre)
         IF es(nombre);permiso(nitem,nombre);ENDIF  //permiso
         KEYBOARD(CHR(24))
      CASE nkey == -3             && F4 Suplencias
         TONE (1000,0)
         marcar(nitem,nombre)
         rr:=RECNO()
      IF es(nombre)

          TONE(1000,0)
          MESSBAR_ON("<ALT-P> Se¤ale el Empleado  a ser Suplido",'N/W','R/W')
      ENDIF
      CASE nkey == 281
       IF ALLTRIM(nombre)!="ACEFALIA"
          @ ROW(),9 SAY ALLTRIM(nitem) COLOR "R/B"
          @ ROW(),22 SAY ALLTRIM(nombre) COLOR "R/B"
          INKEY(1)
          MESSBAR_OFF()
          MESSBAR_ON("<ALT-S> Se¤ale el Empleado  Suplente",'N/W','R/W')
         it1[1]=ALLTRIM(nitem)
         it1[2]=ALLTRIM(nombre)
       ELSE
         MENSAJE("Error no es posible realizar la Suplencia" ,10,17,'W*/R+',.T.,.F.)
         MESSBAR_OFF()
       ENDIF

      CASE nkey==287
       IF ALLTRIM(nombre)!="ACEFALIA"
           @ ROW(),9 SAY ALLTRIM(nitem) COLOR "R/B"
           @ ROW(),22 SAY ALLTRIM(nombre) COLOR "R/B"
           INKEY(1)
           MESSBAR_OFF()
           TONE(1000,0)
           it2[1]=ALLTRIM(nitem)
           it2[2]=ALLTRIM(nombre)
           tran_suple(it1,it2)
           RETURN 1
         ELSE
            MENSAJE("Error no es posible realizar la suplencia ",10,17,'W*/R+',.T.,.F.)
            MESSBAR_OFF()
         ENDIF

      CASE nkey == -4             && F5
         TONE (1000,0)
         marcar(nitem,nombre)
         IF es(nombre);vacacion(nitem,nombre);ENDIF          //Vacaciones
      CASE nkey == -5             && F6
         TONE (1000,0)
         marcar(nitem,nombre)
         IF es(nombre);tran_baja(ALLTRIM(nitem),nombre,RECNO());ENDIF
     CASE nkey == -7
         TONE (1000,0)
         marcar(nitem,nombre)
         IF es(nombre);subsid(nitem,nombre,RECNO());ENDIF              //subsidio
     CASE nkey==-6
         MESSBAR_ON('<F8> Subsidios <F9> Comisi¢n <ALT-G> Congelar <ALT-L> Descongelar <ALT-V> Volver','N/W','R/W')
     CASE nkey == -8
         TONE(1000,0)
         marcar(nitem,nombre)
         IF nombre!="ACEFALIA"
           comision(nitem,nombre,RECNO())
         ENDIF
     CASE nkey == 303
         MESSBAR_OFF()
     CASE nkey == 290  // Congelar
       TONE(1000,0)
       marcar(nitem,nombre)
       conge(nitem,nombre)
      CASE nkey == 294 // Descongelar
       TONE(1000,0)
       marcar(nitem,nombre)
       desconge(nitem,nombre)
ENDCASE

RETURN 1

FUNCTION registro(ite,nom,posi)
LOCAL vecop:={"Revisar","Salir"}
PUBLIC ljort:=30,lhnorm:=0,lfalt:=0,elec,p11,lnoct:=0,lext:=0,lfalt_dia:=0
 area10:=SELECT()

 ldias_em = 0
 SELECT 32
 USE personal INDEX perso_it,perso_nom
 SEEK ALLTRIM(ite)
 IF FOUND()
  ljort    = dias_trab
  lfalt    = faltas
  lfalt_dia= falta_dia
  ldias_em = dias_em
      elec=MULTI_OPCION("Error se intenta registrar nuevamente al empleado el mismo MES",vecop,2,10,.T.)
      IF elec=1
        SELECT 16
        USE hrs_eme  INDEX extra_it
        SEEK ite
        IF FOUND()
          lext =hora_ext
          lnoct=hora_noc
        ELSE
          lext =0
          lnoct=0
        ENDIF
        hora_reg(ite,nom,posi)
      ENDIF
 ELSE
       hora_reg(ite,nom,posi)
 ENDIF
SELECT(area10)
RETURN

FUNCTION hora_reg(ite,nom,posi)
LOCAL pp1,lhor:=0

      qq:=SELECT()
      pp1=OPEN_WINDOW(5,10,22,73,1,'Registro Horas Trabajadas',"B/N+",5,.T.)
      SET CURSOR ON
      SET COLOR TO "W/N+"
      @ 6,26 SAY ALLTRIM(nom)
      @ 8,18 SAY"Dias  Trabajados :"
      @ 10,18 SAY "Horas Trabajadas :"
      @ 12,18 SAY "Atrasos (Minutos):"
      @ 14,18 SAY "Faltas  (D¡as)   :"
      @ 16,18 SAY "Horas Extras     :"
      @ 18,18 SAY "Horas Nocturnas  :"
      @ 20,18 SAY "Dias Serv.Emerg  :"

      SHOWGETS()
      @ 8,38 GET ljort PICTURE "99" VALID ljort>=0 COLOR "B/W"
//    READ
      IF LASTKEY()=27;CLOSE_WINDOW(pp1);SELECT(qq);RETURN;ENDIF
//      @ 8,38 GET ljort PICTURE "99" VALID ljort>=0 COLOR "B/W"
      @10,37 SAY ljort*7
//    @12,37 GET lhor  PICTURE "999" COLOR "B/W" VALID lhor>=0
      @12,40 GET lfalt  PICTURE "99999" COLOR "B/W" VALID lfalt>=0
      @14,40 GET lfalt_dia  PICTURE "99999.99" COLOR "B/W" VALID lfalt_dia>=0
      @16,38 GET lext PICTURE  "9999.99"   COLOR "B/W" VALID lext>=0
      @18,38 GET lnoct PICTURE "9999.99"   COLOR "B/W" VALID lnoct>=0

      @20,38 GET ldias_em PICTURE "99" VALID ldias_em>=0 COLOR "B/W"


      READ

      IF LASTKEY()!=27
         elec=MULTIPLE({"Grabar","Salir sin Grabar"},2,21,23,.F.)
      IF elec=1
       SELECT 32
       USE personal INDEX perso_no,perso_it
       set order to 2
       SEEK ALLTRIM(ite)

        IF !FOUND()
         APPEND BLANK
         REPLACE nitem      WITH ite
         REPLACE nombre     WITH nom
         REPLACE dias_trab  WITH ljort
         REPLACE faltas     WITH lfalt
         REPLACE falta_dia  WITH lfalt_dia
         REPLACE dias_em    WITH ldias_em
        ELSE
         REPLACE nitem      WITH ite
         REPLACE nombre     WITH nom
         REPLACE dias_trab  WITH ljort
         REPLACE faltas     WITH lfalt
         REPLACE falta_dia  WITH lfalt_dia
         REPLACE dias_em    WITH ldias_em
        ENDIF

         SELECT 16
          USE hrs_eme INDEX extra_it
           SEEK ALLTRIM(ite)
            IF !FOUND()
                     APPEND BLANK
            ENDIF
            REPLACE hora_ext WITH lext,;
                    nitem    WITH ite,;
                    hora_noc WITH lnoct

        p11=MENSAJE("Registrando Datos.... ",12,17,'W/R+',.T.,.F.)
        CLOSE_WINDOW(p11)
       ENDIF
      ENDIF
      CLOSE_WINDOW(pp1)
      SELECT(qq)
RETURN

//FUNCTION hora_ext(ite,nom,posi)
//LOCAL pp2,lext:=0
//         uu:=SELECT()
//         SELECT 16

//          SEEK ite

//          IF FOUND()

//            pp2=OPEN_WINDOW(6,49,10,77,1,'Registro de hrs. Extras',"B/N+",5,.T.)
//            @ 8 ,50 SAY "No. Horas Extras :" GET lext PICTURE "99.99"
//            READ
//           REPLACE hora_ext WITH lext
//            CLOSE_WINDOW(pp2)
//          ELSE
//             pp4 := MENSAJE('El empleado no esta registrado en la lista de horas extras ',12,17,'W*/R+',.T.,.F.)
//             CLOSE_WINDOW(pp4)
//          ENDIF

//         SELECT(uu)
//RETURN

FUNCTION marcar(tt,nn)
       @ ROW(),7 SAY  "->"   COLOR "R/W"
       @ ROW(),9 SAY ALLTRIM(tt) COLOR "R/W"
       @ ROW(),22 SAY ALLTRIM(nn) COLOR "R/W"
RETURN

FUNCTION permiso(ite,nom)
LOCAL lperm:=0,lmoti:=SPACE(30),pp3,elec,p13
         ww:=SELECT()
         pp3=OPEN_WINDOW(7,17,15,73,1,'Registro de Permisos',"B/N+",5,.T.)
         SET COLOR TO "W/N+"
         SET CURSOR ON
         @ 9,19  SAY "Nombre del Empleado: "+ALLTRIM(nom)
         @ 11,19 SAY "No. Dias permiso :" GET lperm PICTURE "999" COLOR "B/W"VALID lperm>0
         @ 13,19 SAY "Motivo:" GET lmoti PICTURE "@!" COLOR "B/W"
         READ
         IF LASTKEY()!=27
              elec=MULTIPLE({"Grabar","Salir sin Grabar"},2,21,23,.F.)
             IF elec=1
                SELECT 38
                 USE (dr+"vacacion") INDEX (dr+"vaca_item")
                 SEEK ite
                 IF FOUND()
                  IF duracion>0 .AND. duracion>=lperm
                    REPLACE duracion WITH (duracion - lperm)
                    REPLACE permisos WITH lperm

                    p13=MENSAJE("Registrando Datos ",12,17,'W/R+',.T.,.F.)
                    impri_per(nom,lperm,lmoti)
                    CLOSE_WINDOW(p13)
                  ELSE
                    p13=MENSAJE("Atenci¢n ya no es posible otorgar mas permisos al empleado puesto que excede al de sus vacaciones",12,17,'W/R+',.T.,.F.)
                    CLOSE_WINDOW(p13)
                  ENDIF

                 IF duracion=0
                    DELETE
                    PACK
                    p13=MENSAJE("El empleado ya no tiene vacaciones",12,17,'W/R+',.T.,.F.)
                    CLOSE_WINDOW(p13)
                 ENDIF
                ELSE

                  //empleado sin registro de vacaciones
               ENDIF

             ENDIF
         ENDIF
         CLOSE_WINDOW(pp3)
         SELECT(ww)
RETURN

STATIC  FUNCTION impri_per(nombre_empl,dias_per,moti_per)
LOCAL pp[20]
       area102:=SELECT()
       IF !STARTREPORT(80,'R',4,5)
          RETURN ' '
       ENDIF

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= '^,185,00,'+'PERMISO'
	 pp[4]= '^,000,00, ' +  "Cochabamba, "+ FECHALIT(date(),3,3)
	 pp[5]= '<,000,00'
	 pp[6]= '<,009,5,'+ ' Correspondiente al mes de:' +alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
         pp[7]= '<,009,5,'+' Nombre del Empleado :'+nombre_empl
         pp[8]= '<,009,5,'+' No. de Dias         :'+ STR(dias_per)
         pp[9]='<,009,5,'+' Motivo              :'+moti_per

	 EVERYHEADER (@pp,9)

	 pp[1]= la_empresa
	 pp[2]= el_Lugar
	 pp[3]= 'PERMISO'
	 pp[4]= 'Cochabamba, '+ FECHALIT(date(),3,3)
	 pp[5]= ''
	 pp[6]= ' Correspondiente al mes de:' +alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
         pp[7]= ' Nombre del Empleado :'+nombre_empl
         pp[8]= ' No. de Dias         :'+STR(dias_per)
         pp[9]=' Motivo              :'+moti_per

      	 HEADERSCREEN(@pp,9)
         SKIPLINE(5,.T.)
         jef_perso()
         FINISHREPORT()
         SELECT(area102)
RETURN

STATIC FUNCTION tran_suple(v1,v2)
LOCAL ldias:=0,lfech_sup:=DATE()
LOCAL pan,conf:="S"
LOCAL campo1:={},campo2:={},campo3:={}
PUBLIC lit:=SPACE(40)

      oo:=SELECT()
      IF v1[1]!=v2[1]
            pan=OPEN_WINDOW(6,16,19,75,1,'Transferencia',"B/N",5,.T.)
            SET COLOR TO "W/N"
            SET CURSOR ON
            @ 8,18 SAY "Confirmar suplencia entre :"
            @ 9,24 SAY v1[1] +"   " + v1[2]
            @ 10,37 SAY "Y"
            @ 11,24 SAY v2[1] +"   " + v2[2]
            @ 13,24 SAY " S/N ?" GET conf PICTURE "@!" VALID conf $ "SN"
            @ 15,24 SAY "No. Dias :" GET ldias PICTURE "999" COLOR "B/W"VALID ldias>0
            @ 17,24 SAY "Fecha    :" GET lfech_sup COLOR "B/W" VALID lfech_sup>CTOD("  /  /    ")

            READ
          IF LASTKEY()!=27 .AND. conf="S"
           SELECT 17
            USE suplencia INDEX suple_qu,suple_po
             APPEND BLANK
                 REPLACE  quien  WITH v1[1],;
                      porquien   WITH v2[1],;
                      dias_supl  WITH ldias,;
                      liq_pagabl WITH 0,;
                      fecha      WITH lfech_sup
                  MENSAJE("Registrando Datos...",12,17,'W/R+',.T.,.F.)

/*            SET ORDER TO 1
            SEEK  v1[1]

            IF !FOUND()
              SET ORDER TO 2
              SEEK v1[1]

               IF !FOUND()
                 APPEND BLANK
                 REPLACE  quien  WITH v1[1],;
                      porquien   WITH v2[1],;
                      dias_supl  WITH ldias,;
                      liq_pagabl WITH 0,;
                      fecha      WITH lfech_sup
                  MENSAJE("Registrando Datos...",12,17,'W/R+',.T.,.F.)
               ELSE */
//               MENSAJE("Error el Suplido no puede ser Suplente,ingrese nuevamente",12,17,'W*/R+',.T.,.F.)
//               ENDIF

//            ELSE
//               MENSAJE("Error item ya registrado",12,17,'W*/R+',.T.,.F.)
            ENDIF
//        ENDIF
      CLOSE_WINDOW(pan)
     ELSE
         MENSAJE("Error items duplicados!",12,17,'W*/R+',.T.,.F.)
     ENDIF
     SELECT(oo)

RETURN

FUNCTION val_supli(ll,ll1)
LOCAL g:=.T.,p7
     qq:=SELECT()
     SELECT 17                      // Suplencias
     USE suplencia INDEX suple_qu,suple_por
     SET ORDER TO 1
     SEEK ALLTRIM(ll)               // busqueda por item
     IF !FOUND()
          SELECT 1                  // falta para el literal
          USE dat_empl index nitem,nombre,codigo,eventual,ci
          SEEK ALLTRIM(ll)
          IF !FOUND()
              g=.F.
              p7=MENSAJE("Error el item NO EXISTE, o",12,17,'W*/R+',.T.,.F.)
          ELSE
             IF (nombre="ACEFALIA" .OR. nombre="COMISION")
               MENSAJE("Error el ITEM esta en "+nombre ,12,17,"W*/R+",.T.,.F.)
               g=.F.
             ELSE
               SHOWGET("lit",ll)
             ENDIF

          ENDIF
          IF ll=ll1
               p7=MENSAJE("Error items duplicados ",12,17,'W*/R+',.T.,.F.)
               CLOSE_WINDOW(p7)
               g=.F.
          ENDIF
  ELSE
     p7=MENSAJE("Error intente nuevamente",12,17,'W*/R+',.T.,.F.)
     CLOSE_WINDOW(p7)
     g=.F.
 ENDIF
 SELECT(qq)
RETURN g

//Falta seguimiento de vacaciones
FUNCTION vacacion(ii,nn)
LOCAL pp9,lfech:=date(),lgestion:=space(15),ldura:=0,lobserv:=SPACE(30),elec,pp10
LOCAL p11
LOCAL vecop:={"Revisar","Salir"}

         ff:=SELECT()
         SELECT 38
         USE (dr+"vacacion") INDEX (dr+"vaca_item")
         ? dr
         ? select()
         ? alias()
         inkey(0)

         SEEK ii
         IF FOUND()
           ii       = nitem
           lfech    = fecha
           ldura    = duracion
           lobserv  = observacio
           lgestion = gestion

            elec=MULTI_OPCION("Error se intenta registrar nuevamente al empleado el mismo MES",vecop,2,10,.T.)
            IF elec=2
               SELECT(ff)
               RETURN
            ENDIF
         ENDIF
           pp9=OPEN_WINDOW(7,10,20,70,1,'Registro de Vacaciones',"B/N+",5,.T.)
           SET COLOR TO "W/N+"
           SET CURSOR ON
           @ 10,18 SAY "Solicitante   :"+ALLTRIM(nn)
           @ 12,18 SAY "A partir del  : " GET lfech VALID lfech>CTOD("  /  /    ")
           @ 12,44 SAY "Gesti¢n :"GET lgestion PICTURE "@!S10" COLOR "B/W"VALID lgestion!=SPACE(15)
           @ 14,18 SAY "Duraci¢n (d¡as):"GET ldura PICTURE "999" COLOR "B/W"VALID ldura>0
           @ 16,18 SAY "Observacion :"GET lobserv PICTURE "@!"   COLOR "B/W"
           READ
           IF LASTKEY()!=27
              elec=MULTIPLE({"Grabar","Salir sin Grabar"},2,21,23,.F.)
            IF elec=1
                SELECT 38
                use (dr+"vacacion") index (dr+"vaca_ite")
                SEEK ii
                IF !FOUND()
                   APPEND BLANK
                ENDIF
                REPLACE nitem           WITH ii,;
                        fecha           WITH lfech,;
                        duracion        WITH ldura,;
                        observacio      WITH lobserv,;
                        gestion         WITH lgestion

                MENSAJE("Registrando Datos ",12,17,'W/R+',.T.,.F.)
           ENDIF
//        MENSAJE("Error Se intenta registrar nuevamente al mismo empleado que continua con vacaciones ",12,17,'W*/R+',.T.,.F.)
     ENDIF
     CLOSE_WINDOW(pp9)
     SELECT(ff)
RETURN

//REGISTRO DE BAJAS MEDICAS
FUNCTION tran_baja(ii,nn,rr)
LOCAL pp3,lmedico:=space(40),ldiagnos:=space(40),lfech:=DATE(),ldura:=0
LOCAL elec,p11, vecop:={"Revisar","Salir"},lsueld:=0

         vv:=SELECT()
          SELECT 23
          USE baja INDEX baja_ite
          SEEK ii
          IF FOUND()
              lmedico = ALLTRIM(nom_medico)+SPACE(15)
              ldiagnos= ALLTRIM(diagnost) +SPACE(10)
              ldura   = duracion
              lfech   = fecha
              lsueld  = sueldo
              elec=MULTI_OPCION("Error se intenta registrar nuevamente al empleado el mismo MES",vecop,2,10,.T.)
              IF elec=2
                SELECT(vv)
                RETURN
              ENDIF
         ENDIF
         pp3=OPEN_WINDOW(7,10,20,73,1,'Registro de Bajas M‚dicas',"B/N+",5,.T.)
         SET COLOR TO "W/N+"
         SET CURSOR ON
         @ 9,12 SAY "Asegurado   :"+ALLTRIM(nn)
         @ 11,12 SAY "M‚dico      :" GET lmedico PICTURE "@!" COLOR "B/W"VALID lmedico!=space(40)
         @ 13,12 SAY "Diagn¢stico :" GET ldiagnos PICTURE "@!"COLOR "B/W" VALID lmedico!=space(40)
         @ 15,12 SAY "Fecha   :" GET lfech COLOR "B/W"VALID lfech>CTOD("  /  /    ")
         @ 17,12 SAY "Duraci¢n:" GET ldura PICTURE "999" COLOR "B/W"VALID ldura>0
         @ 19,12 SAY "Sueldo Anterior:" GET lsueld PICTURE "999,999.99" COLOR "B/W" VALID lsueld>=0
         READ
         IF LASTKEY()!=27
            elec=MULTIPLE({"Grabar","Salir sin Grabar"},2,21,23,.F.)
          IF elec=1
            p11=MENSAJE("Registrando Datos ",12,17,'W/R+',.T.,.F.)
            CLOSE_WINDOW(p11)
            SELECT 23
            USE baja INDEX baja_ite
            SEEK ii
            IF !FOUND()
              APPEND BLANK
            ENDIF
              REPLACE nitem   WITH ii,;
                   nom_medico WITH lmedico,;
                   fecha      WITH lfech,;
                   diagnost   WITH ldiagnos,;
                   duracion   WITH ldura,;
                   sueldo     WITH lsueld
           ENDIF
         ENDIF
         CLOSE_WINDOW(pp3)
         SELECT(vv)
RETURN

FUNCTION ES(nombr)
LOCAL sn:=.T.
      IF nombr="ACEFALIA" .OR. nombr="COMISION"
       sn=.F.
       MENSAJE('Error No es posible realizar transacciones con este Item ',10,17,'W*/R+',.T.,.F.)
      ENDIF
RETURN sn

FUNCTION subsid(ite,nom,reg)

LOCAL op:={"Registrar","Eliminar"},opcion
PUBLIC lfech:=DATE(),lbo:="   "

     opcion=menu_dat(13,50,op,"N/W,W/N")
      DO CASE
         CASE opcion=1
           subR(ite,nom,reg)
         CASE opcion=2
          subE(ite,nom,reg)
      ENDCASE
RETURN

FUNCTION subE(ite,nom,reg)
LOCAL pp7,sl

         area6:=SELECT()
         pp7=OPEN_WINDOW(7,10,20,73,1,'Subsidios',"B/N+",5,.T.)
         SET COLOR TO "W/N"
         SET CURSOR ON
         SELECT 18
         USE nac_muer INDEX n_m_item
         SEEK ite
         IF FOUND()
           @ 9,12 SAY "Nombre del Empleado : " + ALLTRIM(nom)
           @10,12 SAY "Subsidios :"
           @11,12 SAY "Prenatalidad :"+SUBSTR(codigo,1,1)
           @12,12 SAY "Natalidad    :"+SUBSTR(codigo,2,1)
           @13,12 SAY "Lactancia    :"+SUBSTR(codigo,3,1)
           @14,12 SAY "Fecha de registro :"+DTOC(fech_ini)
           sl=MULTIPLE({"Borrar","Cancelar"},2,17,23,.F.)
           IF sl=1
              DELETE
              PACK
           ENDIF
         ELSE
           MENSAJE("Error el empleado no tiene asignado Subsidios",12,17,'W*/R+',.T.,.F.)
         ENDIF
         SELECT(area6)
         CLOSE_WINDOW(pp7)
RETURN

FUNCTION subR(ite,nom,reg)
LOCAL pp7,i
         area6:=SELECT()
         SELECT 18
         USE nac_muer INDEX n_m_item
         SEEK ite

         IF FOUND()
           FOR i=1 TO 3
             IF SUBSTR(codigo,i,1)!=" "
                lbo=lbo+"X"
             ELSE
                lbo=lbo+" "
             ENDIF
           NEXT
           SHOWGETS()
         ENDIF

         pp7=OPEN_WINDOW(7,10,15,73,1,'Subsidios',"B/N+",5,.T.)
         SET COLOR TO "W/N"
         SET CURSOR ON
         @ 9,12 SAY "Nombre del Empleado : " + ALLTRIM(nom)
         KEYBOARD(CHR(24))
         @ 11,12 SAY "Seleccione el Bono que le corresponde " GET lbo COLOR"B/W" VALID val_b(lbo)
         @ 13,12 SAY "Fecha origen :" GET lfech COLOR "B/W" VALID lfech>CTOD("  /  /    ")

         READ
      IF LASTKEY()!=27
         elec=MULTIPLE({"Grabar","Salir sin Grabar"},2,21,23,.F.)
           IF elec=1
             SELECT 18
             use nac_muer index n_m_item
             SEEK ite
             IF !FOUND() .AND. lbo!="   "

                APPEND BLANK
                REPLACE nitem WITH ite,;
                    codigo WITH lbo,;
                    fech_ini WITH lfech
              ELSE //

              ENDIF
           ENDIF
     ENDIF
    CLOSE_WINDOW(pp7)
    SELECT(area6)
RETURN

FUNCTION val_b(lbo)
LOCAL sub1:={"Prenatalidad","Natalidad","Lactancia"},sub2:="PNL"
LOCAL ele,cadenita:="",ret:=.T.
           ele=dialog2(12,55,sub1,lbo)
           FOR i=1 TO 3
               IF SUBSTR(ele,i,1)="X"
                  cadenita=cadenita+SUBSTR(sub2,i,1)
               ELSE
                 cadenita=cadenita+" "
               ENDIF
           NEXT

           IF cadenita=sub2 .OR. cadenita="P L" .OR. cadenita="PN "
            ret=.F.
            MENSAJE("Error en la asignaci¢n de bonos intente otra vez",12,17,'W*/R+',.T.,.F.)
           ELSE
             SHOWGET("lbo",cadenita)
           ENDIF
RETURN ret

STATIC FUNCTION conge(it,ni)
LOCAL vector:={"Bono Antiguedad","Indice Efectividad","Subsidio Irrad.","Horas Adic."},opcion
LOCAL pp5,lbo:=0,lefe:=0,lsub:=0,lhora:=0,elec
         area78:=SELECT()

          SELECT 45
          USE congelar INDEX conge_it
          SEEK ALLTRIM(it)
          IF FOUND()
             lbo   = bono_anti
             lefe  = ind_efec
             lsub  = sub_irrad
             lhora = hora_adi
          ENDIF

         opcion=menu_dat(13,53,vector,"N/W,W/N")
         pp5=OPEN_WINDOW(7,10,15,73,1,'Ingresos Congelados',"B/N+",5,.T.)
         SET COLOR TO "W/N+"
         SET CURSOR ON
         @ 9,12  SAY "Nombre del Empleado :"+ALLTRIM(ni)

         DO CASE
          CASE opcion=1
              @ 11,12 SAY "Bono de Antiguedad :" GET lbo PICTURE "999,999.99" VALID lbo>0
          CASE opcion=2
              @ 11,12 SAY "Indice de Efectividad :" GET lefe PICTURE "999,999.99" VALID lefe>0
          CASE opcion=3
              @11,12 SAY "Subsidio Irrad. :" GET lsub PICTURE "999,999.99" VALID lsub>0
          CASE opcion=4
              @11,12 SAY "Horas Adici¢n :" GET lhora PICTURE "999,999.99" VALID lhora>0
        ENDCASE
        READ
        IF LASTKEY()!=27
         elec=MULTIPLE({"Grabar","Salir sin Grabar"},2,21,23,.F.)
           IF elec=1
            SELECT 45
            USE congelar INDEX conge_it
            SEEK ALLTRIM(it)
            IF !FOUND()
               APPEND BLANK
            ENDIF

              REPLACE nitem  WITH it,;
                      bono_anti WITH lbo,;
                      ind_efec  WITH  lefe,;
                      sub_irrad WITH  lsub,;
                      hora_adi  WITH  lhora
             MENSAJE("Registrando Datos...",12,17,'W/R+',.T.,.F.)
           ENDIF
        ENDIF
        CLOSE_WINDOW(pp5)
        SELECT(area78)
RETURN

FUNCTION es_con(nt)
LOCAL sn:={0,0,0,0}
      area56:=SELECT()
      SELECT 45
      USE congelar INDEX conge_it
      SEEK ALLTRIM(nt)

      IF FOUND()
             sn[1]=bono_anti
             sn[2]=ind_efec
             sn[3]=sub_irrad
             sn[4]=hora_adi
      ENDIF
      SELECT(area56)
RETURN sn

STATIC FUNCTION desconge(it,ni)
LOCAL pp5,lbo:=0,lefe:=0,lsub:=0,lhora,elec,sn:="S",registro

         area78:=SELECT()
         pp5=OPEN_WINDOW(7,10,15,73,1,'Ingresos Congelados',"B/N+",5,.T.)
         SET COLOR TO "W/N+"
         SET CURSOR ON
         @ 9,12  SAY "Nombre del Empleado :"+ALLTRIM(ni)

         SELECT 45
         USE congelar INDEX conge_it
         SEEK ALLTRIM(it)
         IF FOUND()
            registro=RECNO()
            DO CASE
               CASE bono_anti!=0
                  @ 11,12 SAY "Bono de Antiguedad :" +ALLTRIM(STR(bono_anti))
               CASE ind_efec!=0
                  @ 11,12 SAY "Indice de Efectividad :"+ALLTRIM(STR(ind_efec))
               CASE sub_irrad!=0
                  @ 11,12 SAY "Subsidio Irradiaci¢n :"+ALLTRIM(STR(sub_irrad))
               CASE hora_adi!=0
                   @11,12 SAY "Horas Adici¢n :" +ALLTRIM( STR(hora_adi))

            ENDCASE
            @ 13,12 SAY "Descongelar S/N ? " GET sn  VALID sn $"SNsn"
            READ
            IF LASTKEY()!=27
             IF sn="S"
                SELECT 45
                use congelar index conge_it
                GO registro
                DELETE
                PACK
                MENSAJE("Borrando Datos...",12,17,'W/R+',.T.,.F.)
              ENDIF
             ENDIF
         ELSE
              MENSAJE("No existe datos congelados...",12,17,'W/R+',.T.,.F.)
         ENDIF
        CLOSE_WINDOW(pp5)
        SELECT(area78)
RETURN

FUNCTION  comision(ni,no,re)
LOCAL  ee,op:={"DECLARAR","ANULAR"},n_a_m,c_i,res:={1,2}
         area15:=SELECT()
         SET COLOR TO "W/R"
         ee=menu_dat(13,50,op,"R/GW",.F.)
         DO CASE
            CASE ee=1
               SELECT 32
               USE personal INDEX perso_it,perso_nom
               SEEK ni
               IF !FOUND()
                 APPEND BLANK
                 REPLACE nitem  WITH ni,;
                         nombre WITH no
               ENDIF
               SELECT 1
                USE dat_empl index nitem,nombre,codigo,eventual,ci
               GO re
               REPLACE nombre WITH "COMISION"
            CASE ee=2
               IF no="COMISION"
                 SELECT 32
                 USE personal INDEX perso_it,perso_nom
                 SEEK ni
                 n_a_m=nombre

                 SELECT 1
                  USE dat_empl index nitem,nombre,codigo,eventual,ci
                 GO re
                 REPLACE nombre WITH n_a_m
               ENDIF
         ENDCASE
         SELECT(area15)
RETURN
