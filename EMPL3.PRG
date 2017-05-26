*****************************************************************************
*  Modulo : Manejo de empleados, recepcion y control de documentos          *
*  Fecha  : 7 de Agosto de 1994                                              *
*  Autor  : Richard Jim‚nez Velasco                                         *
*****************************************************************************

#include "inkey.ch"
#include "dbedit.ch"
#include "setcurs.ch"
#include "common.ch"
SET DATE FRENCH

*****************************************************************************
* Realiza el control de los empleados permanentes                           *
*****************************************************************************
FUNCTION manejo_perman
	//SHOWTIME()
	 SELECT 1
	 USE dat_empl INDEX nitem,nombre,codigo,eventual,ci

	pan0=OPEN_WINDOW(0,0,23,79,1,'Empleados ',"B/W+",5,.T.)
	MESSBAR_ON(" <F2> Nuevo Empl.  <F3> Borrar  <F4> Modificar  <F5> Kardex  <F6> Transferencia  <ESC> Salir",'N/W','R/W')
	IF mes!="@"
	   datos_sal()
	   @5,12 SAY SPACE(12) COLOR "B/W"
	   @5,12 SAY mes COLOR "B/W"
	   pe=1
	   busca_contexto(7,5,20,65)
	ENDIF
	CLOSE_WINDOW(pan0)
	//SHOWTIME(2,70,.F.,"B/W")
RETURN ''

*****************************************************************************
* Realiz  el control de los empleados eventuales                            *
*****************************************************************************
FUNCTION manejo_eventu
LOCAL pan0
	//SHOWTIME()
	SELECT 2
	USE temp_eve INDEX enitem,enombre

	pan0=OPEN_WINDOW(0,0,23,79,1,'Empleados ',"B/W+",5,.T.)
	MESSBAR_ON(" <F2> Nuevo Empl.  <F3> Borrar  <F4> Modificar  <F5> Kardex  <ESC> Salir",'N/W','R/W')
	datos_sal()
	IF mes!="@"
	   @5,12 SAY SPACE(12) COLOR "B/W"
	   @5,12 SAY mes COLOR "B/W"
	   pe=2
	   busca_contexto(7,5,20,65)
	 ENDIF
	CLOSE_WINDOW(pan0)
	// SHOWTIME(2,70,.F.,"B/W")
RETURN ''

*****************************************************************************
*                   Inicializaci¢n de Variables                             *
*****************************************************************************
FUNCTION Iniciar

PUBLIC lci:=SPACE(13),lnombre:=SPACE(20),lpaterno:= SPACE(20),lmaterno:=SPACE(20),llnombre:=space(50)
PUBLIC  lfecha_nac:=CTOD(""),ldomicilio:=SPACE(30),lzona:=SPACE(10),lindice:=0
PUBLIC  lest_civil:=' ',lextension:=1,ltelefono:=SPACE(10),lmadre:=SPACE(40),lpadre:=SPACE(40),lsexo:=" ", lafp:= "   "
PUBLIC  lfech_hijo:={},locupacion:={},lnomb_hijo:={},carg:= " ",lnacion:=SPACE(20),lext:="   ",llext:=1
PUBLIC  identifi,xx,lcodigo:={"Primaria","Secundaria","T‚cnica","Universitaria","Post-Graduaci¢n","Especializaci¢n"}
PUBLIC  control:=.F.,lapellido:=space(40),mod_nombre:=space(40),lreemp:=SPACE(40),lhasta:=CTOD("  /  /    ")
PUBLIC  departa:={"COCHABAMBA","LA PAZ","SANTA CRUZ","ORURO","POTOSI","BENI","PANDO","TARIJA","SUCRE","EXTRANJERO"},lug:=0
PUBLIC  aport:=" ",lescalafon:=0, lescalafo0:=0
RETURN ''

*****************************************************************************
* Despliega la cabecera de datos                                            *
*****************************************************************************
FUNCTION datos_sal()
	 SET COLOR TO "N/W"
	 @ 2,5 SAY  "Cargo:"
	 @ 3,5 SAY  "Departamento:"
	 @ 4,5 SAY "Fecha de Ingreso:"
	 @ 5,43 SAY "Empleado :"
	 @ 5,5 SAY "Mes : "
	 @ 5,12 SAY mes(MONTH(DATE())) COLOR "B/W"
RETURN''

****************************************************************************
* Funci¢n que hace la b£squeda por contexto con 2 indices                  *
* No. de item y Apellido                                                         *
***************************************************************************
FUNCTION Busca_Contexto (fila1,columna1,fila2,columna2)

DECLARE vec_campos := {"nitem","nombre"}
DECLARE vec_pict   := {"@S10", "@S40"}
DECLARE vec_nombre := {"No. Item","Nombre del empleado"}

PRIVATE  codigo_aux,a,formato
PRIVATE  nro,pantalla, pantalla1, antrec[2],antcol,hostkey[2],f1,f2,c1,c2
PRIVATE  it1:={"","",""},it2:={"","",""}
  SET SOFTSEEK ON
  antcol = 1
  f1  = fila1
  f2  = fila2
  c1  = columna1
  c2  = columna2

  a:=SELECT()
  SELECT(pe)
  IF pe=1
    SET FILTER TO tip_empl="P" .OR. tip_empl=" "
  ENDIF

  codigo_aux = SPACE(5)
  formato := codigo_aux :='     '   && modificar en la configuracion

  antrec[1]  := antrec [2] := 0
  hostkey[1] := ''     && inicializo
  hostkey[2] := SPACE(30)
  hostkey[2] :=''

  SET COLOR TO "N/N"
  @ f1+1,c2+1 CLEAR TO f2+1,c2+2

  @ f2+1,c1+2 CLEAR TO f2+1,c2+2

  SETCOLOR("W+/B+,B/W+")

  DBEDIT (f1,c1,f2,c2, vec_campos, "contexto",vec_pict, vec_nombre)

  SET FILTER TO
  SELECT(a)
RETURN codigo_aux

*****************************************************************************
* Activa la b£squeda por Contexto                                           *
*****************************************************************************
FUNCTION contexto (modo, poscol)
LOCAL nkey,cadena,caracter,ncampo,ee,mesa,verifica

  nkey =LASTKEY()

   IF poscol <> antcol
      IF poscol=1
	     @ f2 + 2,21  SAY SPACE(30) COLOR "R/W"
	     hostkey[2]=""
      ELSE
	     @ f2 + 2,7  SAY  SPACE(10)  COLOR "R/W"
	     hostkey[1]=""
      ENDIF
      SELECT (pe)
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

       CASE poscol=2 .AND. ( (nkey >= 65 .AND. nkey <= 90) )  .OR. nkey==32 .OR. (nkey>=97 .AND. nkey<=122) .OR. nkey==44//nombres y apellidos

	   hostkey[poscol]=hostkey[poscol] + UPPER(CHR(nkey))

	   SEEK hostkey[poscol]

	   desplegar(codigo,nitem,tip_empl,nombre)

	      @ f2 + 2, c1 + 16 SAY hostkey[2] COLOR "B/W"
	   antrec[poscol] = RECNO()

	   RETURN 1

       CASE poscol=1 .AND.( nkey=45 .OR. (nkey>=65 .AND. nkey<=90).OR. (nkey>=48 .AND. nkey<=57) .OR. (nkey>=97 .AND. nkey<=122))

	     hostkey[poscol] = hostkey[poscol]+UPPER(CHR(nkey))

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

      CASE nkey == -1 .OR. nkey==K_INS            && F2
	 TONE (1000,0)
	 q:=SELECT()
	 IF pe=1
	   ins_empl(nitem,nombre,RECNO(),0,"P")              //insercion de datos de un nuevo empleado
	 ELSE
	   ins_empl(nitem,nombre,RECNO(),0,"E")
	 ENDIF
	 SELECT(q)

      CASE (nkey == -2 .OR. nkey==K_DEL ) .AND. !EMPTY(nitem)   && F3//borrar
	 TONE (1000,0)
	 marcar(nitem,nombre)
	 IF tip_empl="P"
	    ee=menu_dat(13,50,{"Eliminar","Acefalia"},"R/GW")
	 ELSE
	    ee = 2
	 ENDIF
	 DO CASE
	    CASE ee=1
	      mesa ="Esta Seguro de Borrar los datos de "+ALLTRIM(nombre)+"....!"
	      verifica = MULTI_OPCION(mesa,{'No','Si'},2,11,.T.)
	      IF verifica=2
		 itemp := ALLTRIM(1->nitem)
		 renumera(itemp,1,1)
		 SELECT 22
		 USE iva INDEX iva_item
		 renumera(itemp,22,1)
		 SELECT 1
		 USE dat_empl INDEX nitem,nombre,codigo,eventual,ci

		/* auxr := SELECT()
		 SELECT 22
		 USE  iva INDEX  iva_item
		 SEEK ALLTRIM(1->nitem)
		 IF FOUND()
		   DELETE
		   PACK
		 ENDIF
		 SELECT(auxr)
		 DELETE
		 PACK */
		 RETURN 2

	      ENDIF
	    CASE ee=2
	     IF nombre!="COMISION"
	       IF pe=1
		  Borrar(nitem,nombre,poscol,"P")
	       ELSE
		  Borrar(nitem,nombre,poscol,"E")
	       ENDIF
	     ENDIF
	 ENDCASE

      CASE nkey == -3  .AND. !EMPTY(nitem)                   && F4 MODIFICAR
	 TONE (1000,0)
	 marcar(nitem,nombre)
	 IF nombre!="COMISION"
	   IF nombre="ACEFALIA"
	     IF pe=1
	       ins_empl(nitem,nombre,RECNO(),1,"P")
	     ELSE
	       ins_empl(nitem,nombre,RECNO(),1,"E")
	     ENDIF
	   ELSE
	    IF pe=1
	     modificar(nitem,nombre,poscol,"P")        //modificar los datos de un empleado
	    ELSE
	     modificar(nitem,nombre,poscol,"E")
	    ENDIF
	   ENDIF
	 ENDIF

      CASE nkey == -4    .AND. !EMPTY(nitem)
	 TONE (1000,0)
	 marcar(nitem,nombre)                       //Mostrar datos de empleados
	 mostrar(nitem,nombre,poscol)             && F5

      CASE nkey == - 5                      //Transferencia
       TONE(1000,0)
       MESSBAR_ON("<ALT-P> Se¤ale el primer Empleado ",'N/W','R/W')
      CASE nkey == 281
      IF ALLTRIM(nombre)!="COMISION"
	 @ ROW(),9 SAY ALLTRIM(nitem) COLOR "R/B"
	 @ ROW(),22 SAY ALLTRIM(nombre) COLOR "R/B"
	 INKEY(1)
	 MESSBAR_OFF()
	 MESSBAR_ON("<ALT-S> Se¤ale el segundo Empleado ",'N/W','R/W')
	 it1[1]=ALLTRIM(nitem)
	 it1[2]=ALLTRIM(nombre)
	 it1[3]=ALLTRIM(codigo)
      ELSE
	 MENSAJE("Error no es posible realizar la transferencia" ,10,17,'W*/R+',.T.,.F.)
	 MESSBAR_OFF()
      ENDIF
      CASE nkey==287
       IF ALLTRIM(nombre)!="COMISION"
	   @ ROW(),9 SAY ALLTRIM(nitem) COLOR "R/B"
	   @ ROW(),22 SAY ALLTRIM(nombre) COLOR "R/B"
	   INKEY(1)
	   MESSBAR_OFF()
	   TONE(1000,0)
	   it2[1]=ALLTRIM(nitem)
	   it2[2]=ALLTRIM(nombre)
	   it2[3]=ALLTRIM(codigo)
	   transfer(it1,it2)
	   RETURN 2
       ELSE
	 MENSAJE("Error no es posible realizar la transferencia ",10,17,'W*/R+',.T.,.F.)
	 MESSBAR_OFF()
      ENDIF
  ENDCASE
RETURN 1

*****************************************************************************
* Despliega datos en la cabecera                                            *
*****************************************************************************
FUNCTION desplegar(codigo,item1,tip_empl,nn)
LOCAL car,de,reg,bb,nu_reg
	w := SELECT()
	@ 4,23 SAY fech_ing  COLOR "B/W"
	reg=LEFT(item1,AT("-",item1)-1)
	@5,54 SAY SPACE(12) COLOR "B/W"

	DO CASE
	CASE ALLTRIM(tip_empl)="P"
	   @5,54 SAY "DE PLANTA"  COLOR "B/W"
	   @4,43 SAY SPACE(22) COLOR "W/W"
	CASE ALLTRIM(tip_empl)="E"
	   @5,54 SAY "EVENTUAL"   COLOR "B/W"
	   bb:=SELECT()

	     /* SELECT 2
	       USE temp_eve INDEX enitem,enombre
	       SET ORDER TO 1
	       nu_reg = RECNO()
	       SEEK item1
	      IF FOUND()
	      ELSE
	       SELECT 2
	       USE temp_eve INDEX enitem,enombre
	       SET ORDER TO 2
	       SEEK nn
	      ENDIF */
	      SELECT 2
	      SEEK item1
	      IF !FOUND()
		SEEK nn
	      ENDIF

	       @ 4,43 SAY "Fecha Final.:" COLOR"N/W"
	       @ 4,56 SAY 2->fech_fin COLOR "B/W"

	      //GO nu_reg
	   SELECT(bb)
	ENDCASE

	b:=SELECT()

	 car=SUBSTR(codigo,9,3)
	 SELECT 12
	 USE (dr+"cargo") INDEX (dr+"cargo_co")
	 SEEK car
	 IF FOUND()
		@2,12 SAY SPACE(45)     COLOR "B/W"
		@2,12 SAY ALLTRIM(B)    COLOR "B/W"
	 ENDIF

	SELECT 11
	 USE (dr+"sector") INDEX (dr+"sector")
	de= SUBSTR(codigo,5,3)
	SEEK de
	IF FOUND()
	   @3,19 SAY SPACE(45)  COLOR "B/W"
	   @3,19 SAY ALLTRIM(nombre)   COLOR "B/W"
	ENDIF
	
       SELECT(b)
       SELECT(w)

RETURN ''


*****************************************************************************
* Inserci¢n de los datos de un empleado nuevo                               *
*****************************************************************************
FUNCTION ins_empl(tt,nn,rr,band,lt)
LOCAL fila:=14,cc:=0,car:=0,salir:=.T.,elec,ret:=SPACE(8)
LOCAL dat_i:={CTOD("")," "," ",0,"  " },ldura:=3,lhpol:=0,lhosp:=0
LOCAL codi:="000000000000" ,j,asoc:=SPACE(46),s_eme:=" ",lposicion:=0
LOCAL desc_afp:="S"

      t:=SELECT()
      SET CURSOR ON
      iniciar()
							//BG+
      pan3=OPEN_WINDOW(1,1,15,70,1,'Datos personales',"N/BG+",2,.T.)
      lnacion = "BOLIVIANA"
      SET COLOR TO "N+/BG+"

      @ 2,3  SAY "Nombre :"     GET lnombre    PICTURE "@!"  COLOR "B/W" VALID lnombre!=SPACE(20)
      @ 3,3  SAY "A. Paterno :" GET lpaterno   PICTURE "@!" COLOR "B/W" WHEN val_nom(lnombre)
      @ 4,3  SAY "A. Materno :" GET  lmaterno  PICTURE "@!" COLOR "B/W"
      @ 2,39 SAY "Domicilio :"  GET ldomicilio PICTURE "@S18!" COLOR "B/W"
      @ 3,39 SAY "Tel‚fono :"   GET ltelefono  PICTURE "9-999999999" COLOR "B/W"
      @ 4,39 SAY "C.I. :"       GET lci        PICTURE "9999999999999" COLOR "B/W" VALID val_ci(lci)
      @ 4,60 SAY "Ext. :"       GET lext       COLOR "B/W" VALID val_ext(lext)
      @ 6,3  SAY "Fecha de nacimiento:" GET lfecha_nac COLOR "B/W" VALID lfecha_nac>CTOD("  /  /    ")
      @ 6,39 SAY "Lugar :"        GET lug     PICTURE "99" COLOR "B/W" VALID val_lug(lug)
      @ 7,3  SAY "Nacionalidad :" GET lnacion PICTURE "@!" COLOR "B/W"
      @ 7,39 SAY "Estado Civil [S,C,D,V] :" GET lest_civil PICTURE "@!" COLOR "B/W"VALID val_es(lest_civil) //lest_civil $ "SCDV"
      @ 8,39 SAY "Sexo [F,M] :"   GET lsexo   PICTURE"@!" COLOR "B/W"VALID lsexo $ "FM"
      @ 9,3 SAY "AFP [BBV,FUT] :"   GET lafp   PICTURE"@!" COLOR "B/W"VALID (lafp = "BBV" .OR. lafp = "FUT")

      @ 11,3 SAY "GRADO DE INSTRUCCION" GET  ret COLOR "B/W" VALID val_ret(ret)
      READ

 IF LASTKEY()!=K_ESC //PANTALLA 1

	llnombre=ALLTRIM(lpaterno)+' '+ALLTRIM(lmaterno)+','+ALLTRIM(lnombre)
	iniciar_i()
	pan6=OPEN_WINDOW(2,2,17,70,1,'Datos de Ingreso',"N/BG+",2,.T.)
	SET COLOR TO "N+/BG+"
	SET CURSOR ON
	@ 3,3  SAY "Fecha de Ingreso.:" GET lfech COLOR "B/W" VALID lfech>CTOD("  /  /    ")

	@ 4,3 SAY "Fecha de Ingreso Reconocida :"GET lfech_r COLOR "B/W" VALID lfech>CTOD("  /  /    ")
	IF ALLTRIM(lt)="E"
	   @ 3,35 SAY "Hasta :" GET lhasta COLOR "B/W" VALID lfech>CTOD("  /  /    ")
	ENDIF
	@ 5,3  SAY "Documentos de Ingreso :"  GET ldocs COLOR "B/W" VALID val_doc(ldocs)
	@ 7,3  SAY "Profesi¢n :"              GET lprof COLOR "B/W" PICTURE "@!"
       // @ 7,3  SAY "Tipo de empleo [E,P] :" GET ltip COLOR "B/W" VALID val_tip(ltip)
	@ 9,3  SAY "Jornada de Trabajo   :"   GET ljor PICTURE "9.9" COLOR "B/W" Valid val_jor()
	@ 10,3 SAY "Horas en Policl¡nico :"   GET lhpol COLOR "B/W" PICTURE "99" VALID lhpol>=0
	@ 11,3 SAY "Horas en Hospital    :"   GET lhosp COLOR "B/W" PICTURE "99" VALID lhosp>=0
	@ 12,3 SAY "Indice de Efectividad:"   GET lindice  COLOR "B/W" PICTURE  "99999.99"
	@ 14,3 SAY "Escalaf¢n M‚dico     :"   GET lescalafon COLOR "B/W" PICTURE "99999.99"
        @ 15,3 SAY "Escalaf¢n Congelado  :"   GET lescalafo0 COLOR "B/W" PICTURE "99999.99"

	IF ALLTRIM(lt)="E"
	   @ 12,3 SAY "Reemplazando a  :"     GET  lreemp COLOR "B/W"
	ENDIF
	READ

  IF LASTKEY()!=27                //PANTALLA 2

	bb=cargar()
	iniciar_p()
	pan5=OPEN_WINDOW(3,3,18,78,1,'Datos Laborales',"N/BG+",2,.T.)
	SET COLOR TO "N+/BG+"
	SET CURSOR ON

      IF ALLTRIM(lt)=="P"
	 @ 13,7   SAY "Ingresos Optativos "
	 @ 15,7   SAY "Descuentos a Colegios y Asociaciones "
	 @ 15,53  SAY "Aportes"

      ENDIF

      @ 17,7  SAY "Descuento AFP [S,N]"
      @ 4,7  SAY "Regional : Cochabamba" COLOR "B/W"
      @ 5,7  SAY "Memorandum No.:" GET lmemo  COLOR "B/W"
      IF band=1
	 @ 6,7 SAY "Item No.  :" + tt
	 litem = STUFF(tt,1,4,"")
      ELSE
	@ 6,7   SAY "Item No. :" GET  litem PICTURE "999AAA" VALID val_item(litem,nn) COLOR "B/W"
      ENDIF

      @ 6,20  SAY "Cod. Interno:" GET lposicion PICTURE "99999999"

      @ 7,7   SAY "Nivel    :"              GET lnivel  VALID val_niv()  COLOR "B/W"
      @ 7,38  SAY "Categoria Profesional :" GET lp_prof PICTURE "999"  COLOR "B/W"
      @ 7,66  GET lfecp COLOR "B/W"    //VALID lfecp>CTOD("  /  /    ")

      @ 8,7   SAY "Sub-Programa :"          GET lsub VALID val_subp(lsub,8,25)  COLOR "B/W"
      //@ 8,54  SAY "Antig.Emerg" GET lfeemer COLOR "B/W"

      @ 9,7   SAY "Actividad    :"          GET lact VALID val_act(lact)   COLOR "B/W"
      @ 10,7  SAY "Cargo    :"              GET lcargo VALID val_car(lcargo)   COLOR "B/W"
      @ 11,7  SAY "Personal :"              GET lper VALID val_per(lper)  COLOR "B/W"
      @ 12,7  SAY "Departamento :"          GET ldepto VALID val_dep(ldepto) COLOR "B/W"
      @ 17,27 GET desc_afp  COLOR "B/W" PICTURE "@!A" VALID desc_afp $"SN"

      IF ALLTRIM(lt)="P"
	 @ 14,7  GET bon VALID val_bon(bon) COLOR "B/W"
	 @ 16,7  GET descuentos VALID val_asoc(descuentos) COLOR "B/W"
	 @ 16,54 GET aport PICTURE "@!" VALID val_aport(aport) COLOR "B/W"
     ELSE
	 @ 13,7  SAY "Servicio de Emergencia [S,N] :" GET s_eme COLOR "B/W" PICTURE "@!A" VALID s_eme $"SN"
      ENDIF
      @ 14,10  SAY "Antig.Emerg" GET lfeemer COLOR "B/W"
      READ

   IF LASTKEY()!=27 // Pantalla 3
	 ittem="CBB"+"-"+litem
	 DO CASE
	     CASE lper=="001"
		  lper = "M"
	     CASE lper=="002"
		  lper = "P"
	     CASE lper=="003"
		  lper = "A"
	 ENDCASE

	 codi =lsub+lact+n1[1]+lper+mm[1]+" "

	 bo   = LEFT(bon,1) //radiaci¢n
	 IF bo="X"
	    codi = LEFT(codi,11)+"R"
	 ENDIF
	 bo = SUBSTR(bon,2,1)

	 IF bo="X"  .AND. lt="P"
	   s_eme = "S"
	 ENDIF
	 IF bo=" " .AND. lt="P"
	   s_eme = "N"
	 ENDIF

       asoc=""
       FOR J=1 TO 9
	    CC=SUBSTR(descuentos,j,1)
	    IF cc=="X"
	       asoc=asoc+"0"+CHR(J+48)
	    ELSE
	      asoc=asoc+"00"
	    ENDIF
       ENDFOR

       FOR J=10 TO 23
	    CC=SUBSTR(descuentos,j,1)
	    IF cc=="X"
	      asoc=asoc+alltrim(STR(J))
	    ELSE
	    asoc=asoc+"00"
	    ENDIF
       ENDFOR

       IF ALLTRIM(lt)="P" ; @ 16,7 SAY descuentos COLOR "B/W"; ENDIF

       MESSBAR_ON('<Grabar> Almacena todos los datos del empleado ','N/W','R/W')
       elec=MULTIPLE({"Grabar","Cancelar"},2,21,23,.F.)

       MESSBAR_OFF()
       IF elec=1

	   SELECT 1
	   USE dat_empl INDEX nitem,nombre,codigo,eventual,ci
	   IF nn="ACEFALIA"
	      GO rr
	   ELSE
	      APPEND BLANK
	   ENDIF
	   REPLACE     numero     WITH lposicion
	   REPLACE     codigo     WITH codi
	   REPLACE     nitem      WITH ittem
	   REPLACE     nombre     WITH llnombre
	   REPLACE     jor_trab   WITH ljor
	   REPLACE     fech_ing   WITH lfech
	   REPLACE     tip_empl   WITH lt
	   REPLACE     p_prof     WITH lp_prof
           REPLACE     fecp       WITH lfecp
           REPLACE     feemer     WITH lfeemer

	   REPLACE     asociacion WITH asoc
	   REPLACE     nivel      WITH lnivel
	   REPLACE     memo       WITH lmemo,;
			sexo      WITH lsexo,;
                        afp       WITH lafp,;
			prof      WITH lprof,;
			duracion  WITH ldura
	   REPLACE      domicilio WITH ldomicilio
	   REPLACE      telefono  WITH ltelefono
	   REPLACE      ci        WITH lci
	   REPLACE      extension WITH lext
	   REPLACE      fecha_nac WITH lfecha_nac
	   REPLACE      est_civil WITH lest_cIvil
	   REPLACE      grad_inst WITH ret
	   REPLACE      lugar_nac WITH lug
	   REPLACE      doc_ingres WITH ldocs,;
			serv_emerg WITH s_eme,;
			aportes    WITH aport,;
			hrs_poli   WITH lhpol,;
			hrs_hosp   WITH lhosp,;
			fech_rec  WITH lfech_r,;
			ind_efec  WITH lindice,;
			esc_med   WITH lescalafon,;
                        esc_me0   WITH lescalafo0,;
			aporte_afp WITH desc_afp

	    IF lnacion="BOLIVIANA"
	       REPLACE nacion WITH 0
	    ELSE
	       REPLACE nacion WITH 1
	    ENDIF
	    IF ALLTRIM(lt)="E"
	      SELECT 2
	      USE temp_eve INDEX enitem,enombre
	      APPEND BLANK
	      REPLACE     nitem    WITH ittem
	      REPLACE     nombre   WITH llnombre
	      REPLACE     codigo   WITH codi
	      REPLACE     fech_ing WITH lfech
	      REPLACE     tip_empl WITH "E",;
			  reemp    WITH lreemp,;
			  fech_fin WITH lhasta
	    ENDIF
	  MENSAJE("Grabando ......" ,10,17,'W*/R+',.T.,.F.)
      ENDIF //elec
    ENDIF //Pantalla 3
     CLOSE_WINDOW(pan5)
  ENDIF //Pantalla  2
    CLOSE_WINDOW(pan6)
ENDIF //  Pantalla 1
CLOSE_WINDOW(pan3)
SELECT(t)
RETURN  //INGRESO DE DATOS

*****************************************************************************
*                  Validaci¢n del nombre                                    *
*****************************************************************************
FUNCTION val_nom(nomb)
LOCAL pant111
 iniciar_p()
 lsub:=lact:=ldepto:=lcargo:=SPACE(40)

 DO CASE
     CASE nomb="ACEFALIA"
      CLEAR GETS
      acef_(nomb)
      ll=.T.
      KEYBOARD(CHR(27))
 OTHERWISE
	 ll=.T.
  ENDCASE
RETURN  ll

*****************************************************************************
*                  Validaci¢n del Estado Civil                              *
*****************************************************************************

STATIC FUNCTION  val_es(e)
LOCAL veop:={"Soltero/a","Casado/a","Divorciado/a","Viudo/a"},vere:={"S","C","D","V"}
LOCAL mm
      IF !(e $ "SCDV")
	mm=menuv(7,63,veop,vere,"W+/R",.F.)
      ENDIF
      SHOWGET("lest_civil",mm)
RETURN .T.

*****************************************************************************
*                  Validaci¢n de los aportes                                *
*****************************************************************************
STATIC FUNCTION val_aport(ap)
LOCAL veop:={"Personal Administrativo","Personal M‚dico","Personal Param‚dico"}
LOCAL vere:={"A","M","P"},mm
      IF !(ap $ "AMP")
	 mm=menuv(16,52,veop,vere,"W+/R",.F.)
      ENDIF
      SHOWGET("aport",mm)
RETURN

*****************************************************************************
*                  BUSQUEDA POR CONTEXTO                                    *
*****************************************************************************
FUNCTION lle_acce(modo, poscol)
LOCAL camp,nkey,fila,columna,pict

   camp = vec_ca[poscol]
   pict = vec_pi[poscol]
   nkey = LASTKEY()
   fila = ROW()
   columna = COL()

   SETCOLOR(',W+/W')

   DO CASE
      CASE modo = 1       && TOPE SUPERIOR
	   TONE (300,0)

      CASE modo = 2
	   IF !EMPTY(nombre) .AND. !EMPTY(fecha_nac) //.AND. !EMPTY(ocupacion)
	    APPEND BLANK
	   ENDIF

      CASE nkey = 27       && ESC -- FIN DBEDIT DE REGISTROS SIN KARDEX
	   RETURN 0

      CASE (nkey =13) .OR. (nkey>=32 .AND. nkey<=125)

	   SET CURSOR ON

	   IF nkey<>13
	      KEYBOARD CHR(nkey)
	   ENDIF

	   IF poscol=3
	      @ fila,columna GET &camp PICTURE pict COLOR "GR+/B"
	   ELSE
	      @ fila,columna GET &camp PICTURE pict COLOR "GR+/B"
	   ENDIF
	   READ

	   DBCOMMIT()

	   IF LASTKEY() <> 27
	      KEYBOARD CHR(4)
	   ENDIF

	   SET CURSOR OFF

       CASE nkey = 7
	    IF MULTI_OPCION('  Borrar  El Registro  ?  ',{'Si','No'},2,18,.T.) = 1
	       rec = RECNO()
	       DELETE
	       PACK
	       IF LASTREC()=0
		  APPEND BLANK
	       ELSE
	       GO rec
	       ENDIF
	    ENDIF
	    RETURN 2
      CASE nkey = K_F3
	   TONE(1000,2)
  ENDCASE
RETURN 1

*****************************************************************************
*            Validaci¢n de la extensi¢n del C.I.                            *
*****************************************************************************
STATIC FUNCTION  val_ext(e)
LOCAL veop:={"LP","CBB","STC","OR","CHU","PAN","TAR","BEN","POT"}
LOCAL mm
      mm=menuv(4,66,veop,veop,"W+/R",.F.)
      SHOWGET("lext",mm)
RETURN .T.

*****************************************************************************
*            Validaci¢n del C.I.                                            *
*****************************************************************************
STATIC FUNCTION val_ci(ll)
LOCAL  hh:=.F.
       yy:=SELECT()
       IF !(ll==SPACE(13))
	SELECT 1
	USE dat_empl INDEX nitem,nombre,codigo,eventual,ci
	SET ORDER TO 5
	SEEK ll
	IF FOUND()
	  MESSBAR_ON("Error Identificaci¢n duplicada ",'N/W','R/W')
	   MESSBAR_OFF()
	ELSE
	  hh=.T.
	ENDIF
       ENDIF
       SELECT(yy)
RETURN  hh

*****************************************************************************
*            Elimina los datos de un empleado
*****************************************************************************
FUNCTION borrar(it,nn,pos,te)
LOCAL verifica,aux,ident:=SPACE(13),ccc,mes
      IF !(nn="ACEFALIA")
	 hh:=SELECT()
	 mes ="Esta Seguro de Borrar los datos de "+ALLTRIM(nn)+"....!"
	 verifica = MULTI_OPCION(mes,{'No','Si'},2,11,.T.)
       DO CASE
	CASE verifica == 1

	CASE verifica == 2
	   SELECT 1
	    USE dat_empl INDEX nitem,nombre,codigo,eventual,ci
	   IF pos=1
	      SEEK it
	   ELSE
	     SET ORDER TO 2
	      SEEK nn
	   ENDIF

	   IF FOUND()
	     IF ALLTRIM(te)=="E"
		 DELETE
		 PACK

		 SELECT 2
		 USE temp_eve INDEX enitem,enombre
		 IF pos=1
		    SEEK it
		 ELSE
		   SET ORDER TO 2
		    SEEK nn
		 ENDIF
		 DELETE
		 PACK
	      ELSE
		ident=ci
		REPLACE     nombre   WITH "ACEFALIA"
		REPLACE     jor_trab WITH 0
		REPLACE     fech_ing WITH CTOD("  /  /    ")
		REPLACE     tip_empl WITH ""
		REPLACE     p_prof   WITH 0
		REPLACE     extension WITH ""
		REPLACE     memo WITH ""
		REPLACE     ci   WITH ""
		REPLACE     sexo WITH ""
		REPLACE     prof WITH ""
		REPLACE     duracion WITH 0
		REPLACE     lugar_nac WITH 0
		REPLACE             domicilio WITH ""
		REPLACE            est_civil WITH ""
		REPLACE            telefono  WITH ""
		REPLACE            grad_inst WITH ""
//              REPLACE            doc_ingres WITH ""
//              REPLACE            doc_afili WITH ""
	       ENDIF
		 MENSAJE('Borrando Registro....  Espere Por Favor ',10,17,'W*/R+',.T.,.F.)
	   ENDIF
	 ENDCASE
	 SELECT(hh)
      ELSE
	TONE(1000,1)
	ccc='<ERROR> No es posible borrar este Item   Presione cualquier tecla para Continuar'
	MESSBAR_ON(ccc,'N/W','R*/W')
	INKEY(ccc)
	MESSBAR_OFF()
      ENDIF
RETURN .T.

*****************************************************************************
*            Modifica los datos de un empleado                              *
*****************************************************************************
FUNCTION modificar(tt,nn,pos,ltip)
LOCAL op:={"Datos Personales","Datos de Ingreso","Datos Laborales"},ee,ccc
	 qq:= SELECT()
	 ee=menu_dat(13,50,op,"R/GW")
	 DO CASE
	    CASE ee=1
		 perDat(tt,nn,pos,ltip)
	    CASE ee=2
		 ingDat(tt,nn,pos,ltip)
	    CASE ee=3
		 laborDat(tt,nn,pos,ltip)
	 ENDCASE
      SELECT(qq)
RETURN


FUNCTION most_hijos(id)
	 ww:=SELECT()
	 SELECT  31

	 SEEK id
	 IF FOUND() //nombre de sus padres e hijos continuar aqui
	 ENDIF
	 SELECT(ww)
RETURN

*****************************************************************************
*            Validaci¢n del grado de instrucci¢n                            *
*****************************************************************************
FUNCTION val_ret(par)
	 MESSBAR_ON(' <Espacio> Marcar/Desmarcar  <Esc> Finalizar con valores por omisi¢n','N/W','R/W')
	 INKEY()
	 ret=dialog2(11,26,lcodigo,par)
	 SHOWGET("ret",ret)
	 MESSBAR_OFF()
RETURN .T.

*****************************************************************************
*             Validaci¢n del lugar de nacimiento                            *
*****************************************************************************
FUNCTION val_nac(ll)
LOCAL vere:={"B","O"}
LOCAL mm
      mm=menuv(6,50,departa,vere,"W+/R",.F.)
      SHOWGET("lug",mm)
      @ 6,50 SAY SPACE(20)
      @ 6,50 SAY departa[mm]
     
RETURN .T.


*****************************************************************************
*             Validaci¢n del lugar de nacimiento
*****************************************************************************
FUNCTION val_lug(ll)
LOCAL vere:={1,2,3,4,5,6,7,8,9,10,11}
LOCAL mm
      IF ll>0
	@ 6, 50 SAY departa[ll]
	mm=ll
      ENDIF
      mm=menuv(6,50,departa,vere,"W+/R",.F.)
      SHOWGET("lug",mm)
      @ 6,50 SAY SPACE(20)
      @ 6,50 SAY departa[mm]
     
RETURN .T.

*****************************************************************************
*            Despliega un men£ de opciones                                  *
*****************************************************************************
FUNCTION menu_dat(x,y,op,color)
LOCAL escg:=1,pant
PRIVATE col:="     "
	col=SETCOLOR()
	set color to "N/W+"
	pant=OPEN_WINDOW(x-1,y-1,x+3,y+19,1,'',color,5,.T.)
	escg:=ACHOICE(x,y,x+3,y+17,op)
	CLOSE_WINDOW(pant)
	SETCOLOR("&col")
RETURN escg

*****************************************************************************
*            Modifica los datos Personales                                 *
*****************************************************************************
FUNCTION perDat(tt,nn,pos,lt)
LOCAL pan3,ret:=SPACE(8),pan6,elec,num_reg:=0,reg1:=0,reg2:=0

    pan3=OPEN_WINDOW(0,0,14,70,1,'Datos personales',"N/BG+",2,.T.)
    iniciar()
    iniciar_i()
    q:=SELECT()

    SELECT 1
     USE dat_empl INDEX nitem,nombre,codigo,eventual,ci

      IF pos=2
	   SET ORDER TO 2
	   SEEK nn
	   num_reg=RECNO()
      ELSE
	   SET ORDER TO 1
	   SEEK ALLTRIM(tt)
	   num_reg=RECNO()
      ENDIF

	 IF FOUND()
	   lnombre   = RIGHT(nn,LEN(nn)-RAT(",",nn))
	   lapellido = LEFT(nn,AT(",",nn)-1) +SPACE(6)
	   lsexo     = sexo
           lafp      = afp
	   ljor      = jor_trab
	   ltip      = tip_empl
	   lprof     = prof
	   lci       = ci

	   ldomicilio  = domicilio
	   ltelefono   = telefono
	   lext        = extension
	   lfecha_nac  = fecha_nac
	   lest_civil  = est_civil
	   IF nacion = 0
		    lnacion="BOLIVIANA"
	   ELSE
		lnacion ="EXTRANJERA"
	   ENDIF
	   lug         = lugar_nac

	   IF lugar_nac>0
		@6,50 SAY departa[lugar_nac]
	   ENDIF
	   ldocs = doc_ingres
	   reg1  = RECNO()
	   ret   = grad_inst
	ELSE
	 ?"no SE ENCONTRO DATOS"
	ENDIF

	 SET COLOR TO "N+/BG+"
	 SHOWGETS()
	 SET CURSOR ON

      @ 2,3  SAY "Nombre :" GET lnombre PICTURE "@!"  COLOR "B/W"
      @ 3,3  SAY "Apellidos :" GET lapellido PICTURE "@!S27" COLOR "B/W"
      @ 4,3  SAY "Domicilio :" GET ldomicilio PICTURE "@!S23" COLOR "B/W"
      @ 3,39 SAY "Tel‚fono :" GET ltelefono PICTURE "9-999999999" COLOR "B/W"
      @ 4,39 SAY "C.I. :"  GET lci PICTURE "9999999999999" COLOR "B/W"
      @ 4,60 SAY "Ext. :" GET lext COLOR "B/W" VALID val_ext(lext)
      @ 6,3  SAY "Fecha de nacimiento:" GET lfecha_nac COLOR "B/W" VALID lfecha_nac>CTOD("  /  /    ")
      @ 6,39 SAY "Lugar :" GET lug PICTURE "99" COLOR "B/W" VALID val_lug(lug)
      @ 7,3  SAY "Estado Civil [S,C,D,V] :" GET lest_civil PICTURE "@!" COLOR "B/W"VALID val_es(lest_civil) //lest_civil $ "SCDV"
      @ 7,39 SAY "Nacionalidad :" GET lnacion PICTURE "@!" COLOR "B/W"
      @ 8,3  SAY "Sexo [F,M] :" GET lsexo PICTURE"@!S13" COLOR "B/W"VALID lsexo $ "FM"
@ 8,39 SAY "AFP [BBV,FUT] :"   GET lafp   PICTURE"@!" COLOR "B/W"VALID (lafp = "BBV" .OR. lafp = "FUT")

      @ 9,3  SAY "GRADO DE INSTRUCCION" GET  ret COLOR "B/W" VALID val_ret(ret)
      READ

      IF LASTKEY()!=K_ESC
       elec=MULTIPLE({"Grabar Modificaciones","Salir sin Modificar"},2,21,23,.F.)
       IF elec=1
	 mod_nombre=ALLTRIM(lapellido)+","+ALLTRIM(lnombre)
	 IF num_reg!=0
	   SELECT 1
	   USE dat_empl INDEX nitem,nombre,codigo,eventual,ci
	   GO num_reg
	    REPLACE nombre WITH mod_nombre,;
		   ci         WITH lci,;
		   extension  WITH lext,;
		   sexo       WITH lsexo,;
                   afp        WITH lafp
	   REPLACE      domicilio WITH ldomicilio
	   REPLACE      telefono  WITH ltelefono
	   REPLACE      ci        WITH lci
	   REPLACE      extension WITH lext
	   REPLACE      fecha_nac WITH lfecha_nac
	   REPLACE      est_civil WITH lest_cIvil,;
			grad_inst WITH ret,;
			lugar_nac WITH lug
	   IF lnacion="BOLIVIANA"
	       REPLACE nacion WITH 0
	   ELSE
	       REPLACE nacion WITH 1
	   ENDIF

	   IF ALLTRIM(lt)="E"
	      SELECT 2
	      use temp_eve INDEX enitem,enombre
	      SEEK tt
	      IF FOUND()
		REPLACE nombre WITH mod_nombre
	      ENDIF
	   ENDIF

	  ENDIF
       ENDIF
    ENDIF
  close_window(pan3)
  select(Q)
RETURN

*****************************************************************************
*            Modifica los datos de ingreso                                  *
*****************************************************************************
FUNCTION ingDat(ite,nom,pp,lt)

LOCAL dat_i:={CTOD("")," "," ",0},identifi,reg_nu:=0,elec,ldura,lhpol:=0,lhosp:=0
LOCAL lhasta:=CTOD("  /  /    "),anr:=0,mer:=0,dir:=0,reinc:=" "

SET SOFTSEEK OFF
      tt:=SELECT()
       iniciar_i()

	SELECT 1
	 USE dat_empl INDEX nitem,nombre,codigo,eventual,ci
	 IF pp=2
	   SET ORDER TO 2
	   SEEK nom
	 ELSE
	    SET ORDER TO 1
	    SEEK ALLTRIM(ite)
	 ENDIF

      IF FOUND()
	reg_nu   = RECNO()
	identifi = ci
	lfech    = fech_ing
	lfech_r  = fech_rec
	lprof    = prof
	ltip     = tip_empl
	ljor     = jor_trab
	ldura    = duracion
	ldocs    = doc_ingres
	lhpol    = hrs_poli
	lhosp    = hrs_hosp
	lindice = ind_efec
	lescalafon = esc_med
        lescalafo0 = esc_me0
	anr        = anio_rec
	mer        = mes_rec
	dir        = dia_rec
	reinc      = reincorp
       ENDIF
       x:= SELECT()

       SELECT 2
       use temp_eve index enitem,enombre
       IF pp=2
	   SET ORDER TO 2
	   SEEK nom
	ELSE
	    SET ORDER TO 1
	    SEEK ALLTRIM(ite)
	ENDIF

       IF FOUND()
	lhasta=fech_fin
       ENDIF
       SELECT(x)
      pan6=OPEN_WINDOW(2,2,17,70,1,'Datos de Ingreso',"N/BG+",2,.T.)
      SET COLOR TO "N/BG+"
      SET CURSOR ON
      SHOWGETS()
      @ 3,3  SAY "Fecha de Ingreso :" GET lfech COLOR "B/W" VALID lfech>CTOD("  /  /    ")

      //@ 3,3  SAY "Fecha de Ingreso Reconocida:" GET lfech_r COLOR "B/W"
      @ 4,3 SAY  "A¤os Mes Dias reconocidos"
      @ 5,3 GET anr COLOR "B/W" PICTURE "9999"
      @ 5,10 GET mer COLOR "B/W" PICTURE "99"
      @ 5,20 GET dir COLOR "B/W" PICTURE "99"
      
      IF lt="E"
	 @ 3,35 SAY "Fecha de Finalizaci¢n:" GET lhasta COLOR "B/W" VALID lfech>CTOD("  /  /    ")
      ENDIF
      @ 6,3  SAY "Documentos de Ingreso :" GET ldocs COLOR "B/W" VALID val_doc(ldocs)
      @ 7,3  SAY "Profesi¢n :" GET lprof COLOR "B/W" PICTURE "@!"
      @ 8,3  SAY "Tipo de empleo [E,P] :" +ltip COLOR "N/BG+"

      /*IF ALLTRIM(ltip)=""
	@7,27 GET ltip COLOR "B/W" VALID val_tip(ltip)
      ENDIF*/

      @ 9,3  SAY "Jornada de Trabajo   :" GET ljor PICTURE "9.9"COLOR "B/W" Valid val_jor()
      @ 10,3 SAY "Reincorporado S/N : "  GET reinc COLOR "B/W" PICTURE "A!"
     /* IF ALLTRIM(ltip)="E"
	 @11,3 SAY "Tiempo del Contrato (meses):" GET ldura PICTURE "999" VALID ldura>0
      ENDIF */

      @ 12,3 SAY "Horas en Policl¡nico :"   GET lhpol COLOR "B/W" PICTURE "99" VALID lhpol>=0
      @ 13,3 SAY "Horas en Hospital    :"   GET lhosp COLOR "B/W" PICTURE "99" VALID lhosp>=0
      @ 14,3 SAY "Indice de Efectividad:" GET lindice COLOR "B/W" PICTURE "999999.99"
      @ 15,3 SAY "Escalaf¢n M‚dico     :" GET lescalafon COLOR "B/W" PICTURE "999999.99"
      @ 16,3 SAY "Escalaf¢n Congelado. :" GET lescalafo0 COLOR "B/W" PICTURE "999999.99"
      READ
      IF LASTKEY()!=27
	 elec=MULTIPLE({"Grabar Modificaciones","Salir sin Modificar"},2,21,23,.F.)

      IF elec=1

	 IF reg_nu!=0
	 SELECT 1
	 USE dat_empl INDEX nitem,nombre,codigo,eventual,ci
	 GO reg_nu
	 REPLACE fech_ing    WITH lfech,;
		  prof       WITH lprof,;
		  tip_empl   WITH ltip,;
		  jor_trab   WITH ljor,;
		  doc_ingres WITH ldocs,;
		  hrs_poli   WITH lhpol,;
		  hrs_hosp   WITH lhosp,;
		  fech_rec   WITH halla_fecha(lfech,anr,mer,dir),;
		  ind_efec   WITH lindice,;
		  esc_med    WITH lescalafon,;
                  esc_me0    WITH lescalafo0,;
		  anio_rec     WITH anr,;
		  mes_rec      WITH mer,;
		  dia_rec      WITH dir,;
		  reincorp    WITH reinc

	 //lfechr

	 IF ALLTRIM(lt)="E"  .AND. ALLTRIM(ltip)="E"
	    SELECT 2
	    use temp_eve index enitem,enombre
	     IF pp=2
	      SET ORDER TO 2
	      SEEK nom
	     ELSE
	      SET ORDER TO 1
	      SEEK ALLTRIM(ite)
	     ENDIF

	    IF FOUND()
	      REPLACE fech_ing WITH lfech,;
		      fech_fin WITH lhasta
	    ENDIF
	 ENDIF

	 ENDIF
	ENDIF
      ENDIF

      SELECT (tt)
      CLOSE_WINDOW(pan6)
      SET SOFTSEEK ON
RETURN

*****************************************************************************
*            Modifica los datos laborales                                   *
*****************************************************************************
FUNCTION  laborDat(ite,nomb,pos,lt)
LOCAL codi:="000000000000" ,j,cc,asoc:=SPACE(46),reg_nu:=0,bon:=SPACE(2)
LOCAL desc_afp:=" "

      xx=SELECT()
      bb=cargar()
      iniciar_p()

      SELECT 1
      USE dat_empl INDEX nitem,nombre,codigo,eventual,ci
	 IF pos=2
	   SET ORDER TO 2
	   SEEK nomb
	 ELSE
	   SET ORDER TO 1
	    SEEK ALLTRIM(ite)
	 ENDIF

      IF FOUND()
	reg_nu  = RECNO()
	ittem   = nitem
	nomb    = nombre
	lmemo   = memo
	lnivel  = nivel
	lp_prof = p_prof
        lfecp   = fecp
        lfeemer = feemer

	ltip    = tip_empl
	aport   = aportes
	lposicion = numero
	desc_afp  = aporte_afp

	//decodificando
	descuentos = asociacion
	asoc       = ""

	FOR J=1 TO 46 STEP 2
	    CC = SUBSTR(descuentos,j,2)
	    IF cc!="00"
	       asoc = asoc+"X"
	    ELSE
	      asoc  = asoc+" "
	    ENDIF
	ENDFOR

	descuentos = asoc
	dias_trab  = 30
	lregional  = LEFT(ite,AT("-",ite)-1)
	litem      = STUFF(ite,1,4,"")
	lsub       = LEFT(codigo,2)

	lact       = SUBSTR(codigo,3,2)
	lcargo     = SUBSTR(codigo,9,3)
	ldepto     = SUBSTR(codigo,5,3)
	lper       = SUBSTR(codigo,8,1)

	IF RIGHT(codigo,1)!=" "
	   bon="X"
	ELSE
	   bon=" "
	ENDIF

	IF serv_emerg="S"
	  s_eme = 'S'
	  bon=bon+"X"
	ELSE
	  s_eme = 'N'
	  bon=bon+" "
	ENDIF
      ENDIF //found

      pan5 = OPEN_WINDOW(3,3,18,78,1,'Datos Laborales',"N/BG+",2,.T.)
      SET COLOR TO "N/BG+"

      IF lt="P"
       @ 13,7  SAY "Ingresos Optativos "
       @ 15,7  SAY "Descuentos a Colegios y Asociaciones "
       @ 15,53 SAY "Aportes"
      
      ENDIF

      @ 17,7  SAY "Descuento AFP [S,N]"
      SET CURSOR ON

      @ 10,22 SAY ALLTRIM(bus_cargo(codigo))
      @ 12,27 SAY ALLTRIM(bus_sector(codigo))

      @ 4,7  SAY "Regional : Cochabamba" COLOR "B/W"
      @ 5,7  SAY "Memorandum No.:" GET lmemo PICTURE "@!"  COLOR "B/W"
      @ 6,7  SAY "Item No. :" GET  litem PICTURE "999AAA!" COLOR "B/W" // VALID val_item(litem,"")  !=SPACE(10)
      
      @ 6,20 SAY "Cod. Interno:" GET lposicion PICTURE "99999999"  COLOR "B/W"

      @ 7,7  SAY "Nivel    :" GET lnivel  VALID val_niv(lnivel)  COLOR "B/W"
      @ 7,38 SAY "Categoria Profesional :"GET lp_prof PICTURE "999"  COLOR "B/W"
      @ 7,66  GET lfecp COLOR "B/W"       //VALID lfecp>CTOD("  /  /    ")

      @ 8,7  SAY "Sub-Programa :" GET lsub VALID val_subp(lsub,8,25)  COLOR "B/W"
      @ 8,25 SAY ALLTRIM(veop1[VAL(lsub)])
     // @ 8,54 SAY "Antig.Emerg" GET lfeemer COLOR "B/W"

      @ 9,7   SAY "Actividad    :" get lact VALID val_act(lact)   COLOR "B/W"
      @ 9,25  SAY ALLTRIM(veop2[VAL(lact)])
      @ 10,7  SAY "Cargo    :" GET lcargo VALID val_car(lcargo)   COLOR "B/W"

      //@ 10,22 SAY RTRIM(bus_cargo(lcargo))

      @ 11,7  SAY "Personal :" get lper VALID val_per(lper)  COLOR "B/W"
      @ 12,7  SAY "Departamento :" GET ldepto VALID val_dep(ldepto)  COLOR "B/W"
      @ 17,27 GET desc_afp COLOR "B/w" PICTURE "@!A"  VALID desc_afp $ 'SN'

     // @12,27 SAY ALLTRIM(bus_sector(ldepto))

      IF lt="P"
	  @ 14,7  GET bon VALID val_bon(bon) COLOR "B/W"
	  @ 16,7  GET descuentos VALID val_asoc(descuentos) COLOR "B/W"
	  @ 16,54 GET aport PICTURE "@!" VALID val_aport(aport) COLOR "B/W"

      ELSE
	  @ 14,7  SAY "Servicio de Emergencia [S,N] :" GET s_eme COLOR "B/W" PICTURE "@!A"  VALID s_eme $ 'SN'
      ENDIF
       @ 14,10 SAY "Antig.Emerg" GET lfeemer COLOR "B/W"
      SHOWGETS()
      READ

      IF LASTKEY()!=27 //K_ESC
	 ittem=lregional+"-"+litem
	 DO CASE
	     CASE lper=="001"
		lper="M"
	     CASE lper=="002"
		lper="P"
	     CASE lper=="003"
		lper="A"
	 ENDCASE

	 codi = lsub+lact+n1[1]+lper+mm[1]+" "

	 bo   = LEFT(bon,1) //radiaci¢n

	 IF bo="X"
	    codi = LEFT(codi,11)+"R"
	 ENDIF

	 bo    = SUBSTR(bon,2,1)

	 IF bo="X"  .AND. lt="P"
	   s_eme = "S"
	 ENDIF
	 IF bo=" " .AND. lt="P"
	   s_eme = "N"
	 ENDIF

       asoc = ""
       FOR J=1 TO 9
	    CC = SUBSTR(descuentos,j,1)
	    IF cc=="X"
	       asoc = asoc+"0"+CHR(J+48)
	    ELSE
	       asoc = asoc+"00"
	    ENDIF
       ENDFOR

       FOR J=10 TO 23
	    CC = SUBSTR(descuentos,j,1)
	    IF cc=="X"
	      asoc = asoc+alltrim(STR(J))
	    ELSE
	      asoc = asoc+"00"
	    ENDIF
       ENDFOR

       IF ltip="P";@ 16,7 SAY descuentos COLOR "B/W";ENDIF
       MESSBAR_ON('<Grabar> Almacena todos los datos del empleado ','N/W','R/W')
       elec=MULTIPLE({"Grabar Modificaciones","Salir sin Modificar"},2,21,23,.F.)

       MESSBAR_OFF()
       IF elec=1                 
	  MENSAJE("Grabando ...",12,17,'W/R+',.T.,.F.)
	  IF reg_nu!=0
	   SELECT 1
	   USE dat_empl INDEX nitem,nombre,codigo,eventual,ci
	   GO reg_nu
	   REPLACE  p_prof     WITH lp_prof,;
                    fecp       WITH lfecp,;
                    feemer     WITH lfeemer,;
		    nivel      WITH lnivel,;
		    asociacion WITH asoc,;
		    memo       WITH lmemo,;
		    asociacion WITH asoc,;
		    codigo     WITH codi,;
		    serv_emerg WITH s_eme,;
		    aportes    WITH aport,;
		    nitem      WITH "CBB-"+litem,;
		    numero     WITH lposicion,;
		    aporte_afp WITH desc_afp

	   IF ALLTRIM(lt)="E"
	    SELECT 2
	    use temp_eve index enitem,enombre
	    SEEK ite
	    IF FOUND()
	      REPLACE  codigo WITH codi,;
		       nitem  WITH "CBB-"+litem
	    ENDIF
	 ENDIF

	 ENDIF
	 MESSBAR_OFF()
      ENDIF
    ENDIF
  CLOSE_WINDOW(pan5)
 SELECT(xx)
RETURN

*****************************************************************************
*            Extrae las regionales                                          *
*****************************************************************************
FUNCTION regi(ite)
LOCAL rr:=" "
     uu=SELECT()
     SELECT 40
     USE regional INDEX COD_REG
     SEEK  LEFT(ite,AT("-",ite)-1)
     IF FOUND()
       rr=REGIONAL
     ENDIF
     SELECT(uu)
RETURN rr


***************************************************************************
* Entrada de datos para datos de trabajo                                  *
*                                                                         *
***************************************************************************
FUNCTION Iniciar_p
PUBLIC lregional:=SPACE(15),lmemo:=space(10),litem:=space(10),lnivel:="  "
PUBLIC lcargo:="  ",ldepto:="  ",mm,n1,lsub:=space(20),lact:=space(20),lper:="    "
PUBLIC ittem:=space(10),lp_prof:=0,aport:=" ", lfecp:=CTOD("")
PUBLIC bon:=SPACE(2),descuentos:=SPACE(46),bb, lfeemer:=CTOD("")
RETURN

FUNCTION datos_labor(nomb,lfech,lprof,ltip,ljor,ext,id,sex,dur,tt,nnb,rr)
LOCAL codi:="000000000000" ,j,cc,asoc:=SPACE(46),s_eme

      area2:=SELECT()
      bb=cargar()
      iniciar_p()
      pan5=OPEN_WINDOW(3,3,18,78,1,'Datos Laborales',"N/BG+",2,.T.)
      SET COLOR TO "N+/BG+"
      SET CURSOR ON

      IF ltip=="P"
	 @ 13,7  SAY "Ingresos Optativos "
	 @ 15,7  SAY "Descuentos a Colegios y Asociaciones "
      ENDIF

      IF nnb="ACEFALIA" .OR. nnb="COMISION"
       SELECT 1
	USE dat_empl INDEX nitem,nombre,codigo,eventual,ci
	GO rr
	litem=STUFF(tt,1,4,"")
	lsub=LEFT(codigo,2)
	lact=SUBSTR(codigo,3,2)
	lcargo=SUBSTR(codigo,9,3)
	ldepto=SUBSTR(codigo,5,3)
	lper=SUBSTR(codigo,8,1)
	bon=" "
	IF RIGHT(codigo,1)!=" "
	   bon="X"
	ENDIF

	IF serv_emerg="S"
	  bon=bon+"X"
	ELSE
	  bon=bon+" "
	ENDIF
     ENDIF
     SHOWGETS()

      @ 4,7  SAY "Regional : Cochabamba" COLOR "B/W"
      @ 5,7  SAY "Memorandum No.:" GET lmemo  COLOR "B/W"
      //@ 3,30 SAY "Matricula
      @ 6,7   SAY "Item No. :" GET  litem PICTURE "999999" VALID val_item(litem,nnb) COLOR "B/W"
      @ 7,7   SAY "Nivel    :" GET lnivel  VALID val_niv()  COLOR "B/W"
      @ 7,38  SAY "Categoria Profesional :"GET lp_prof PICTURE "999"  COLOR "B/W"
      @ 7,66  GET lfecp COLOR "B/W"       //VALID lfecp>CTOD("  /  /    ")

      @ 8,7   SAY "Sub-Programa :" GET lsub VALID val_subp(lsub,8,25)  COLOR "B/W"
      //@ 8,54  SAY "Antig.Emerg" GET lfeemer COLOR "B/W"
      @ 9,7   SAY "Actividad    :" get lact VALID val_act(lact)   COLOR "B/W"
      @ 10,7  SAY "Cargo    :" GET lcargo VALID val_car(lcargo)   COLOR "B/W"
      @ 11,7  SAY "Personal :" get lper VALID val_per(lper)  COLOR "B/W"
      @ 12,7  SAY "Departamento :" GET ldepto VALID val_dep(ldepto)  COLOR "B/W"

      IF ALLTRIM(ltip)="P"
	 @ 14,7  GET bon VALID val_bon(bon) COLOR "B/W"
	 @ 16,7  GET descuentos VALID val_asoc(descuentos) COLOR "B/W"
      ENDIF
      @ 14,10  SAY "Antig.Emerg" GET lfeemer COLOR "B/W"
      READ
      IF LASTKEY()!=27 //K_ESC
	 ittem="CBB"+"-"+litem
	 DO CASE
	     CASE lper=="001"
		lper="M"
	     CASE lper=="002"
		lper="P"
	     CASE lper=="003"
	       lper="A"
	 ENDCASE

	 codi=lsub+lact+nn[1]+lper+mm[1]+" "

	 bo=left(bon,1) //radiaci¢n
	 IF bo="X"
	    codi=LEFT(codi,11)+"R"
	 ENDIF
	 bo=SUBSTR(bon,2,1)
	 s_eme="N"
	 IF bo="X"
	   s_eme="S"
	 ENDIF
       asoc=""
       FOR J=1 TO 9
	    CC=SUBSTR(descuentos,j,1)
	    IF cc=="X"
	       asoc=asoc+"0"+CHR(J+48)
	    ELSE
	      asoc=asoc+"00"
	    ENDIF
       ENDFOR

       FOR J=10 TO 23
	    CC=SUBSTR(descuentos,j,1)
	    IF cc=="X"
	      asoc=asoc+alltrim(STR(J))
	    ELSE
	    asoc=asoc+"00"
	    ENDIF
       ENDFOR

       IF ALLTRIM(ltip)="P" ; @ 16,7 SAY descuentos COLOR "R/B"; ENDIF

       MESSBAR_ON('<Grabar> Almacena todos los datos del empleado ','N/W','R/W')
       elec=MULTIPLE({"Grabar","Cancelar"},2,21,23,.F.)

       MESSBAR_OFF()
       IF elec=1
	  control=.T.
	  SELECT 1
	   USE dat_empl INDEX nitem,nombre,codigo,eventual,ci
	   MESSBAR_ON("Grabando ...",'N/W','R/W')
	   MESSBAR_OFF()
	   IF nnb="ACEFALIA" .OR. nnb="COMISION"
	      GO rr
	   ELSE
	      APPEND BLANK
	   ENDIF
	   REPLACE     codigo WITH codi
	   REPLACE     nitem WITH ittem
	   REPLACE     nombre WITH nomb
	   REPLACE     jor_trab WITH ljor
	   REPLACE     fech_ing WITH lfech
	   REPLACE     tip_empl WITH ltip
	   REPLACE     dias_trab WITH 30
	   REPLACE     p_prof WITH lp_prof
           REPLACE     fecp    WITH lfecp
           REPLACE     feemer  WITH lfeemer
	   REPLACE     asociacion WITH asoc
	   REPLACE     nivel WITH lnivel
	   REPLACE     extension WITH ext
	   REPLACE     memo WITH lmemo,;
		       ci   WITH id,;
		       sexo WITH sex,;
		       prof WITH lprof,;
		       duracion WITH dur,;
		       serv_emerg WITH s_eme
      ENDIF

    ENDIF

       CLOSE_WINDOW(pan5)
       SELECT(area2)
RETURN

*****************************************************************************
*            Validaci¢n del Cargo                                           *
*****************************************************************************
FUNCTION val_car(h)
      mm    = entrada_22(2)
      mm[2] = ALLTRIM(mm[2])
      IF mm[2]!="  "
	 @ 10,22 SAY SPACE(45)
	 @ 10,22 SAY mm[2]
	 SHOWGET("lcargo",mm[1])
      ELSE
	 @ 10,22 SAY SPACE(45)
	 @ 10,22 SAY h
	 SHOWGET("lcargo",h)
      ENDIF
      SET CURSOR ON
RETURN .T.

*****************************************************************************
*            Validaci¢n del Departamento                                    *
*****************************************************************************
FUNCTION val_dep(h)
      n1=entrada_22(1)
      IF n1[2]!="   "
	 @12,27 SAY SPACE(50)
	 @12,27 SAY n1[2]
	SHOWGET("ldepto",n1[1])
      ELSE
	 @12,27 SAY SPACE(50)
	 @12,27 SAY h
	 SHOWGET("ldeptop",h)
      ENDIF
	 SET CURSOR ON
RETURN .T.

*****************************************************************************
*            Validaci¢n del Subprograma                                     *
*****************************************************************************
FUNCTION  val_subp(e,fil,col)
LOCAL sub:="   "

       sub=menuv(6,17,veop1,vere1,"W+/R",.T.)
       IF sub="01" .OR. sub="02"
	 SHOWGET("lsub",sub)
	 @ fil,col SAY SPACE(20)
	 @ fil,col SAY veop1[VAL(sub)]
	 SET CURSOR ON
      ELSE
	SHOWGET("lsub",e)
      ENDIF

RETURN .T.

*****************************************************************************
*            Validaci¢n de la Actividad                                     *
*****************************************************************************
FUNCTION  val_act(e)
LOCAL sub
     sub=menuv(7,17,veop2,vere2,"W+/R",.T.)
     IF sub="01" .or. sub="02" .or. sub="03"
       SHOWGET("lact",sub)
       @ 9,25 SAY SPACE(20)
       @ 9,25 SAY veop2[VAL(sub)]
       SET CURSOR ON
     ELSE
       SHOWGET("lact",e)
     ENDIF
RETURN .T.

*****************************************************************************************
*            Modifica los datos del tipo del personal (administrativo,m‚dico,param‚dico *                                 *
*****************************************************************************************
FUNCTION  val_per(per)
LOCAL sub
     sub=menuv(11,19,veop3,vere3,"W+/R",.T.)
     IF sub="001" .or. sub="002" .or. sub="003"
	SHOWGET("lper",sub)
	@ 11,23 SAY SPACE(20)
	@ 11,23 SAY veop3[VAL(sub)]
	SET CURSOR ON
    ELSE
       SHOWGET("lper",per)
    ENDIF

RETURN .T.

*****************************************************************************
*            Validaci¢n de la regional
*****************************************************************************
FUNCTION  val_reg()
LOCAL veop:={"COCHABAMBA","LA PAZ","SANTA CRUZ","CAMIRI"}
LOCAL vere:={"CBB","LP","STC","CAM"}
LOCAL sub,ii
      sub=menuv(2,15,veop,vere,"W+/R",.F.)
      SHOWGET("lregional",sub)

      FOR ii=1 TO 4
	 IF sub=vere[ii]
	   EXIT
	 ENDIF
      ENDFOR
      @ 4,23 say veop[ii]
      SET CURSOR ON
RETURN .T.

*****************************************************************************
*            Validaci¢n del nivel salarial                                  *
*****************************************************************************
FUNCTION val_niv(h)
      mm=entrada_22(3)
      IF mm[1]!="   "
	 SHOWGET("lnivel",mm[1])
      ELSE
	 SHOWGET("lnivel",h)
      ENDIF
      SET CURSOR ON
RETURN .T.

*****************************************************************************
*            Validaci¢n de los bonos irradiaci¢n y emergencia               *
*****************************************************************************
FUNCTION val_bon(k)
LOCAL dd:={"Subsidio Irradiaci¢n","Servicio de Emergencia"},bonif
	inkey()
	bon=DIALOG2(14,8,dd,k)
	SHOWGET("bon",bon)
RETURN .T.

FUNCTION val_asoc(l)
       INKEY()
       descuentos=DIALOG2(7,31,bb,l)
       SHOWGET("descuentos",descuentos)
RETURN .T.

*****************************************************************************
*            Inicializa variables                                           *
*****************************************************************************
FUNCTION Iniciar_i
PUBLIC lfech:=CTOD(""),lprof:=space(40),ldocs:=space(8),ltip:=" ",lfech_r:=CTOD("")
PUBLIC ljor:=0
RETURN

*****************************************************************************
*            Validaci¢n de los documentos de entrada                        *
*****************************************************************************
FUNCTION  val_doc(ddd)
LOCAL vep:={"SOLICITUD DE EMPLEO","CERTIF. BUENA CONDUCTA","CERTIF. EXAMEN MEDICO",;
	     "DOCUMS. MILITARES","DOCUMS. SUFRAGIO","CERTIF. NACIMIENTO","CALIF. MERITOS",;
	     "FOTOGRAFIAS","CONTRATO DE TRABAJO"}

       INKEY()
       ddd=DIALOG2(6,30,vep,ddd)
       SHOWGET("ldocs",ddd)

RETURN .T.

*****************************************************************************
*            Validaci¢n del tipo de empleado                                *
*****************************************************************************
FUNCTION  val_tip(e)
LOCAL veop:={"DE PLANTA","EVENTUAL"}
LOCAL vere:={"P","E"}
LOCAL sub
      IF !(e $ "PE")
      sub=menuv(7,17,veop,vere,"W+/R",.F.)
      SHOWGET("ltip",sub)
      ENDIF
RETURN .T.

*****************************************************************************
*            Validaci¢n de la jornada de trabajo                            *
*****************************************************************************
FUNCTION  val_jor()
LOCAL veop:={"TIEMPO COMPLETO","  MEDIO TIEMPO   "}
LOCAL vere:={1,0.5}
LOCAL sub
      sub=menuv(9,25,veop,vere,"W+/R",.F.)
      SHOWGET("ljor",sub)
      IF sub=1
	  @9,37 SAY veop[1] color "N/BG+"
      ELSE
	  @9,37 SAY veop[2] COLOR  "N/BG+" //"R/B"
      ENDIF
RETURN .T.

//Realiz  un listado de los datos personales y laborales de un empleado

FUNCTION mostrar(ite,nn,pos)
LOCAL tituloscrren, referescreen, rec
LOCAL lci:=space(13),lnmb:=" ",lnm
LOCAL pp[47],lfech_ing:=CTOD("  /  /    "),lcargo,ldepto,lsueld,lmemo,lsubp,lact
LOCAL ret:=SPACE(8),pan6,elec,lug
LOCAL veop:={"LP","CBB","STC","OR","CHU","PAN","TAR","BEN","POT"}

     IF nn="ACEFALIA"
	 MENSAJE('No existe Reporte para este Item ',10,17,'W*/R+',.T.,.F.)
	 RETURN ''
     ENDIF

     ww:=SELECT()
     IF !STARTREPORT(80,'O',4,5)
	 SELECT(ww)
	 RETURN ' '
     ENDIF

     iniciar()
     iniciar_i()
     iniciar_p()
       SELECT 1
	USE dat_empl INDEX nitem,nombre,codigo,eventual,ci
	 IF pos=2
	   SET ORDER TO 2
	   SEEK nn
	 ELSE
	    SEEK ite
	 ENDIF

	 IF FOUND()
	   lnm   = nn
	   lsexo = sexo
           lafp  = afp
	   ljor  = jor_trab
	   ltip  = tip_empl
	   lprof = prof
	   lfech_ing = fech_ing
	   lcargo    = bus_cargo(codigo)
	   ldepto    = bus_sector(codigo)
	   lsueld    = STR(sueldo(nivel,jor_trab))
	   lmemo     = memo
	   lsubp     = veop1[VAL(LEFT(codigo,2))]
	   lact      = veop2[VAL(SUBSTR(codigo,3,2))]
	   lci       = ci

	   ldomicilio = domicilio
	   ltelefono  = telefono
	   lext       = extension
	   lfecha_nac = '         '

	   IF fecha_nac!=CTOD("  /  /    ")
		 lfecha_nac=FECHALIT(fecha_nac,10,0)
	   ENDIF

	   lug          = DEPARTA[lugar_nac]
	   lest_civil   = lit_est(est_civil,lsexo)
	   IF nacion=0
	      lnacion = "BOLIVIANO"
	    ELSE
	      lnacion = "EXTRANJERO"
	    ENDIF

	   ldocs        = doc_ingres
	   ret          = grad_inst
	   lnmb         = STUFF(nombre,AT(",",nombre),1," ")
       ELSE
	   FINISHREPORT()
	   RETURN

       ENDIF

	 tituloscreen = 'DATOS DE UN EMPLEADO'
	 referescreen = ' Ref: Datos personales y laborales'

	 pp[1]= '<,057,07,'+la_empresa
	 pp[2]= '<,155,07,'+el_Lugar
	 pp[3]= '^,185,00,'+tituloscreen
	 pp[4]= '^,000,00, ' +  "Cochabamba, " + FECHALIT(DATE(),3,3)
	 pp[5]= '<,000,00'
	 pp[6]= '<,009,7,'+ referescreen
	 pp[7]= '^,185,00,Antecedentes Personales'
	 pp[8]= '<,024,5,'+REPLICATE('_',70)
	 pp[9]= '<,024,5, Apellidos y Nombres         :' + lnmb
	 pp[10]='<,024,5, Fecha y Lugar de Nacimiento :'+STUFF(lfecha_nac,1,2,"")+ ' ' +lug
	 pp[11]='<,024,5, Nacionalidad                :'+lnacion+'  Estado Civil '+lest_civil
	 pp[12]='<,024,5, Domicilio                   :'+ldomicilio
	 pp[13]='<,024,5, Carnet de Identidad         :'+lci + ' Ext. ' + lext
	 pp[14]='<,024,5, Profesi¢n                   :'+lprof
	 pp[15]='<,024,5,'
	 pp[16]='^,185,00,Grado de Instrucci¢n'
	 pp[17]='<,000,00'
	 pp[18]='<,000,00'
	 pp[19]='<,024,5, Primaria.......:'+SUBSTR(ret,1,1)
	 pp[20]='<,024,5, Secundaria.....:'+SUBSTR(ret,2,1)
	 pp[21]='<,024,5, T‚cnica........:'+SUBSTR(ret,3,1)
	 pp[22]='<,024,5, Universitaria..:'+SUBSTR(ret,4,1)
	 pp[23]='<,024,5, Post Grado.....:'+SUBSTR(ret,5,1)
	 pp[24]='<,024,5, Especializaci¢n:'+SUBSTR(ret,6,1)
	 pp[25]='<,000,00'
	 pp[26]='<,000,00'
	 pp[27]='^,185,00,Antecedentes Laborales'
	 pp[28]='<,024,5, Fecha de Ingreso  :' +FECHALIT(lfech_ing,10,0)
	 pp[29]='<,024,5, Memorandum No.    :'+lmemo
	 pp[30]='<,024,5, Item No.          :'+ite
	 pp[31]='<,024,5, Sub-Programa      :'+lsubp
	 pp[32]='<,024,5, Actividad         :'+lact
	 pp[33]='<,024,5, Profesi¢n         :'+lprof
	 pp[34]='<,024,5, Cargo             :'+lcargo
	 pp[35]='<,024,5, Departamento      :'+ldepto
	 pp[36]='<,024,5, Sueldo base       :'+lsueld
	 pp[37]='<,000,00'
	 pp[38]='^,185,00,Documentos de Ingreso'
	 pp[39]='<,000,00'
	 pp[40]='<,024,5, Solicitud de empleo................: '+ SUBSTR(ldocs,1,1)
	 pp[41]='<,024,5, Certificado de buena Conducta......: '+ SUBSTR(ldocs,2,1)
	 pp[42]='<,024,5, Certificado de ex men m‚dico.......: '+ SUBSTR(ldocs,3,1)
	 pp[43]='<,024,5, Documentos militares y de sufragio.: '+ SUBSTR(ldocs,4,1)
	 pp[44]='<,024,5, Certificado de nacimiento..........: '+ SUBSTR(ldocs,5,1)
	 pp[45]='<,024,5, Calificaci¢n de meritos............: '+ SUBSTR(ldocs,6,1)
	 pp[46]='<,024,5, Fotografias........................: '+ SUBSTR(ldocs,7,1)
	 pp[47]='<,024,5, Contrato de trabajo................: '+ SUBSTR(ldocs,8,1)

	 EVERYHEADER (@pp,47)

	 pp[1]= la_empresa
	 pp[2]= el_Lugar
	 pp[3]= tituloscreen

//pp[1] = '^,002,00, ................................................................................................................................................................................... Continua en la Siguiente Pagina'
//EVERYTRAILER(@pp,1)

	 pp[4]=  "Cochabamba, " + FECHALIT(DATE(),3,3)
	 pp[5]= ''
	 pp[6]= referescreen
	 pp[7]='                  Antecedentes Personales'
	 pp[8]= REPLICATE('_',70)
	 pp[9]= 'Apellidos y Nombres         :'+lnmb
	 pp[10]='Fecha y Lugar de Nacimiento :'+STUFF(lfecha_nac,1,2,"")+ ' '+lug
	 pp[11]='Nacionalidad                :'+lnacion+'  Estado Civil '+lest_civil
	 pp[12]='Domicilio                   :'+ldomicilio
	 pp[13]='Carnet de Identidad         :'+lci + 'Ext.  '+lext
	 pp[14]='Profesi¢n                   :'+ lprof
	 pp[15]=' '
	 pp[16]='                 Grado de Instrucci¢n'
	 pp[17]=''
	 pp[18]='Primaria        '+SUBSTR(ret,1,1)
	 pp[19]='Secundaria......'+SUBSTR(ret,2,1)
	 pp[20]='T‚cnica.........'+SUBSTR(ret,3,1)
	 pp[21]='Universitaria...'+SUBSTR(ret,4,1)
	 pp[22]='Post Grado......'+SUBSTR(ret,5,1)
	 pp[23]='Especializaci¢n.'+SUBSTR(ret,6,1)
	 pp[24]=''
	 pp[25]=''
	 pp[26]='                  Antecedentes Laborales'
	 pp[27]=''
	 pp[28]='Fecha de Ingreso  :' +FECHALIT(lfech_ing,10,0)
	 pp[29]='Memorandum No.    :'+lmemo
	 pp[30]='Item No.          :'+ite
	 pp[31]='Sub-Programa      :'+lsubp
	 pp[32]='Actividad         :'+lact
	 pp[33]='Cargo             :'+lcargo
	 pp[34]='Departamento      :'+ldepto
	 pp[35]='Sueldo base Bs.   :'+lsueld
	 pp[36]=''
	 pp[37]='                  Documentos de Ingreso'
	 pp[38]=''
	 pp[39]='Solicitud de empleo................: '+ SUBSTR(ldocs,1,1)
	 pp[40]='Certificado de buena Conducta......: '+ SUBSTR(ldocs,2,1)
	 pp[41]='Certificado de ex men m‚dico.......: '+ SUBSTR(ldocs,3,1)
	 pp[42]='Documentos militares y de sufragio : '+ SUBSTR(ldocs,4,1)
	 pp[43]='Certificado de nacimiento..........: '+ SUBSTR(ldocs,5,1)
	 pp[44]='Calificaci¢n de meritos............: '+ SUBSTR(ldocs,6,1)
	 pp[45]='Fotografias........................: '+ SUBSTR(ldocs,7,1)
	 pp[46]='Contrato de trabajo................: '+ SUBSTR(ldocs,8,1)

	 HEADERSCREEN(@pp,46)

   pp[1] = ''
   IF !EVERYROW(REPLICATE("_",70),pp,.T.)
       return ''
   ENDIF

   FINISHREPORT()

   SELECT(ww)
RETURN  ''

FUNCTION lit_est(es,sex)
LOCAL ll:={'SOLTER','CASAD','DIVORCIAD','VIUD'},pre,resul

      IF sex='F'
	 pre='A'
      ELSE
	 pre='O'
      ENDIF

      DO CASE
	 CASE es='S'
	      resul=ll[1]+pre
	 CASE es='C'
	      resul=ll[2]+pre
	 CASE es='D'
	      resul=ll[3]+pre
	 CASE es='V'
	      resul=ll[4]+pre
      ENDCASE
RETURN resul

FUNCTION val_item(ii,nb)
LOCAL it:=.F.
 ooo:=SELECT()
    SELECT 1
    USE dat_empl INDEX nitem,nombre,codigo,eventual,ci
    SET ORDER TO 1
    SEEK ALLTRIM("CBB"+"-"+ii)
    IF FOUND() //.AND. ii!=SPACE(20) .AND. !(nb="ACEFALIA" .OR. nb="COMISION")
	  cad="<ERROR> NUMERO DE ITEM DUPLICADO        Presione cualquier Tecla para Continuar"
	  MESSBAR_ON(cad,'N/W','R*/W')
	  INKEY(LEN(cad))
	  MESSBAR_OFF()
    ELSE
       it=.T.
    ENDIF
 SELECT(ooo)
RETURN it

FUNCTION acef_(parametro)
LOCAL codi:="000000000000" ,j,cc,asoc:=SPACE(46)

      tt:=SELECT()
      bb=cargar()
      iniciar_p()
      pan5=OPEN_WINDOW(3,3,18,78,1,'Datos Laborales',"N/BG+",2,.T.)
      SET COLOR TO "N+/BG+"
      SET CURSOR ON

      @ 4,7   SAY "Regional : Cochabamba" COLOR "B/W"
      @ 6,7   SAY "Item No. :" GET litem  PICTURE "999999" VALID val_item(litem) COLOR "B/W"
      @ 7,7   SAY "Nivel    :" GET lnivel  VALID val_niv()  COLOR "B/W"
      @ 8,7   SAY "Sub-Programa :" GET lsub VALID val_subp(lsub,8,25)  COLOR "B/W"
      @ 9,7   SAY "Actividad    :" get lact VALID val_act(lact)   COLOR "B/W"
      @ 10,7  SAY "Cargo    :" GET lcargo VALID val_car(lcargo)   COLOR "B/W"
      @ 11,7  SAY "Personal :" get lper VALID val_per(lper)  COLOR "B/W"
      @ 12,7  SAY "Departamento :" GET ldepto VALID val_dep(ldepto)  COLOR "B/W"

      READ
      IF LASTKEY()!=27 //K_ESC
	 ittem="CBB"+"-"+litem
	 DO CASE
	     CASE lper=="001"
		lper="M"
	     CASE lper=="002"
		lper="P"
	     CASE lper=="003"
	       lper="A"
	 ENDCASE

	 codi=lsub+lact+n1[1]+lper+mm[1]+" "

	 bo=left(bon,1) //radiaci¢n
	 IF bo="X"
	    codi=LEFT(codi,11)+"R"
	 ENDIF
       asoc=""
       FOR J=1 TO 9
	    CC=SUBSTR(descuentos,j,1)
	    IF cc=="X"
	       asoc=asoc+"0"+CHR(J+48)
	    ELSE
	      asoc=asoc+"00"
	    ENDIF
       ENDFOR

       FOR J=10 TO 23
	    CC=SUBSTR(descuentos,j,1)
	    IF cc=="X"
	      asoc=asoc+alltrim(STR(J))
	    ELSE
	    asoc=asoc+"00"
	    ENDIF
       ENDFOR

       MESSBAR_ON('<Grabar> Almacena todos los datos del empleado ','N/W','R/W')
       elec=MULTIPLE({"Grabar","Cancelar"},2,21,23,.F.)

       MESSBAR_OFF()
       IF elec=1
	  control=.T.
	  SELECT 1
	  USE dat_empl INDEX nitem,nombre,codigo,eventual,ci
	   MESSBAR_ON("Grabando ...",'N/W','R/W')
	   APPEND BLANK
	   REPLACE     codigo WITH codi
	   REPLACE     nitem  WITH ittem,;
		       nombre WITH parametro,;
		       asociacion WITH asoc
	   MESSBAR_OFF()
      ENDIF
    ENDIF
   CLOSE_WINDOW(pan5)
   SELECT(tt)
RETURN


FUNCTION   transfer(ni1,ni2)
LOCAL pan,conf:="S",ind
LOCAL campo1:={},campo2:={},campo3:={}
      area:= SELECT()
      ind  = INDEXORD()

      IF ni1[1]!=ni2[1]
	    pan=OPEN_WINDOW(10,16,20,75,1,'Transferencia',"B/N",5,.T.)
	    SET COLOR TO "W/N"
	    @ 12,18 SAY "Confirmar transferencia entre :"
	    @ 14,24 SAY ni1[1] +"   " + ni1[2]
	    @ 15,37 SAY "Y"
	    @ 16,24 SAY ni2[1] +"   " + ni2[2]
	    @ 18,24 SAY " S/N ?" GET conf PICTURE "@!" VALID conf $ "SN"
	    READ
	  IF LASTKEY()!=27 .AND. conf="S"
	    area103:=SELECT()
	    SELECT 1
	    USE dat_empl INDEX nitem,nombre,codigo,eventual,ci
	    SET ORDER TO 1
	    SEEK ALLTRIM(ni1[1])
	    IF FOUND()
	      REPLACE nitem WITH ni2[1]
	      REPLACE codigo WITH ni2[3]
	    ENDIF

	    SELECT 1
	     USE dat_empl INDEX nitem,nombre,codigo,eventual,ci
	    SET ORDER TO 1
	    SEEK ALLTRIM(ni2[1])
	    IF FOUND()
	      REPLACE nitem WITH ni1[1]
	      REPLACE codigo WITH ni1[3]
	    ENDIF

	    SELECT 22
	    USE iva INDEX iva_item
	    SEEK ALLTRIM(ni1[1])
	    REPLACE nitem WITH ni2[1]

	    SELECT 22
	    USE iva INDEX iva_item
	    SEEK ALLTRIM(ni2[1])
	    REPLACE nitem WITH ni1[1]

	    interc(ni1[1]+'01P',ni2[1]+'01P')
	    interc(ni1[1]+'01H',ni2[1]+'01H')

	    /* SELECT 21
	    USE (dr+"prestamo") INDEX (dr+"prest_it")
	    SEEK ALLTRIM(ni1[1])+'01P'
	    IF FOUND()
	      REPLACE nitem WITH ni2[1]+'01P'
	    ENDIF
	    BROWSE()
	    SELECT 21
	    USE (dr+"prestamo") INDEX (dr+"prest_it")
	    SEEK ALLTRIM(ni1[1])+'01H'
	    IF FOUND()
	      REPLACE nitem WITH ni2[1]+'01H'
	    ENDIF
	    BROWSE()
	    SELECT 21
	    USE (dr+"prestamo") INDEX (dr+"prest_it")
	    SEEK ALLTRIM(ni2[1])+'01P'
	    IF FOUND()
	      REPLACE nitem WITH ni1[1]+'01P'
	    ENDIF
	    BROWSE()
	    SELECT 21
	    USE (dr+"prestamo") INDEX (dr+"prest_it")
	    SEEK ALLTRIM(ni2[1])+'01H'
	    IF FOUND()
	      REPLACE nitem WITH ni1[1]+'01H'
	    ENDIF
	    BROWSE() */
	    SELECT(area103)
	  ENDIF
	  CLOSE_WINDOW(pan)
    ELSE
	MENSAJE("Error no es posible realizar la Transferencia" ,10,17,'W*/R+',.T.,.F.)
    ENDIF

    SELECT(area)
    SET ORDER TO ind
RETURN

// Funci'on de Intercambio de registros
FUNCTION interc(A,B)
LOCAL va:=.F.,vb:=.F.
LOCAL dira,dirb

	    SELECT 21
	    USE (dr+"prestamo") INDEX (dr+"prest_it")
	    SEEK ALLTRIM(A)
	    IF FOUND()
		va   =.T.
		dira = RECNO()
	    ENDIF

	    SELECT 21
	    USE (dr+"prestamo") INDEX (dr+"prest_it")
	    SEEK ALLTRIM(B)
	    IF FOUND()
	       vb   =.T.
	       dirb = RECNO()
	    ENDIF

	    IF va .AND. vb
	       GO dira
	       REPLACE nitem WITH B
	       GO dirb
	       REPLACE nitem WITH A
	    ENDIF

	    IF va .AND. vb=.F.
	       GO dira
	       REPLACE nitem WITH B
	    ENDIF

	    IF va=.F. .AND. vb
	       GO dirb
	       REPLACE nitem WITH A
	    ENDIF
RETURN

FUNCTION renumera(cod,ar_sel,ind)
local nn,i,tt,f:=.F.,g:=.F.
	 area:=select()
	 // Verificar si tiene prestamos

	 SELECT 21
	 USE (dr+"prestamo") INDEX (dr+"prest_it")
	 SEEK ALLTRIM(cod)
	 IF FOUND()
	     f = .T.
	 ENDIF

	 SELECT 91
	 USE presta INDEX presta_it
	 SEEK ALLTRIM(cod)
	 IF FOUND()  .OR. f
	     MENSAJE("No se puede Eliminar los datos. El empleado todavia tiene pr‚stamos." ,10,17,'W*/R+',.T.,.F.)
	     SELECT(area)
	     RETURN
	 ENDIF

	 SELECT(ar_sel)
	 SET ORDER TO ind
	 SEEK cod
	  SKIP

	  tt=SUBSTR(nitem,8,1)
	 DO WHILE tt!=" "
	   SKIP
	   tt = substr(nitem,8,1)
	 END DO
	 nt = nitem
	 SEEK cod
	 DELETE
	 PACK

	SELECT(ar_sel)
	SET ORDER TO ind
	 SEEK nt
	 DO WHILE !EOF()
	  tem=substr(nitem,8,1)
	   IF tem=" "
		 i= val(substr(nitem,5,3))
	     SELECT(ar_sel)
	     REPLACE nitem WITH "CBB-"+padl(i-1,3,"0")
	   ENDIF
	   SKIP
	 END DO

	 MENSAJE("Borrando ...." ,10,17,'W*/R+',.T.,.F.)
	 SELECT(area)
RETURN

