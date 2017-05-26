
*****************************************************************************
* Modulo : Transacciones Financieras                                        *
* Autor  : Richard Jim‚nez Velasco                                          *
* Fecha  : Septiembre 23 de 1994                                            *
* Version : 1.00                                                            *
*                                                                           *
*****************************************************************************

#include "inkey.ch"
#include "dbedit.ch"
#include "setcurs.ch"
#include "common.ch"

SET DATE FRENCH


FUNCTION man_finan
PUBLIC vect:={},vec_cod:={}

      IF cerr_abi="A"
	area11:=SELECT()
	llen_acre()
	pan0=OPEN_WINDOW(0,0,23,79,1,'Transacciones Financieras ',"B/W+",5,.T.)
//	MESSBAR_ON(' <F2> Anticip. Descu.  <F3> IVA  <F4> Cr‚dito F.C.CPS  <F5> Otros Cr‚ditos  <F6> Reten.Jud.','N/W','R/W')
	MESSBAR_ON(' <F2> Ant.Desc  <F3> IVA  <F4> Cr‚d.F.C.CPS  <F5> Otros Cr‚ds. <F6> Reten.Jud. <F7> Otros','N/W','R/W')
	IF mes!="@"
	  datos_sal()
	  @5,12 SAY SPACE(12) COLOR "B/W"
	  @5,12 SAY mes COLOR "B/W"
	  bus_finan(7,5,20,65)
	ENDIF
	  CLOSE_WINDOW(pan0)
	  SELECT(area11)
       ELSE
         MENSAJE("Ya no es Posible Realizar Transacciones en este mes,todas las operaciones fueron CERRADAS",12,17,'W/R+',.T.,.F.)
       ENDIF
RETURN ''

*  -------------------------------------------------------
* Funci¢n que hace la busqueda por contexto con 2 indices
* ---------------------------------------------------------

FUNCTION Bus_Finan(fila1,columna1,fila2,columna2)

DECLARE vec_campos := {"nitem","nombre"}
DECLARE vec_pict   := {"@S10", "@S40"}
DECLARE vec_nombre := {"No. Item","Nombre del empleado"}

PRIVATE  codigo_aux,a,formato
PRIVATE  nro,pantalla, pantalla1, antrec[2],antcol,hostkey[2],f1,f2,c1,c2


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


  DBEDIT (f1,c1,f2,c2, vec_campos, "conteF",vec_pict, vec_nombre)

  SELECT(a)

RETURN codigo_aux

//-------------------------------------------------------------------------

FUNCTION conteF(modo, poscol)
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

      CASE nkey == -1             && F2 ANTICIPOS
	 TONE (1000,0)
	 marcar(nitem,nombre)
	 IF es(nombre);anti_descu(nitem,nombre,RECNO());ENDIF
	 KEYBOARD(CHR(24))
      CASE nkey == -2             && F3/DESCUENTOS
	 TONE (1050,0)
	 marcar(nitem,nombre)
	 IF es(nombre);i_v_a(nitem,nombre,mes);ENDIF
	 KEYBOARD(CHR(24))
      CASE nkey == -3             && F4   CREDITOS,PRESTAMOS DE INSTITUCIONES ,ETC
	 TONE (1000,0)
	 marcar(nitem,nombre)        // Fondo complementario CPS
	 IF es(nombre);prest_caja(nitem,nombre,RECNO());ENDIF
      CASE nkey == -4             && F5
	 TONE (1000,0)
	 marcar(nitem,nombre)
	 IF es(nombre);credito(nitem,nombre);ENDIF //OTROS CREDITOS
      CASE nkey == -5             && F6
	 TONE (1000,0)
	 marcar(nitem,nombre)
	 IF es(nombre);retencion(nitem,nombre,RECNO());ENDIF
    CASE nkey == -6             && F7
	 TONE (1000,0)
	 marcar(nitem,nombre)
         IF es(nombre);modif_neto(nitem,nombre);ENDIF

  ENDCASE

RETURN 1

FUNCTION anti_descu(ite,nom,rr)
LOCAL pp3,lpul:=0,lref:=0,elec,lant:=0,lmul:=0,lmul1:=0,mul_sind:=''

	    ss:=SELECT()
	     SELECT 32
	     USE personal INDEX perso_it,perso_nom
	      SEEK alltrim(ite)
	      IF FOUND()
		  lpul=pulperia
		  lref=refrigerio
		  lmul=otra_multa
		  lmul1=mul_medico
	      ENDIF
	     SELECT 16
	     USE hrs_eme  INDEX extra_it
	     SEEK alltrim(ite)
	     IF FOUND()
	       lant=anticipo
	     ENDIF
	 IF (lant>0 .OR. lpul>0 .OR. lref>0 .OR. lmul>0)
	    elec=MULTI_OPCION("Error se intenta registrar nuevamente al empleado el mismo MES",{"Revisar","Salir"},2,10,.T.)
	   IF elec=2
		SELECT(ss)
		RETURN
	   ENDIF
	 ENDIF

	    pp3=OPEN_WINDOW(6,43,19,79,1,'',"B/N+",5,.T.)
	    SET COLOR TO "W/N+"
	    SET CURSOR ON
	    @ 7 ,45 SAY "Anticipo   Bs. :" GET lant PICTURE "99999.99" VALID lant>=0
	    @ 9 ,45 SAY " Descuentos por concepto de :"
	    @ 11,45 SAY "Anticipo II Bs. :" GET lpul PICTURE "99999.99" VALID lpul>=0
          * @ 11,45 SAY "Pulperia   Bs. :" GET lpul PICTURE "99999.99" VALID lpul>=0
	    @ 13,45 SAY "Refrigerio Bs. :" GET lref PICTURE "99999.99" VALID lref>=0
	    @ 15,45 SAY "Multa al Sindicato :" GET mul_sind VALID val_sindi(@mul_sind)
            @ 17,45 SAY "Monto de la Multa Bs.:" GET lmul PICTURE "99999.99" VALID lMUL>=0

//	    @ 15,45 SAY "Multa Sind. Adm. Bs. :" GET lmul PICTURE "99999.99" VALID lMUL>=0
//	    @ 17,45 SAY "Multa Sind. Med. Bs. :" GET lmul1 PICTURE "99999.99"  VALID lmul1>=0
	    READ
	    IF LASTKEY()!=27
	       elec=MULTIPLE({"Grabar","Salir sin Grabar"},2,21,23,.F.)

	       IF elec=1
		   SELECT 32
		   USE personal INDEX perso_it,perso_nom
		   SEEK ALLTRIM(ite)
		   IF !FOUND()
		    APPEND BLANK
                    REPLACE dias_trab WITH 30
		   ENDIF
                   //otra multa es para el sindicato administrativo
		   REPLACE pulperia     WITH lpul,;
			   refrigerio   WITH lref,;
			   nitem        WITH ite,;
			   nombre       WITH nom
                   DO  CASE
                       CASE mul_sind="A"
                             REPLACE otra_multa WITH lmul
                       CASE mul_sind="M"
                            REPLACE mul_medico WITH lmul
                       CASE mul_sind="E"
                            //REPLACE mul_enfer WITH lmul
                  ENDCASE

		   SELECT 16
		   USE hrs_eme  INDEX extra_it
		   SEEK ite
		   IF !FOUND()
		     APPEND BLANK
		   ENDIF

		   REPLACE  nitem WITH ite,;
			    anticipo WITH lant

		  MENSAJE("Registrando Datos...",12,17,'W/R*',.T.,.F.)
	       ELSE
		  MENSAJE("No se Registraron Datos",12,17,'W/R*',.T.,.F.)
	       ENDIF
	    ENDIF
     CLOSE_WINDOW(pp3)
     SELECT(ss)
RETURN

STATIC FUNCTION prest_caja(ite,nom,re)
LOCAL op:={"Otorgar Pr‚stamo","Registrar Pr‚stamo","Registro Autom tico"},ee,ccc
         qq:= SELECT()
         ee=menu_dat(13,50,op,"R/GW")
         DO CASE
            CASE ee=1
                 cred_pres(ite,nom,re)
            CASE ee=2
                 reg_pre(ite,nom,re)
            CASE ee=3
                 copia_prestamo()
	   /* CASE ee= 4
		 borra_pres(ite,nom,re) */
         ENDCASE
      SELECT(qq)
RETURN

FUNCTION cred_pres(ite,nom,re)

LOCAL pp9,p12,salir
LOCAL lfech_prest:=CTOD("  /  /    "),lmonto:=0,lplazo:=0,lobser:=SPACE(50),lcuota:=0,cant_desc:=0
LOCAL lfech_per:=CTOD("  /  /    "),lplazo_per:=0,lpres_per:=0
LOCAL lfech_hip:=CTOD("  /  /    "),lplazo_hip:=0,lpres_hip:=0,e1:=.F.,e2:=.F.
LOCAL b1:=0,b2:=0,lcuota_per:=0,lcuota_hip:=0,ldesc:=0,pcuota_mes:=0,hcuota_mes:=0

PUBLIC linst:="  ",ltip_pres:=" "

	   ll:=SELECT()
	   pp9=OPEN_WINDOW(6,3,22,77,1,'Registro de Prestamos',"B/N+",5,.T.)
	   SET COLOR TO "W/N+"
	   SET CURSOR ON
	   linst="01"
           ldolar=cambio_dol()
	   @ 8,10  SAY "Instituci¢n   : Fondo Complementario '+la_empresa //Caja Petrolera de Salud" COLOR "GB/N"  // GET linst PICTURE "@!S30" VALID val_inst(ite)
	   @ 9,10  SAY "Solicitante   : "+ALLTRIM(nom) COLOR "GB/N"
           SELECT 91
           USE presta INDEX presta_it

           SELECT 21
	   USE (dr+"prestamo ") INDEX (dr+"prest_it")
	   SEEK ALLTRIM(ite)+"01P"

	   IF FOUND()
	    lplazo_per = plazo
	    lfech_per  = fech_prest
	    lpres_per  = monto
            //lcuota_per = cuota
            pcuota_mes = monto_mes
            b1=1
           ENDIF

          SELECT 21
          use (dr+"prestamo") index (dr+"prest_it")
	  SEEK ALLTRIM(ite)+"01H"

	  IF FOUND()
	    lplazo_hip = plazo
	    lfech_hip  = fech_prest
	    lpres_hip  = monto
            //lcuota_hip = cuota
            hcuota_mes = monto_mes
            b2=1
          ENDIF

           IF  b1=1;ltip_pres="P";ENDIF
           IF  b2=1;ltip_pres="H";ENDIF
           IF b1=1 .AND. b2=1
              ltip_pres="A"
           ENDIF
           IF ltip_pres!=" "
	     elec=MULTI_OPCION("Error se intenta registrar nuevamente al empleado el mismo MES",{"Revisar","Salir"},2,10,.T.)
   	     IF elec=2
                CLOSE_WINDOW(pp9)
		SELECT(ll)
		RETURN
	     ENDIF
           ENDIF
             @ 10,10 SAY "Tipo del Pr‚stamo [P,H,A] :" GET ltip_pres PICTURE "@!" VALID val_tip(ltip_pres)
             READ
             IF LASTKEY()=27;SELECT(ll);CLOSE_WINDOW(pp9);RETURN;ENDIF

           DO CASE
             CASE ltip_pres=="P"
                  @ 13,5 SAY "Pr‚stamo Personal $us:" GET lpres_per PICTURE "999,999.99" VALID lpres_per>=0
                  @ 15,5 SAY "Fecha del Pr‚stamo   :" GET lfech_per VALID lfech_per>=CTOD("  /  /    ")
                  @ 17,5 SAY "Plazo (meses)        :" GET lplazo_per VALID lplazo_per>=0
       //          @ 16,5 SAY "Cuota                :" GEt lcuota_per VALID lcuota_per>=0
                  @ 19,5 SAY "Cuota Mensual ($us)  :" GET pcuota_mes PICTURE "999,999.99" VALID pcuota_mes>=0
            CASE ltip_pres=="H"

                 @ 13,5 SAY "Pr‚stamo Hipotecario $us :" GET lpres_hip PICTURE "999,999.99" VALID lpres_per>=0
                 @ 15,5 SAY "Fecha del Pr‚stamo       :" GET lfech_hip VALID lfech_hip>=CTOD("  /  /    ")
                 @ 17,5 SAY "Plazo (meses)            :" GET lplazo_hip VALID lplazo_hip>=0
         //        @ 16,5 SAY "Cuota                    :" GEt lcuota_hip VALID lcuota_hip>=0
                  @ 19,5 SAY "Cuota Mensual ($us)  :" GET hcuota_mes PICTURE "999,999.99" VALID hcuota_mes>0

            CASE ltip_pres=="A"

                  @ 13,5 SAY "Pr‚stamo Personal $us:" GET lpres_per PICTURE "999,999.99" VALID lpres_per>=0
                  @ 15,5 SAY "Fecha del Pr‚stamo   :" GET lfech_per VALID lfech_per>=CTOD("  /  /    ")
                  @ 17,5 SAY "Plazo (meses)        :" GET lplazo_per VALID lplazo_per>=0
             //     @ 16,5 SAY "Cuota                :" GEt lcuota_per VALID lcuota_per>=0
                  @ 19,5 SAY "Cuota Mensual ($us)  :" GET pcuota_mes PICTURE "999,999.99" VALID pcuota_mes>0

                  @ 13,40 SAY "Pr‚stamo Hipotecario $us :" GET lpres_hip PICTURE "999,999.99" VALID lpres_per>=0
                  @ 15,40 SAY "Fecha del Pr‚stamo       :" GET lfech_hip VALID lfech_hip>=CTOD("  /  /    ")
                  @ 17,40 SAY "Plazo (meses)            :" GET lplazo_hip VALID lplazo_hip>=0
           //       @ 16,40 SAY "Cuota                    :" GEt lcuota_hip VALID lcuota_hip>=0
                  @ 19,40 SAY "Cuota Mensual ($us)  :" GET hcuota_mes PICTURE "999,999.99" VALID hcuota_mes>0
           ENDCASE
           READ

	   IF LASTKEY()!=27

//               @ 20,5 SAY "Monto a Descontar en Bs.  :" GET cant_desc PICTURE "999,999.99" VALID cant_desc>=0
//               READ
//               IF LASTKEY()=27;SELECT(ll);CLOSE_WINDOW(pp9);RETURN;ENDIF

	         elec=MULTIPLE({"Grabar","Salir sin Grabar"},2,21,23,.F.)

	       IF elec=1
		   DO CASE
                     CASE ltip_pres="P"
                       SELECT 21
                       use (dr+"prestamo") index (dr+"prest_it")
 		       SEEK ALLTRIM(ite)+"01P"

	     	       IF !FOUND()
 		         APPEND BLANK
		       ENDIF

		         REPLACE nitem   WITH ALLTRIM(ite)+"01P",;
			      plazo      WITH lplazo_per,;
			      monto      WITH lpres_per,;
			      fech_prest WITH lfech_per,;
                              monto_mes  WITH pcuota_mes

		         p12=MENSAJE("Registrando Datos ...... ",12,17,'W/R+',.T.,.F.)
		         CLOSE_WINDOW(p12)
                   CASE ltip_pres="H"
                       SELECT 21
                       use (dr+"prestamo") index (dr+"prest_it")
 		       SEEK ALLTRIM(ite)+"01H"

	     	      IF !FOUND()
 		        APPEND BLANK
		      ENDIF
		         REPLACE nitem WITH ALLTRIM(ite)+"01"+"H",;
			      plazo WITH lplazo_hip,;
			      monto WITH lpres_hip,;
			      fech_prest WITH lfech_hip,;
                              monto_mes  WITH hcuota_mes

                         p12=MENSAJE("Registrando Datos ...... ",12,17,'W/R+',.T.,.F.)
		         CLOSE_WINDOW(p12)
                    CASE ltip_pres="A"
                       SELECT 21
                       use (dr+"prestamo") index (dr+"prest_it")
 		       SEEK ALLTRIM(ite)+"01"+"P"

                      IF !FOUND()
                        APPEND BLANK
                      ENDIF

	 	         REPLACE nitem WITH ALLTRIM(ite)+"01"+"P",;
			      plazo WITH lplazo_per,;
			      monto WITH lpres_per,;
			      fech_prest WITH lfech_per,;
                              monto_mes  WITH pcuota_mes

                      SELECT 21
                      use (dr+"prestamo") index (dr+"prest_it")
                      SEEK ALLTRIM(ite)+"01"+"H"

                      IF !FOUND()
                        APPEND BLANK
                      ENDIF

		         REPLACE nitem WITH ALLTRIM(ite)+"01"+"H",;
			      plazo WITH lplazo_hip,;
			      monto WITH lpres_hip,;
			      fech_prest WITH lfech_hip,;
                              monto_mes  WITH hcuota_mes

                      p12=MENSAJE("Registrando Datos ...... ",12,17,'W/R+',.T.,.F.)
		      CLOSE_WINDOW(p12)

                  ENDCASE
               ENDIF

	       /*
		   ELSE
		    salir=.T.
		     DO WHILE salir .AND. nitem=ite  .AND. !EOF()
			IF linst=instituci
			   salir=.F.
			ENDIF
		      SKIP
		     ENDDO
		      IF salir  // NO ENCONTRO
			 APPEND BLANK
			 REPLACE nitem      WITH ite,;
				 instituci  WITH linst,;
				 plazo      WITH lplazo,;
				 monto      WITH lmonto,;
				 fech_prest WITH lfech_prest,;
				 cuota      WITH lcuota,;
                                 tip_prest  WITH ltip_pres
		      ELSE
			 MENSAJE("Error no es posible dar Credito",12,27,'W/R+',.T.,.F.)
		      ENDIF
		   ENDIF
	       ENDIF */


	   ENDIF
	  CLOSE_WINDOW(pp9)
	  SELECT(ll)
RETURN

STATIC FUNCTION   reg_pre(ite,nom,re)
LOCAL pp9,p12,salir,ldolar:=0.0,c1:=0,c2:=0
LOCAL lfech_prest:=CTOD("  /  /    "),lmonto:=0,lplazo:=0,lobser:=SPACE(50),lcuota:=0,cant_desc:=0
LOCAL lfech_per:=CTOD("  /  /    "),lplazo_per:=0,lpres_per:=0
LOCAL lfech_hip:=CTOD("  /  /    "),lplazo_hip:=0,lpres_hip:=0,e1:=.F.,e2:=.F.
LOCAL b1:=0,b2:=0,lcuota_per:=0,lcuota_hip:=0,ldesc:=0,per_bol:=0,hip_bol:=0
LOCAL inst
PUBLIC linst:="  ",ltip_pres:=" "

       area123:=SELECT()
           ldolar=cambio_dol()
           SELECT 91
           USE presta INDEX presta_it

           SELECT 21
 	   USE (dr+"prestamo ") INDEX (dr+"prest_it")
	   SEEK ALLTRIM(ite)+"01P"

	   IF FOUND()
	     lplazo_per = plazo
	     lfech_per  = fech_prest
	     lpres_per  = monto
             lcuota_per = cuota
             per_bol    = monto_mes
             b1=1
            ENDIF

/*           SELECT 91
           SEEK ALLTRIM(ite)+"01P"
           IF FOUND()
             per_bol    = monto
           ENDIF

           SELECT 91
           SEEK ALLTRIM(ite)+"01H"
           IF FOUND()
             hip_bol    = monto
           ENDIF */


           SELECT 21
	   SEEK ALLTRIM(ite)+"01H"

	   IF FOUND()
	     lplazo_hip = plazo
	     lfech_hip  = fech_prest
	     lpres_hip  = monto
             lcuota_hip = cuota
             hip_bol    = monto_mes
             b2=1
           ENDIF

            IF b1=0 .AND. b2=0;SELECT(area123);RETURN;ENDIF
            IF  b1=1;ltip_pres="P";ENDIF
            IF  b2=1;ltip_pres="H";ENDIF
            IF b1=1 .AND. b2=1
               ltip_pres="A"
            ENDIF

            pp9=OPEN_WINDOW(6,3,22,77,1,'Registro de Prestamos',"B/N+",5,.T.)
	    SET COLOR TO "W/N+"
	    SET CURSOR ON
	    linst="01"
            ldolar=cambio_dol()                    //Caja Petrolera de Salud"
            inst = "Instituci¢n   :"+la_empresa
	    @ 8,10  SAY inst   COLOR "GB/N"  // GET linst PICTURE "@!S30" VALID val_inst(ite)
	    @ 9,10  SAY "Solicitante   : "+ALLTRIM(nom) COLOR "GB/N"

            IF ltip_pres!=" "
	      elec=MULTI_OPCION("Error se intenta registrar nuevamente al empleado el mismo MES",{"Revisar","Salir"},2,10,.T.)
   	      IF elec=2
                 CLOSE_WINDOW(pp9)
		 SELECT(ll)
		 RETURN
	      ENDIF
            ENDIF
            @ 10,10 SAY "Cambio del Dolar          :" GET ldolar PICTURE "999.99" VALID ldolar>0
            @ 11,10 SAY "Tipo del Pr‚stamo [P,H,A] :" + ltip_pres

            DO CASE
              CASE ltip_pres="P"
                    c1=per_bol //ldolar

                   @ 13,5 SAY "Pr‚stamo Personal $us:" +STR(lpres_per)
                   @ 14,5 SAY "Fecha del Pr‚stamo   :" +DTOC(lfech_per)
                   @ 15,5 SAY "Plazo (meses)        :" +STR(lplazo_per)
                   @ 16,5 SAY "Cuota                :" GET lcuota_per PICTURE "999" VALID lcuota_per>=0 .AND. lcuota_per<=lplazo_per
                   @ 17,5 SAY "Descuento ($us) :" GET c1  PICTURE "999,999.99" VALID c1>=0
                   READ
                    per_bol=ROUND(c1*ldolar,2)
                   @ 18,5 SAY "Descuento (Bs.) :" GET per_bol PICTURE "999,999.99" VALID per_bol>=0
                   READ
             CASE ltip_pres="H"
                  c2=hip_bol   //ldolar
                  //hipbol=c2*ldolar
                  @ 13,5 SAY "Pr‚stamo Hipotecario $us :" +STR(lpres_hip)
                  @ 14,5 SAY "Fecha del Pr‚stamo       :" +DTOC(lfech_hip)
                  @ 15,5 SAY "Plazo (meses)            :" +STR(lplazo_hip)
                  @ 16,5 SAY "Cuota                   :" GET lcuota_hip PICTURE "9999" VALID lcuota_hip>=0 .AND. lcuota_hip<=lplazo_hip
                  @ 17,5 SAY "Descuento ($us):" GET c2  PICTURE "999,999.99" VALID c2>=0
                  READ
                  hip_bol=ROUND(c2*ldolar,2)

                  @ 18,5 SAY "Descuento (Bs.):" GET hip_bol PICTURE "999,999.99" VALID hip_bol>=0
                  READ
             CASE ltip_pres="A"
                   c1=per_bol           //ldolar

                   c2=hip_bol           //ldolar

                   @ 13,5 SAY "Pr‚stamo Personal $us:" +STR(lpres_per)
                   @ 14,5 SAY "Fecha del Pr‚stamo   :" +DTOC(lfech_per)
                   @ 15,5 SAY "Plazo (meses)        :" +STR(lplazo_per)
                   @ 16,5 SAY "Cuota                :" GET lcuota_per PICTURE "9999" VALID lcuota_per>=0 .AND. lcuota_per<=lplazo_per
                   @ 17,5 SAY "Descuento  ($us) :" GET c1  PICTURE "999,999.99" VALID c1>=0
                   READ
                   per_bol=ROUND(c1*ldolar,2)
                   @ 18,5 SAY "Descuento (Bs.) :" GET per_bol PICTURE "999,999.99" VALID per_bol>=0
                   READ
                  @ 13,40 SAY "Pr‚stamo Hipotecario $us :" +STR(lpres_hip)
                  @ 14,40 SAY "Fecha del Pr‚stamo       :" +DTOC(lfech_hip)
                  @ 15,40 SAY "Plazo (meses)            :" +STR(lplazo_hip)
                  @ 16,40 SAY "Cuota                    :"  GET lcuota_hip PICTURE "9999" VALID lcuota_hip>=0 .AND. lcuota_hip<=lplazo_hip
                  @ 17,40 SAY "Descuento  ($us):" GET c2  PICTURE "999,999.99" VALID c2>=0
                  READ
                  hip_bol=ROUND(c2*ldolar,2)
                  @ 18,40 SAY "Descuento  (Bs.):" GET hip_bol PICTURE "999,999.99" VALID hip_bol>=0
                  READ
                  @ 20,5 SAY "Monto Total Descontar en Bs.  :" +STR(hip_bol+per_bol)

            ENDCASE
            READ


	    IF LASTKEY()!=27
               elec=MULTIPLE({"Grabar","Salir sin Grabar"},2,21,23,.F.)
	       IF elec=1
		   DO CASE
                     CASE ltip_pres=="P"
                          SELECT 91
                          use presta index presta_i
                          SEEK ALLTRIM(ite)+"01P"
                          IF !FOUND()
                            APPEND BLANK
                          ENDIF //c1 remplazado por per_bol
                           REPLACE nitem WITH ALLTRIM(ite)+"01P",;
			           monto WITH c1,;
                                   cuota WITH lcuota_per

                          SELECT 21
                          SEEK ALLTRIM(ite)+"01P"
                          IF FOUND()
                             REPLACE cuota WITH lcuota_per
                          ENDIF


		         p12=MENSAJE("Registrando Datos ...... ",12,17,'W/R+',.T.,.F.)
		         CLOSE_WINDOW(p12)
                   CASE ltip_pres=="H"
                        SELECT 91
                        use presta index presta_i
                        SEEK ALLTRIM(ite)+"01H"
                        IF !FOUND()
                          APPEND BLANK
                        ENDIF //c2 reeplazado por hip_bol
                         REPLACE  nitem WITH ALLTRIM(ite)+"01H"
			 REPLACE  monto WITH c2
                         REPLACE  cuota WITH lcuota_hip

                         SELECT 21
                          SEEK ALLTRIM(ite)+"01H"
                          IF FOUND()
                            REPLACE cuota WITH lcuota_hip
                          ENDIF


                         p12=MENSAJE("Registrando Datos ...... ",12,17,'W/R+',.T.,.F.)
		         CLOSE_WINDOW(p12)
                    CASE ltip_pres=="A"
                       SELECT 91
                       use presta index presta_i
                        SEEK ALLTRIM(ite)+"01P"
                        IF !FOUND()
                          APPEND BLANK
                        ENDIF
                         REPLACE nitem WITH ALLTRIM(ite)+"01P",;
			         monto WITH c1,;
                                 cuota WITH lcuota_per

                       SELECT 91
                       use presta index presta_i
                        SEEK ALLTRIM(ite)+"01H"
                        IF !FOUND()
                          APPEND BLANK
                        ENDIF
                         REPLACE nitem WITH ALLTRIM(ite)+"01H",;
			      monto WITH c2,;
                              cuota WITH lcuota_hip

                         SELECT 21
                          SEEK ALLTRIM(ite)+"01P"
                          IF FOUND()
                           REPLACE cuota WITH lcuota_per
                          ENDIF
                           

                         SELECT 21
                          SEEK ALLTRIM(ite)+"01H"
                          IF FOUND()
                           REPLACE cuota WITH lcuota_hip
                          ENDIF

                      p12=MENSAJE("Registrando Datos ...... ",12,17,'W/R+',.T.,.F.)

                  ENDCASE
               ENDIF
           // Control de Cuotas
               IF lplazo_per=lcuota_per
                    SELECT 21
                    SEEK ALLTRIM(ite)+"01P"
                    IF FOUND()
                      DELETE
                      PACK
                     MENSAJE("Esta es la £ltima cuota de su pr‚stamo  Personal ...",12,17,'W/R+',.T.,.F.)
                    ENDIF
               ENDIF
               IF lplazo_hip=lcuota_hip
                 SELECT 21
                 SEEK ALLTRIM(ite)+"01H"
	         IF FOUND()
                   DELETE
                   PACK
                   MENSAJE("Esta es la £ltima cuota de su pr‚stamo Hipotecario ..",12,17,'W/R+',.T.,.F.)
                 ENDIF
               ENDIF
            ENDIF
       CLOSE_WINDOW(pp9)
       SELECT(area123)
RETURN

STATIC FUNCTION val_tip(hh)
LOCAL ti_pres:={"PERSONAL","HIPOTECARIO","AMBOS"},opcion,pres_cod:={"P","H","A"}
   IF !(hh $ "PHA")
       opcion=menuv(11,40,ti_pres,pres_cod,"W+/R",.F.)
       SHOWGET("ltip_pres",opcion)
   ENDIF
RETURN .T.

//Instituciones de Cr‚dito
FUNCTION val_inst(ite)
LOCAL opcion,ins:=.T.,sal,elec

     IF ite!=SPACE(20)
	 MESSBAR_ON('Presione <ENTER> para seleccionar una opci¢n','N/W','R/W')
	 area4:=SELECT()
	 opcion=menuv(12,30,acre,acre_cod,"W+/R",.F.)
	 SHOWGET("linst",opcion)
	  SELECT 91
	  USE presta INDEX presta_i
	  SEEK ALLTRIM(ite)+linst
	  IF !FOUND()
	    @ 12,32 SAY acre[VAL(opcion)-1] COLOR "W/N"
	  ELSE
	     elec=MULTI_OPCION("Error se intenta registrar nuevamente al empleado el mismo MES",{"Revisar","Salir"},2,10,.T.)
	      IF elec=2
		 ins=.F.
	      ELSE
		 INS=.T.
		 lmont=monto
	     ENDIF
	  ENDIF
     ENDIF
    MESSBAR_OFF()
RETURN ins


FUNCTION i_v_a(it,no,mes3)
LOCAL pp10,ldecla:=0,elec,anterior:=0

       area16:=SELECT()
       SELECT 22
	 USE iva INDEX iva_item
	 SEEK it
         IF FOUND()
	    anterior=nada
        ENDIF
       IF formu101>0
	  ldecla=formu101
	  elec=MULTI_OPCION("Error se intenta registrar nuevamente al empleado el mismo MES",{"Revisar","Salir"},2,10,.T.)
	 IF elec=2
	    SELECT(area16)
	    RETURN
	 ENDIF
	ENDIF
	   pp10=OPEN_WINDOW(6,5,16,76,1,'Declaraci¢n del I.V.A.',"B/N+",5,.T.)
	   SET COLOR TO "W/N+"
	   SET CURSOR ON
	   @ 8,10 SAY "Nombre del Empleado     :"+ALLTRIM(no)
	   @ 10,10 SAY "Saldo Anterior (Bs.)   :" GET anterior  COLOR "B/W" PICTURE "9999,999.99" //+ALLTRIM(STR(anterior))
	   @ 12,10 SAY "Monto a Declarar  Formulario 101 (Bs.) :" GET ldecla COLOR "B/W" PICTURE "9999,999.99" VALID ldecla>=0

	   READ
	   IF LASTKEY()!=27
	       elec=MULTIPLE({"Grabar","Salir sin Grabar"},2,21,23,.F.)
	       IF elec=1
		SELECT 22
		 USE iva INDEX iva_item
		SEEK it
                IF !FOUND()
                  APPEND BLANK
                ENDIF
		 REPLACE formu101 WITH ldecla,;
			 nada     WITH anterior,;
                         nitem    WITH it

		 MENSAJE("Registrando ...",12,17,'W/R+',.T.,.F.)
	       ENDIF
	   ENDIF
	 CLOSE_WINDOW(pp10)
	 SELECT(area16)
RETURN

STATIC FUNCTION cal_ret(nit)


       limpiar()
       area1:=SELECT()
       	 SELECT 32
	 USE personal  INDEX perso_it,perso_nom
	 SEEK alltrim(nit)
	 IF FOUND()
	    dias=dias_trab
         ELSE
           dias=30
	 ENDIF

         SELECT 1
         USE dat_empl index nitem,nombre,codigo,eventual,ci
         SEEK ALLTRIM(nit)

	 IF anio_rec > 0 .OR. mes_rec > 0
		fechita = halla_fecha(fech_ing,anio_rec,mes_rec,dia_rec)
	 ELSE
		fechita = fech_ing
	 ENDIF

	 sue_mens=sueldo(nivel,jor_trab)

	*indice=ind_efec
        ***
	   IF reincorp = "S"
	     indice = efectivo("B",fechita,sue_mens)
	   ELSE
	     indice = efectivo("B",fech_ing,sue_mens)  // Fecha de ingreso institucional
	   ENDIF

        ***

	 bon_antig= bono_antig("B",fech_ing)*jor_trab
         IF dias=0
	   limpiar()
           dias=0
           //iva(nitem,totalG,desc_ley)
	 ENDIF

         IF dias<30
	   sue_mens=ROUND((sue_mens/30)*dias,2)
	   bon_antig=ROUND( (bon_antig/30)*dias,2)
	   indice=ROUND((ind_efec/30)*dias,2)
         ENDIF

         conge:=es_con(nit)   // ingresos congelados
	 IF conge[2]!=0;indice=conge[2];ENDIF
	 IF conge[1]!=0;bon_antig=conge[1];ENDIF


         por_prof = porce("B",1->fech_ing,CTOD("  /  /    "))        // porcentaje profesional
         por_irra = porce("R",1->fech_ing,CTOD("  /  /    "))

         hrs_adic = hrs_adicion(codigo,prof,sue_mens)     // parametro it para octubre 1994
         cat_profe= categ_prof(sue_mens,p_prof,codigo,prof,hrs_adic)

         IF conge[3]=0;sub_irrad= sub_irradia(codigo,fech_ing,sue_mens);ELSE;sub_irrad=conge[3];ENDIF

         emerg    = servicio_em(serv_emerg,sueldo(nivel,1.0),bon_antig,cat_profe)
         v_h      = valor_hora(sexo,prof,sue_mens,bon_antig,cat_profe,dias)
       //v_h      = valor_hora(sexo,prof,sueldo(nivel,1.0),bon_antig,cat_profe,dias)
         hrs_extras= hora_extra(nit,v_h)
         recarg    = recarg_noct(nit,v_h)
         bon_nat   = bonos_mater(nit)

*       	   IF nitem = "CBB-110"
*	      ? "sue_mens "+str(sue_mens)       //gus
*              ? "bon_antig "+str(bon_antig)
*              ? "indice "+str(indice)
*              ? "v_h "+str(v_h)
*              ? "hrs_extras "+str(hrs_extras)

*              ? "dias "+str(dias)


*	   ENDIF

         *bon_nat[4]=60*jor_trab
         bon_nat[4]=95*jor_trab
	 
*              ? "bon_nat4 "+str(bon_nat[4])

         TotalG    = sue_mens+bon_antig+indice+cat_profe+hrs_adic+sub_irrad+emerg+hrs_extras+recarg+bon_nat[1]+;
		bon_nat[2]+bon_nat[3]+bon_nat[4]

*        ? "totalG "+str(TotalG)
*              inkey(0)
       SELECT(area1)

RETURN  totalG

FUNCTION retencion(ii,nn,rr)
LOCAL pp10,elec,lreten:=cal_ret(ii)*0.2,lobser:=SPACE(40)

	 area56:=SELECT()
	 SELECT 32
	 USE personal INDEX perso_it,perso_nom
	 SEEK ii
	 IF FOUND() .AND. reten_jud>0
	     lreten=reten_jud
             lobser=nomb_rete
	     elec=MULTI_OPCION("Error se intenta registrar nuevamente al empleado el mismo MES",{"Revisar","Salir"},2,10,.T.)
	     IF elec=2
	       SELECT(area56)
	       RETURN
	     ENDIF
	 ENDIF
	 pp10=OPEN_WINDOW(10,5,18,76,1,'Retenci¢n Judicial',"B/N+",5,.T.)
	   SET COLOR TO "W/N+"
	   SET CURSOR ON
	   @ 12,10 SAY "Nombre del Empleado   :"+ALLTRIM(nn)
	   @ 14,10 SAY "Monto de la Retenci¢n :" GET lreten PICTURE"999,999.99" VALID lreten>=0
	   @ 16,10 SAY "Observaci¢n  :"GET lobser PICTURE "@S40!"
	   READ
	   IF LASTKEY()!=27
	       elec=MULTIPLE({"Grabar","Salir sin Grabar"},2,21,23,.F.)
	       IF elec=1
	       SELECT 32
	       USE personal INDEX perso_it,perso_nom
	       SEEK ii
	       IF !FOUND()
		  APPEND BLANK
                  REPLACE dias_trab WITH 30
	       ENDIF
		  REPLACE nitem     WITH ii,;
			  nombre    WITH nn,;
			  reten_jud WITH lreten,;
                          nomb_rete WITH lobser
	       ENDIF
               MENSAJE("Registrando Datos ....",12,17,'W/R+',.T.,.F.)
	   ENDIF
	  CLOSE_WINDOW(pp10)
	  SELECT(area56)
RETURN


FUNCTION credito(ite,nom)
LOCAL pp9,p12,salir,ldolar:=4.67
LOCAL lfech_prest:=CTOD("  /  /    "),lplazo:=0,lobser:=SPACE(50),lcuota:=0,cant_desc:=0
PUBLIC linst:="  ",lmont:=0

	   ll:=SELECT()
	   pp9=OPEN_WINDOW(6,5,18,73,1,'Registro de Prestamos',"B/N+",5,.T.)
	   SET COLOR TO "W/N+"
	   SET CURSOR ON
	   @ 10,10 SAY "Solicitante   :"+ALLTRIM(nom)
	   @ 12,10 SAY "Empresa       :" GET linst VALID val_inst(ite)
	   @ 14,10 SAY "Monto a Descontar Bs. :" GET lmont PICTURE "999,999.99" VALID lmont>=0
	   READ

	   IF LASTKEY()!=27
	       elec=MULTIPLE({"Grabar","Salir sin Grabar"},2,21,23,.F.)
	       IF elec=1
		   SELECT 91
		   USE presta INDEX presta_i
		   SEEK ALLTRIM(ite)+linst
		   IF !FOUND()
		      APPEND BLANK
		   ENDIF
		      REPLACE nitem WITH ALLTRIM(ite)+linst,;
			      monto WITH lmont
		     p12=MENSAJE("Registrando Datos ...... ",12,17,'W/R+',.T.,.F.)
		     CLOSE_WINDOW(p12)
	      ENDIF
	   ENDIF
	  CLOSE_WINDOW(pp9)
	  SELECT(ll)
RETURN

STATIC FUNCTION val_sindi(lsindi)
      IF  lsindi <> "D" .AND. lsindi<> "C" .AND. lsindi<>"E"
          lsindi := MENUV(14,59,{"Administrativo","M‚dico","Enfermeras" },{"A","M","E"},'W+/R',.T.,5)
      ENDIF

      IF LASTKEY() = K_ESC
         RETURN .T.
      ENDIF

RETURN .T.

// modif sueldo neto Mayo de 1998

FUNCTION modif_neto(ii,nn)
LOCAL pp11,elec,lneto:=0

	 area57:=SELECT()
	 SELECT 32
	 USE personal INDEX perso_it,perso_nom
	 SEEK ii
	 IF FOUND() .AND. bono>0
	     lneto=bono
	     elec=MULTI_OPCION("Error se intenta registrar nuevamente al empleado el mismo MES",{"Revisar","Salir"},2,10,.T.)
	     IF elec=2
	       SELECT(area57)
	       RETURN
	     ENDIF
	 ENDIF
	 pp11=OPEN_WINDOW(10,5,18,76,1,'Otros',"B/N+",5,.T.)
	   SET COLOR TO "W/N+"
	   SET CURSOR ON
	   @ 12,10 SAY "Nombre del Empleado   :"+ALLTRIM(nn)
	   @ 14,10 SAY "Otros                 :" GET lneto PICTURE"999,999.99" VALID lneto>=0
	   READ
	   IF LASTKEY()!=27
	       elec=MULTIPLE({"Grabar","Salir sin Grabar"},2,21,23,.F.)
	       IF elec=1
	         SELECT 32
	         USE personal INDEX perso_it,perso_nom
	         SEEK ii
	         IF !FOUND()
		   APPEND BLANK
                   REPLACE nitem     WITH ii,;
		          nombre    WITH nn,;
                          dias_trab WITH 30
	         ENDIF
		   REPLACE bono WITH lneto
                   MENSAJE("Registrando Datos ....",12,17,'W/R+',.T.,.F.)
	       ENDIF

	   ENDIF
	  CLOSE_WINDOW(pp11)
	  SELECT(area57)
RETURN
