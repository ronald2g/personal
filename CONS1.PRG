*****************************************************************************
*****************************************************************************
**                                                                          *
**  MODULO              :     CONSULTAS                                     *
**  PARTE DEL SISTEMA   :     REPORTES                                      *
**  PROPIETARIO         :     CAJA PETROLERA DE SALUD                       *
**  AUTOR               :     RICHARD JIMENEZ                               *
**  VERSION             :     1.0                                           *
**  CREACION            :     30 DE DICIEMBRE DE 1994                       *
**  ULTIMA REVISION     :                                                   *
**                                                                          *
*****************************************************************************
*****************************************************************************
**                                                                          *
**  DESCRIPCION     : Emite la papeleta de Pago                             *
**                                                                          *
*****************************************************************************
*****************************************************************************

FUNCTION prest_h_p()
LOCAL pp[20],vec[5],ni,forma1,sh:=0,sp:=0,salir
       crea_pres()

       area102:=SELECT()
       IF !STARTREPORT(80,'R',4,5)
	  RETURN ' '
       ENDIF

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= '^,185,00,'+'PRESTAMOS PERSONALES E HIPOTECARIOS'
	 pp[5]= '^,000,00, ' +  "Cochabamba, "+ FECHALIT(date(),3,3)
	 pp[6]= '<,000,00'
	 pp[7]= '<,009,5,'+ ' Correspondiente al mes de:' +alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[8]='<,009,5,'+' Cambio dolar :'+ALLTRIM(STR(cambio_dol()))
	 pp[9]='<,009,5,'+' Nombre del Empleado       Prest. Personal (Bs.)        Prest. Hipotecario (Bs.)'
	 pp[10]='<,009,5,'+REPLICATE('_',90)

	 EVERYHEADER (@pp,10)
	 pp[1]= la_empresa
	 pp[2]= el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= 'PRESTAMOS PERSONALES E HIPOTECARIOS'
	 pp[5]= "Cochabamba, " //+ FECHALIT(cad_fecha,3,3)
	 pp[6]= ''
	 pp[7]=  'Correspondiente al mes de:'// +lmeses


	 pp[1]="Nombre del Empleado                           Prest. Personal (Bs.)           Prest. Hipotecario (Bs.)"
	 pp[2]=REPLICATE('_',90)
	 forma ="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX       ###,###,###.##"
	 forma1="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                         ###,###,###.##"
	 HEADERSCREEN(@pp,2)
	 salir=.T.
	 SELECT 99
	 USE temp_pres INDEX pnombre
	 GO TOP
	 DO WHILE !EOF()
	   DO CASE
	      CASE tp="P" .AND. monto>0
	       vec[1]=espacios(nombre,31)
	       vec[2]=ROUND(monto,2);sp=sp+monto
	       IF !EVERYROW(forma,vec,.T.)
		salir:=.F.
		EXIT
	       ENDIF
	       IF ultima_linea<6;NEWPAGE();ENDIF

	      CASE tp="H" .AND. monto>0
	       vec[1]=espacios(nombre,31)
	       vec[2]=ROUND(monto,2)
	       sh=sh+monto
	       IF !EVERYROW(forma1,vec,.T.)
		salir:=.F.

		EXIT
	      ENDIF
	      IF ultima_linea<6;NEWPAGE();ENDIF
	   ENDCASE
	  SKIP
	 END DO

	 IF salir=.F.
	   FINISHREPORT()
	   RETURN
	 ENDIF
	 SKIPLINE(2,.T.)
	 forma="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX         ###,###,###.##                    ###,###,###.##"
	 vec[1]=espacios('TOTAL PRESTAMOS ',30)
	 vec[2]=sp
	 vec[3]=sh
	 IF !EVERYROW(forma,vec,.T.)
	   FINISHREPORT()
	   SELECT(area102)
	   RETURN
	 ENDIF
	 CLOSE ALL
	 FINISHREPORT()
	 SELECT(area102)
RETURN


STATIC FUNCTION crea_pres()
LOCAL vec[3],adbf:={}

	aa1:=SELECT()
/*         AADD(adbf,{"nombre","C",50,0})
	 AADD(adbf,{"monto","N",10,2})
	 AADD(adbf,{"tp","C",1,0})

	 DBCREATE("temp_pres",adbf)
	 SELECT 99
	 USE temp_pres
	 INDEX ON nombre TO pnombre */

	 SELECT 99
	 USE temp_pres INDEX pnombre
	 ZAP

	 SELECT 91
	 USE presta INDEX presta_i

	 GO TOP
	 DO WHILE !EOF()
	 tip_prest=RIGHT(ALLTRIM(nitem),1)
	   DO CASE
	    CASE tip_prest="P" .AND. monto>0
	      ni=LEFT(nitem,7)
	      vec[1]=rec_apell(ni,0)
	      vec[2]=ROUND(monto*ldol,2)
	      SELECT 99
	      APPEND BLANK
	      REPLACE nombre WITH vec[1],;
		      monto  WITH vec[2],;
		      tp     WITH "P"

	     CASE tip_prest="H" .AND. monto>0
	      ni=LEFT(nitem,7)
	      vec[1]=rec_apell(ni,0)
	      vec[2]=ROUND(monto*ldol,2)
	      SELECT 99
	      APPEND BLANK
	      REPLACE nombre WITH vec[1],;
		      monto  WITH vec[2],;
		      tp     WITH  "H"

	   END CASE
	   SELECT 91
	   SKIP
	 ENDDO

       SELECT(aa1)
RETURN

FUNCTION pulp()
LOCAL COMP,k,nit
LOCAL ff:="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                          ###,###.##"
LOCAL vect[4],pp[20]

	area110:=SELECT()
	 IF !STARTREPORT(80,'O',4,5)
	    RETURN ' '
	 ENDIF

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= '^,185,00,'+'DETALLE CORRESPONDIENTE A ANTICIPO II'
	 pp[5]= '^,000,00, ' +  "Cochabamba, "+ FECHALIT(date(),3,3)
	 pp[6]= '<,000,00'
	 pp[7]= '<,009,5,'+ ' Correspondiente al mes de:' +alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[8]='<,009,5,'+' Nombre del Empleado                             Monto (Bs.)'

	 EVERYHEADER (@pp,8)

	 pp[1]="Nombre del Empleado                                          Monto (Bs.)"
	 pp[2]=REPLICATE('_',80)

	 HEADERSCREEN(@pp,2)

       vect[3]=0
       vect[4]=0
	IF !EVERYROW('ANTICIPO II',vect,.T.,9);FINISHREPORT();ENDIF
	SKIPLINE(1,.T.)
	vect[1]=''
	 IF !EVERYROW(REPLICATE("_",80),vect,.T.)
	   FINISHREPORT()
	   SELECT(area101)
	   return ''
	 ENDIF

	SELECT 32
	USE personal  INDEX perso_nom
	GO TOP
	DO WHILE !EOF()
		  IF pulperia>0
		    vect[1]=espacios(nombre,40)
		    vect[2]=pulperia
		    vect[3]=ROUND(vect[3]+pulperia,2)
		    IF !EVERYROW(ff,vect,.T.)
		      EXIT
		    ENDIF
		  ENDIF
	   IF ultima_linea<4
	      NEWPAGE()
	   ENDIF
	   SKIP
	   ENDDO

	  IF ultima_linea<4
	      NEWPAGE()
	  ENDIF
	  SKIPLINE(1,.T.)
	  vect[1]=espacios('TOTAL ANTICIPO II' ,40);vect[2]=vect[3]
	  IF !EVERYROW(ff,vect,.T.)
		 FINISHREPORT()
	  ENDIF

	 SELECT(area110)
	 FINISHREPORT()
RETURN

FUNCTION otro_p()
LOCAL pp[20],vec[5],ni,forma,sh:=0,sp:=0,mm:=0,k,totO:=0
       crea_pres()

       area102:=SELECT()
       IF !STARTREPORT(80,'R',4,5)
	  RETURN ' '
       ENDIF

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= '^,185,00,'+'Detalle de OTROS'
	 pp[5]= '^,000,00, ' +  "Cochabamba, "+ FECHALIT(date(),3,3)
	 pp[6]= '<,000,00'
	 pp[7]= '<,009,5,'+ ' Correspondiente al mes de:' +alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[8]='<,009,5,'+' Concepto                             Cantidad'
	 pp[9]='<,009,5,'+REPLICATE('_',80)

	 EVERYHEADER (@pp,9)
	 pp[1]= la_empresa
	 pp[2]= el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= 'Detalle de OTROS'
	 pp[5]= "Cochabamba, " //+ FECHALIT(cad_fecha,3,3)
	 pp[6]= ''
	 pp[7]=  'Correspondiente al mes de:'// +lmeses


	 pp[1]="Concepto                                         Cantidad"
	 pp[2]=REPLICATE('_',90)
	 forma ="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX       ###,###,###.##"
	 HEADERSCREEN(@pp,2)

	 SELECT 48
	 USE ACREEDOR INDEX acreedor

	  rr=LASTREC()

	  FOR k:=2 TO rr
	     IF k<10
		comp="0"+alltrim(str(k))
	     ELSE
		comp=alltrim(str(k))
	     ENDIF

	     SELECT 48
	     use acreedor index acreedor
	     seek comp
	     vec[1]=''
	     IF !EVERYROW(espacios(acreedor,40),vec,.T.,9)
		EXIT
	     ENDIF
	     SKIPLINE(1,.T.)

	     SELECT 91
	     USE presta INDEX presta_i
	     GO TOP
	     nit=RIGHT(ALLTRIM(nitem),2)
	     mm=0
	     DO WHILE !EOF()
		 nit=RIGHT(ALLTRIM(nitem),2)
		 IF nit=comp  .AND. monto >0
		  vec[1]=espacios(rec_apell(left(nitem,7),0),40)
                  IF LEN(ALLTRIM(nitem)) = 10
                     vec[1]=espacios(rec_apell(left(nitem,8),0),40)
                  ENDIF

                  // se cambio left(nitem,7)
		  vec[2]=monto
		  mm=ROUND(mm+monto,2)

		  IF !EVERYROW(forma,vec,.T.)
		      FINISHREPORT()
		      RETURN
		      SELECT(area102)
		  ENDIF

		 IF ultima_linea<6
		   NEWPAGE()
		 ENDIF

		ENDIF

		SKIP
	     ENDDO
	     SKIPLINE(1,.T.)
	     IF mm>0
		vec[1]=espacios('TOTAL',40)
		vec[2]=mm
		toto=toto+mm
		IF !EVERYROW(forma,vec,.T.,9)
		   EXIT
		 ENDIF
	     ENDIF
		SKIPLINE(1,.T.)
	  NEXT
	    vec[1]=espacios('TOTAL OTROS ',40)
	    vec[2]=toto
		IF !EVERYROW(forma,vec,.T.,9)
		   FINISHREPORT()
		ENDIF
	  FINISHREPORT()
	  SELECT(area102)
RETURN

****************************************************************************
*  Detalle de horas Extras al personal Medico                              *
*  Actualizado en fecha 26 de Diciembre de 1995                            *
****************************************************************************

FUNCTION dist_med()
LOCAL pp[20],vec[20],aux[2],i,form,ax,vt[30]

	 area111:=SELECT()
	 vt  = AFILL(vt,0.0)
	 vec = AFILL(vec,0.0)
	 pre_pla("Distribuci¢n de horas del personal M‚dico")
	 IF !STARTREPORT(132,'R',4,5)
	     RETURN ' '
	 ENDIF

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= '^,155,00,'+'DETALLE DE DISTRIBUCION DE HORAS DEL PERSONAL MEDICO EN POLICLINICO'
	 pp[5]='^,000,00, ' +  "Cochabamba, "+ FECHALIT(cad_fecha,5,3)
	 pp[6]='<,000,00'
	 pp[7]='<,009,10,'+ ' Correspondiente al mes de:'+ alltrim(lmeses)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[8]='<,009,00,                                                             POLICLINICO                                               '
	 pp[9] ='<,004,10, ============================================================================================================================================'
	 pp[10]='<,004,10, No. Item    Apellidos y Nombres             HORAS  HABER      BONO        INDICE       CATEGORIA    HORAS    BONO       ESCALAFON   TOTAL   '
	 pp[11]='<,004,10,                                             TRAB.  BASICO     ANTIG.      EFECTIVIDAD  PROFESIONAL  EXTRAS   IRRAD.                 GANADO  '
	 pp[12]='<,004,10, ============================================================================================================================================='

		    form='          XXXXXXXX     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX   ##  #,###,###.##  ###,###.##  ###,###.##  ###,###.##  ###,###.##  ###,###.## ###,###.##  ###,###.##'
		   formt='                       XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX       #,###,###.##  ###,###.##  ###,###.##  ###,###.##  ###,###.##  ###,###.## ###,###.##  ###,###.##'
	 EVERYHEADER (@pp,12)

	 pp[1]= 'DETALLE DE DISTRIBUCION DE HORAS DEL PERSONAL MEDICO EN POLICLINICO'
	 pp[2]= "Cochabamba, "+ FECHALIT(cad_fecha,5,3)
	 pp[3]=''
	 pp[4]=' Correspondiente al mes de:'+ alltrim(lmeses)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[5]='                                                   POLICLINICO'
	 pp[6]='        ============================================================================================================================================'
	 pp[7]='        No. Item    Apellidos y Nombres             HORAS     HABER    BONO        INDICE        CATEGORIA      HORAS      BONO    ESCALAFON  TOTAL '
	 pp[8]='                                                    TRAB.     BASICO   ANTIG.     EFECTIVIDAD    PROFESIONAL    EXTRAS     IRRAD              GANADO'
	 pp[9]='        ============================================================================================================================================'

	 HEADERSCREEN(@pp,9)


	 vec[1]=''
	 IF !EVERYROW('',vec,.T.)
	    FINISHREPORT()
	 ENDIF
	 SELECT 1
	 use dat_empl index nitem,nombre,codigo,eventual,ci
	 SET ORDER TO 1
	 SET FILTER TO tip_empl="P"

	 GO TOP
	 DO WHILE !EOF()

	  IF  SUBSTR(codigo,8,1)=="M" .AND. (LEFT(prof,6)=='MEDICO' .OR. LEFT(prof,12)="FARMACEUTICA" .OR. LEFT(prof,10)="BIOQUIMICA");
	     .AND. !( ALLTRIM(nombre)="COMISION" .OR. ALLTRIM(nombre)="ACEFALIA" ) .AND. hrs_poli > 0

	     ax        = hrs_poli+hrs_hosp
	     aux[1]    = hrs_poli

	     area100:=SELECT()
	     SELECT 32
	     USE personal  INDEX perso_it
	     SEEK alltrim(1->nitem)
	     IF FOUND()
		dias=dias_trab
	     ELSE
		dias=30
	     ENDIF
	     SELECT(area100)

	     vec = AFILL(vec,0.0)

	     vec[1] = espacios(nitem,8)
	     vec[2] = espacios(nombre,31)
	     vec[3] = aux[1]

	     IF dias > 0

	     IF anio_rec > 0
		fechita = halla_fecha(fech_ing,anio_rec,mes_rec,dia_rec)
	     ELSE
	       fechita  = fech_ing
	     ENDIF


	     sue_mens   = sueldo(nivel,jor_trab)
	     bon_antig  = bono_antig("B",fechita)*jor_trab

	     IF reincorp ="S"
		ind        = efectivo("B",fechita,sue_mens)  // Fecha de ingreso institucional
	     ELSE
		ind        = efectivo("B",fech_ing,sue_mens)
	     endif

	     IF dias<30
	       sue_mens  = ROUND(ROUND(sue_mens/30,2)*dias,2)
	       bon_antig = ROUND(ROUND(bon_antig/30,2)*dias,2)
	       ind    = ROUND(ROUND(ind/30,2)*dias,2)
	     ENDIF
	     //
		dos_por   = escalafon(prof,sue_mens,nitem)


	     //
	     conge:=es_con(nitem)
	     IF conge[2]!=0;indice=conge[2];ENDIF
	     IF conge[1]!=0;bon_antig=conge[1];ENDIF

	     hrs_adic    = hrs_adicion(codigo,prof,sue_mens+dos_por)
	     cat_profe   = categ_prof(sue_mens+dos_por,p_prof,codigo,prof,hrs_adic)
	     IF conge[3] = 0;sub_irrad= sub_irradia(codigo,fechita,sue_mens);ELSE;sub_irrad=conge[3];ENDIF

	     emerg      = servicio_em(serv_emerg,sueldo(nivel,1.0)+dos_por,bon_antig,cat_profe)
	     v_h      = valor_hora(sexo,prof,sueldo(nivel,1.0)+dos_por,bon_antig,cat_profe,dias)
	     
	     hrs_extras = hora_extra(nitem,v_h)
	     recarg     = recarg_noct(nitem,v_h)
	     sub_irrad  = sub_irradia(codigo,fechita,sue_mens)

	     vec[1] = espacios(nitem,8)
	     vec[2] = espacios(nombre,31)
	     vec[3] = aux[1]
	     vec[4] = ROUND(sue_mens/ax*aux[1],2)
	     vec[5] = ROUND(bon_antig/ax*aux[1],2)
	     vec[6] = ROUND(ind/ax*aux[1],2)
	     vec[7] = ROUND(cat_profe/ax*aux[1],2)
	     vec[8] = ROUND((hrs_extras+emerg)/ax*aux[1],2)
	     vec[10] =  ROUND((dos_por/ax)*aux[1],2)
	     vec[9] = ROUND(sub_irrad/ax*aux[1],2)
	     vec[11] = vec[4]+vec[5]+vec[6]+vec[7]+vec[8]+vec[9]+vec[10]
	     FOR i:=4 TO 11
		vt[i-2]=vt[i-2]+vec[i]
	     ENDFOR
	   ENDIF

	    IF !EVERYROW(form,vec,.T.)
	       FINISHREPORT()
	       SELECT(area111)
	       RETURN
	    ENDIF
	 ENDIF
	 SKIP
	 ENDDO

	 SKIPLINE(1,.T.)
	 vt[1]=espacios('TOTALES  ',31)

	 IF !EVERYROW(formt,vt,.T.);ENDIF

	 NEWPAGE()

	 // HOSPITAL
	 // Aqui es la otra parte
	 // saltar de pagina

	 VecEvHeader:={}
	 vt  = AFILL(vt,0.0)
	 vec = AFILL(vec,0.0)

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= '^,155,00,'+'DETALLE DE DISTRIBUCION DE HORAS DEL PERSONAL MEDICO EN HOSPITAL'
	 pp[5]='^,000,00, ' +  "Cochabamba, "+ FECHALIT(cad_fecha,5,3)
	 pp[6]='<,000,00'
	 pp[7]='<,009,10,'+ ' Correspondiente al mes de:'+ alltrim(lmeses)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[8]='<,009,00,                                                            HOSPITAL                                                  '

	 pp[9] ='<,004,10, ==================================================================================================================================================================='
	 pp[10]='<,004,10, No. Item    Apellidos y Nombres                HORAS       HABER       BONO      INDICE       CATEGORIA      HORAS       RECARGO      BONO     ESCALAFON     TOTAL '
	 pp[11]='<,004,10,                                                TRAB.       BASICO      ANTIG.    EFECTIVIDAD  PROFESIONAL    EXTRAS      NOCTURNO     IRRAD.                 GANADO'
	 pp[12]='<,004,10, ==================================================================================================================================================================='
	 //        004
		    form='          XXXXXXXX     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX      ##  #,###,###.##  ###,###.##  ###,###.##      ###,###.##  ###,###.##  ###,###.##  ###,###.## ###,###.## ###,###.##'
		   formt='                       XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX          #,###,###.##  ###,###.##  ###,###.##      ###,###.##  ###,###.##  ###,###.##  ###,###.## ###,###.## ###,###.##'

	 EVERYHEADER (@pp,12)

	 pp[1]= 'DETALLE DE DISTRIBUCION DE HORAS DEL PERSONAL MEDICO EN POLICLINICO                                                               '
	 pp[2]= "Cochabamba, "+ FECHALIT(cad_fecha,5,3)
	 pp[3]=''
	 pp[4]=' Correspondiente al mes de:'+ alltrim(lmeses)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[5]='                                                   HOSPITAL'
	 pp[6]='======================================================================================================================================================                      '
	 pp[7]='No. Item    Apellidos y Nombres             HORAS     HABER    BONO      INDICE       CATEGORIA    HORAS      RECARGO     BONO      ESCALAFON   TOTAL                        '
	 pp[8]='                                            TRAB.     BASICO   ANTIG.  EFECTIVIDAD   PROFESIONAL   EXTRAS     NOCTURNO    IRRAD.                GANADO                       '
	 pp[9]='=======================================================================================================================================================                      '

	 HEADERSCREEN(@pp,9)

	 vec[1]=''
	 IF !EVERYROW('',vec,.T.)
	    FINISHREPORT()
	 ENDIF
	 SELECT 1
	 use dat_empl index nitem,nombre,codigo,eventual,ci
	 SET ORDER TO 1
	 SET FILTER TO tip_empl="P"

	 GO TOP
	 DO WHILE !EOF()

	  IF  SUBSTR(codigo,8,1)=="M" .AND. (LEFT(prof,6)=='MEDICO' .OR. LEFT(prof,12)="FARMACEUTICA" .OR. LEFT(prof,10)="BIOQUIMICA");
	     .AND. !( ALLTRIM(nombre)="COMISION" .OR. ALLTRIM(nombre)="ACEFALIA" ) .AND. hrs_hosp>0

	     ax =hrs_poli+hrs_hosp
	     aux[2]=hrs_hosp
	     area100:=SELECT()
	     SELECT 32
	     USE personal  INDEX perso_it
	     SEEK alltrim(1->nitem)
	     IF FOUND()
		dias=dias_trab
	     ELSE
		dias=30
	     ENDIF

	     SELECT(area100)

	     vec = AFILL(vec,0.0)
	     vec[1] = espacios(nitem,8)
	     vec[2] = espacios(nombre,31)
	     vec[3] = aux[1]

	     IF dias > 0

	     IF anio_rec>0
		fechita = halla_fecha(fech_ing,anio_rec,mes_rec,dia_rec)
	     ELSE
	       fechita  = fech_ing
	     ENDIF

	     sue_mens   = sueldo(nivel,jor_trab)
	     bon_antig  = bono_antig("B",fechita)*jor_trab

	     IF reincorp ="S"
		ind        = efectivo("B",fechita,sue_mens)  // Fecha de ingreso institucional
	     ELSE
		ind        = efectivo("B",fech_ing,sue_mens)
	     endif

	     IF dias<30
	       sue_mens  = ROUND(ROUND(sue_mens/30,2)*dias,2)
	       bon_antig = ROUND(ROUND(bon_antig/30,2)*dias,2)
	       ind    = ROUND(ROUND(ind/30,2)*dias,2)
	     ENDIF

	     dos_por    = escalafon(prof,sue_mens,nitem)

	     conge:=es_con(nitem)
	     IF conge[2]!=0;indice=conge[2];ENDIF
	     IF conge[1]!=0;bon_antig=conge[1];ENDIF

	     hrs_adic   = hrs_adicion(codigo,prof,sue_mens+dos_por)
	     cat_profe  = categ_prof(sue_mens+dos_por,p_prof,codigo,prof,hrs_adic)
	     IF conge[3]=0;sub_irrad= sub_irradia(codigo,fechita,sue_mens);ELSE;sub_irrad=conge[3];ENDIF

	     emerg      = servicio_em(serv_emerg,sueldo(nivel,1.0)+dos_por,bon_antig,cat_profe)
	     v_h      = valor_hora(sexo,prof,sueldo(nivel,1.0)+dos_por,bon_antig,cat_profe,dias)
	     hrs_extras = hora_extra(nitem,v_h)
	     recarg     = recarg_noct(nitem,v_h)
	     sub_irrad  = sub_irradia(codigo,fechita,sue_mens)

	     vec[1] = espacios(nitem,8)
	     vec[2] = espacios(nombre,31)
	     vec[3] = aux[2]
	     vec[4] = ROUND(sue_mens/ax*aux[2],2)
	     vec[5] = ROUND(bon_antig/ax*aux[2],2)
	     vec[6] = ROUND(ind/ax*aux[2],2)
	     vec[7] = ROUND(cat_profe/ax*aux[2],2)
	     vec[8] = ROUND(((hrs_extras+emerg)/ax)*aux[2],2)
	    vec[10] = ROUND(sub_irrad/ax*aux[2],2)
	     vec[9] = ROUND(recarg/ax*aux[2],2)
	    vec[11] = ROUND((dos_por/ax)*aux[2],2)
	    vec[12] = vec[4]+vec[5]+vec[6]+vec[7]+vec[8]+vec[9]+vec[10]+vec[11]

	     FOR i:=4 TO 12
		vt[i-2]=vt[i-2]+vec[i]
	     ENDFOR
	   ENDIF

	     /*vec[10] = aux[2]
	     vec[11] = ROUND(sue_mens/ax*aux[2],2)
	     vec[12] = ROUND(bon_antig/ax*aux[2],2)
	     vec[13] = ROUND(ind/ax*aux[2],2)
	     vec[14] = ROUND(cat_profe/ax*aux[2],2)
	     vec[15] = ROUND((hrs_extras+emerg)/ax*aux[2],2)
	     vec[16] = ROUND(recarg/ax*aux[2],2)
	     vec[17] = ROUND(sub_irrad/ax*aux[2],2)
	     vec[18] = vec[11]+vec[12]+vec[13]+vec[14]+vec[15]+vec[16]+vec[17]
	     FOR i:=11 TO 18
		vt[i-3]=vt[i-3]+vec[i]
	     ENDFOR */

	    IF !EVERYROW(form,vec,.T.)
	       FINISHREPORT()
	       SELECT(area111)
	       RETURN
	    ENDIF
	 ENDIF
	 SKIP
	 ENDDO

	 SKIPLINE(1,.T.)
	 vt[1]=espacios('TOTALES  ',31)

	 SKIPLINE(1,.T.)
	 vt[1]=espacios('TOTALES  ',31)
	 IF !EVERYROW(formt,vt,.T.)
	    FINISHREPORT()
	    SELECT(area111)
	    RETURN
	 ENDIF

	 FINISHREPORT()
	 SELECT(area111)
RETURN

FUNCTION hextra()

 LOCAL pp[20],ve[3],forma,sumE:=0,sumN:=0
       area140:=SELECT()
       IF !STARTREPORT(80,'R',4,5)
	  RETURN ' '
       ENDIF

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= '^,185,00,'+'DETALLE DE HORAS EXTRAS Y NOCTURNAS'
	 pp[5]= '^,000,00, ' +  "Cochabamba, "+ FECHALIT(DATE(),3,3)
	 pp[6]= '<,000,00'
	 pp[7]= '<,009,5,'+ ' Correspondiente al mes de:' +alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[8]='<,009,5,'+' Nombre del Empleado               Horas Extras        Horas Nocturnas'
	 pp[9]='<,009,5,'+REPLICATE('_',90)

	 EVERYHEADER (@pp,9)
	 pp[1]= la_empresa
	 pp[2]= el_Lugar
	 pp[3]= 'DETALLE DE HORAS EXTRAS Y NOCTURNAS'
	 pp[4]= "Cochabamba, " + FECHALIT(date(),3,3)
	 pp[5]= ''
	 pp[6]=  'Correspondiente al mes de:'+mes
	 pp[7]='Nombre del Empleado                            Horas Extras             Horas Nocturnas                                            '
	 pp[8]=REPLICATE('_',80)
	 HEADERSCREEN(@pp,8)

	 forma='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX           ####.##                     ####.##'


	  SELECT 16
	  USE hrs_eme  INDEX extra_it
	  SET FILTER TO hora_ext>0 .OR. hora_noc>0
	  GO TOP
	  WHILE !EOF()

		ve[1]=espacios(rec_apell(nitem,0),40)
		ve[2]=hora_ext
		sumE=sumE+ve[2]
		ve[3]=hora_noc
		sumN=sumN+ve[3]

	    IF !EVERYROW(forma,ve,.T.)
	       EXIT
	    ENDIF
	    SKIP
	  ENDDO
	  SKIPLINE(2,.T.)

	  ve[1] = espacios("TOTAL",40)
	  ve[2] = sumE
	  ve[3] = sumN

	  EVERYROW(forma,ve,.T.)
       FINISHREPORT()
       SELECT(area140)
RETURN

FUNCTION jor_trab()
LOCAL pp[20],ve[3],forma
       area140:=SELECT()
       IF !STARTREPORT(80,'R',4,5)
	  RETURN ' '
       ENDIF

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= '^,185,00,'+'DETALLE DE DIAS Y HORAS TRABAJADAS'
	 pp[5]= '^,000,00, ' +  "Cochabamba, "+ FECHALIT(DATE(),3,3)
	 pp[6]= '<,000,00'
	 pp[7]= '<,009,5,'+ ' Correspondiente al mes de:' +alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[8]='<,009,5,'+' Nombre del Empleado                 Dias           Horas '
	 pp[9]='<,009,5,'+REPLICATE('_',90)

	 EVERYHEADER (@pp,9)
	 pp[1]= la_empresa
	 pp[2]= el_Lugar
	 pp[3]= 'DETALLE DE DIAS Y HORAS TRABAJADAS'
	 pp[4]= "Cochabamba, " + FECHALIT(date(),3,3)
	 pp[5]= ''
	 pp[6]=  'Correspondiente al mes de:'+alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[7]='Nombre del Empleado                             Dias           Horas '
	 pp[8]=REPLICATE('_',80)
	 HEADERSCREEN(@pp,8)

	 forma='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX           ####                     ####'
//        SELECT 32
//        USE personal  INDEX perso_nom

	  SELECT 1
	  use dat_empl index nitem,nombre,codigo,eventual,ci
	  SET ORDER TO 1
	  GO TOP

	  WHILE !EOF()
	       IF !(ALLTRIM(nombre)="ACEFALIA" .OR. ALLTRIM(nombre)="COMISION")
		 ve[1] = espacios(nombre,40)
		 ve[2] = dias_tr(1->nitem)
		 ve[3] = hrs_poli+hrs_hosp
		 IF !EVERYROW(forma,ve,.T.)
		   EXIT
		 ENDIF
	      ENDIF
	     SELECT 1
	    SKIP
	  ENDDO
       FINISHREPORT()
       SELECT(area140)
RETURN

FUNCTION retraso()
LOCAL pp[20],ve[3],forma,tg,i,vt[3]
pre_pla("Detalle de Retrasos del Personal")
       vt = AFILL(vt,0.0)
       area140:=SELECT()
       IF !STARTREPORT(80,'R',4,5)
	  RETURN ' '
       ENDIF

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= '^,185,00,'+'DETALLE DE RETRASOS DEL PERSONAL'
	 pp[5]= '^,000,00, ' +  "Cochabamba, "+ FECHALIT(DATE(),3,3)
	 pp[6]= '<,000,00'
	 pp[7]= '<,009,5,'+ ' Correspondiente al mes de:' +alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[8]='<,009,5,'+' Nombre del Empleado               Retraso (Minutos)   Descuento (Bs.)'
	 pp[9]='<,009,5,'+REPLICATE('_',90)

	 EVERYHEADER (@pp,9)
	 pp[1]= la_empresa
	 pp[2]= el_Lugar
	 pp[3]= 'DETALLE DE RETRASOS DEL PERSONAL'
	 pp[4]= "Cochabamba, " + FECHALIT(date(),3,3)
	 pp[5]= ''
	 pp[6]=  'Correspondiente al mes de:'+alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[7]='Nombre del Empleado                           Retraso (Minutos)   Descuento (Bs.)                   '
	 pp[8]=REPLICATE('_',80)
	 HEADERSCREEN(@pp,8)

	 forma='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX           ####.##              ###,###,###.##'
	  SELECT 32
	  USE personal  INDEX perso_it,perso_nom
	  SET ORDER TO 2
	  GO TOP

	  WHILE !EOF()
	   IF faltas>0
		ve[1] = ESPACIOS(nombre,40)
		ve[2] = faltas
		i     = RECNO()
		aa :=select()
		tg    = total_ganado(ALLTRIM(32->nitem))
		select(aa)
		go i
		ve[3] = multas(32->faltas,tg)

		vt[2] = vt[2]+ ve[2]
		vt[3] = vt[3]+ ve[3]

	       IF !EVERYROW(forma,ve,.T.)
		  EXIT
	       ENDIF
	    ENDIF
	    SELECT 32
	    SET ORDER TO 2
	    SKIP
	ENDDO

	SKIPLINE(2,.T.)

	vt[1] = espacios("TOTAL",40)
	EVERYROW(forma,vt,.T.)
	FINISHREPORT()
	SELECT(area140)
RETURN

FUNCTION faltas()
LOCAL pp[20],ve[3],forma,tg,i,vt[3]
pre_pla("Detalle de Faltas del Personal")
       vt = AFILL(vt,0.0)
       area140:=SELECT()
       IF !STARTREPORT(80,'R',4,5)
	  RETURN ' '
       ENDIF

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= '^,185,00,'+'DETALLE DE FALTAS DEL PERSONAL'
	 pp[5]= '^,000,00, ' +  "Cochabamba, "+ FECHALIT(DATE(),3,3)
	 pp[6]= '<,000,00'
	 pp[7]= '<,009,5,'+ ' Correspondiente al mes de:' +alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[8]='<,009,5,'+' Nombre del Empleado                 Faltas (Dias)    Descuento (Bs.)'
	 pp[9]='<,009,5,'+REPLICATE('_',90)

	 EVERYHEADER (@pp,9)
	 pp[1]= la_empresa
	 pp[2]= el_Lugar
	 pp[3]= 'DETALLE DE FALTAS DEL PERSONAL'
	 pp[4]= "Cochabamba, " + FECHALIT(date(),3,3)
	 pp[5]= ''
	 pp[6]=  'Correspondiente al mes de:'+alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[7]='Nombre del Empleado                             Faltas (Dias)    Descuento (Bs.)                                    '
	 pp[8]=REPLICATE('_',80)
	 HEADERSCREEN(@pp,8)

	 forma='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX              ####.##           ###,###,###.##'
	  SELECT 32
	  USE personal  INDEX perso_it,perso_nom
	  SET ORDER TO 2
	  GO TOP

	  WHILE !EOF()
	   IF falta_dia>0
		ve[1] = ESPACIOS(nombre,40)
		ve[2] = falta_dia
		ar := select ()
		tg    = total_ganado(ALLTRIM(32->nitem))-emerg-recarg-hrs_extras
		select (ar)
		ve[3] = ROUND(tg/30,2)*falta_dia
		vt[2] = vt[2]+ ve[2]
		vt[3] = vt[3]+ ve[3]

	       IF !EVERYROW(forma,ve,.T.)
		  EXIT
	       ENDIF
	    ENDIF
	    SELECT 32
	    SET ORDER TO 2
	    SKIP
	ENDDO

	SKIPLINE(2,.T.)

	vt[1] = espacios("TOTAL",40)
	EVERYROW(forma,vt,.T.)
	FINISHREPORT()
	SELECT(area140)
RETURN

STATIC FUNCTION dias_tr(nt)
LOCAL dt:=30
       area113:=SELECT()
       SELECT 32
       USE personal  INDEX perso_it
       SEEK ALLTRIM(nt)
       IF FOUND()
	 dt=dias_trab
       ENDIF
       SELECT(area113)
RETURN dt

FUNCTION saldo_iva()
LOCAL pp[20],ve[3],forma
      area123:=SELECT()

      SELECT 22
      USE iva INDEX iva_item
      IF !STARTREPORT(80,'R',4,5)
	  RETURN ' '
       ENDIF

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= '^,185,00,'+'SALDO I.V.A.'
	 pp[5]= '^,000,00, ' +  "Cochabamba, "+ FECHALIT(DATE(),3,3)
	 pp[6]= '<,000,00'
	 pp[7]= '<,009,5,'+ ' Correspondiente al mes de:' +alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[8]='<,009,5,'+'Nombre del Empleado                   Saldo Anterior         Formulario 101'
	 pp[9]='<,009,5,'+REPLICATE('_',90)

	 EVERYHEADER (@pp,9)
	 pp[1]= la_empresa
	 pp[2]= el_Lugar
	 pp[3]= 'SALDO IVA'
	 pp[4]= "Cochabamba, " + FECHALIT(date(),3,3)
	 pp[5]= ''
	 pp[6]=  'Correspondiente al mes de:'+alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[7]='Nombre del Empleado                              Saldo Anterior         Formulario 101'
	 pp[8]=REPLICATE('_',80)
	 HEADERSCREEN(@pp,8)

	 forma='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX           ######.##            ######.##'

	 SELECT 22
	 USE iva INDEX iva_item

	 GO TOP
	  WHILE !EOF()
		ve[1]=espacios(rec_apell(nitem,0),40)
		IF !(ALLTRIM(ve[1])="COMISION" .OR. ALLTRIM(ve[1])="ACEFALIA" .OR. EMPTY(nitem))
		  ve[2]=nada
		  ve[3]=formu101
		  IF !EVERYROW(forma,ve,.T.)
		     EXIT
		  ENDIF
		ENDIF
	    SKIP
	 ENDDO
       FINISHREPORT()

      SELECT(area123)
RETURN

FUNCTION anti()
LOCAL pp[20],ve[3],forma
       area140:=SELECT()
       IF !STARTREPORT(80,'R',4,5)
	  RETURN ' '
       ENDIF

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= '^,185,00,'+'DETALLE DE ANTICIPOS'
	 pp[5]= '^,000,00, ' +  "Cochabamba, "+ FECHALIT(DATE(),3,3)
	 pp[6]= '<,000,00'
	 pp[7]= '<,009,5,'+ ' Correspondiente al mes de:' +alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[8]='<,009,5,'+' Nombre del Empleado                 Monto (Bs.)'
	 pp[9]='<,009,5,'+REPLICATE('_',90)

	 EVERYHEADER (@pp,9)
	 pp[1]= la_empresa
	 pp[2]= el_Lugar
	 pp[3]= 'DETALLE DE ANTICIPOS'
	 pp[4]= "Cochabamba, " + FECHALIT(date(),3,3)
	 pp[5]= ''
	 pp[6]=  'Correspondiente al mes de:'+alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[7]='Nombre del Empleado                             Monto (Bs.)'
	 pp[8]=REPLICATE('_',80)
	 HEADERSCREEN(@pp,8)

	 forma='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX           ####.##        '
	 SELECT 16
	 USE hrs_eme  INDEX extra_it
	 SET FILTER TO anticipo>0

	  GO TOP

	  WHILE !EOF()
		ve[1]=espacios(rec_apell(nitem,0),40)
		ve[2]=anticipo
	    IF !EVERYROW(forma,ve,.T.)
	       EXIT
	    ENDIF
	    SKIP
	  ENDDO
       FINISHREPORT()
       SELECT(area140)
RETURN
****************************************************************************
*  Reporte de Refrigerios
****************************************************************************
FUNCTION refri()
LOCAL pp[20],ve[3],forma,tg,i,vt[3]

       vt = AFILL(vt,0.0)
       area140:=SELECT()
       IF !STARTREPORT(80,'R',4,5)
	  RETURN ' '
       ENDIF

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= '^,185,00,'+'DETALLE DE REFRIGERIOS DEL PERSONAL'
	 pp[5]= '^,000,00, ' +  "Cochabamba, "+ FECHALIT(DATE(),3,3)
	 pp[6]= '<,000,00'
	 pp[7]= '<,009,5,'+ ' Correspondiente al mes de:' +alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[8]='<,009,5,'+' Nombre del Empleado                Monto (Bs.)'
	 pp[9]='<,009,5,'+REPLICATE('_',90)

	 EVERYHEADER (@pp,9)
	 pp[1]= la_empresa
	 pp[2]= el_Lugar
	 pp[3]= 'DETALLE DE REFRIGERIOS'
	 pp[4]= "Cochabamba, " + FECHALIT(date(),3,3)
	 pp[5]= ''
	 pp[6]=  'Correspondiente al mes de:'+alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[7]='Nombre del Empleado                             Monto (Bs.)'
	 pp[8]=REPLICATE('_',80)
	 HEADERSCREEN(@pp,8)

	 forma='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX           ####.##'

	  SELECT 32
	  USE personal  INDEX perso_it,perso_nom
	  SET ORDER TO 2
	  GO TOP

	  WHILE !EOF()
	   IF refrigerio>0
		ve[1] = ESPACIOS(nombre,40)
		ve[2] = refrigerio
		vt[2] = vt[2]+ve[2]

	       IF !EVERYROW(forma,ve,.T.)
		  EXIT
	       ENDIF
	    ENDIF
	    SELECT 32
	    SET ORDER TO 2
	    SKIP
	ENDDO

	SKIPLINE(2,.T.)

	vt[1] = espacios("TOTAL",40)
	EVERYROW(forma,vt,.T.)
	FINISHREPORT()
	SELECT(area140)
RETURN
*----------------------------------------------------------------------------
****************************************************************************
*  Reporte del Rol de Vacaciones                                           *
****************************************************************************

FUNCTION vaca()
LOCAL pp[20],ve[6],forma
       area140:=SELECT()
       IF !STARTREPORT(80,'R',4,5)
	  RETURN ' '
       ENDIF

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= '^,185,00,'+'ROL DE VACACIONES'
	 pp[5]= '^,000,00, ' +  "Cochabamba, "+ FECHALIT(DATE(),3,3)
	 pp[6]= '<,000,00'
	 pp[7]= '<,009,5,'+ ' Correspondiente al mes de:' +alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[8]='<,009,5,'+ ' Nombre del Empleado            Gesti¢n   Permisos  Duraci¢n   Fecha  Observaciones'
	 pp[9]='<,009,5,'+REPLICATE('_',80)

	 EVERYHEADER (@pp,9)
	 pp[1]= la_empresa
	 pp[2]= el_Lugar
	 pp[3]= 'ROL DE VACACIONES'
	 pp[4]= "Cochabamba, " + FECHALIT(date(),3,3)
	 pp[5]= ''
	 pp[6]=  'Correspondiente al mes de:'+alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[7]=' Nombre del Empleado                        Gesti¢n         Permisos  Duraci¢n Fecha       Observaciones'
	 pp[8]=REPLICATE('_',105)
	 HEADERSCREEN(@pp,8)

	 forma='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX     XXXXXXXXXXXXXX    ####        ####          XXXXXXXX    XXXXXXXXXXXXXXXXXXX'

	 SELECT 38
	 USE (dr+"vacacion")  INDEX (dr+"vaca_item")

	  GO TOP

	  WHILE !EOF()
		ve[1]=espacios(rec_apell(nitem,0),40)
		ve[2]=gestion
		ve[3]=permisos
		ve[4]=duracion
		ve[5]=fecha
		ve[6]=observacio
	    IF !EVERYROW(forma,ve,.T.)
	       EXIT
	    ENDIF
	    SKIP
	  ENDDO
       FINISHREPORT()
       SELECT(area140)
RETURN

FUNCTION prestamos()
LOCAL pp[20],vec[9],ni,forma1,sh:=0,sp:=0,salir,dol:=cambio_dol()

       area102:=SELECT()
       IF !STARTREPORT(80,'R',4,5)
	  RETURN ' '
       ENDIF

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= '^,185,00,'+'DETALLE DE PRESTAMOS DEL PERSONAL'
	 pp[5]= '^,000,00, ' +  "Cochabamba, "+ FECHALIT(date(),3,3)
	 pp[6]= '<,000,00'
	 pp[7]= '<,009,5,'+ ' Correspondiente al mes de:' +alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))
	 pp[8]='<,009,5,'+' Cambio dolar :'+ALLTRIM(STR(cambio_dol()))
	 pp[9]='<,009,5,'+' Nombre del Empleado             Tipo     Monto         Plazo    Fecha Prest.   Cuota No. Monto $us  Bs.'

	 pp[10]='<,009,5,'+REPLICATE('_',110)

	 EVERYHEADER (@pp,10)
	 pp[1]= la_empresa
	 pp[2]= el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= 'DETALLE  DE PRESTAMOS DEL PERSONAL'
	 pp[5]= "Cochabamba, " //+ FECHALIT(cad_fecha,3,3)
	 pp[6]= ''
	 pp[7]=  'Correspondiente al mes de:'// +lmeses


	 pp[1]="Nombre del Empleado                         Tipo     Monto         Plazo    Fecha Prest.   Cuota No. Monto $us  Bs. "
	 pp[2]=REPLICATE('_',90)
	 forma ="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX    #    #########.##  ####     ########         #####  #######.##   #######.##"
	 forma1="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                         ###,###,###.##"
	 HEADERSCREEN(@pp,2)
	 salir=.T.
	 SELECT 21
	 USE (dr+"prestamo") INDEX (dr+"prest_it")

	 GO TOP
	 DO WHILE !EOF()
	       ni=LEFT(nitem,7)
	       vec[1]=rec_apell(ni,0)
	       IF ALLTRIM(vec[1])<>"ACEFALIA"
	       vec[2]=RIGHT(ALLTRIM(nitem),1)
	       vec[3]=monto
	       vec[4]=plazo
	       vec[5]=fech_prest
	       vec[6]=cuota
	       vec[7]=Monto_mes         
	       vec[8]=vec[7]*dol

	      IF !EVERYROW(forma,vec,.T.)
		 EXIT
	       ENDIF
	      END IF            
	  SKIP
	 END DO

	 IF salir=.F.
	   FINISHREPORT()
	   RETURN
	 ENDIF
	 SKIPLINE(2,.T.)
	   FINISHREPORT()
	   SELECT(area102)
	   RETURN
	 CLOSE ALL
	 FINISHREPORT()
	 SELECT(area102)
RETURN
