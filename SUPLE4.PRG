*****************************************************************************
**                                                                          *
**  MODULO              :     REPORTE DE PLANILLA DE SUPLENCIAS             *
**  PROPIETARIO         :     CAJA PETROLERA DE SALUD                       *
**  AUTOR(ES)           :     RICHARD JIMENEZ                               *
**  VERSION             :     1.0                                           *
**  CREACION            :     29 DE AGOSTO DE 1994                          *
**  ULTIMA REVISION     :                                                   *
**                                                                          *
*****************************************************************************
*****************************************************************************
**                                                                          *
**  DESCRIPCION     : El presente modulo tiene como objetivo de             *
**                    sacar un listado de suplencias                        *
**                                                                          *
*****************************************************************************
*****************************************************************************

FUNCTION list_suple()

LOCAL tituloscrren, referescreen, rec,codi,aux
LOCAL forma, forma_, forma1, i,val1,val2,sal,cont:=1
LOCAL pp[12], vector[14], vec_tot[15], vec_sub[1]

aa:=select()
vec_tot = AFILL (vec_tot,0.0)
prepla("Planilla de Suplencias")

IF LASTKEY()=27;RETURN;ENDIF
SELECT 17
USE suplenci  INDEX suple_qu,suple_po
//USE suplenci  INDEX suple_po,suple_qu
SET ORDER TO 1

IF LASTREC() = 0
   RETURN
ENDIF

IF !STARTREPORT(135,'R',4,5)
   RETURN ' '
ENDIF
cab('POLICLINICO')
       forma  =  "XXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXX     ###,###.##           ###,###.##   ####  ###,###.##  ###,###.##  ###,###.##    ###,###.##   ###,###.##   ###,###.##  #,###,###.##"
       forma1 =  "XXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXX     ###,###.##"
       forma2 =  "      XXXXXXXXXXXXXXXXXXXX                                                                              XXXXXXXXXXXXXXXXXXXX                                                         XXXXXXXXXXXXXXXXXXXX"
       forma3 =  "                                                                                    XXXXXXXXXXXXXXXXXXXXX                       ###,###.##  ###,###.##   ###,###.##  ###,###.##   ###,###.##   ###,###.##  #,###,###.##"
       forma_     = REPLICATE('X',220)
       vec_sub[1] = REPLICATE('-',220) //192

	  pp[1]= '====================================================================================================================================================================================================================='
	  pp[2]= 'ITEM       APELLIDOS Y NOMBRES                                 CARGO             | SAL. MENS. SUPLIDO     |  DIFERENCIA| DIAS |  TOTAL   | A.F.P.    |A.F.P.      | FONVIS    | FONVIS    | TOTAL     | LIQUIDO      '
	  pp[3]= '                                                                                 | SAL. MENS. SUPLENTE    |  HABERES   | TRAB |  GANADO  | BBV       |FUTURO      | BBV       | FUTURO    | DESCUENTOS| PAGABLE      '
	  pp[4]= '====================================================================================================================================================================================================================='

HEADERSCREEN(@pp,4)

pp[1] = '^,002,05,.................................................................................................................................................................................... Continua en la Siguiente P gina'
EVERYTRAILER(@pp,1)

SELECT 1
use dat_empl index nitem,nombre,codigo,eventual,ci
SET ORDER TO 1

SELECT 17
use suplencia index suple_qu,suple_po
SET ORDER TO 1
GO TOP
DO WHILE !EOF()
    nit = alltrim(17->quien)
    SELECT 1
    //use dat_empl index nitem,nombre,codigo,eventual,ci
    //SET ORDER TO 1
    SEEK nit

    IF !FOUND()
      SELECT 17
      //use suplencia index suple_po,suple_qu
      SKIP
      nit=ALLTRIM(quien)
      //nit=ALLTRIM(porquien)
      LOOP
    ELSE
     codi = codigo
    ENDIF

   IF cont = 1
	 val1 = "01"
	 val2 = "03"
   ELSE
	 vec_tot  = AFILL (vec_tot,0.0)
	 val1 = "02"
	 val2 = "02"
   ENDIF

   cont = cont+1
   sal  = .F.
   DO WHILE  (SUBSTR(codi,3,2)==val1 .OR. SUBSTR(codi,3,2)==val2) .AND. !sal

      SELECT 1
      //use dat_empl index nitem,nombre,codigo,eventual,ci
      SEEK (17->quien)
      vector[1]  =  17->quien
      vector[2]  =  ESPACIOS(rec_apell(17->quien,0), 40)
      vector[3]  =  ESPACIOS(bus_cargo(codigo),28)
      vector[4]  =  sueldo(nivel,jor_trab)

      SELECT 1
      //use dat_empl index nitem,nombre,codigo,eventual,ci
      SEEK (17->porquien)

      aux   = sueldo(nivel,jor_trab)
      IF vector[4]=aux .OR. aux>vector[4]
	vector[5] = ROUND(0.20*aux,2)
      ELSE
	 vector[5]  = abs( vector[4]-aux)
      ENDIF

      vector[6]  = 17->dias_supl
      vector[7]  = ROUND((vector[5]/30)*17->dias_supl,2)

      IF aporte_afp = "S"

	fonvi     =  fonvis(vector[7])
	if dat_empl->afp = "BBV"
		if viejo(fecha_nac)
			afp_bbv = ROUND(vector[7]*primaRiesgo,2)
		else
			afp_bbv = ROUND(vector[7]*porc_afp,2)
		endif
		//afp_bbv = ROUND(vector[7]*porc_afp,2)
		afp_fut = 0
		fonv_bbv = fonvi
		fonv_fut = 0
       else
	  if viejo(fecha_nac)
	   
	   afp_fut = ROUND(vector[7]*primaRiesgo,2)
	  else

	    afp_fut = ROUND(vector[7]*porc_afp,2)
	  endif

	  //afp_fut = ROUND(vector[7]*porc_afp,2)
	  afp_bbv = 0
	  fonv_fut = fonvi
	  fonv_bbv = 0
       end if
      ELSE
	 afp_bbv = 0
	 afp_fut = 0
	 fonv_fut = 0
	 fonv_bbv = 0
      ENDIF
      vector[8]  = afp_bbv //cps(prof,vector[7])
      vector[9]  = afp_fut //fpm(prof,vector[7])
      vector[10] = fonv_bbv
      vector[11] = fonv_fut
      vector[12] = ROUND(vector[8]+vector[9]+vector[10]+vector[11],2)
      vector[13] = abs(vector[7]-vector[12])

      IF !EVERYROW(forma,vector,.T.)
	 EXIT
      ENDIF

      FOR i := 7 TO 13       //totales
	  vec_tot[i-7+2] = ROUND(vec_tot[i-7+2] + vector[i],2)
      ENDFOR

      vector[1]  =  17->porquien
      vector[2]  =  ESPACIOS(rec_apell(17->porquien,0), 40)
      vector[3]  =  ESPACIOS( bus_cargo(codigo),28)
      vector[4]  =  sueldo(nivel,jor_trab)
      IF !EVERYROW(forma1,vector,.T.)
	 EXIT
      ENDIF

      SKIPLINE(1,.T.)

      IF ultima_linea = 3 .AND. !EOF()

	 vec_sub[1] = REPLICATE('-',235) //235
	 IF !EVERYROW(forma_,vec_sub,.T.)
	    EXIT
	 ENDIF
      ENDIF

      //REPLACE 15->liq_pagabl WITH liq_pagable
      SELECT 17
      //use suplencia index suple_po,suple_qu
      SKIP

      SELECT 1
      //use dat_empl index nitem,nombre,codigo,eventual,ci
      SEEK ALLTRIM(17->quien)
      //SEEK ALLTRIM(17->porquien)
      IF FOUND()
	  codi=codigo
      ENDIF

      SELECT 17
      IF EOF()
	sal = .T.
      ENDIF

  ENDDO
     vec_sub[1] = REPLICATE('-',215)

     IF !EVERYROW(forma_,vec_sub,.T.)
	FINISHREPORT()
	RETURN
     ENDIF
     //NEWPAGE()
     vec_tot[1] = ESPACIOS('Total',30)
     IF !EVERYROW(forma3,vec_tot,.T.)
	FINISHREPORT()
	RETURN
     ENDIF

     SKIPLINE(2,.T.)
     vec_tot[1] = numero_lite(vec_tot[8])
     IF !EVERYROW(forma_,vec_tot,.T.)
	FINISHREPORT()
	RETURN
     ENDIF

     SKIPLINE(4,.T.)

     quien_firma()
     IF sal
	EXIT
     ENDIF

     NEWPAGE()
     cab('HOSPITAL')
ENDDO

FINISHREPORT()
select(aa)
RETURN

STATIC FUNCTION cab(seccion)
LOCAL pp[13]
VecEvHeader:={}
tituloscreen = ' P L A N I L L A   D E   P A G O   D E   S U P L E N C I A S'
referescreen = ' Ref: Suplencias del Mes de :' +ALLTRIM(lmeses+ "/"+STR(YEAR(cad_fecha)),2)

pp[1]= '<,057,04,'+la_empresa
pp[2]= '<,155,04,'+el_Lugar
pp[3]= 'pagino'
pp[4]= '^,185,00,'+tituloscreen
pp[5]= '^,000,00, ' +  "Cochabamba, " + FECHALIT(DATE(),3,3)
pp[6]= '<,000,00'
pp[7]= '<,009,5,'+ referescreen+SPACE(40)+_ci(155)+seccion
pp[8] = '<,004,5,===================================================================================================================================================================================================================='
pp[9] = '<,004,5, ITEM       APELLIDOS Y NOMBRES                                 CARGO             | SAL. MENS. SUPLIDO     |  DIFERENCIA| DIAS |   TOTAL   | A.F.P.    |  A.F.P.    | FONVIS    | FONVIS    | TOTAL     | LIQUIDO   '
pp[10]= '<,004,5,                                                                                  | SAL. MENS. SUPLENTE    |  HABERES   | TRAB |   GANADO  | BBV       |  FUTURO    | BBV       | FUTURO    | DESCUENTOS| PAGABLE   '
pp[11]= '<,004,5,===================================================================================================================================================================================================================='
pp[12]= '<,000,00'
EVERYHEADER (@pp,11)
RETURN


FUNCTION repo_desc(sel)
LOCAL cad,tit,opcion:=0
    DO CASE
      CASE sel=1
	// Opcion 1 indica que se descuenta el sueldo basico
	cad="01";tit="COLEGIO MEDICO";opcion = 1
      CASE sel=3
	cad="02";tit="COLEGIO DE BIOQUIMICA"
      CASE sel=5
	cad="03";tit="COLEGIO DE ENFERMERAS";opcion=1
      CASE sel=7
	cad="04";tit="COLEGIO DE ODONTOLOGOS";opcion=1
      CASE sel=9
	cad="05";tit="ASOC. DE CIRUGIA"
      CASE sel=11
	cad="06" ;tit="ASOC. DE PEDIATRIA"
      CASE sel=13
	cad="07";tit="SINDICATO ADMINISTRATIVO"
      CASE sel=15
	cad="08";tit="SINDICATO MEDICO";
	 //opcion=1
      CASE sel=17
	cad="09";tit="SINDICATO DE ENFERMERAS"
      CASE sel=19
	cad="10";tit="CONF. SINDICATO ADMINISTRATIVO";opcion=1
	//;opcion=1
      CASE sel=23
	cad="12";tit="CENTRAL OBRERA DEPARTAMENTAL"
      CASE sel=21
	cad="11";tit="PRO DEPORTES"

    ENDCASE
      repor_desc(cad,sel,2,tit,opcion)
RETURN


FUNCTION repor_desc(param,ini,fin,titulo,opc)
LOCAL porc,mont,p[10],forma,forma1,vecto[4],vt[4],mont1:=0,tg:="Total Ganado      "

vt=AFILL(vt,0.0)
	 area201:=SELECT()
	 IF opc=1
	    tg = "Sueldo Mensual      "
	 ENDIF

	 pre_pla("Descuentos")

	 IF LASTKEY()=27;SELECT(area201);RETURN;ENDIF

	 IF !STARTREPORT(80,'R',4,5)
	    RETURN ' '
	 ENDIF

	 SELECT 19
	 USE impuesto INDEX impu_cod
	 SEEK param
	 IF FOUND()
	   porc=porcentaje/100
	   mont=monto
	 ENDIF

	 p[1]= '<,057,04,'+la_empresa
	 p[2]= '<,155,04,'+el_Lugar
	 p[3]= 'pagino'
	 p[4]= '^,185,00,'+titulo
	 p[5]= '^,000,00, ' +  "Cochabamba, "+ FECHALIT(date(),3,3)
	 p[6]= '<,000,00'
	 p[7]= '<,009,5,'+ ' Correspondiente al mes de:'+alltrim(lmeses)+ "/"+ALLTRIM(RIGHT(STR(anio),2))
	 p[8]='<,009,5,'+' Nombre del Empleado              '+tg+'Porcentaje (Bs.)   Monto (Bs.)'
	 p[9]='<,009,5,'+REPLICATE('_',95)
	 EVERYHEADER (@p,9)
	 p[1]='Nombre del Empleado                                 '+tg+'Porcentaje (Bs.)   Monto (Bs.)                                '
//                     p[1]='Nombre del Empleado                            Total Ganado          Porcentaje (Bs.)       Monto Fijo (Bs.)'
	 p[2]=REPLICATE('_',95)
		  forma ="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX       ###,###,###.##        ###,###,###.##         ###,###,###.##"
	 HEADERSCREEN(@p,2)

	 SELECT 1
	 use dat_empl index nitem,nombre,codigo,eventual,ci
	 SET ORDER TO 1
	 GO TOP
	 WHILE !EOF()
	       IF substr(asociacion,ini,fin)=param .AND. !(ALLTRIM(nombre)="ACEFALIA" .OR. ALLTRIM(nombre)=="COMISION" .OR. tip_empl="E")
		   area100:=SELECT()
		   SELECT 32
		   USE personal  INDEX perso_it
		   SEEK alltrim(1->nitem)
		   IF FOUND()
		      dias=dias_trab
                      ldias_em = dias_em
		      DO CASE
			CASE param="07"
			   mont1=otra_multa
			CASE param="08"
			   mont1=mul_medico
			CASE param="09"
			  //mont=mul_enfer
		      ENDCASE
		   ELSE
		      dias=30
		      mont1=0
                      ldias_em = 30
		   ENDIF

		   SELECT(area100)
		   IF anio_rec > 0 .OR. mes_rec > 0
		      fechita = halla_fecha(fech_ing,anio_rec,mes_rec,dia_rec)
		   ELSE
		      fechita = fech_ing
		   ENDIF

		   sue_mens=sueldo(nivel,jor_trab)
		   //indice=ind_efec
		   IF reincorp ="S"
		      indice = efectivo("B",fechita,sue_mens)  // Fecha de ingreso institucional
		   ELSE
		      indice = efectivo("B",fech_ing,sue_mens)
		   ENDIF

	           IF nitem = "CBB-001"
	              indice = 0       //gus
	           ENDIF

		   bon_antig= bono_antig("B",fechita)*jor_trab
		   IF dias=0
		     limpiar()
		     skip
		     LOOP
		   ENDIF

		   IF dias<30
		       sue_mens  = ROUND(ROUND(sue_mens/30,2)*dias,2)
		       bon_antig = ROUND(ROUND(bon_antig/30,2)*dias,2)
		      *indice    = ROUND(ROUND(ind_efec/30,2)*dias,2)
                       indice    = ROUND(ROUND(indice/30,2)*dias,2)
		   ENDIF

		    conge:=es_con(nitem)   // ingresos congelados
		    IF conge[2]!=0;indice=conge[2];ENDIF
		    IF conge[1]!=0;bon_antig=conge[1];ENDIF

		 por_prof = porce("B",fechita,CTOD("  /  /  "))        // porcentaje profesional
		 por_irra = porce("R",fechita,CTOD("  /  /  "))
		 dos_por   = escalafon(prof,sue_mens,1->nitem)

		*** IF conge[4]=0;hrs_adic = hrs_adicion(codigo,prof,sue_mens+dos_por);ELSE;hrs_adic=conge[4];ENDIF

	*	 cat_profe= categ_prof(sue_mens+dos_por,p_prof,codigo,prof,hrs_adic)

*                 IF !EMPTY(fecp) .AND. fecp >= CTOD('01/01/2003')
*                    cat_profe= categ_prof(sue_mens,p_prof,codigo,prof,hrs_adic)
*                 ELSE
*                    cat_profe= categ_prof(sue_mens+dos_por,p_prof,codigo,prof,hrs_adic)
*                 ENDIF
                 ******* em Congel.
                 em1 = escalafo0(prof,sue_mens,nitem,0)   // esc. med congel. al 2002
                 dos_por = em1
               * IF conge[4]=0;hrs_adic = hrs_adicion(codigo,prof,sue_mens+dos_por);ELSE;hrs_adic=conge[4];ENDIF
                 IF conge[4]=0;hrs_adic = hrs_adicion(codigo,prof,sue_mens);ELSE;hrs_adic=conge[4];ENDIF     // parametro it para octubre        1994
              *  IF !EMPTY(fecp) .AND. fecp >= CTOD('01/01/2003')
                 IF (!EMPTY(fecp) .AND. fecp >= CTOD('01/01/2003') ) .OR. ;
                    (LEFT(prof,6)=="MEDICO" .AND. SUBSTR(codigo,8,1)=="A")
                   *cat_profe= categ_prof(sue_mens,p_prof,codigo,prof,hrs_adic)
                   cat_profe= categ_prof(sue_mens,p_prof,codigo,prof,0)
                 ELSE
                   *cat_profe= categ_prof(sue_mens+dos_por,p_prof,codigo,prof,hrs_adic)
                    cat_profe= categ_prof(sue_mens+em1,p_prof,codigo,prof,hrs_adic)
                 ENDIF
                 dif_esc = escalafo0(prof,sue_mens,nitem,1)   // difer.escal. actual y congel.
                 *******

		 IF conge[3]=0;sub_irrad= sub_irradia(codigo,fechita,sue_mens);ELSE;sub_irrad=conge[3];ENDIF

		* emerg     = servicio_em(serv_emerg,sueldo(nivel,1.0)+dos_por,bon_antig,cat_profe)
                * emerg     = servicio_em(serv_emerg,sueldo(nivel,1.0)+em1,bon_antig,cat_profe)

               IF ldias_em<30        // dias serv. emergencia
                  IF !EMPTY(feemer) .AND. feemer >= CTOD('01/01/2003')
                   emerg    = servicio_em(serv_emerg,sueldo(nivel,1.0),0,0)
                  ELSE
                   emerg    = servicio_em(serv_emerg,sueldo(nivel,1.0)+em1,bon_antig,cat_profe)
                  ENDIF
                  emerg  = ROUND(ROUND(emerg/30,2)*ldias_em,2)
               ELSE
                  IF !EMPTY(feemer) .AND. feemer >= CTOD('01/01/2003')
                   emerg    = servicio_em(serv_emerg,sueldo(nivel,1.0),0,0)
                  ELSE
                   emerg    = servicio_em(serv_emerg,sueldo(nivel,1.0)+em1,bon_antig,cat_profe)
                  ENDIF

               ENDIF


		*v_h  = valor_hora(sexo,prof,sueldo(nivel,1.0)+dos_por,bon_antig,cat_profe,dias)
                *v_h  = valor_hora(sexo,prof,sue_mens+dos_por,bon_antig,cat_profe,dias)
                // v_h  = valor_hora(sexo,prof,sue_mens+em1,bon_antig,cat_profe,dias)

               IF !EMPTY(fech_ing) .AND. fech_ing >= CTOD('01/01/2003')
                v_h  = valor_hora(sexo,prof,sue_mens,0,0,dias)
               ELSE
                //v_h  = valor_hora(sexo,prof,sue_mens+em1,bon_antig,cat_profe,dias)
                v_h  = valor_hora(sexo,prof,sue_mens,bon_antig,cat_profe,dias)
               ENDIF


               *IF nitem = "CBB-156" .OR. nitem = "CBB-159"
               *   v_h  = valor_hora(sexo,prof,sue_mens+em1,bon_antig,0,dias)
               *ENDIF

               IF ALLTRIM(nombre) = "MOREIRA PALENQUE,ALVARO" .OR. ALLTRIM(nombre) = "VILLARROEL RIVERO,JULIO"
                  //v_h  = valor_hora(sexo,prof,sue_mens+em1,bon_antig,0,dias)
                  v_h  = valor_hora(sexo,prof,sue_mens,bon_antig,0,dias)
               ENDIF

               IF ALLTRIM(nombre) = "DEHEZA UGARTE,RUTH PATRICIA"
                  //v_h  = valor_hora(sexo,prof,sue_mens+em1,bon_antig,0,dias)
                  v_h  = valor_hora(sexo,prof,sue_mens,bon_antig,0,dias)
               ENDIF


		hrs_extras = hora_extra(1->nitem,v_h)
		 recarg    = recarg_noct(1->nitem,v_h)
		 bon_nat   = bonos_mater(1->nitem)

		 *IF  p_prof > 0
		    *bon_nat[4]=45*jor_trab
                    *bon_nat[4]=95*jor_trab
		 *ELSE
		 *   bon_nat[4]=0
		 *ENDIF

                IF dias<30
                  bon_nat[4]= ROUND(ROUND(95/30,2)*dias*jor_trab,2)
                ELSE
                 bon_nat[4]=95*jor_trab
                ENDIF

                
	       IF nitem = "CBB-001" 
	          bon_nat[4] = 0      //gus
	       ENDIF

*		 TotalG    = sue_mens+bon_antig+indice+cat_profe+hrs_adic+sub_irrad+emerg+hrs_extras+recarg+bon_nat[1]+
*			  bon_nat[2]+bon_nat[3]+bon_nat[4]+dos_por

*                 TotalG    = ROUND(sue_mens+bon_antig+indice+cat_profe+hrs_adic+sub_irrad+emerg+hrs_extras+recarg+bon_nat[1]+;
*		             bon_nat[2]+bon_nat[3]+bon_nat[4]+dos_por,2)

    TotalG    = ROUND(sue_mens+bon_antig+indice+cat_profe+hrs_adic+sub_irrad+emerg+hrs_extras+recarg+bon_nat[1]+;
		   bon_nat[2]+bon_nat[3]+bon_nat[4]+dos_por+dif_esc,2) 

	IF jor_trab=1
 		limite=22986	
 	else
       	limite=10215
	endif
	
	if TotalG > limite
		indice = indice - TotalG + limite
		if indice < 0
			bon_antig = bon_antig + indice
			indice = 0
			if bon_antig < 0
				emerg = emerg + bon_antig
				bon_antig = 0
				if emerg < 0
					recarg = recarg + emerg	
					emerg = 0
					if recarg < 0
						hrs_extras = hrs_extras + recarg
						recarg = 0
						if hrs_extras < 0
							dos_por = dos_por + hrs_extras
							hrs_extras = 0
							if dos_por < 0
								cat_profe = cat_profe + dos_por
								dos_por = 0
								if cat_profe < 0
									cat_profe = 0
								endif
							endif
						endif
					endif
				endif
			endif
		endif
	endif
	
	TotalG    = ROUND(sue_mens+bon_antig+indice+cat_profe+hrs_adic+sub_irrad+emerg+hrs_extras+recarg+bon_nat[1]+;
		   bon_nat[2]+bon_nat[3]+bon_nat[4]+dos_por+dif_esc,2) 

		     IF  opc=1
			TotalG=sue_mens
		     ENDIF
		     vecto[1] = espacios(nombre,40)
		     vecto[2] = TotalG
		     vecto[3] = ROUND(TotalG*porc,2)
		     vecto[4] = mont+mont1

		     IF !EVERYROW(forma,vecto,.T.)
			EXIT
		     ENDIF
		     vt[2]=vt[2]+vecto[2]
		     vt[3]=vt[3]+vecto[3]
		     vt[4]=vt[4]+vecto[4]
	       ENDIF
	       SKIP
	 ENDDO
	 SELECT(area201)
	 SKIPLINE(1,.T.)
	 vt[1]=espacios("Total General :",40)
	 EVERYROW(forma,vt,.T.)
	 FINISHREPORT()
RETURN
FUNCTION afp_rep(op)

   IF op=.T.
      repor_afp(.T.,"AFP BBV")
   ELSE
      repor_afp(.F.,"AFP FUTURO")
   ENDIF
RETURN

FUNCTION repor_afp(opc,titulo)
LOCAL porc,mont,p[10],forma,forma1,vecto[4],vt[4],mont1:=0,tg:="Total Ganado      "

	 vt=AFILL(vt,0.0)
	 area201:=SELECT()

	 pre_pla(titulo)

	 IF LASTKEY()=27;SELECT(area201);RETURN;ENDIF

	 IF !STARTREPORT(80,'R',4,5)
	    RETURN ' '
	 ENDIF

	 p[1]= '<,057,04,'+la_empresa
	 p[2]= '<,155,04,'+el_Lugar
	 p[3]= 'pagino'
	 p[4]= '^,185,00,'+titulo
	 p[5]= '^,000,00, ' +  "Cochabamba, "+ FECHALIT(date(),3,3)
	 p[6]= '<,000,00'
	 p[7]= '<,009,5,'+ ' Correspondiente al mes de:'+alltrim(lmeses)+ "/"+ALLTRIM(RIGHT(STR(anio),2))
	 p[8]='<,009,5,'+' Nombre del Empleado              '+tg+'APORTE LABORAL 12.5 %'
	 p[9]='<,009,5,'+REPLICATE('_',95)
	 EVERYHEADER (@p,9)
	 p[1]='Nombre del Empleado                                 '+tg+'Porcentaje (Bs.)   '
	 p[2]=REPLICATE('_',95)
		  forma ="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX       ###,###,###.##        ###,###,###.##"
	 HEADERSCREEN(@p,2)

	 SELECT 1
	 use dat_empl index nitem,nombre,codigo,eventual,ci
	 SET ORDER TO 1
	 GO TOP
	 WHILE !EOF()
	       IF cual_afp(fecha_nac)=opc .AND. !(ALLTRIM(nombre)="ACEFALIA" .OR. ALLTRIM(nombre)=="COMISION" .OR. tip_empl="E")
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
		   IF anio_rec > 0
		      fechita = halla_fecha(fecha_ing,anio_rec,mes_rec,dia_rec)
		   ELSE
		      fechita = fech_ing
		   ENDIF

		   sue_mens=sueldo(nivel,jor_trab)
		   //indice=ind_efec
		   IF reincorp ="S"
			indice = efectivo("B",fechita,sue_mens)
		   ELSE
		      indice = efectivo("B",fech_ing,sue_mens)  // Fecha de ingreso institucional
		   ENDIF

	           IF nitem = "CBB-001"
	              indice = 0       //gus
	           ENDIF

		   bon_antig= bono_antig("B",fechita)*jor_trab
		   IF dias=0
		     limpiar()
		     skip
		     LOOP
		   ENDIF

		   IF dias<30
		       sue_mens  = ROUND(ROUND(sue_mens/30,2)*dias,2)
		       bon_antig = ROUND(ROUND(bon_antig/30,2)*dias,2)
		       indice    = ROUND(ROUND(ind_efec/30,2)*dias,2)
		   ENDIF

		    conge:=es_con(nitem)   // ingresos congelados
		    IF conge[2]!=0;indice=conge[2];ENDIF
		    IF conge[1]!=0;bon_antig=conge[1];ENDIF

		 por_prof = porce("B",fechita,CTOD("  /  /  "))        // porcentaje profesional
		 por_irra = porce("R",fechita,CTOD("  /  /  "))
		 dos_por   = escalafon(prof,sue_mens,1->nitem)

		 IF conge[4]=0;hrs_adic = hrs_adicion(codigo,prof,sue_mens+dos_por);ELSE;hrs_adic=conge[4];ENDIF

		 cat_profe= categ_prof(sue_mens+dos_por,p_prof,codigo,prof,hrs_adic)

		 IF conge[3]=0;sub_irrad= sub_irradia(codigo,fechita,sue_mens);ELSE;sub_irrad=conge[3];ENDIF

		 emerg     = servicio_em(serv_emerg,sueldo(nivel,1.0)+dos_por,bon_antig,cat_profe)
		 v_h       = valor_hora(sexo,prof,sue_mens+dos_por,bon_antig,cat_profe,dias)
		 hrs_extras = hora_extra(1->nitem,v_h)
		 recarg    = recarg_noct(1->nitem,v_h)
		 bon_nat   = bonos_mater(1->nitem)

		 *IF  p_prof > 0 ; bon_nat[4]=45*jor_trab ;ELSE;bon_nat[4]=0;ENDIF
                 *IF  p_prof > 0 ; bon_nat[4]=60*jor_trab ;ELSE;bon_nat[4]=0;ENDIF
                 *bon_nat[4]=60*jor_trab
                 bon_nat[4]=95*jor_trab

	         IF nitem = "CBB-001" 
	            bon_nat[4] = 0      //gus
	         ENDIF
		 

    TotalG    = ROUND(sue_mens+bon_antig+indice+cat_profe+hrs_adic+sub_irrad+emerg+hrs_extras+recarg+bon_nat[1]+;
		   bon_nat[2]+bon_nat[3]+bon_nat[4]+dos_por+dif_esc,2) 

	IF jor_trab=1
 		limite=22986	
 	else
       	limite=10215
	endif
	
	if TotalG > limite
		indice = indice - TotalG + limite
		if indice < 0
			bon_antig = bon_antig + indice
			indice = 0
			if bon_antig < 0
				emerg = emerg + bon_antig
				bon_antig = 0
				if emerg < 0
					recarg = recarg + emerg	
					emerg = 0
					if recarg < 0
						hrs_extras = hrs_extras + recarg
						recarg = 0
						if hrs_extras < 0
							dos_por = dos_por + hrs_extras
							hrs_extras = 0
							if dos_por < 0
								cat_profe = cat_profe + dos_por
								dos_por = 0
								if cat_profe < 0
									cat_profe = 0
								endif
							endif
						endif
					endif
				endif
			endif
		endif
	endif
	
       TotalG    = ROUND(sue_mens+bon_antig+indice+cat_profe+hrs_adic+sub_irrad+emerg+hrs_extras+recarg+bon_nat[1]+;
		   bon_nat[2]+bon_nat[3]+bon_nat[4]+dos_por+ dif_esc,2) 


		     vecto[1] = espacios(nombre,40)
		     vecto[2] = TotalG
		     vecto[3] = ROUND(TotalG*porc_afp,2)

		     IF !EVERYROW(forma,vecto,.T.)
			EXIT
		     ENDIF
		     vt[2]=vt[2]+vecto[2]
		     vt[3]=vt[3]+vecto[3]
	       ENDIF
	       SKIP
	 ENDDO
	 SELECT(area201)
	 SKIPLINE(1,.T.)
	 vt[1]=espacios("Total General :",40)
	 EVERYROW(forma,vt,.T.)
	 FINISHREPORT()
RETURN

FUNCTION repor_fonvi(opc,titulo)
LOCAL porc,mont,p[10],forma,forma1,vecto[4],vt[4],mont1:=0,tg:="Total Ganado      "

	 vt=AFILL(vt,0.0)
	 area201:=SELECT()

	 pre_pla(titulo)

	 IF LASTKEY()=27;SELECT(area201);RETURN;ENDIF

	 IF !STARTREPORT(80,'R',4,5)
	    RETURN ' '
	 ENDIF

	 p[1]= '<,057,04,'+la_empresa
	 p[2]= '<,155,04,'+el_Lugar
	 p[3]= 'pagino'
	 p[4]= '^,185,00,'+titulo
	 p[5]= '^,000,00, ' +  "Cochabamba, "+ FECHALIT(date(),3,3)
	 p[6]= '<,000,00'
	 p[7]= '<,009,5,'+ ' Correspondiente al mes de:'+alltrim(lmeses)+ "/"+ALLTRIM(STR(anio))
       p[8]='<,009,5,'+' Nombre del Empleado              '+tg
	 p[9]='<,009,5,'+REPLICATE('_',95)
	 EVERYHEADER (@p,9)
	 p[1]='Nombre del Empleado                                 '+tg+'Porcentaje (Bs.)   '
	 p[2]=REPLICATE('_',95)
		  forma ="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX       ###,###,###.##        ###,###,###.##"
	 HEADERSCREEN(@p,2)

	 SELECT 1
	 use dat_empl index nitem,nombre,codigo,eventual,ci
	 SET ORDER TO 1
	 GO TOP
	 WHILE !EOF()
	       IF cual_afp(fecha_nac)=opc .AND. !(ALLTRIM(nombre)="ACEFALIA" .OR. ALLTRIM(nombre)=="COMISION" .OR. tip_empl="E")
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
		   IF fech_rec!=CTOD("  /  /  ")
		      fechita = fech_rec
		   ELSE
		      fechita = fech_ing
		   ENDIF

		   sue_mens=sueldo(nivel,jor_trab)
		   //indice=ind_efec
		   indice = efectivo("B",fech_ing,sue_mens)  // Fecha de ingreso institucional
		   bon_antig= bono_antig("B",fechita)*jor_trab
		   IF dias=0
		     limpiar()
		     skip
		     LOOP
		   ENDIF

		   IF dias<30
		       sue_mens  = ROUND(ROUND(sue_mens/30,2)*dias,2)
		       bon_antig = ROUND(ROUND(bon_antig/30,2)*dias,2)
		       indice    = ROUND(ROUND(ind_efec/30,2)*dias,2)
		   ENDIF

		    conge:=es_con(nitem)   // ingresos congelados
		    IF conge[2]!=0;indice=conge[2];ENDIF
		    IF conge[1]!=0;bon_antig=conge[1];ENDIF

		 por_prof = porce("B",fechita,CTOD("  /  /  "))        // porcentaje profesional
		 por_irra = porce("R",fechita,CTOD("  /  /  "))
		 dos_por   = escalafon(prof,sue_mens,1->nitem)

		 IF conge[4]=0;hrs_adic = hrs_adicion(codigo,prof,sue_mens+dos_por);ELSE;hrs_adic=conge[4];ENDIF

		 cat_profe= categ_prof(sue_mens+dos_por,p_prof,codigo,prof,hrs_adic)

		 IF conge[3]=0;sub_irrad= sub_irradia(codigo,fechita,sue_mens);ELSE;sub_irrad=conge[3];ENDIF

		 emerg     = servicio_em(serv_emerg,sueldo(nivel,1.0)+dos_por,bon_antig,cat_profe)
		 v_h       = valor_hora(sexo,prof,sue_mens+dos_por,bon_antig,cat_profe,dias)
		 hrs_extras = hora_extra(1->nitem,v_h)
		 recarg    = recarg_noct(1->nitem,v_h)
		 bon_nat   = bonos_mater(1->nitem)

		 TotalG    = sue_mens+bon_antig+indice+cat_profe+hrs_adic+sub_irrad+emerg+hrs_extras+recarg+bon_nat[1]+;
			  bon_nat[2]+bon_nat[3]+bon_nat[4]+dos_por


		     vecto[1] = espacios(nombre,40)
		     vecto[2] = TotalG
		     vecto[3] = fonvis(TotalG,2) // fonvi

		     IF !EVERYROW(forma,vecto,.T.)
			EXIT
		     ENDIF
		     vt[2]=vt[2]+vecto[2]
		     vt[3]=vt[3]+vecto[3]
	       ENDIF
	       SKIP
	 ENDDO
	 SELECT(area201)
	 SKIPLINE(1,.T.)
	 vt[1]=espacios("Total General :",40)
	 EVERYROW(forma,vt,.T.)
	 FINISHREPORT()
RETURN

FUNCTION fonv_rep(op)

   IF op=.T.
      repor_fonvi(.T.,"AFP BBV")
   ELSE
      repor_fonvi(.F.,"AFP FUTURO")
   ENDIF
RETURN
