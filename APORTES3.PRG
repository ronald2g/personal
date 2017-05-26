*****************************************************************************
*****************************************************************************
**                                                                          *
**  MODULO              :     REPORTE DE PLANILLA DE APORTES LABORALES      *
**                            Y PATRONALES
**  PARTE DEL SISTEMA   :     ALMACENES                                     *
**  PROPIETARIO         :     CAJA PETROLERA DE SALUD                       *
**  AUTOR(ES)           :     RICHARD JIMENEZ                               *
**                            JULIO ANTEZANA R                              *
**  VERSION             :     1.0                                           *
**  CREACION            :     SEPTIEMBRE 5 DE 1994                          *
**  ULTIMA REVISION     :                                                   *
**                                                                          *
*****************************************************************************
*****************************************************************************
**                                                                          *
**  DESCRIPCION     : El presente modulo tiene como objetivo de             *
**                    sacar un reporte de los aportes laborales             *
**                    y patronales                                          *
**                                                                          *
*****************************************************************************
*****************************************************************************


FUNCTION lab_pat()
LOCAL tituloscrren, referescreen, rec
LOCAL forma, forma_, forma1, i
LOCAL pp[13], vector[15], vec_tot[16], vec_sub[1]
LOCAL personal:={"Pers. Administrativo","Pers. Para-M‚dico    ","Pers. M‚dico         "}

pre_pla("Planilla de Aportes Laborales y Patronales")
vec_tot = AFILL (vec_tot,0.0)

IF !STARTREPORT(135,'R',4,5)
   RETURN ' '
ENDIF

tituloscreen = ' PLANILLA  DE  APORTES   LABORALES  Y  PATRONALES'
referescreen = ' Ref: Aportes correspondientes al mes de :'+lmeses+ "/"+ALLTRIM(RIGHT(STR(YEAR(cad_fecha)),2))

pp[1]= '<,057,04,'+la_empresa
pp[2]= '<,155,04,'+el_Lugar
pp[3]= ''
pp[4]= '^,185,00,'+tituloscreen
pp[5]= '^,000,00, ' +  "Cochabamba, " + FECHALIT(cad_fecha,3,3)
pp[6]= '<,000,00'
pp[7]= '<,009,5,'+ referescreen
pp[8] = '<,004,5,======================================================================================================================================================================================================='
pp[9] = '<,004,5, C O N C E P T O                  SALARIO              TOTAL             FONVI BBV.                          FONVI FUTURO                   A.F.P. BBV               A.F.P.  FUTURO          C.P.S     '
pp[10]= '<,004,5                                   COTIZABLE            SALARIO       LABORAL       PATRONAL           LABORAL        PATRONAL         LABORAL       PATRONAL     LABORAL       PATRONAL     PATRONAL   '
pp[11]= '<,004,5,                            AFP BBV       AFP FUTURO                     1%            2%               1%              2%             12.5%           2%          12.5%         2%          10%       '
pp[12]= '<,004,5,======================================================================================================================================================================================================='
pp[13]= '<,000,00'
EVERYHEADER (@pp,13)

       forma  =  "XXXXXXXXXXXXXXXXXXXX      ###,###.##      ###,###.##   ###,###.##    ###,###.##         ###,###.##    ###,###.##     ###,###.##      ###,###.##    ###,###.##   ###,###.##   ###,###.##   ###,###.##"
       forma1 =  "XXXXXXXXXXXXXXXXXXXX      ###,###.##      ###,###.## #####,###.##    ###,###.##         ###,###.##    ###,###.##     ###,###.##      ###,###.##    ###,###.##   ###,###.##   ###,###.##   ###,###.##"
       forma2 =  "                    XXXXXXXXXXXXXXXXXXXX                       XXXXXXXXXXXXXXXXXXXX                                    XXXXXXXXXXXXXXXXXXXXX                                   "
       forma_     = REPLICATE('X',198)
       vec_sub[1] = REPLICATE('-',178) //192
pp[1]= '======================================================================================================================================================================================================='
pp[2]= ' C O N C E P T O                  SALARIO              TOTAL             FONVI BBV.                          FONVI FUTURO                   A.F.P. BBV               A.F.P.  FUTURO          C.P.S     '
pp[3]= '                                  COTIZABLE            SALARIO       LABORAL       PATRONAL           LABORAL        PATRONAL         LABORAL       PATRONAL     LABORAL       PATRONAL     PATRONAL   '
pp[4]= '                            AFP BBV       AFP FUTURO                     1%            2%               1%              2%             12.5%           2%          12.5%         2%          10%       '
pp[5]= '======================================================================================================================================================================================================='


HEADERSCREEN(@pp,5)

pp[1] = '^,002,05,.................................................................................................................................................................................... Continua en la Siguiente Pagina'
EVERYTRAILER(@pp,1)
SELECT 50
USE total
GO 1
 salADM=admin;salpara=paramedico;salMED=medico

FOR J=1 TO 3

      vector[1]  =  personal[J]

      DO CASE
	CASE J=1
	  vector[2]  = salADM
	  vector[3]  = admin1
	  vector[4]  = vector[2]+vector[3]
	  vector[5]  = fonvAbbv
	  vector[6]  = ROUND(vector[2]*pat_fonvi/100,2)
	  vector[7]  = fonvAfut
	  vector[8]  = ROUND(vector[3]*pat_fonvi/100,2)
	  vector[9]  = fopebaA  // bbv Administrativos
	  vector[10] = bbv_patA
	  vector[11] = fcpA     // futuro administrativos
	  vector[12] = fut_patA

	CASE J=2
	  vector[2]  =  salPARA
	  vector[3]  =  PARA1
	  vector[4]  =  vector[2]+vector[3]
	  vector[5]  =  fonvPbbv
	  vector[6]  =  ROUND(vector[2]*pat_fonvi/100,2)
	  vector[7]  =  fonvPfut
	  vector[8]  =  ROUND(vector[3]*pat_fonvi/100,2)
	  vector[9]  =  fopebaP
	  vector[10]  =  bbv_patP
	  vector[11]  =  fcpP
	  vector[12] =  fut_patP

	CASE J=3
	  vector[2] = salMED
	  vector[3] = MEDICO1
	  vector[4] = vector[2]+vector[3]
	  vector[5] = fonvMbbv
	  vector[6] = ROUND(vector[2]*pat_fonvi/100,2)
	  vector[7] = fonvMfut
	  vector[8] = ROUND(vector[3]*pat_fonvi/100,2)
	  vector[9] = fopebaM
	  vector[10] = bbv_patM
	  vector[11] = fcpM
	  vector[12] =  fut_patM

      ENDCASE


      vector[13]  =  ROUND(vector[4]*pat_cps/100,2)


      IF !EVERYROW(forma,vector,.T.)
	 EXIT
      ENDIF

      FOR i := 2 TO 13       //totales
	  vec_tot[i] = ROUND(vec_tot[i] + vector[i],2)
      ENDFOR
ENDFOR
      
SKIPLINE(2,.T.)

vec_sub[1] = REPLICATE('=',198)
IF !EVERYROW(forma_,vec_sub,.T.)
   FINISHREPORT()
   RETURN
ENDIF

vec_tot[1] =ESPACIOS( 'T O T A L E S :',20)

IF !EVERYROW(forma1,vec_tot,.T.)
   FINISHREPORT()
   RETURN
ENDIF

SKIPLINE(10,.T.)

jef_perso()
FINISHREPORT()

RETURN

STATIC FUNCTION rec_apell(item)
LOCAL a, cad := ''

       a = SELECT()

       SELECT 1
       use dat_empl index nitem,nombre,codigo,eventual,ci
       SEEK item
       IF FOUND()
	  cad = nombre
       ENDI

       SELECT (a)
RETURN cad

STATIC FUNCTION espacios(cadena, tam)
LOCAL cad := ''

      cad = LEFT(cadena,tam)
      cad = cad + SPACE(tam - LEN(cad))

RETURN cad

FUNCTION resu_tot
LOCAL pp[50],ff:="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                          ###,###.##"
PUBLIC suma:=0
	 area101:=SELECT()
	 SELECT 50
	 USE total
	 GO 1
	 IF !STARTREPORT(80,'O',4,5)
	    RETURN ' '
	 ENDIF
	 tituloscreen = ' RESUMEN DE DESCUENTOS'
	 referescreen = ' Ref:  Res£men correspondiente al mes de :'+alltrim(mes)+ "/"+ALLTRIM(RIGHT(STR(anio),2))

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= '^,185,00,'+tituloscreen
	 pp[5]= '^,000,00, ' +  "Cochabamba, " + FECHALIT(date(),3,3)
	 pp[6]= '<,000,00'
	 pp[7]= '<,009,5,'+ referescreen
	 pp[8]= '<,009,5,'+' Todas las cantidades estan expresadas en  Bolivianos (Bs.)'
	 EVERYHEADER (@pp,8)

	 SKIPLINE(1,.T.)
	 impri('I.V.A',iva);suma=suma+iva
	 impri('Colegio M‚dico',col_med);suma=suma+col_med
	 impri('Colegio de Bioqu¡mica y Farmacia',col_b_f);suma=suma+col_b_f
	 impri('Colegio de Enfermeras',col_enfer);suma=suma+col_enfer
	 impri('Anticipos',anticipo);suma=suma+anticipo
	 impri('Asociaci¢n de Cirug¡a ',aso_cir);suma=suma+aso_cir
	 impri('Asociaci¢n de Pediatria ',aso_ped);suma=suma+aso_ped
	*impri('Asociaci¢n de Auxiliares de Enfermeria ',aso_aux_e);suma=suma+aso_aux_e
         impri('Colegio de Odont¢logos ',aso_aux_e);suma=suma+aso_aux_e
	 impri('Sindicato  Administrativo ',sind_adm);suma=suma+sind_adm
	 impri('Sindicato de Enfermeras ',sind_enfer);suma=suma+sind_enfer
	 impri('Sindicato M‚dico ',sind_medic);suma=suma+sind_medic
	 impri('F.S.T.S.S. ',confe_s_ad);suma=suma+confe_s_ad   // antes: Confederaci¢n Sindicato Administrativo 
	 impri('Pro Deportes ',fuerte);suma=suma+fuerte
	 impri('Multas por Atrasos',multas);suma=suma+multas
	 impri('Central Obrera Deptal.',fondo_soc);suma=suma+fondo_soc
	 impri('Anticipos II',pulperia);suma=suma+pulperia
	 impri('Refrigerio',refrige);suma=suma+refrige

	 pp[21]='<,000,00, Pulperia  .............................'+ STR(ROUND(pulperia,2))
	 pp[22]='<,000,00, Retenciones Judiciales ................'+ STR(ROUND(retencion,2))
	 pp[23]='<,000,00, Multas por Atrasos ....................'+ STR(ROUND(multas,2))
	 pp[24]='<,000,00, Central Obrera Deptal..................'+ STR(ROUND(fondo_soc,2))
	 pp[25]='<,000,00, Pr‚stamo Complementario ...............'+ STR(ROUND(prest_comp,2))
	 pp[26]='<,000,00, Otros .................................'+ STR(ROUND(otros,2))


	 pp[1]= la_empresa
	 pp[2]= el_Lugar
	 pp[3]= ''
	 pp[4]= tituloscreen
	 pp[5]= "Cochabamba, " + FECHALIT(cad_fecha,3,3)
	 pp[6]= REPLICATE('_',80)
	 pp[7]=  referescreen
	 HEADERSCREEN(@pp,7)
	 pp[1] = ''
/*       IF !EVERYROW(REPLICATE("_",80),pp,.T.)
	   FINISHREPORT()
	   SELECT(area101)
	   return ''
	 ENDIF */

	 pres()
	 SKIPLINE(1,.T.)
	 pul_ret('RETENCIONES JUDICIALES',2)
	 SKIPLINE(1,.T.)
	 otros()
	 SKIPLINE(5,.T.)
	 pp[1]=espacios("TOTAL DESCUENTOS ",40)
	 pp[2]=suma
	 IF !EVERYROW(ff,pp,.T.);FINISHREPORT();ENDIF
	 FINISHREPORT()
	 SELECT(area101)
RETURN

FUNCTION ite_emp()
  listado(1,'por No. de Item')
RETURN

FUNCTION ape_emp()
   listado(2,'por orden Alfabetico')
RETURN




STATIC FUNCTION listado(parame,cadena)
LOCAL pp[25],forma,vec[2]
	 area102:=SELECT()
	SELECT 1
	use dat_empl index nitem,nombre,codigo,eventual,ci
	SET ORDER TO parame
	IF !STARTREPORT(80,'R',4,5)
	    RETURN ' '
	 ENDIF
	 tituloscreen = 'Lista de Empleados '+ cadena
	 referescreen = ' Ref:  Res£men correspondiente al mes de :'+ alltrim(lmeses) + "/"+ALLTRIM(RIGHT(STR(anio),2))

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= '^,185,00,'+tituloscreen
	 pp[5]= '^,000,00, ' +  "Cochabamba," + FECHALIT(date(),3,3)
	 pp[6]= '<,000,00'
	 pp[7]= '<,009,5,'+ referescreen

	 EVERYHEADER (@pp,7)
	 pp[1]= la_empresa
	 pp[2]= el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= tituloscreen
	 pp[5]= "Cochabamba,"  + FECHALIT(date(),3,3)
	 pp[6]= ''
	 pp[7]= referescreen


	 pp[1]="No. de Item                 Nombre del Empleado"
	 pp[2]=REPLICATE('_',80)
	 forma="XXXXXXXXXX                  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
	 
	 HEADERSCREEN(@pp,2)
	 GO TOP
	 DO WHILE !EOF()
	    vec[1]=nitem
	    vec[2]=nombre
	   IF !EVERYROW(forma,vec,.T.)
	      salir:=.F.
	      EXIT
	   ENDIF
	    SKIP
	 ENDDO

	 FINISHREPORT()
	 SELECT(area102)
RETURN

FUNCTION otros()
LOCAL  sum_vec[30],k:=0,fin,cant_empl[30],vec[8],pp[10],acr[30],ss:=0,nit
LOCAL  ff2:="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                          ###,###.##"
       vec[1]=''
       EVERYROW("OTROS",vec,.T.,9)
       SKIPLINE(1,.T.)

       AFILL(sum_vec,0.0)
       AFILL(cant_empl,0)
       area104:=SELECT()
	 acr=llen_acre()
	 SELECT 48
	 USE ACREEDOR INDEX Acreedor
	 GO top
	 fin=LASTREC()
	 SELECT 91
	 USE presta INDEX presta_i
	 DO WHILE !EOF()
	    //ind=STUFF(nitem,1,7,"")
	      nit = ALLTRIM(nitem)

	      ind=right(nit,2)
	     
	    //IF isdigit(SUBSTR(nitem,8,1))
	      sum_vec[VAL(ind)]=sum_vec[VAL(ind)]+monto
	   // ENDIF
	   // cant_empl[VAL(ind)]=cant_empl[VAL(ind)]+1
	    SKIP
	 ENDDO


	 forma="XXXXXXXXXXXXXXXXXXXXXX         #,###,###.##"
	 //HEADERSCREEN(@pp,9)


	 FOR k=2 TO fin
	   vec[1]=acr[k-1]
	   //vec[2]=cant_empl[k]
	   //vec[3]=ROUND(sum_vec[k]/cant_empl[k],2)
	   vec[2]=sum_vec[k]
	   ss=ss+vec[2]
	   IF !EVERYROW(forma,vec,.T.)
	      salir:=.F.
	      EXIT
	   ENDIF
	   IF ultima_linea<4
	      NEWPAGE()
	   ENDIF

	 ENDFOR
	 SKIPLINE(1,.T.)
	vec[1]=espacios("TOTAL OTROS",40);vec[2]=ss
	suma=suma+vec[2]
	EVERYROW(ff2,vec,.T.)
	IF ultima_linea<4
	      NEWPAGE()
	 ENDIF

       SELECT(area104)
RETURN

FUNCTION pul_ret(tit,param)
LOCAL form:="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX    XXXXXXXXXXXXXXXXXXXX    ###,###.##"
LOCAL ff:="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                          ###,###.##"
LOCAL vect[4]

       area110:=SELECT()
       vect[1]=''
       vect[3]=0
       vect[4]=0
	IF !EVERYROW(tit,vect,.T.,9);FINISHREPORT();ENDIF
	SKIPLINE(1,.T.)
	SELECT 32
	USE personal  INDEX perso_it
	GO TOP
	DO WHILE !EOF()
	   DO CASE
	      CASE param=1
		  IF pulperia>0
		    vect[1]=espacios(nombre,30)
		    vect[2]=pulperia
		    vect[3]=ROUND(vect[3]+pulperia,2)
		    IF !EVERYROW(form,vect,.T.)
		      EXIT
		    ENDIF

		  ENDIF
	     CASE param=2
		  IF reten_jud>0 .AND. RIGHT(ALLTRIM(nitem),1)!="E"
		    vect[1]=espacios(nombre,30)
		    vect[2]=espacios(nomb_rete,20)
		    vect[3]=reten_jud
		    vect[4]=ROUND(vect[4]+reten_jud,2)
		    IF !EVERYROW(form,vect,.T.)
		      EXIT
		    ENDIF
		  ENDIF
	     CASE param=3
		  IF refrigerio>0
		    vect[1]=espacios(nombre,30)
		    vect[2]=''
		    vect[3]=refrigerio
		    IF !EVERYROW(form,vect,.T.)
		      EXIT
		    ENDIF
		    vect[4]=ROUND(vect[4]+refrigerio,2)
		  ENDIF

	   ENDCASE

	   IF ultima_linea<4
	      NEWPAGE()
	   ENDIF
	  SKIP
	ENDDO
	  IF ultima_linea<4
	      NEWPAGE()
	  ENDIF
	  SKIPLINE(1,.T.)
	  vect[1]=espacios('TOTAL '+tit ,40);vect[2]=vect[4]
	  suma=suma+vect[4]
	   IF !EVERYROW(ff,vect,.T.)
		 FINISHREPORT()
	   ENDIF
       SELECT(area110)
RETURN

STATIC FUNCTION impri(nomb,cant)
LOCAL ff,vv[2]
	 ff="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                          ###,###.##"
	 vv[1]=espacios(nomb,40);vv[2]=cant
	 EVERYROW(ff,vv,.T.)
	  IF ultima_linea<4
	      NEWPAGE()
	  ENDIF
RETURN

STATIC FUNCTION pres()
LOCAL ff,vv[2],pp:=0.0,hh:=0.0,tip_prest
LOCAL ff1:="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                          ###,###.##"
       ff="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX              ###,###.##"
       area111:=SELECT()
       vv[1]=''
       EVERYROW('PRESTAMO COMPLEMENTARIO',vv,.T.,9)
       SKIPLINE(1,.T.)
       SELECT 91
       USE presta INDEX presta_i

       DO WHILE !EOF()
	 tip_prest=RIGHT(ALLTRIM(nitem),1,1)
	  DO CASE
	     CASE tip_prest="H"
		 HH=ROUND(HH+ROUND(monto*ldol,2),2)
	     CASE tip_prest="P"
		 PP=ROUND(PP+ROUND(monto*ldol,2),2)
	  ENDCASE
	 SKIP
       ENDDO
       vv[1]=espacios('Pr‚stamo Personal',40);vv[2]=pp
       EVERYROW(ff,vv,.T.)

       vv[1]=espacios('Pr‚stamo Hipotecario',40);vv[2]=hh
       EVERYROW(ff,vv,.T.)
       vv[1]=espacios('TOTAL PRESTAMOS',40);vv[2]=hh+pp
       suma=suma+vv[2]
       EVERYROW(ff1,vv,.T.)
	  IF ultima_linea<4
	      NEWPAGE()
	  ENDIF
       SELECT(area111)
RETURN

