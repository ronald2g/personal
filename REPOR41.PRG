*****************************************************************************
*****************************************************************************
**                                                                          *
**  MODULO              :     REPORTE DE PLANILLA DE PAGOS                  *
**  PROPIETARIO         :     CAJA PETROLERA DE SALUD                       *
**  AUTORES             :     RICHARD JIMENEZ                               *
**                            JULIO ANTEZANA R                              *
**  VERSION             :     1.0                                           *
**  CREACION            :     29 DE AGOSTO DE 1994                          *
**  ULTIMA REVISION     :     17 DE FEBRERO DE 1997                                                   *
**                                                                          *
*****************************************************************************
*****************************************************************************
**                                                                          *
**  DESCRIPCION     : El presente modulo tiene como objetivo el de poder    *
**                    sacar un reporte de la planilla de pagos de un mes    *
**                    determinado                                           *
*****************************************************************************
*****************************************************************************

FUNCTION ini_var
PUBLIC f:={},bon_nat:={0.0,0.0,0.0,0.0},pulpe:=0
PUBLIC bon_antig:=0,sue_mens:=0,cat_profe:=0,v_h:=0,hrs_adic:=0,TotalG:=0,TotalD:=0,sub_irrad:=0
PUBLIC emerg:=0,recarg:=0,hrs_extras:=0,col_bio_fa:=0,retencion:=0
PUBLIC desc_ley:=0.0,afp_bbv:=0,afp_fut:=0,fopeba:=0,fonvi:=0,prest:={},sindicatoA:=0,multas:=0, otros:=0, el_fuerte:=0
PUBLIC col_medico:=0,col_enferm:=0,por_prof:=0,aso_cir:=0,aso_ped:=0,totald:=0,renta:=0,aso_aux_enf:=0,anticipos:=0,por_irra:=0
PUBLIC c_petrol_s:=0,a_p_a:=0,fdo_labor:=0,fdo_pen_ca:=0,fdo_pen_in:=0,col_econom:=0,cta_person:=0,refrige:=0
PUBLIC fdo_vivien:=0,ptmo_escol:=0,gar_fdo_cps:=0,c_s_t_s_S:=0,cuota_mort:=0,sindicatoM:=0,sindiC:=0,sindicatoE:=0,fdo_social:=0
PUBLIC prest_caja:=0,dias:=0,medio:="    ",indice:=0
PUBLIC fonv_bbv:=0,fonv_fut:=0,bon_prof:=0,dif_esc:=0, ldias_em:=0
RETURN

****s* BASEDD/CARGO
*  DATOS
*    codigo : [ PP SS AAA E CCC ]  
*        PP : Programa
*        SS : Subprograma
*       AAA : Actividad (departamento)
*         E : Sector
*       CCC : Cargo
******

****f* REPORTE/PLANILLA
*  DESCRIPCION
*    Genera el reporte de planilla del personal regular.
*  SINOPSIS
FUNCTION planilla()
*  ALGORITMO	
LOCAL tituloscrren, referescreen, rec,salir:=.T.
LOCAL forma, forma1, forma2, forma3, forma4, formal, anterior, lcargo
LOCAL format,forma1t,forma2t,forma3t,forma4t
LOCAL pp[20],vector[16], vector1[16], vector2[16], vector3[16], vector4[16]
LOCAL vec_tot[20],vec_tot1[20], vec_tot2[20], vec_tot3[20], vec_tot4[20]
LOCAL temp_tot[20],temp_tot1[20], temp_tot2[20], temp_tot3[20], temp_tot4[20]
LOCAL ante,tempo_vec[20],ante_act,cabeza
PUBLIC subprograma:=SPACE(10),actividad:=SPACE(10),apo[3],ric[3],ric1[3],ric2[3],fcssmra:=0
PUBLIC ric3[3],ric4[3],apo1[3],ri[3]
//showTIME()

*    * Establecer mes de emision de la planilla
prepla("Planilla de Sueldos y Salarios")  

IF LASTKEY()=27;RETURN;ENDIF
*    * Inicializar variables con 0
hoy[1]="   "
vec_tot  = AFILL (vec_tot,0.0)
vec_tot1 = AFILL (vec_tot1,0.0)
vec_tot2 = AFILL (vec_tot2,0.0)
vec_tot3 = AFILL (vec_tot3,0.0)
vec_tot4 = AFILL (vec_tot4,0.0)
apo      = AFILL (apo,0.0)
ric      = afill(ric,0.0)
ri       = afill(ri,0.0)
ric1     = afill(ric1,0.0)
ric2     = afill(ric2,0.0)
ric3     = afill(ric3,0.0)
ric4     = afill(ric4,0.0)
apo1     = afill(apo1,0.0)

//tempo_vec= AFILL(tempo_vec,0.0)
temp_tot  = AFILL (temp_tot,0.0)
temp_tot1 = AFILL (temp_tot1,0.0)
temp_tot2 = AFILL (temp_tot2,0.0)
temp_tot3 = AFILL (temp_tot3,0.0)
temp_tot4 = AFILL (temp_tot4,0.0)

aDbf:= { {"NITEM","C",10,0},{"CARGO","C",50,0},{"NOMBRE","C",50,0},{"NIVEL","C",3,0},;
          {"DIAS","N",6,0},{"SUE_MENS","N",10,2},{"HRS_EXTRAS","N",10,2},;
          {"BON_ANTIG","N",10,2},{"P_PROF","N",3,0},{"CAT_PROFE","N",10,2},;
          {"INDICE","N",10,2},{"HRS_ADIC","N",10,2},;
          {"SUB_IRRAD","N",10,2},{"BONO_PROF","N",10,2},{"HRS_NOCT","N",10,2},;
          {"EMERG","N",10,2},{"ESC_MED","N",10,2},{"DOS_POR","N",10,2},{"TOTALG","N",10,2},;
          {"AFP_BBV","N",10,2},{"AFP_FUT","N",10,2},{"IVA","N",10,2},;
          {"ANTICIPOS","N",10,2},{"COL_MEDICO","N",10,2},{"COL_BIO_FA","N",10,2},;
          {"COL_ENFERM","N",10,2},{"ASO_CIR","N",10,2},{"ASO_PED","N",10,2},{"COL_ODONT","N",10,2},;
          {"SINDICATOA","N",10,2},{"SINDICATOM","N",10,2},{"CONFED","N",10,2},;
          {"EL_FUERTE","N",10,2},{"MULTAS","N",10,2},{"RETENCION","N",10,2},;
          {"PULPE","N",10,2},{"REFRIGE","N",10,2},{"OTROS","N",10,2},;
          {"TOT_DESC","N",10,2},{"LIQUIDO","N",10,2},{"DIF_ESC","N",10,2} }

IF !FILE("reint.dbf")
   DBCREATE("reint",aDbf)
ENDIF


SELECT 7
USE reint
ZAP
//BROWSE()
//USE


SELECT 1
USE dat_empl index nitem,nombre,codigo,eventual,ci
//USE dat_empl INDEX orden
SET ORDER TO 1
IF LASTREC() = 0
   RETURN
ENDIF
*    * Iniciar hoja de reportes con STARTREPORT
IF !STARTREPORT(132,'R',4,2)
   RETURN ' '
ENDIF

//tituloscreen = 'PLANILLA DE PAGO DE HABERES DE LA 'la _empresa //CAJA PETROLERA DE SALUD'
//referescreen = ' Ref: sueldos del Mes de: '+lmeses
  SELECT 1
  USE dat_empl index nitem,nombre,codigo,eventual,ci
  //USE dat_empl INDEX orden
  SET ORDER TO 1
  GO TOP


  
*    * Establecer encabezado del reporte mostrando el [Programa y subprograma]
	cabeza=encabezado(codigo)
	
*    * Guardar el [subprograma] que se esta listando
  	ante_act=substr(codigo,3,2)
  	
*    * Define la cabecera para el reporte en [pantalla,impresora]
	cabecera(cabeza[1],cabeza[2])
	
       forma  = "XXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX          XXX ### ####,###.## ####,###.## ####,###.##              ###,###.## ###,###.## ###,###.## ###,###.## ###,###.## ####,###.## ####,###.## ####,###.##  "
       forma1 = "           XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        ####,###.## ####,###.## ####,###.##              ###,###.## ###,###.## ###,###.## ###,###.## ###,###.## ####,###.## ####,###.## ####,###.##  "
       forma2 = "           XXXXXXXXXXXXX                               ### ####,###.## ####,###.## ####,###.## ####,###.##  ###,###.## ###,###.## ###,###.## ###,###.## ###,###.## ####,###.## ####,###.## ####,###.##  ####,###.##  ####,###.##"
       forma3 = "           XXXXXXXXXX                                      ####,###.## ####,###.## ####,###.##              ###,###.## ###,###.## ###,###.##"
       forma4 = "           XXXXXXXXXX      XXXXXXXXXX                      ####,###.## ####,###.## ####,###.##"
       formal = REPLICATE('X',40)
       format = "                               XXXXXXXXXXXXXXXXXXXX        ####,###.## ####,###.## ####,###.##              ####,###.## ####,###.## ####,###.## ####,###.## ####,###.## ####,###.## ####,###.## ####,###.##  "
       forma1t= "                                                           ####,###.## ####,###.## ####,###.##              ####,###.## ####,###.## ####,###.## ####,###.## ####,###.## ####,###.## ####,###.## ####,###.##  "
       forma2t= "                                                           ####,###.## ####,###.## ####,###.## ####,###.##  ####,###.## ####,###.## ####,###.## ####,###.## ####,###.## ####,###.## ####,###.## ####,###.##  ####,###.##  ####,###.##"
       forma3t= "                                                           ####,###.## ####,###.## ####,###.##              ####,###.## ####,###.## ####,###.## ####,###.## ####,###.## ####,###.## ####,###.## "
       forma4t= "                                                           ####,###.## ####,###.## ####,###.##"
       formax = "          XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                   ####,###.## ####,###.## ####,###.##              ####,###.## ####,###.## ####,###.## ####,###.## ####,###.## ####,###.## ####,###.## ####,###.##  "

*    * Define cabecera para reporte en [pantalla]
	pp[1]= '========================================================================================================================================================================================================================================'
	pp[2]= '    I      CARGO                                    N DIAS SUE.MENS.   HRS. EXTRAS                           AFP  BBV    IVA       COL.MEDICO  AS.CIRUGIA  SIND.MEDICO PRO-DEPORTE MULTAS        REFRIGERIO                           '
	pp[3]= '    T      APELLIDOS Y NOMBRES                      I      BON.ANTIG.  SUPLENCIAS               T O T A L    AFP FUTURO  ANTICIPOS COL.BIO.FA  AS.PEDIATR. SIND.ENFER. PTMO.COMPL. RETEN. JUD.   OTROS       T O T A L   L I Q U I D O'
	pp[4]= '    E      CARNET DE IDENTIDAD                      V PORC.CAT.PROFE.  IND.EFECT.  DIF.ESCAL.  G A N A D O   F.SOL. BBV            COL.ODONT.  SIND.ADMIN. F.S.T.S.S.S C.O.D.      ANTICIPO II               DESCUENTOS  P A G A B L E'
	pp[5]= '    M      FECHA DE NACIMIENTO                      E      HRS.ADIC.   SUB.IRRAD.  B.R.PROF.                 F.SOL. FUT            COL.ENFERM                                                                                         '
	pp[6]= '           FECH. ING.      FECH. REC.               L      HRS.NOCTUR. SER.EMERG.  ESCAL.ANT.                                                                                                                                           '
	pp[7]= '========================================================================================================================================================================================================================================'
	pp[8]= ' '
	HEADERSCREEN(@pp,8)

*    * Define pie para reporte 
pp[1] = '^,002,00, ................................................................................................................................................................................... Continua en la Siguiente P gina'
EVERYTRAILER(@pp,1)


*    * recuperar la actividad segun el codigo de cargo
vector[1] = ALLTRIM(nom_act(SUBSTR(codigo,5,3))) + _CI(4) + ' '  

IF !EVERYROW(formal,vector,.T.,172)
    //go bottom
ENDIF

SELECT 1
USE dat_empl index nitem,nombre,codigo,eventual,ci
//USE dat_empl INDEX orden
SET ORDER TO 1
SET FILTER TO tip_empl!="E"
GO TOP


*    * recorrer la lista del personal:
salir=.T.
DO WHILE !EOF()
	IF ALLTRIM(tip_empl)="E"
		SKIP
		LOOP
	ENDIF
	anterior = codigo
   
	*    * Encuentrar Programa y subprograma
	encabezado(codigo)

	*    * Recorrer items en el mismo programa y subprograma
	DO WHILE LEFT(anterior,4) = LEFT(codigo,4)
		IF ALLTRIM(tip_empl)="E"
			SKIP
			LOOP
		ENDIF

		area_:= SELECT()
		SELECT 7
		APPEND BLANK
		select(area_)

		*    * Calcular sueldo individual
		proceso(lmeses,nitem)

		vector[1]   = n_item                          // nro de item
		lcargo      = SUBSTR(codigo,9,3)              // codigo del cargo
		vector[2]   = ESPACIOS(nom_car(lcargo),26)+medio   // nombre del cargo
		vector[3]   = nivel                          // nivel
		vector[4]   = dias                           // porcentaje
		vector[5]   = sue_mens                       // sueldo mensual
		vector[6]   = hrs_extras                     // horas extras
		vector[7]   = bon_nat[1]                     // prenatal
		vector[8]   = afp_bbv                        // AFP BBV
		vector[9]   = renta                          // renta = iva c_petrol_s                         C. PETROL. S
		vector[10]  = col_medico                     // COLEGIO MEDICOsindicato                      sindicato
		vector[11]  = aso_cir                        // ASOCIACION CIRUGIA Y PEDIATRIA
		vector[12]  = sindicatoM                     //sindicato medico      cuota_mort                 cuota mortoria
		vector[13]  = el_fuerte                      // EL FUERTE
		vector[14]  = multas                         // multas
		vector[15]  = refrige  
		                      //
		FOR i = 5 TO 15  //2
			vec_tot[i -5 + 2] =ROUND( vec_tot[i - 5 + 2] + vector[i],2)
			temp_tot[i -5 + 2] =ROUND( temp_tot[i - 5 + 2] + vector[i],2)
		ENDFOR


		// genera planilla de reintegros
		
		REPLACE reint->nitem WITH n_item
		REPLACE reint->cargo WITH ESPACIOS(nom_car(lcargo),26)+medio
		REPLACE reint->nivel WITH nivel
		REPLACE reint->dias WITH dias
		REPLACE reint->sue_mens WITH sue_mens
		REPLACE reint->hrs_extras WITH hrs_extras
		REPLACE reint->afp_bbv WITH afp_bbv
		REPLACE reint->iva WITH renta
		REPLACE reint->col_medico WITH col_medico
		REPLACE reint->aso_cir WITH aso_cir
		REPLACE reint->sindicatoM WITH sindicatoM
		REPLACE reint->el_fuerte WITH el_fuerte
		REPLACE reint->multas WITH multas
		REPLACE reint->refrige WITH refrige



		vector1[1]   = ESPACIOS(STRTRAN(nombre,","," "),40)           // Apellidos y nombres
		vector1[2]   = bon_antig                      // Bono de Antiguedad
		vector1[3]   = 0                             //falta suplencias
		vector1[4]   = bon_nat[2]                    // natalidad
		vector1[5]   = afp_fut                    // AFP FUTURO
		vector1[6]   = anticipos
		vector1[7]   = col_bio_fa                     //COLEGIO DE BIOQUIMICA
		vector1[8]   = aso_ped                    // ASOCIACION CIRUGIA Y PEDIATRIA col_enferm                       // COLEGIO DE ENFERMERAS
		vector1[9]   = sindicatoE
		vector1[10]  = prest_caja                              //FALTA PRESTAMO COMPLEMENTARIO
		vector1[11]  = retencion                     // retenciones judiciales
		vector1[12]  = otros                              //
		FOR i = 2 TO 12
			vec_tot1[i -2 + 1] =ROUND( vec_tot1[i - 2 + 1] + vector1[i],2)
			temp_tot1[i -2 + 1] =ROUND( temp_tot1[i - 2 + 1] + vector1[i],2)
		ENDFOR

		REPLACE reint->nombre WITH ESPACIOS(STRTRAN(nombre,","," "),40)
		REPLACE reint->bon_antig WITH bon_antig
		REPLACE reint->afp_fut WITH afp_fut
		REPLACE reint->anticipos WITH anticipos
		REPLACE reint->col_bio_fa WITH col_bio_fa
		REPLACE reint->aso_ped WITH aso_ped
		REPLACE reint->retencion WITH retencion
		REPLACE reint->otros WITH otros


		vector2[1]   = ESPACIOS(ci,13)               // CI
		vector2[2]   = p_prof                        // porcentaje profesional
		vector2[3]   = cat_profe                     // categoria profesional
		vector2[4]   = indice                        // ind. efectividad.
		*vector2[5]   = bon_nat[3]                    // lactancia
		vector2[5]   = dif_esc                     // difer. escalafon

		vector2[6]   = totalG                        // total ganado
		vector2[7]   = fonv_bbv                      // fopeba
		vector2[8]   = bon_nat[3]                    // lactancia
		vector2[9]   = aso_aux_enf                   // ASOCIACION DE ENFERMERAS
		vector2[10]  = sindicatoA                    // sindicato admiistrativo
		vector2[11]  = sindiC                        // confederacion sindicato admi
		vector2[12]  = fdo_social
		vector2[13]  = pulpe
		vector2[14]  = 0
		vector2[15]  = totald                        // total descuento
		vector2[16]  = ROUND(totalg - totald,2)      // liquido pagable
		FOR i = 3 TO 16
			vec_tot2[i -3 + 1] =ROUND( vec_tot2[i - 3 + 1] + vector2[i],2)
			temp_tot2[i -3 + 1] =ROUND( temp_tot2[i - 3 + 1] + vector2[i],2)
		ENDFOR

		REPLACE reint->p_prof WITH p_prof
		REPLACE reint->cat_profe WITH cat_profe
		REPLACE reint->indice WITH indice
		REPLACE reint->totalg WITH totalg
		REPLACE reint->sindicatoA WITH sindicatoA
		REPLACE reint->col_odont WITH aso_aux_enf
		REPLACE reint->dif_esc WITH dif_esc

		REPLACE reint->confed WITH sindiC
		REPLACE reint->pulpe WITH pulpe
		REPLACE reint->tot_desc WITH totald
		REPLACE reint->liquido WITH ROUND(totalg - totald,2)

		vector3[1]   = fecha_nac                      // fecha de nacimiento
		vector3[2]   = hrs_adic                      // hrs. adicionales
		vector3[3]   = sub_irrad                     // sub. irradiacion
		vector3[4]   = bon_nat[4]                    //ANTES  sepelio ahora bono profesional
		//vector3[4]   = bon_prof                     Bono profesional
		vector3[5]   = fonv_fut                      // fonvi
		vector3[6]   = bon_nat[1]                    // prenatal
		vector3[7]   = col_enferm
		FOR i = 2 TO 7
			vec_tot3[i -2 + 1] =ROUND( vec_tot3[i - 2 + 1] + vector3[i],2)
			temp_tot3[i -2 + 1] =ROUND( temp_tot3[i - 2 + 1] + vector3[i],2)
		ENDFOR

		REPLACE reint->hrs_adic WITH hrs_adic
		REPLACE reint->sub_irrad WITH sub_irrad
		REPLACE reint->bono_prof WITH bon_nat[4]
		REPLACE reint->col_enferm WITH col_enferm

		vector4[1]   = fech_ing                      // fecha ingreso
		vector4[2]   = "          "
		IF fech_rec  != CTOD("  /  /    ") ;vector4[2]   = fech_rec;ENDIF
		vector4[3]   = recarg                        // horas nocturnas
		vector4[4]   = emerg                         // servicio de emergencia
		vector4[5]   = dos_por                       // escalafon de 2% medicos

		REPLACE reint->hrs_noct WITH recarg
		REPLACE reint->emerg WITH emerg
		REPLACE reint->esc_med WITH esc_med
		REPLACE reint->dos_por WITH dos_por

		FOR i = 3 TO 5
			vec_tot4[i -3 + 1] =ROUND( vec_tot4[i - 3 + 1] + vector4[i],2)
			temp_tot4[i -3 + 1] =ROUND( temp_tot4[i - 3 + 1] + vector4[i],2)
		ENDFOR

		IF ultima_linea < 7 //6
			NEWPAGE()
			encabezado(codigo)
		ENDIF

		IF !EVERYROW(formal,hoy,.T.)
			salir:=.F.
			EXIT
		ENDIF

		IF !EVERYROW(forma,vector,.T.)  //.OR. sale = .F.
			salir:=.F.
			EXIT
		ENDIF
		
		IF !EVERYROW(forma1,vector1,.T.)
	      		salir:=.F.
			EXIT
		ENDIF
		
		IF !EVERYROW(forma2,vector2,.T.)
			salir:=.F.
			EXIT
		ENDIF
		
		IF !EVERYROW(forma3,vector3,.T.)
			salir:=.F.
			EXIT
		ENDIF
		
		IF !EVERYROW(forma4,vector4,.T.)
			salir:=.F.
			EXIT
		ENDIF

		anterior = codigo
		
		SKIP 1
		IF !EOF()
			cabeza=encabezado(codigo)
		ELSE
			salir=.F.
			EXIT
		ENDIF

		IF ante_act<>substr(codigo,3,2)
    			** browse()
     			//SUMAS
     			// tempo_vec[1]="TOTALES " +veop2[VAL(SUBSTR(anterior,3,2))]
     			NEWPAGE()
     			temp_tot[1]=espacios("TOTALES " +veop2[VAL(SUBSTR(anterior,3,2))],31)

			IF !EVERYROW(formax,temp_tot,.T.);   FINISHREPORT();RETURN;   ENDIF
			IF !EVERYROW(forma1T,temp_tot1,.T.); FINISHREPORT();RETURN;   ENDIF
			IF !EVERYROW(forma2t,temp_tot2,.T.); FINISHREPORT();RETURN;   ENDIF
			IF !EVERYROW(forma3t,temp_tot3,.T.); FINISHREPORT();RETURN;   ENDIF
			IF !EVERYROW(forma4t,temp_tot4,.T.); FINISHREPORT();RETURN;   ENDIF

			/*vec_tot[1]=''
			SKIPLINE(10,.T.)
			quien_firma()*/

	     		IF ante_act="03" //Consulta Externa
				vec_tot[1] = ''
				IF !EVERYROW(REPLICATE('-',229),vec_tot,.T.);  FINISHREPORT(); RETURN; ENDIF
				SKIPLINE(1,.T.)
				vec_tot[1] = ESPACIOS('TOTAL POLICLINICO : ',24)
				IF !EVERYROW(formaT,vec_tot,.T.);   FINISHREPORT();RETURN;   ENDIF
				IF !EVERYROW(forma1T,vec_tot1,.T.); FINISHREPORT();RETURN;   ENDIF
				IF !EVERYROW(forma2t,vec_tot2,.T.); FINISHREPORT();RETURN;   ENDIF
				IF !EVERYROW(forma3t,vec_tot3,.T.); FINISHREPORT();RETURN;   ENDIF
				IF !EVERYROW(forma4t,vec_tot4,.T.); FINISHREPORT();RETURN;   ENDIF

				vec_tot[1]=''
				SKIPLINE(10,.T.)
				quien_firma()

				SKIPLINE(1,.T.)

			END IF
			// Fin  Aumento

			temp_tot  = AFILL (temp_tot,0.0)
			temp_tot1 = AFILL (temp_tot1,0.0)
			temp_tot2 = AFILL (temp_tot2,0.0)
			temp_tot3 = AFILL (temp_tot3,0.0)
			temp_tot4 = AFILL (temp_tot4,0.0)

			ante_act=substr(codigo,3,2)
			NEWPAGE()
			cabecera(cabeza[1],cabeza[2])
			// avector[1] = espacios(ALLTRIM(nom_act(SUBSTR(codigo,5,3))),40) + _CI(4) + ' '  //ej adm. regional
			vector[1] = ALLTRIM(nom_act(SUBSTR(codigo,5,3))) + _CI(4) + ' '  //ej adm. regional

			IF !EVERYROW(formal,vector,.T.,172)
				EXIT
				salir:=.F.
			ENDIF
		ENDIF

		IF ALLTRIM(tip_empl)="E" .OR. ALLTRIM(tip_empl)="e"
			SKIP
			//LOOP
		ENDIF

		IF LEFT(anterior,4) = LEFT(codigo,4)
			IF  SUBSTR(anterior,5,3) <> SUBSTR(codigo,5,3)
				SKIPLINE(1,.T.)
				IF ultima_linea <= 7 //9
					NEWPAGE()
				ENDIF
				//vector[1] = espacios(ALLTRIM(nom_act(SUBSTR(codigo,5,3))),40) + _CI(4) + ' '  //ej adm. regional
				vector[1] = ALLTRIM(nom_act(SUBSTR(codigo,5,3))) + _CI(4) + ' '  //ej adm. regional
				IF !EVERYROW(formal,vector,.T.,172)
					EXIT
					salir:=.F.
				ENDIF
			ENDIF
		ENDIF
	// fin impresion de items del mismo programa y subprograma
	ENDDO //WHILE

	IF salir=.F.
		EXIT
	ENDIF

ENDDO //WHILE
SELECT 1
USE dat_empl index nitem,nombre,codigo,eventual,ci
SET FILTER TO

almacenar(vec_tot,vec_tot1,vec_tot2,vec_tot3,vec_tot4,apo,ric,ri,ric1,ric2,fcssmra,ric3,ric4,apo1)

SKIPLINE(1,.T.)

IF ultima_linea <= 7
	NEWPAGE()
ENDIF

// ultimo total
/*
temp_tot[1]=espacios("TOTALES " +veop2[VAL(SUBSTR(anterior,3,2))],31)

IF !EVERYROW(formax,temp_tot,.T.);   FINISHREPORT();RETURN;   ENDIF
IF !EVERYROW(forma1T,temp_tot1,.T.); FINISHREPORT();RETURN;   ENDIF
IF !EVERYROW(forma2t,temp_tot2,.T.); FINISHREPORT();RETURN;   ENDIF
IF !EVERYROW(forma3t,temp_tot3,.T.); FINISHREPORT();RETURN;   ENDIF
IF !EVERYROW(forma4t,temp_tot4,.T.); FINISHREPORT();RETURN;   ENDIF


 vec_tot[1] = ''
IF !EVERYROW(REPLICATE('-',229),vec_tot,.T.);  FINISHREPORT(); RETURN; ENDIF
SKIPLINE(1,.T.)
vec_tot[1] = ESPACIOS('TOTAL GENERAL : ',20)
IF !EVERYROW(formaT,vec_tot,.T.);   FINISHREPORT();RETURN;   ENDIF
IF !EVERYROW(forma1T,vec_tot1,.T.); FINISHREPORT();RETURN;   ENDIF
IF !EVERYROW(forma2t,vec_tot2,.T.); FINISHREPORT();RETURN;   ENDIF
IF !EVERYROW(forma3t,vec_tot3,.T.); FINISHREPORT();RETURN;   ENDIF
IF !EVERYROW(forma4t,vec_tot4,.T.); FINISHREPORT();RETURN;   ENDIF

vec_tot[1]=''

SKIPLINE(10,.T.)
quien_firma()
	      */
SKIPLINE(1,.T.)

IF !EVERYROW(REPLICATE('-',229),vec_tot,.T.);  FINISHREPORT(); RETURN; ENDIF
	
FINISHREPORT()
//showTIME(2,70,.F.,"B/W")
SELECT 7                  // REINT
*browse()
USE
RETURN
******



******************************************************************************
******* FIN  planilla()    ***************************************************
******************************************************************************


*****************************************************************************
*             PROCESOS DE PLANILLA                                ***********
*****************************************************************************


****f* PLANILLA/limpiar
*  FUNCION
*    inicializa las variables usadas en 0
*  SINOPSIS
PROCEDURE limpiar()
******
       sue_mens = 0;por_prof = 0;por_irra = 0;bon_antig=0;cat_profe=0;hrs_adic=0
       sub_irrad= 0;emerg=0;v_h = 0;hrs_extras = 0;recarg=0;bon_nat:={0.0,0.0,0.0,0.0}
       totalG   = 0; afp_bbv =0;afp_fut = 0;fopeba= 0;fonvi= 0
       desc_ley  = 0;renta = 0;col_medico = 0
       col_bio_fa = 0;col_enferm   = 0;aso_aux_enf = 0;aso_ped = 0;aso_cir=0
       sindicatoA  = 0;sindicatoM  = 0;sindicatoE  = 0;sindiC  = 0
       el_fuerte   = 0;anticipos   = 0;prest= 0;fdo_social= 0;c_petrol_s  = 0
       a_p_a = 0;fdo_labor   = 0; fdo_pen_ca  = 0;fdo_pen_in  = 0;col_econom  = 0
       cta_person  = 0;fdo_vivien  = 0;ptmo_escol  = 0;gar_fdo_cps = 0
       c_s_t_s_S   = 0;cuota_mort  = 0; multas = 0; otros       = 0
       refrige     = 0;retencion   = 0; pulpe = 0;indice=0
       totald      = 0;dias = 30;medio="    ";prest_caja=0
       dos_por     = 0
       fonv_bbv    = 0
       fonv_fut    = 0
	 bon_prof    = 0
         dif_esc     = 0
         ldias_em     = 30
RETURN

****f* PLANILLA/sueldo
*  FUNCION
*    Calcula sueldos del mes 
*  SINOPSIS
FUNCTION proceso(ndir_mes,it)
	
	LOCAL mul:=0,conge,mul1:=0
	
	IF (ALLTRIM(nombre)=="ACEFALIA" .OR. ALLTRIM(nombre)=="COMISION") .OR. tip_empl="E" .OR. tip_empl="e"
		limpiar()
	ELSE
		IF jor_trab=0.5
			medio="1/2t"
		ELSE
			medio="    "
		ENDIF

		area100:=SELECT()
	 	SELECT 32
	 		USE personal  INDEX perso_it,perso_nom
	 		SET ORDER TO 1
	 		SEEK alltrim(it)
	 		IF FOUND()
		    		dias=dias_trab
        	    		ldias_em = dias_em
	 		ELSE
	  			dias=30
          			ldias_em = 30
	 		ENDIF
	 	SELECT(area100)


	 /*
	 IF fech_rec!=CTOD("  /  /    ")
	    fechita = fech_rec
	 ELSE
	   fechita  = fech_ing
	 ENDIF
	 */

	 IF anio_rec > 0 .OR. mes_rec > 0
		fechita = halla_fecha(fech_ing,anio_rec,mes_rec,dia_rec)
	 ELSE
		fechita = fech_ing
	 ENDIF

	 sue_mens=sueldo(nivel,jor_trab)

	 // indice=ind_efec
	 // IF nitem ="CBB-001" .OR. nitem="CBB-003"
	 //   indice=ind_efec
	 // ELSE

	   IF reincorp = "S"
	     indice = efectivo("B",fechita,sue_mens)
	   ELSE
	     indice = efectivo("B",fech_ing,sue_mens)  // Fecha de ingreso institucional
	   ENDIF

	   IF nitem = "CBB-001" 
	      indice = 0       //gus
	   ENDIF

	// ENDIF

	 bon_antig= bono_antig("B",fechita)*jor_trab

	IF dias=0
	   limpiar()
	   dias=0
	   iva(nitem,totalG,desc_ley)
	   return
	ENDIF
	
       IF dias<30
	   sue_mens  = ROUND(ROUND(sue_mens/30,2)*dias,2)
	   bon_antig = ROUND(ROUND(bon_antig/30,2)*dias,2)
	  *indice    = ROUND(ROUND(ind_efec/30,2)*dias,2)
          indice    = ROUND(ROUND(indice/30,2)*dias,2)

       ENDIF
       conge:=es_con(it)   // ingresos congelados
	 IF conge[2]!=0;indice=conge[2];ENDIF
	 IF conge[1]!=0;bon_antig=conge[1];ENDIF

       por_prof = porce("B",fechita,CTOD("  /  /    "))        // porcentaje profesional
       por_irra = porce("R",fechita,CTOD("  /  /    "))

      *dos_por   = escalafon(prof,sue_mens,nitem)
  ***     IF conge[4]=0;hrs_adic = hrs_adicion(codigo,prof,sue_mens+dos_por);ELSE;hrs_adic=conge[4];ENDIF     // parametro it para octubre        1994

       ******* em Congel.
       em1 = escalafo0(prof,sue_mens,nitem,0)   // esc. med congel. al 2002
       dos_por = em1
      *IF conge[4]=0;hrs_adic = hrs_adicion(codigo,prof,sue_mens+dos_por);ELSE;hrs_adic=conge[4];ENDIF     // parametro it para octubre        1994
       IF conge[4]=0;hrs_adic = hrs_adicion(codigo,prof,sue_mens);ELSE;hrs_adic=conge[4];ENDIF     // parametro it para octubre        1994
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

      *emerg    = servicio_em(serv_emerg,sueldo(nivel,1.0)+dos_por,bon_antig,cat_profe)
      *emerg    = servicio_em(serv_emerg,sueldo(nivel,1.0)+em1,bon_antig,cat_profe)


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


      IF !EMPTY(fech_ing) .AND. fech_ing >= CTOD('01/01/2003')
       v_h  = valor_hora(sexo,prof,sue_mens,0,0,dias)
      ELSE
       //v_h  = valor_hora(sexo,prof,sue_mens+em1,bon_antig,cat_profe,dias)
       v_h  = valor_hora(sexo,prof,sue_mens,bon_antig,cat_profe,dias)
      ENDIF

       IF ALLTRIM(nombre) = "MOREIRA PALENQUE,ALVARO" .OR. ALLTRIM(nombre) = "VILLARROEL RIVERO,JULIO"
          //v_h  = valor_hora(sexo,prof,sue_mens+em1,bon_antig,0,dias)
          v_h  = valor_hora(sexo,prof,sue_mens,bon_antig,0,dias)
       ENDIF

       IF ALLTRIM(nombre) = "DEHEZA UGARTE,RUTH PATRICIA"
          //v_h  = valor_hora(sexo,prof,sue_mens+em1,bon_antig,0,dias)
          v_h  = valor_hora(sexo,prof,sue_mens,bon_antig,0,dias)
       ENDIF

       hrs_extras= hora_extra(nitem,v_h)
       
       if hrs_extras>=sue_mens .AND. sue_mens>0
       		hrs_extras = sue_mens - 1.0
       endif
       
       recarg    = recarg_noct(nitem,v_h)
       bon_nat   = bonos_mater(nitem)

*       	   IF nitem = "CBB-110"
*	      ? "sue_mens "+str(sue_mens)       //gus
*              ? "bon_antig "+str(bon_antig)
*              ? "cat_profe "+str(cat_profe)
*              ? "v_h "+str(v_h)
*              ? "hrs_extras "+str(hrs_extras)
*               ? "dias "+str(dias)
*               ? "indice "+str(indice)
*               ? "bon_nat2 "+str(bon_nat[2])
*              inkey(0)
*	   ENDIF


*	IF  p_prof > 0
*		gus bon_nat[4]=45*jor_trab
*		bon_nat[4]=60*jor_trab

		IF dias<30
			bon_nat[4]= ROUND(ROUND(95/30,2)*dias*jor_trab,2)
        ELSE
			bon_nat[4]=95*jor_trab
		ENDIF
	  
	// bono profesional no se paga a administradores
	   IF n_item = "CBB-001" 
	      bon_nat[4] = 0      //gus
	   ENDIF

*    if n_item = "CBB-052"   // durandal
*		dif_esc=0           // diferencia escalafon profesional
*		bon_nat[4]=0        // bono de riesgo salud
*	ENDIF   



       TotalG    = ROUND(sue_mens+bon_antig+indice+cat_profe+hrs_adic+sub_irrad+emerg+hrs_extras+recarg+bon_nat[1]+;
		   bon_nat[2]+bon_nat[3]+bon_nat[4]+dos_por,2) + dif_esc
     
     
       
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
		   bon_nat[2]+bon_nat[3]+bon_nat[4]+dos_por,2) + dif_esc

*		restar = hora2_extra(nitem,v_h,TotalG-limite)
*		hrs_extras = hrs_extras - restar
*		TotalG = TotalG - restar
*		restar = recarg2_noct(nitem,v_h,TotalG-limite)
*		recarg = recarg - restar
*  		TotalG = TotalG - restar

*          IF nitem = "CBB-110"
*               ? "bon_nat4 "+str(bon_nat[4])
*               ? "totalGanado "+str(totalG)
*              inkey(0)
*	   ENDIF

       // Descuentos a la AFPs
	IF aporte_afp = "S"
  		if viejo(fecha_nac)
	   		desc_afp = TotalG*desc_afp_b 
		else
			desc_afp = TotalG*porc_afp
	  	endif
  	ELSE
  		if viejo(fecha_nac)
	   		desc_afp = TotalG*desc_afp_d
		else
			desc_afp = TotalG*desc_afp_c
  		endif
	ENDIF
	
	
	desc_uno =  fsolidario(TotalG)
	//fonvi     =  fonvis(TotalG)
	
	if dat_empl->afp = "BBV"
		afp_bbv = ROUND(desc_afp,2)
		afp_fut = 0
		fonv_bbv = desc_uno
		fonv_fut = 0
	else
		afp_fut = ROUND(desc_afp,2)
		afp_bbv = 0
		fonv_fut = desc_uno
		fonv_bbv = 0
	endif


       //f_p_a_c_p_s = cps(prof,TotaLG)
       //afp_fut =  fpm(prof,TotaLG)
       //fopeba  =  fpb(TotaLG)
       //fonvi     =  fonvis(TotalG)

       DO CASE             //Salario Cotizable
	 CASE aportes="A"
	   if dat_empl->afp = "BBV"
	      apo[1]=ROUND(apo[1]+TotalG,2) //ROUND(apo[1]+ROUND(TotalG-desc_ley,2),2)
	      ric3[1]=ric3[1]+ROUND(TotalG*pat_bbv/100,2)
	   else
	      apo1[1]=ROUND(apo1[1]+TotalG,2)
	      ric4[1]=ric4[1]+ROUND(TotalG*pat_fut/100,2)
	   endif
	      ric[1]=ric[1]+fonv_bbv
	       ri[1]=ri[1]+fonv_fut
	      ric1[1]=ric1[1]+afp_bbv
	      ric2[1]=ric2[1]+afp_fut


	 CASE aportes="M"
	     if dat_empl->afp = "BBV"
	      apo[2]=ROUND(apo[2]+TotalG,2) //ROUND(apo[2]+ROUND(TotalG-desc_ley,2),2)
	       ric3[2]=ric3[2]+ROUND(TotalG*pat_bbv/100,2)
	     else
	      apo1[2]=ROUND(apo1[2]+TotalG,2)
	      ric4[2]=ric4[2]+ROUND(TotalG*pat_fut/100,2)
	     endif
	      ric[2]=ric[2]+fonv_bbv
	       ri[2]=ri[2]+fonv_fut
	      ric1[2]=ric1[2]+afp_bbv
	      ric2[2]=ric2[2]+afp_fut

	      
	      fcssmra=fcssmra+afp_fut
	 CASE aportes="P"
	     if dat_empl->afp = "BBV"
	      apo[3]=ROUND(apo[3]+TotalG,2) //ROUND(apo[3]+ROUND(TotalG-desc_ley,2),2)
	       ric3[3]=ric3[3]+ROUND(TotalG*pat_bbv/100,2)
	     else
	      apo1[3]=ROUND(apo1[3]+TotalG,2)
	      ric4[3]=ric4[3]+ROUND(TotalG*pat_fut/100,2)
	     endif

	      ric[3]=ric[3]+fonv_bbv
       	  ri[3]=ri[3]+fonv_fut
	      ric1[3]=ric1[3]+afp_bbv
	      ric2[3]=ric2[3]+afp_fut
	     
	      
       ENDCASE


       desc_ley   = afp_bbv+afp_fut+fopeba+desc_uno

       renta      = iva(nitem,TotalG,desc_ley)              //  IVA

       col_medico = general(asociacion,sue_mens,1,sue_mens)           //COLEGIO MEDICO
       col_bio_fa = general(asociacion,totalG,3,sue_mens)           //COLEGIO DE BIOQUIMICA
       col_enferm = general(asociacion,totalG,5,sue_mens)           //COLEGIO DE ENFERMERAS
       aso_aux_enf = general(asociacion,totalG,7,sue_mens)          //antes: ASOCIACION DE ENFERMERAS
                                                      //ahora: Col.Odontologos

       aso_cir = general(asociacion,totaLG,9,sue_mens)          //ASOCIACION CIRUGIA Y PEDIATRIA
       aso_ped = general(asociacion,totaLG,11,sue_mens)

       sindicatoA  = general(asociacion,totalG,13,sue_mens)         //SINDICATO ADMINISTRATIVO
       sindicatoM  = general(asociacion,totalG,15,sue_mens)   //SINDICATO MEDICO
       sindicatoE  = general(asociacion,totalG,17,sue_mens)  //SINDICATO DE ENFERMERAS
       sindiC      = general(asociacion,sue_mens,19,sue_mens)  //CONFEDERACION SIND. ADMI.
       el_fuerte   = general(asociacion,totalG,21,sue_mens)        //EL FUERTE

       anticipos   = anti_cipo(it)
       prest_caja  = halla_pres(it,"01P")+halla_pres(it,"01H")
       otros       = control_prest(it)
       fdo_social  = general(asociacion,totalG,23,sue_mens) //FONDO SOCIAL

	area25:=SELECT()
	 SELECT 32
	 USE personal  INDEX perso_it
	 set order to 1
	 SEEK alltrim(it)
	 IF FOUND()
	    multas    = multas(faltas,TotalG)+ROUND((TotalG - hrs_extras - emerg-recarg)/30,2)*falta_dia
	    refrige  = refrigerio
	    retencion= reten_jud
	    pulpe    = pulperia
	    mul      = otra_multa
	    mul1     = mul_medico
	    dias     = dias_trab
	 ELSE
	   multas    = 0
	   mul       = 0
	   mul1      = 0
	   retencion = 0
	   refrige   = 0
	   pulpe     = 0

	 ENDIF
	  sindicatoA=sindicatoA+mul
	  sindicatoM=sindicatoM+mul1
	 SELECT(area25)


//totald =ROUND(afp_bbv + afp_fut + col_medico+col_enferm + sindicatoA + sindicatoM+sindiC,2)

totald =ROUND(afp_bbv + afp_fut + fopeba + fonv_bbv + fonv_fut + renta + anticipos+col_medico+;
  col_bio_fa + col_enferm + aso_aux_enf + aso_cir+aso_ped + sindicatoA + sindicatoM+sindiC+sindicatoE+;
  el_fuerte +  multas + otros + prest_caja+fdo_social+retencion+pulpe+refrige,2)

  ENDIF
RETURN
******


****f* sueldo/cargo
*  FUNCION
*    Recupera el cargo de una persona de la tabla [ cargo.dbf ]
*  SINOPSIS
STATIC FUNCTION nom_car(cod)
*  FUENTE
	LOCAL a, cad := ''
	a = SELECT()
	SELECT 12
	use (dr+"cargo") index (dr+"cargo_co")
	SEEK cod
	IF FOUND()
		cad = B
	ENDIF
	SELECT(a)
RETURN cad
******

****f* sueldo/actividad
*  FUNCION
*    Recupera la actividad de una persona de la tabla [ sector.dbf ]
*  SINOPSIS
STATIC FUNCTION nom_act(cod)
*  FUENTE
	LOCAL a, cad := ''
	a = SELECT()
	SELECT (11)
	use (dr+"sector") index (dr+"sector")
	SEEK cod
	IF FOUND()
		cad = nombre
	ENDIF
	SELECT(a)
RETURN cad
******

//    Encuentra el Programa y Subprograma               
FUNCTION encabezado(codi)
LOCAL ret:={space(25),space(25)}
  ret[1]= SUBSTR(codi,1,2) +' '+ veop1[VAL(LEFT(codi,2))]
  ret[2]= SUBSTR(codi,3,2) +' '+ veop2[VAL(SUBSTR(codi,3,2))]
RETURN  ret

/*LOCAL formato, vector_aux[3], formal

      formato = REPLICATE('X',14) + ' ### ' + REPLICATE ('X',33)
      formal = REPLICATE('X',35)

      vector_aux[1] = 'PROGRAMA      '
      vector_aux[2] = '01'
      vector_aux[3] = 'PRESTACION DE SALUD '
      IF !EVERYROW(formato,vector_aux ,.T.,30)
	 RETURN .F.
      ENDIF

      vector_aux[1] = 'SUB. PROGRAMA '
      vector_aux[2] = SUBSTR(codi,1,2)
      vector_aux[3] = veop1[VAL(LEFT(codi,2))]  nom_area(SUBSTR(codi,1,4),0)
      IF !EVERYROW(formato,vector_aux ,.T.,24)
	 RETURN .F.
      ENDIF

      vector_aux[1] = 'ACTIVIDAD     '
      vector_aux[2] = SUBSTR(codi,3,2)
      vector_aux[3] = veop2[VAL(SUBSTR(codi,3,2))]  nom_area(SUBSTR(codi,1,4),1)

      IF !EVERYROW(formato,vector_aux ,.T.,53)
	 RETURN .F.
      ENDIF

      SKIPLINE(1,.T.)

      vector_aux[1] = ALLTRIM(nom_act(SUBSTR(codi,5,3))) + _CI(4) + ' '  ej adm. regional

      IF !EVERYROW(formal,vector_aux,.T.,172)
	 RETURN .F.
      ENDIF
RETURN      */


*****************************************************************************
**     EXTRAE EL NOMBRE DE LA UBICACION DE UN ITEM                         **
*****************************************************************************

STATIC FUNCTION nom_area(codi,pos)
LOCAL a, nom := ''

      a = SELECT()
      SELECT (10)
      use (dr+"meses") index (dr+"meses")
      SEEK codi
      IF FOUND()
	 IF pos =0
	    nom = nombre
	 ELSE
	    nom = nombre1
	 ENDIF
      ENDIF
      SELECT(a)

RETURN nom

function liqui()
   liqui_bene("CBB-097","00000056","JUBILACION","09/07/94")
return



FUNCTION liqui_bene(item,memo,motivo,fecha_ret)
LOCAL tituloscrren, referescreen, rec
LOCAL forma, forma_, forma1,niv,jort,T1:=0,T2:=0,T3:=0,TOTAL:=0,AA:=0,BB:=0,CC:=0,DD:=0,EE:=0.0
LOCAL pp[6], vector[13], vec_tot[7], vec_sub[1],NOMB,ident

select 1
USE dat_empl index nitem,nombre,codigo,eventual,ci

vec_tot = AFILL (vec_tot,0.0)

IF !STARTREPORT(80,'R',10,5)
   RETURN ' '
ENDIF

tituloscreen = 'LIQUIDACION DE BENEFICIOS SOCIALES'

pp[1]= '<,057,04,'+la_empresa
pp[2]= '<,155,04,'+el_Lugar
pp[3]= ''
pp[4]= '^,185,00,'+tituloscreen
pp[5]= '^,000,00, ' +  "Cochabamba, " + FECHALIT(DATE(),3,3)
pp[6]= '<,000,00'
//pp[7]= '<,009,5,'+ referescreen

EVERYHEADER (@pp,6)
pp[1]= la_empresa
pp[2]= el_Lugar
pp[3]= '                                                                         '
pp[4]= '------------------------------------------------------------------------------------'

HEADERSCREEN(@pp,4)

pp[1] = '^,002,05,.................................................................................................................................................................................... Continua en la Siguiente Pagina'
EVERYTRAILER(@pp,1)


	forma_ = REPLICATE('=',80)
	forma  = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
	forma1 = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX   XXXXXXXXXXXXXXXXX"
	forma2 = "XXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXX                                                "
	forma3 = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
	forma4 = "XXXXXXXXXXXXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
	forma5 = "XXXXXXXXXXXXXXXXXXXXXX     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
	forma6 = "XXXXXXXXXXXXXXX  XXXXXXXXXX  XXXXXXX #### XXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
	forma7 = "XXXXXXXXXXXXXXXXXXXXXXX   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
	forma8 = "XXXXXX XXXXXXXXXX     XXXXXXXXXXXXXXXXX  XXXXXXX   XXXXXXXXXXXXXXXX  XXXXXXXX   "
	forma9=  "XXXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXX    ###,###.##           "
	forma10= "XXXXXXXXXXXXXXXXXXX ###   XXXX ### XXXXXXX ### XXXX                              "

	forma11= "XXXXXXXXX  XXXXXXXXXXX   XXXXXXXXXXX   XXXXXXXXXXX   XXXXXXX                     "
	forma12= "XXXXXXXXX   ###,###.##    ###,###.##     ###,###.##     ###,###.##               "

	forma13="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX    ###,###,###.##"
	forma14="XXXXXXXXXXXXXXXXXXXXX ### XXXXXXXXX ###,###.##                                    "
	forma15="XXXXXXXXXXXXXXXXXXXXXXXXX #### XXXXXXXXX ### XXXXXXXXXXXXXXXXXXXXXXXXX  ###,###.##"
	forma16="XXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ###,###.##"
	forma17="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ###,###.##                               "
	forma18="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ###,###.##              XXX ###,###.##   "
	forma19="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

      SELECT 1
       USE dat_empl index nitem,nombre,codigo,eventual,ci
      SEEK item
      f_ingreso=fech_ing
      niv      =nivel
      jort     =jor_trab
      ident    =ci
      vector[1]  = _ci(8)+"LIQUIDACION DE BENEFICIOS SOCIALES EN FAVOR :"
      vector[2]  = _ci(7)+ ESPACIOS(rec_apell(ITEM), 40)
      nomb       =vector[2]
      EVERYROW(forma,vector,.T.)
      vector[1] = _ci(8)+"EX FUNCIONARIO DE LA C.P.S. REGIONAL COCHABAMBA SEGUN MEMO No."
      vector[2] = _ci(7)+ memo
      EVERYROW(forma1,vector,.T.)

      vector[1] = _ci(8)+"DE FECHA "
      vector[2] = _ci(7)+FECHALIT(DATE(),3,3)
      EVERYROW(forma2,vector,.T.)

      EVERYROW(REPLICATE("_",80),' ',.T.)

      vector[1] =_ci(8)+"RAZON SOCIAL O NOMBRE DE LA EMPRESA :"
      vector[2] =_ci(7)+ la_empresa //"CAJA PETROLERA DE SALUD"
      EVERYROW(forma3,vector,.T.)

      vector[1] =_ci(8)+"RAMA DE ACTIVIDAD ECONOMICA :"
      vector[2] = "............."
      vector[3] = "DOMICILIO"
      vector[4] = _ci(7)+"Av. San Martin No. 567"        //insertar domicilio de la institucion
      EVERYROW(forma4,vector,.T.)

      vector[1] = "NOMBRE DEL TRABAJADOR :"
      vector[2] = _ci(7)+ESPACIOS(rec_apell(ITEM), 40)
      EVERYROW(forma5,vector,.T.)

      SELECT 24
      SEEK item

      vector[1] = _CI(8)+"ESTADO CIVIL"
      vector[2] = _CI(7)+ estado(est_civil)
      vector[3] = _CI(8)+"EDAD"
      vector[4] =  YEAR(DATE())-YEAR(fecha_nac)
      vector[5] = "DOMICILIO "
      vector[6] = "Av. San Martin No.456" // hallar el domicilio del empleado
      EVERYROW(forma6,vector,.T.)

      vector[1] = "PROFESION U OCUPACION :"
      vector[2] = "LIC. INFORMATICA" // hallar la profesion
      EVERYROW(forma7,vector,.T.)

      vector[1] = "C.I. :"
      vector[2] =  ident
      vector[3] = "FECHA DE INGRESO:"
      vector[4] =  f_ingreso
      vector[5] = "FECHA DE RETIRO :"
      vector[6] = fecha_ret
      EVERYROW(forma8,vector,.T.)

      vector[1] = "MOTIVO DEL RETIRO:"
      vector[2] =  motivo
      vector[3] = "REMUNERACION MENSUAL"
      vector[4] =  12213.45 //Calcular sueldo
      EVERYROW(forma9,vector,.T.)

      vector[1] = "TIEMPO DE SERVICIO"
      vector[2] =  YEAR(DATE())-YEAR(f_ingreso)
      vector[3] = "A¥OS"
      vector[4] =  MONTH(DATE())
      vector[5] = "MESES Y "
      vector[6] =  DAY(DATE())
      vector[7] =  "DIAS"
      EVERYROW(forma10,vector,.T.)

      EVERYROW(REPLICATE("_",80),' ',.T.)
      EVERYROW("LIQUIDACION DE LA REMUNERACION PROMEDIO INDEMNIZABLE (3 ULTIMOS MESES)",' ',.T.)
      EVERYROW(REPLICATE("_",80),' ',.T.)

      vector[1]="A) MESES"
      vector[2]=mes(MONTH(DATE()))    //hallar los 3 ultimos meses
      vector[3]=mes(MONTH(DATE())-1)
      vector[4]=mes(MONTH(DATE())-2)
      vector[5]="TOTALES"
      EVERYROW(forma11,vector,.T.)
      EVERYROW("REMUNERACION",' ',.T.)

      vector[1]="  MENSUAL"
      vector[2]= sueldo(niv,jort)      //hallar los 3 ultimos meses
      vector[3]= sueldo(niv,jort)
      vector[4]= sueldo(niv,jort)
      vector[5]= ROUND((vector[2]+vector[3]+vector[4]),2)
      aa:=vector[5]
      EVERYROW(forma12,vector,.T.)

      EVERYROW("B) OTROS CONCEPTOS",'',.T.)
      EVERYROW("PERCIBIDOS EN EL MES",'',.T.)

      vector[1]="  BONO1" // bono de antiguedad + de radiacion, etc
      vector[2]= 12213.56
      vector[3]= 12213.56
      vector[4]= 12213.56
      vector[5]= ROUND((vector[2]+vector[3]+vector[4]),2)
      T1=T1+vector[2]
      T2=T2+vector[3]
      T3=T3+vector[4]
      TOTAL=TOTAL+vector[5]
      EVERYROW(forma12,vector,.T.)

      vector[1]="  BONO2"
      vector[2]= 12213.56
      vector[3]= 12213.56
      vector[4]= 12213.56
      vector[5]= ROUND((vector[2]+vector[3]+vector[4]),2)
      T1=T1+vector[2]
      T2=T2+vector[3]
      T3=T3+vector[4]
      TOTAL=TOTAL+vector[5]
      EVERYROW(forma12,vector,.T.)

      vector[1]="  BONO3"
      vector[2]= 12213.56
      vector[3]= 12213.56
      vector[4]= 12213.56
      vector[5]= ROUND((vector[2]+vector[3]+vector[4]),2)
      T1=T1+vector[2]
      T2=T2+vector[3]
      T3=T3+vector[4]
      TOTAL=TOTAL+vector[5]
      bb=TOTAL
      EVERYROW(forma12,vector,.T.)

      vector[1]="  TOTAL"
      vector[2]= T1
      vector[3]= T2
      vector[4]= T3
      vector[5]= TOTAL
      EVERYROW(forma12,vector,.T.)

      EVERYROW(REPLICATE("_",80),' ',.T.)
      vector[1]= "III TOTAL REMUNERACION PROMEDIO INDEMNIZABLE (A+B) DIVIDIDO 3 Bs."
      vector[2]= ROUND((AA+BB)/3,2)
      EVERYROW(forma13,vector,.T.)
      EVERYROW(REPLICATE("_",80),' ',.T.)

      vector[1]= "C) DESAHUCIO TRES MESES (EN CASO DE RETIRO FORZOSO)        Bs."
      IF motivo="FORZOSO"
	    vector[2]= AA
      ELSE
	   vector[2]=0.0
      ENDIF
      CC:=vector[2]
      EVERYROW(forma13,vector,.T.)

      vector[1]= "INDEMNIZACION POR TIEMPO DE TRABAJO                        Bs."
      vector[2]= 14324.56
      DD=vector[2]
      EVERYROW(forma13,vector,.T.)

      vector[1]= "                  DE"
      vector[2]= 45
      vector[3]="A¥OS  Bs."
      vector[4]= 1244.56
      EVERYROW(forma14,vector,.T.)

      vector[1]= "                  DE"
      vector[2]= 12
      vector[3]="MESES Bs."
      vector[4]= 1244.56
      EVERYROW(forma14,vector,.T.)

      vector[1]= "                  DE"
      vector[2]= 12
      vector[3]="DIAS  Bs."
      vector[4]= 1244.56
      EVERYROW(forma14,vector,.T.)

      vector[1]="AGUINALDO DE NAVIDAD DE :"
      vector[2]= 12
      vector[3]="MESES Y"
      vector[4]= 23
      vector[5]="DIAS                  Bs."
      vector[6]=34556.56
      EVERYROW(forma15,vector,.T.)

      vector[1]="VACACION              DE :"
      vector[2]= 12
      vector[3]="MESES Y"
      vector[4]= 23
      vector[5]="DIAS                  Bs."
      vector[6]=34556.56
      EVERYROW(forma15,vector,.T.)

      vector[1]="PRIMA (SI CORRESPONDE)  :"
      vector[2]= 12
      vector[3]="MESES Y"
      vector[4]= 23
      vector[5]="DIAS                  Bs."
      vector[6]=34556.56
      EVERYROW(forma15,vector,.T.)

      vector[1]="OTROS :"
      vector[2]=".......................................                 Bs."
      vector[3]=47656.78
      EVERYROW(forma16,vector,.T.)

      vector[1]="               GESTION  :"
      vector[2]= 12
      vector[3]="MESES Y"
      vector[4]= 23
      vector[5]="DIAS                  Bs."
      vector[6]=34556.56
      EVERYROW(forma15,vector,.T.)

      EVERYROW(REPLICATE("_",80),' ',.T.)
      vector[1]= "IV TOTAL DE BENEFICIOS                                       Bs."
      vector[2]= ROUND((CC+DD),2)
      EVERYROW(forma13,vector,.T.)
      EVERYROW(REPLICATE("_",80),' ',.T.)

      EVERYROW("E) DEDUCCIONES :",'',.T.)

      vector[1]="................................... Bs."
      vector[2]=245.67
      EE=EE+vector[2]
      EVERYROW(forma17,vector,.T.)

      vector[1]="................................... Bs."
      vector[2]=245.67
      EE=EE+vector[2]
      EVERYROW(forma17,vector,.T.)

      vector[1]="................................... Bs."
      vector[2]=25.67
      EE=EE+vector[2]
      EVERYROW(forma17,vector,.T.)

      vector[1]="....................................Bs."
      vector[2]=2.67
      vector[3]="Bs."
      vector[4]=EE
      EVERYROW(forma18,vector,.T.)

      EVERYROW(REPLICATE("_",80),' ',.T.)
      vector[1]= "V  IMPORTE LIQUIDO A PAGAR : C + D - E =                      Bs."
      vector[2]= ROUND((CC+DD-EE),2)
      EVERYROW(forma13,vector,.T.)
      EVERYROW(REPLICATE("_",80),' ',.T.)

      vector[1]=_ci(70)+numero_lite(vector[2])
      EVERYROW(forma19,vector,.T.)
     

SKIPLINE(1,.T.)
vec_sub[1] = REPLICATE('-',222)
IF !EVERYROW(forma_,vec_sub,.T.)
   FINISHREPORT()
   RETURN
ENDIF
vec_tot[1] =ESPACIOS( 'Total General',20)
IF !EVERYROW(forma3,vec_tot,.T.)
   FINISHREPORT()
   RETURN
ENDIF

SKIPLINE(4,.T.)

//vec_tot[1] = numero_lite(vec_tot[7])

//IF !EVERYROW(forma_,vec_tot,.T.)
  // FINISHREPORT()
   //RETURN
//ENDIF

SKIPLINE(3,.T.)

vec_tot[1] = '---------------------'
vec_tot[2] = '---------------------'
vec_tot[3] = '---------------------'

IF !EVERYROW(forma2,vec_tot,.T.)
   FINISHREPORT()
   RETURN
ENDIF

vec_tot[1] = '    JEFE PERSONAL    '
vec_tot[2] = '      CONTADOR       '
vec_tot[3] = ' SERVICIOS GENERALES '
IF !EVERYROW(forma2,vec_tot,.T.)
   FINISHREPORT()
   RETURN
ENDIF

FINISHREPORT()

RETURN ''
STATIC FUNCTION rec_apell(item)
LOCAL a, cad := ''

       a = SELECT()

       SELECT 1
	USE dat_empl index nitem,nombre,codigo,eventual,ci
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


STATIC FUNCTION estado(c,sexo)
LOCAL ee:=" "

       DO CASE
	  CASE c="S"
	       ee="SOLTER"
	       IF sexo="M"
		ee=ee+"O"
	       ELSE
		ee=ee+"A"
	       ENDIF
	  CASE c="C"
	       ee="CASAD"
	       IF sexo="M"
		ee=ee+"O"
	       ELSE
		ee=ee+"A"
	       ENDIF
	  CASE c="D"
	       ee="DIVORCIAD"
	       IF sexo="M"
		ee=ee+"O"
	       ELSE
		ee=ee+"A"
	       ENDIF
	  CASE c="V"
	       ee="VIUD"
	       IF sexo="M"
		ee=ee+"O"
	       ELSE
		ee=ee+"A"
	       ENDIF
       ENDCASE
RETURN ee

*** MODULO 2

*****************************************************************************
*****************************************************************************
**                                                                          *
**  MODULO              :     REPORTE DEL IVA                               *
**  PARTE DEL SISTEMA   :     ALMACENES                                     *
**  PROPIETARIO         :     CAJA PETROLERA DE SALUD                       *
**  AUTOR(ES)           :     RICHARD JIMENEZ                               *
**                            JULIO ANTEZANA R                              *
**  VERSION             :     1.0                                           *
**  CREACION            :     29 DE AGOSTO DE 1994                          *
**  ULTIMA REVISION     :                                                   *
**                                                                          *
*****************************************************************************
*****************************************************************************
**                                                                          *
**  DESCRIPCION     : El presente modulo tiene como objetivo de             *
**                    sacar un listado del IVA                              *
**                                                                          *
*****************************************************************************
*****************************************************************************

FUNCTION list_iva()
LOCAL tituloscrren, referescreen, rec
LOCAL forma, forma_, forma1, i,sal_m,forma2
LOCAL pp[15], vector[17], vec_tot[16], vec_sub[1],tg,des,profes:=SPACE(20)
LOCAL vc[2]


//ldolar_act = busdolar()

//ldolar_ant = busdolar_ant()

//showTIME()

prepla("Planilla I.V.A.")  //mes de emisi¢n de la planilla

aumentar()
//modif_neto()

IF LASTKEY()=27;RETURN;ENDIF
sal_m = 0
vec_tot = AFILL (vec_tot,0.0)
SELECT 1
 USE dat_empl index nitem,nombre,codigo,eventual,ci
SET ORDER TO 1

SELECT 22
USE iva INDEX iva_item
GO TOP
IF LASTREC() = 0
   RETURN
ENDIF

IF !STARTREPORT(135,'R',4,5)
   RETURN ' '
ENDIF

tituloscreen = 'I M P U E S T O    A L    V A L O R   A G R E G A D O'
referescreen = ' Ref: Impuestos del Mes de '+lmeses

pp[1]= '<,057,04,'+la_empresa
pp[2]= '<,155,04,'+el_Lugar
pp[3]= 'pagino'
pp[4]= '^,185,00,'+tituloscreen
pp[5]= '^,000,00, ' +  "Cochabamba, " + FECHALIT(cad_fecha,5,3)
pp[6]= '<,000,00'
pp[7]= '<,009,5,'+ referescreen
//                  1              2                                       3        4          5           6        7           8        9        10    11           12            13              14         15         16           17
//pp[8] = '<,004,5,=============================================================================================================================================================================================================================='
//pp[9] = '<,004,5, ITEM       APELLIDOS Y NOMBRES                       SUEL.NETO  MINIMO     DIF. IMP.  IMPUESTO IVA (101)    13% sal   FISCO    DEPEN. SALDO_ANT    SALD_MANT.   SUBTOTAL     SALDO TOTAL  SALDO UTI. LIQUI.RET  SALDO A FAVOR'
//pp[10]= '<,004,5,=============================================================================================================================================================================================================================='
//pp[11]= '<,000,00'

pp[8] = '<,004,5,=============================================================================================================================================================================================================================='
pp[9] = '<,004,5, ITEM       APELLIDOS Y NOMBRES                       SUELDO     MINIMO NO  DIF. IMP.  IMPUESTO IVA          13% DOS   FISCO    DEPEN. SALDO_ANT    SALD_MANT.   TOTAL        SALDO TOTAL  SALDO UTI. IMP.RET.   SALDO AL     '
pp[10]= '<,004,5,                                                      NETO       INPONIBLE             (13 %)   FORM(101)    SAL.MIN.                  SALDO_ANT    SALD_MANT.   SALD_ANT.    FAVOR DEP.   SALDO UTI. A PAGAR    MES SIGUENTE '
pp[11]= '<,004,5,=============================================================================================================================================================================================================================='
pp[12]= '<,000,00'


       forma  =  "XXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX ###,###.## ###,###    ###,###.## ###,###.## ###,###    ###,###   ###,###  ###,###  ###,###     ###,###      ###,###       ###,###     ###,###    ###,###     ###,###      "
       forma1 =  "           XXXXXXXXXXXXXXXXXXXXX                 ##,###,###.## ##,###,### ##,###,###.## #####,###.## ##,###,###  #####,### #####,### ####,### ####,###  ##,###,###   ##,###,###    ##,###,###  ##,###,###  #####,###  ##,###,###      "
       forma2 =  "           XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX ###,###.## ###,###    ###,###.## ###,###.## ###,###    ###,###   ###,###  ###,###  ###,###     ###,###      ###,###       ###,###     ###,###    ###,###     ###,###      "

       vec_tot[1] = 'Total Pagina Anterior'
       forma_     = REPLICATE('X',222)
       vec_sub[1] = REPLICATE('-',222)

EVERYHEADER (@pp,11)

//pp[1]= '=============================================================================================================================================================================================================================='
//pp[2]= ' ITEM       APELLIDOS Y NOMBRES                       SUEL.NETO  MINIMO     DIF. IMP.  IMPUESTO IVA (101)    13% sal   FISCO    DEPEN. SALDO_ANT    SALD_MANT.   SUBTOTAL     SALDO TOTAL  SALDO UTI. LIQUI.RET  SALDO A FAVOR'
//pp[3]= '=============================================================================================================================================================================================================================='
//pp[4]= ' '

pp[1]= '=============================================================================================================================================================================================================================='
pp[2]= ' ITEM       APELLIDOS Y NOMBRES                       SUEL.NETO  MINIMO     DIF. IMP.  IMPUESTO IVA (101)    13% sal   FISCO    DEPEN. SALDO_ANT    SALDO        SUBTOTAL     SALDO TOTAL  SALDO      LIQUI.RET  SALDO A FAVOR'
pp[3]= '                                                      NETO       INPONIBLE             (13 %)   FORM(101)    SAL.MIN.                  ANTERIO      MANT.VALOR   SALD_ANT.    FAVOR DEP.   UTILIZADO  A PAGAR    MES SIGUENTE '
pp[4]= '=============================================================================================================================================================================================================================='
pp[5]= ' '


HEADERSCREEN(@pp,5)

pp[1] = '^,002,05,.................................................................................................................................................................................... Continua en la Siguiente P gina'
EVERYTRAILER(@pp,1)
SELECT 22
USE iva INDEX iva_item
GO TOP
tg:=0
DO WHILE !EOF()

  IF  !EMPTY(nitem)
   //IF  rec_apell(nitem)!=''
    IF _es(nitem)  //     .and. neto!=0
       vector= AFILL(vector,0)
       vector[1]  =  nitem
       vector[2]  =  ESPACIOS(rec_apell(nitem), 40)
    ELSE
     profes=rec_prof(nitem)
     tg = total_ganado(nitem)

     vc=total_afp(nitem,tg)
     afp_bbv=vc[1]
     afp_fut=vc[2]

     des=(afp_bbv+afp_fut+fonvis(tg))
     tg = tg - des

      IF tg=0;sal_m=0;ELSE;sal_m=ROUND(sal_min_iva,0);ENDIF
      vector[1]  =  nitem
      vector[2]  =  ESPACIOS(rec_apell(nitem), 40)
      vector[3]  =  ROUND(tg+fneto(nitem),2)
      vector[4]  =  ROUND(2*sal_m,2)

    IF vector[3] < vector[4] 
	SKIP
	LOOP
    END IF

      vector[5]  =  ROUND(vector[3] - vector[4],2) //diferencia
      vector[6]  =  ROUND((imp_iva/100)*vector[5],2) //impu_liqu
      vector[7]  =  ROUND(formu101,0)
      vector[8]  =  ROUND(2*sal_m * (imp_iva/100),0)
      //vector[9]=  ROUND(vector[6]-vector[8],0)
      vector[9]  =  ROUND(vector[6]-vector[7]-vector[8],0) // saldo_fisc

      IF vector[9] < 0
	 vector[10]= ROUND((-1)*vector[9],0) //  saldo del dependiente
	 vector[9] = 0                       // saldo al fisco
      ELSE
	 vector[10]= 0 //saldo del dependiente
      ENDIF

      /*IF vector[6]<vector[8]
		  vector[10] =ROUND((-1)*vector[9],0)  saldo_depe
		  vector[9] = 0
      ELSE
	    vector[10] = 0
      ENDIF*/

      vector[11] =  ROUND(saldo_ante,0)
      
      //vector[12] =  ROUND(mante_ant,0)  mantenimiento de valor

      vector[12] = ROUND(vector[11]*ldolar_act/ldolar_ant - vector[11],0)

      vector[13] =  ROUND(vector[11]+vector[12],0) //subtotal

      vector[14] =  ROUND(vector[13]+vector[10],0) 
      // Saldo a favor del dependiente
      //Correción 15/02/2000 Mrs. Humerez

      //vector[14] =  ROUND(vector[13],0) // Saldo a favor del dependiente

      IF  vector[9] <= vector[14]
	  vector[15] =  ROUND(vector[9],0) //saldo utilizado
      ELSE
	  vector[15] = ROUND(vector[14],0)
	  
      ENDIF

      /*
      vector[16] =  ROUND(vector[9]-vector[15],0)

      IF vector[16] < 0 
	vector[16]=0
	vector[17] =  ROUND(vector[14]+vector[15],0)

      ELSE
	vector[16]=ABS(vector[16])
	vector[17] =  ROUND(ABS(vector[14]-vector[15]),0)
      ENDIF
      */

      //vector[17] =  ROUND(ABS(vector[14]-vector[15]),0)

      // Anterior
       vector[17] =  ROUND(ABS(vector[14]-vector[15]),0)
       vector[16] =  ROUND(ABS(vector[9]-vector[15]),0)
      // Modificado 15/03/2000


      /*IF vector[7]>0
	   vector[14] = ROUND(vector[7]+vector[13]+vector[10],0)
      else
	   vector[14]= ROUND(vector[7]+vector[13]+vector[10],0)
      endif*/

      //vector[15] =  vector[9] //saldo_util

      /*
      IF vector[7]>0
	     vector[14] = ROUND(vector[13]+vector[10],0)
      else
	  vector[14]= ROUND(vector[13]+vector[10],0)
      endif
      */

      /*IF vector[14] = 0
	   vector[15] = 0
      ELSE
	   vector[15] =  ROUND(vector[9],0) saldo_util
      ENDIF
      */


      // Aumento

      //vector[16]= ROUND(ABS(vector[9]-vector[15]),0) //liquid retencion

     //vector[17]= ROUND(ABS(vector[14]-vector[15]),0) //saldo a favor


     //IF vector[14]<vector[15]
	//       vector[16] =  ROUND(vector[15]-vector[14],0) liquid_ret
	//       vector[17]= 0
     //ELSE
	//  vector[16] =  ROUND(ABS(vector[9]-vector[15]),0)
	  //vector[17] =  ROUND(ABS(vector[14]-vector[15]),0) sald_depen
    // ENDIF

    ENDIF

//    IF vector[2]!=SPACE(40)
      IF !EVERYROW(forma,vector,.T.)
	 EXIT
      ENDIF
//    ENDIF
      SKIP 1

      FOR i = 3 TO 5
	 vec_tot[i-3+2] = ROUND(vec_tot[i-3+2]+vector[i],2)
      ENDFOR


      FOR i := 6 TO 17        //totales
	  vec_tot[i-3+2] = ROUND(vec_tot[i-3+2] + vector[i],0)
      ENDFOR

      IF ultima_linea = 3 .AND. !EOF()
	/*
	 vec_sub[1] = REPLICATE('-',222)
	 IF !EVERYROW(forma_,vec_sub,.T.)
	    EXIT
	 ENDIF

	 Modificado 
	 vec_tot[1] = 'Total P gina         '

	 IF !EVERYROW(forma1,vec_tot,.T.)
	    EXIT
	 ENDIF
	 SKIPLINE(1,.T.)

	 vec_tot[1] = 'Total P gina Anterior'
	 IF !EVERYROW(forma1,vec_tot,.T.)  // ENCABEZADOn DE SUBTOTALES
	    EXIT                           // SIGUIENTE
	 ENDIF                             // PAGINA
	 IF !EVERYROW(forma_,vec_sub,.T.)  // LINEA SUBRRAYADA
	    EXIT
	 ENDIF
       */
      ENDIF

 // ENDIF

 ELSE
  SKIP
 ENDIF
ENDDO

SELECT 50
USE AUMENTO
DO WHILE !EOF()

  vector    = AFILL(vector,0)
  vector[1] = SPACE(10)
  vector[2] = ESPACIOS(nombre,40)

  IF neto = 0

  vector[11] = ROUND(saldo_ant,0)
  vector[13] = ROUND(saldo_ant,0)
  vector[14] = ROUND(saldo_ant,0)
  vector[15] = ROUND(saldo_uti,0)
  vector[17] = ROUND(saldo_ant-saldo_uti,0)

  ELSE
      vector[3]  =  ROUND(neto,0)
      vector[4]  =  ROUND(2*sal_m,2)
      vector[5]  =  ROUND(vector[3] -vector[4],2) //diferencia
      vector[6]  =  ROUND((imp_iva/100)*vector[5],2) //impu_liqu
      vector[7]  =  ROUND(f101,0) //ROUND(formu101,2)
      vector[8]  =  ROUND(2*sal_m * (imp_iva/100),0)
      vector[9]  =  ROUND(vector[6]-vector[8],0) // saldo_fisc

      IF vector[6]<vector[8]
		  vector[10] =ROUND((-1)*vector[9],0) //  saldo_depe
		  vector[9] = 0
      ELSE
	    vector[10] = 0
      ENDIF

      vector[11] =  ROUND(saldo_ant,0)
      vector[12] =  0 //mante_ant
      vector[13] =  ROUND(vector[11]+vector[12],0) //subtotal

      IF vector[7]>0
	   vector[14] = ROUND(vector[7]+vector[13]+vector[10],0)
      ELSE
	   vector[14]= ROUND(vector[7]+vector[13]+vector[10],0)
      ENDIF

      IF vector[14] = 0
	   vector[15] = 0
      ELSE
	   vector[15] =  ROUND(vector[9],0) //saldo_util
      ENDIF

     IF vector[14]<vector[15]
		 vector[16] =  ROUND(vector[15]-vector[14],0) //liquid_ret
		 vector[17]= 0
     ELSE
	  vector[16] =  ROUND(ABS(vector[9]-vector[15]),0)
	  vector[17] =  ROUND(ABS(vector[14]-vector[15]),0) //sald_depen
     ENDIF
  END IF


  IF !EVERYROW(forma,vector,.T.)
     EXIT
  ENDIF
   FOR i = 3 TO 5
      vec_tot[i-3+2] = ROUND(vec_tot[i-3+2]+vector[i],2)
   ENDFOR

   FOR i := 6 TO 17        //totales
     vec_tot[i-3+2] = ROUND(vec_tot[i-3+2] + vector[i],0)
   ENDFOR

  SKIP
ENDDO

SKIPLINE(1,.T.)
vec_sub[1] = REPLICATE('-',222)
IF !EVERYROW(forma_,vec_sub,.T.)
   FINISHREPORT()
   RETURN
ENDIF

vec_tot[1] = 'Total General        '
IF !EVERYROW(forma1,vec_tot,.T.)
   FINISHREPORT()
   RETURN
ENDIF

FINISHREPORT()
//showTIME(2,70,.F.,"B/W")
RETURN


**** MODULO 3

*****************************************************************************
*****************************************************************************
**                                                                          *
**  MODULO              :     REPORTE DE EVENTUALES                         *
**  PARTE DEL SISTEMA   :     PLANILLAS                                     *
**  PROPIETARIO         :     CAJA PETROLERA DE SALUD                       *
**  AUTOR(ES)           :     RICHARD JIMENEZ                               *
**                            JULIO ANTEZANA R                              *
**  VERSION             :     1.0                                           *
**  CREACION            :     02 DE AGOSTO DE 1994                          *
**  ULTIMA REVISION     :                                                   *
**                                                                          *
*****************************************************************************
*****************************************************************************
**                                                                          *
**  DESCRIPCION     : El presente modulo tiene como objetivo de             *
**                    sacar un listado de EVENTUALES                        *
**                                                                          *
*****************************************************************************
*****************************************************************************

FUNCTION list_eventu()

LOCAL tituloscrren, referescreen
LOCAL rec,cont,val1,val2,sal,seccion
LOCAL forma, forma_, forma1, form2,form3, i,nit
LOCAL pp[15], vector[16],vector1[16],vec_tot[15],vec_tot1[15], vec_sub[1]
LOCAL totaldesc:=0,carac,sal_diar,fechita
//showTIME()
prepla("Planilla del Personal Eventual")  //mes de emisi¢n de la planilla

IF LASTKEY()=27;RETURN;ENDIF

ini_var()
vec_tot  = AFILL (vec_tot,0.0)
vec_tot1 = AFILL (vec_tot1,0.0)

SELECT 1
USE dat_empl index nitem,nombre,codigo,eventual,ci
SET ORDER TO 1
// El ultimo caracter de item nos indica si es eventual o no

GO TOP

IF !STARTREPORT(135,'R',4,5)
   RETURN ' '
ENDIF

cabe('POLICLINICO')

       forma  = "XXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX #,###,###.##  #,###,###.##  ###,###.##   #,###,###.##    ###,###.##  ###,###.##  ###,###.##  ###,###.##  ###,###.##    ###,###.##        #,###,###.##     #,###,###.##"
       forma1 = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  XXXXXXXXXX XXXXXXXXXX    ##       #,###,###.##  ###,###.##   #,###,###.##    ###,###.##  ###,###.##  ###,###.##  ###,###.##  ###,###.## "

       form2  = "                    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX #,###,###.##   #,###,###.##  ###,###.##   #,###,###.##    ###,###.##  ###,###.##  ###,###.##  ###,###.##  ###,###.##    ###,###.##        #,###,###.##     #,###,###.##"
       form3  = "                                                                  #,###,###.##  ###,###.##   #,###,###.##    ###,###.##  ###,###.##  ###,###.##  ###,###.##  ###,###.## "
       forma_     = REPLICATE('X',222)
       vec_sub[1] = REPLICATE('-',222)


pp[1] = '==========================================================================================================================================================================================================================='
pp[2] = ' ITEM              APELLIDOS Y NOMBRES             SALARIO MENS.   SAL.GAN.     HRS.EXTRAS       BONO ANTIG.   AFP BBV      F.SOL.BBV     IVA        ANTICIPOS   RETEN.JUD.    REFRIGERIO        TOTAL           LIQUIDO'
pp[3] = ' CARGO                         FECH.ING. FECH.NAC. DIAS TRAB.      SERV.EMERG.  HRS.NOCT.        TOTAL GANADO  AFP FUTURO   F.SOL.FUT.    MULTAS       PULPERIA    OTROS                           DESCUENTOS      PAGABLE'
pp[4] = ' ========================================================================================================================================================================================================================='
pp[5] = ' '

HEADERSCREEN(@pp,5)

pp[1] = '^,002,05,.................................................................................................................................................................................... Continua en la Siguiente P gina'
EVERYTRAILER(@pp,1)

SELECT 1
  USE dat_empl index nitem,nombre,codigo,eventual,ci
  set order to 1
GO TOP

SELECT 2
use temp_eve index enitem,enombre
SET ORDER TO 1
GO TOP

cont = 1

DO WHILE !EOF()
    nit = alltrim(2->nitem)
    SELECT 1
    //USE dat_empl index nitem,nombre,codigo,eventual,ci
    //SET ORDER TO 1
    SEEK nit

    IF !FOUND()
      SELECT 2
      //use temp_eve index enitem,enombre
      SKIP
      nit=ALLTRIM(nitem)
      LOOP
    ENDIF

   IF cont = 1
	 val1 = "01"
	 val2 = "03"
   ELSE
	 vec_tot  = AFILL (vec_tot,0.0)
	 vec_tot1 = AFILL (vec_tot1,0.0)
	 val1 = "02"
	 val2 = "02"
   ENDIF

   cont = cont+1
   sal  = .F.
   DO WHILE  (SUBSTR(codigo,3,2)==val1 .OR. SUBSTR(codigo,3,2)==val2) .AND. !sal
	SELECT 1
	 //USE dat_empl index nitem,nombre,codigo,eventual,ci
	SEEK nit
	IF !FOUND()
	   SELECT 2
	   //use temp_eve index enitem,enombre
	   SKIP
	   nit=ALLTRIM(nitem)
	   LOOP
	ENDIF

	anterior = codigo

	IF jor_trab=0.5
	  medio="1/2t"
	ELSE
	  medio="    "
	ENDIF

	 area100:=SELECT()
	 SELECT 32
	 USE personal  INDEX perso_it,perso_nom
	 SEEK alltrim(2->nitem)
	 IF FOUND()
	    dias=dias_trab
	 ELSE
	    dias=30
	 ENDIF

	 SELECT(area100)

	 sue_mens = sueldo(nivel,jor_trab)
	 sal_diar = sue_mens
	 IF dias=0
	   limpiar()
	   SELECT 2
	   //use temp_eve index enitem,enombre
	   SKIP
	    nit=ALLTRIM(nitem)
	   LOOP
	 ENDIF

	 IF anio_rec > 0 .OR. mes_rec > 0
	    fechita = halla_fecha(fech_ing,anio_rec,mes_rec,dia_rec)
	 ELSE
	   fechita  = fech_ing
	 ENDIF

       bon_antig= bono_antig("B",fechita)*jor_trab

	IF dias<30
	   //sue_mens = ROUND(ROUND(sue_mens/30,2)*dias,2)
	   sal_diar   = ROUND(ROUND(sue_mens/30,2)*dias,2)
	   bon_antig = ROUND(ROUND(bon_antig/30,2)*dias,2)
	ENDIF

       hrs_adic  = hrs_adicion(codigo,prof,sue_mens)     // parametro it para octubre 1994
       cat_profe = categ_prof(sue_mens,p_prof,codigo,prof,hrs_adic)
       emerg     = servicio_em(serv_emerg,sueldo(nivel,1.0),bon_antig,cat_profe)
       emerg     = ROUND(ROUND(emerg/30,2)*dias,2)
       *v_h       = valor_hora(sexo,prof,sueldo(nivel,1.0),bon_antig,cat_profe,30)
       //v_h       = valor_hora(sexo,prof,sal_diar,bon_antig,cat_profe,dias)

       IF !EMPTY(fech_ing) .AND. fech_ing >= CTOD('01/01/2003')
        v_h  = valor_hora(sexo,prof,sueldo(nivel,1.0),0,0,30)
       ELSE
        v_h  = valor_hora(sexo,prof,sueldo(nivel,1.0),bon_antig,cat_profe,30)
       ENDIF

       hrs_extras= hora_extra(2->nitem,v_h)
       recarg    = recarg_noct(2->nitem,v_h)

       totalg=sal_diar+hrs_extras+recarg+emerg+bon_antig           //entra

       // Descuentos a la AFPs
	IF aporte_afp = "S"
  		if viejo(fecha_nac)
	   		desc_afp = TotalG*desc_afp_b
		else
			desc_afp = TotalG*porc_afp
	  	endif
  	ELSE
  		if viejo(fecha_nac)
	   		desc_afp = TotalG*desc_afp_d
		else
			desc_afp = TotalG*desc_afp_c
  		endif
	ENDIF
	
	desc_uno =fsolidario(TotalG)
	//fonvi     =  fonvis(TotalG)
	if dat_empl->afp = "BBV"
		afp_bbv = ROUND(desc_afp,2)
		afp_fut = 0
		fonv_bbv = desc_uno
		fonv_fut = 0
	else
		afp_fut = ROUND(desc_afp,2)
		afp_bbv = 0
		fonv_fut = desc_uno
		fonv_bbv = 0
	endif


       // renta      = ROUND(((imp_iva+imp_t)/100)*TotalG,0)
      //Cambiar el 15 porciento

       desc_ley   = afp_bbv+afp_fut+fopeba+desc_uno

       renta   = iva_even(2->nitem,totalG,imp_iva+imp_t,desc_ley)
       anticipos  = anti_cipo(2->nitem)

	area25:=SELECT()
	 SELECT 32
	 USE personal  INDEX perso_it,perso_nom
	 SEEK alltrim(2->nitem)
	 IF FOUND()
	    multas    = multas(faltas,TotalG)+ROUND((TotalG - hrs_extras - emerg-recarg)/30,2)*falta_dia
	  //multas    = multas(faltas,TotalG)
	    refrige   = refrigerio
	    retencion = reten_jud
	    pulpe     = pulperia
	 ELSE
	   multas    = 0
	   retencion = 0
	   refrige   = 0
	   pulpe     = 0
	 ENDIF
	 SELECT(area25)

       otros      = control_prest(2->nitem)
       totaldesc  = ROUND(afp_bbv+afp_fut+fonv_bbv+fonv_fut+renta+anticipos+multas+retencion+pulpe+refrige+otros,2)

       liq_pagable=ROUND(totalg-totaldesc,2)                             //entra

      vector[1]  =  2->nitem
      vector[2]  =  ESPACIOS(nombre, 40)
      vector[3]  =  sue_mens
      vector[4]  =  sal_diar
      vector[5]  =  hrs_extras
      vector[6]  =  bon_antig //totalG
      vector[7]  =  afp_bbv
      vector[8]  =  fonv_bbv
      vector[9]  =  renta
      vector[10] =  anticipos
      vector[11] =  retencion
      vector[12] =  refrige
      vector[13] =  totaldesc
      vector[14] =  liq_pagable

      IF !EVERYROW(forma,vector,.T.)
	 EXIT
      ENDIF
      FOR j:=3 TO 14
	 vec_tot[j-1]=vec_tot[j-1]+vector[j]
      ENDFOR

      vector1[1] = ESPACIOS(nom_car(SUBSTR(codigo,9,3)),30)   // nombre del cargo
      vector1[2] = fech_ing
      vector1[3] = fecha_nac
      vector1[4] = dias
      vector1[5] = emerg
	vector1[6] = recarg
      vector1[7] = totalG
      vector1[8] = afp_fut
      vector1[9] = fonv_fut
      vector1[10]= multas
      vector1[11]= pulpe
      vector1[12]= otros


      IF !EVERYROW(forma1,vector1,.T.)
	 EXIT
      ENDIF
      SKIPLINE(1,.T.)

      FOR j:=5 TO 12
	 vec_tot1[j-5+1]=vec_tot1[j-5+1]+vector1[j]
      ENDFOR
		     //=3
      IF ultima_linea <= 4 .AND. !EOF()
	 NEWPAGE()
      ENDIF

      SELECT 2
      //use temp_eve index enitem,enombre
      SKIP
      IF EOF()
	sal = .T.
      ENDIF
      nit = ALLTRIM(nitem)
  ENDDO
     vec_sub[1] = REPLICATE('-',220)

     IF !EVERYROW(forma_,vec_sub,.T.)
	FINISHREPORT()
	RETURN
     ENDIF
     IF ultima_linea <=13
	NEWPAGE()
     ENDIF
     vec_tot[1] = ESPACIOS('Total',30)
     IF !EVERYROW(form2,vec_tot,.T.)
	FINISHREPORT()
	RETURN
     ENDIF

     IF !EVERYROW(form3,vec_tot1,.T.)
	FINISHREPORT()
	RETURN
     ENDIF

     SKIPLINE(2,.T.)
     vec_tot[1] = numero_lite(vec_tot[13])
     IF !EVERYROW(forma_,vec_tot,.T.)
	FINISHREPORT()
	RETURN
     ENDIF

     SKIPLINE(4,.T.)
     quien_firma()

     IF !sal
       NEWPAGE()
       cabe('HOSPITAL')
     ELSE
       EXIT
     ENDIF
ENDDO

FINISHREPORT()
SELECT 1
 USE dat_empl index nitem,nombre,codigo,eventual,ci
SET ORDER TO 1
GO TOP
//showTIME(2,70,.F.,"B/W")
RETURN

PROCEDURE cabe(seccion)
LOCAL pp[14]
VecEvHeader:={}

tituloscreen = 'PLANILLA DE PAGO AL PERSONAL EVENTUAL'
referescreen = ' Ref: Salarios del Mes de: '+mes

pp[1]= '<,057,04,'+la_empresa
pp[2]= '<,155,04,'+el_Lugar
pp[3]= 'pagino'
pp[4]= '^,185,00,'+tituloscreen
pp[5]= '^,000,00, ' +  "Cochabamba, " + FECHALIT(cad_fecha,3,3)
pp[6]= '<,000,00'
pp[7]= '<,009,5,'+ referescreen +SPACE(40)+_ci(155)+seccion
pp[8] = '<,004,5,==========================================================================================================================================================================================================================='
pp[9] = '<,004,5, ITEM                 APELLIDOS Y NOMBRES           SALARIO MENS.  SAL.GAN.    HRS.EXTRAS    BONO ANTIG.   AFP BBV       F.SOL. BBV  IVA       ANTICIPOS   RETEN.JUD.    REFRIGERIO        TOTAL           LIQUIDO       '
pp[10]= '<,004,5, CARGO                         FECH. ING. FECH.NAC. DIAS TRAB.     SERV.EMERG. HRS.NOCT.     TOTAL GANADO  AFP FUTURO    F.SOL. FUT  MULTAS      PULPERIA    OTROS                           DESCUENTOS      PAGABLE       '
pp[11]= '<,004,5,==========================================================================================================================================================================================================================='
pp[12]= '<,000,00'
EVERYHEADER (@pp,12)

RETURN

******* MODULO 4

*****************************************************************************
*****************************************************************************
**                                                                          *
**  MODULO              :     REPORTE DE INCAPACIDAD                        *
**  PARTE DEL SISTEMA   :     PLANILLAS                                     *
**  PROPIETARIO         :     CAJA PETROLERA DE SALUD                       *
**  AUTOR(ES)           :     RICHARD JIMENEZ                               *
**  VERSION             :     1.0                                           *
**  CREACION            :     02 DE AGOSTO DE 1994                          *
**  ULTIMA REVISION     :                                                   *
**
*****************************************************************************

FUNCTION list_inca()
LOCAL tituloscrren, referescreen, rec,por
LOCAL forma, forma_, forma1, forma2, i,lcodigo,val1,val2,sal,cont:=1
LOCAL pp[14], vector[15],vector1[15], vec_tot[12],vec_tot1[12], vec_sub[1]
LOCAL sal_diar,sal_coti,subs_diar,Totalg,totaldesc,fond_cps,pension_bas,fonv,liq_pagable
LOCAL subsi2_diar,sal2_diar
area203:=SELECT()
//showTIME()
prepla("Planilla de Incapacidad Temporal")//mes de emisi¢n de la planilla

IF LASTKEY()=27;RETURN;ENDIF

vec_tot = AFILL (vec_tot,0.0)
vec_tot1 = AFILL (vec_tot1,0.0)

SELECT 1
USE dat_empl index nitem,nombre,codigo,eventual,ci
SET ORDER TO 1
GO TOP

SELECT 23
USE baja INDEX baja_ite
GO TOP

IF !STARTREPORT(135,'R',4,5)
   RETURN ' '
ENDIF

cab('POLICLINICO')
       forma  = "XXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX #,###,###.##       ###       #,###,###.##      #,###,###.##    ###,###.##  ###,###.##   ###,###.##   ###,###.##  ###,###.##    #,###,###.##     #,###,###.##"
       forma1 = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX   XXXXXXXXXX         #,###,###.##                 #,###,###.## XXXXXX               ###,###.##  ###,###.##   ###,###.##   ###,###.##  ###,###.## "

       form2  = "                    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  #,###,###.##                  #,###,###.##     #,###,###.##    ###,###.##  ###,###.##   ###,###.##   ###,###.##  ###,###.##     #,###,###.##    #,###,###.##"
       form3  = "                                                    #,###,###.##                  #,###,###.##                     ###,###.##  ###,###.##   ###,###.##   ###,###.##  ###,###.## "

       forma_     = REPLICATE('X',222)
       vec_sub[1] = REPLICATE('-',222)


pp[1] = '====================================================================================================================================================================================================================='
pp[2] = ' ITEM      APELLIDO Y NOMBRE                      SALARIO  MENS.   DIAS ENFER.    SAL. COT. ENFERM.   TOTAL SUBSI.  AFP BBV       F.SOL. BBV  ANTICIPOS    RETEN.JUD.  REFRIGERIO      TOTAL           LIQUIDO       '
pp[3] = ' CARGO                  FECH. ING.                SAL. DIARIO                     SUBSIDIO DIAR.      POR INCAPAC.  AFP FUTURO    F.SOL. FUT  MULTAS       ANTICIPO_II OTROS         DESCUENTOS        PAGABLE       '
pp[4] = '====================================================================================================================================================================================================================='

HEADERSCREEN(@pp,4)

pp[1] = '^,002,05,.................................................................................................................................................................................... Continua en la Siguiente P gina'
EVERYTRAILER(@pp,1)

SELECT 23
USE baja INDEX baja_ite
GO TOP

DO WHILE !EOF()
    nit = alltrim(23->nitem)
    SELECT 1
    //USE dat_empl index nitem,nombre,codigo,eventual,ci
    //SET ORDER TO 1
    SEEK nit

    IF !FOUND()
      SELECT 23
      //use baja index baja_ite
      SKIP
      nit=ALLTRIM(nitem)
      LOOP
    ELSE
     codi = codigo
    ENDIF

   IF cont = 1
	 val1 = "01"
	 val2 = "03"
   ELSE
	 vec_tot  = AFILL (vec_tot,0.0)
	 vec_tot1 = AFILL (vec_tot1,0.0)
	 val1 = "02"
	 val2 = "02"
   ENDIF

   cont = cont+1
   sal  = .F.
   DO WHILE  (SUBSTR(codi,3,2)==val1 .OR. SUBSTR(codi,3,2)==val2) .AND. !sal

       SELECT 1
	 //USE dat_empl index nitem,nombre,codigo,eventual,ci
	 SEEK ALLTRIM(23->nitem)
	 IF FOUND()
	    sue_mens=sueldo(nivel,jor_trab)    //entra
	    sal_diar=ROUND(sue_mens/30,2)     //entra

          * el_fuerte   = general(asociacion,0,21,sue_mens)        //EL FUERTE
          * el_fuerte   = general(asociacion,totalG,21,sue_mens)        //EL FUERTE
	  SELECT 23
	 //use baja index baja_ite
	 sal_diar= ROUND(23->sueldo/30,2)
	 sal2_diar=23->sueldo/30
	 
	 sal_coti= ROUND(sal2_diar*(23->duracion),2)  //entra Ndias = dias de enfermedad
	 IF LEFT(diagnost,9)='ACCIDENTE'   .OR. LEFT(diagnost,10)='MATERNIDAD'
	   porcent = 1.00   //accidente o maternidad
	   por     = "(90%) "
	 ELSE
	   porcent = 1.00  //despues de tres dias de inasistencia
	   por     = "(75%) "
	 ENDIF

	 subsi_diar = ROUND(porcent*sal_diar,2)    //entra
	 subsi2_diar= porcent*sal2_diar
	 
	 total_subsi= ROUND(subsi2_diar*duracion,2) //entra
	 totalg     = 23->sueldo

	 IF 23->sueldo=0
	     totalg  = sue_mens
	     sal_diar= ROUND(sue_mens/30,2)
	 ENDIF

	IF 1->aporte_afp = "S"
		fonvi =  fonvis(sal_coti)
		if dat_empl->afp = "BBV"
	  		if viejo(1->fecha_nac)
	  			//afp_bbv = ROUND(TotalG*primaRiesgo,2)
				afp_bbv = ROUND(sal_coti*primaRiesgo,2)
	  		else
				afp_bbv = ROUND(sal_coti*porc_afp,2)
	   		endif
	    		afp_fut = 0
	    		fonv_bbv = fonvi
	    		fonv_fut = 0
	  	else
	  		if viejo(1->fecha_nac)
	  			//afp_fut = ROUND(TotalG*primaRiesgo,2)
				afp_fut = ROUND(sal_coti*primaRiesgo,2)
	  		else
				afp_fut = ROUND(sal_coti*porc_afp,2)
	  		endif
	    		afp_bbv = 0
	    		fonv_fut = fonvi
	    		fonv_bbv = 0
		endif
		
      	ELSE
		afp_fut = 0
		afp_bbv = 0
		fonv_fut= 0
	 	fonv_bbv= 0
	ENDIF


       //fonvi       = fonvis(sal_coti)
       renta       = iva(1->nitem,sal_coti,desc_ley)                  //  IVA
       anticipos   = anti_cipo(1->nitem)
       prest_caja  = halla_pres(1->nitem,"01P")+halla_pres(1->nitem,"01H")

	area25:=SELECT()
	 SELECT 32
	 USE personal  INDEX perso_it,perso_nom
	 SEEK alltrim(1->nitem)
	 IF FOUND()
	    multas    = multas(faltas,sal_coti)
	    refrige   = refrigerio
	    retencion = reten_jud
	    pulpe     = pulperia
	    dias      = dias_trab
	 ELSE
	   multas  = 0
	   refrige = 0
	   retencion = 0
	   pulpe     = 0
	 ENDIF
	 SELECT(area25)

	 otros      = control_prest(1->nitem)
	 IF 23->duracion = 30
	    sal_coti = totalg
	 ELSE
	   //IF 23-> duracion <=5
	     anticipos = 0
	     multas    = 0
	     retencion = 0
	     pulpe     = 0
	     refrige   = 0
	     otros     = 0
	     // antes estaba  aqui el if IF 23-> duracion <=5
	       prest_caja = 0
	     //ENDIF
	 ENDIF


      totaldesc  = ROUND(afp_bbv+afp_fut+fonv_bbv+fonv_fut+anticipos+multas+retencion+pulpe+refrige+otros+prest_caja,2)
      * totaldesc  = ROUND(afp_bbv+afp_fut+fonv_bbv+fonv_fut+el_fuerte+multas+retencion+pulpe+refrige+otros+prest_caja,2)
       liq_pagable= ROUND(total_subsi- totaldesc,2)

      vector[1]  =  1->nitem
      vector[2]  =  ESPACIOS(1->nombre, 40)
      vector[3]  =  totalg
      vector[4]  =  23->duracion
      vector[5]  =  sal_coti
      vector[6]  =  total_subsi
      vector[7]  =  afp_bbv
      vector[8]  =  fonv_bbv
     *vector[9]  =  anticipos
      vector[9]  =  el_fuerte
      vector[10] =  retencion
      vector[11] =  refrige
      vector[12] =  totaldesc
      vector[13] =  liq_pagable

      IF !EVERYROW(forma,vector,.T.)
	   EXIT
      ENDIF

      vector1[1] = ESPACIOS(nom_car(SUBSTR(1->codigo,9,3)),30)   // nombre del cargo
      IF 1->fech_rec=CTOD("  /  /    ")
	 vector1[2] = 1->fech_ing
      ELSE
	  vector1[2] =1-> fech_rec
      ENDIF

      vector1[3] = sal_diar
      vector1[4] = subsi_diar
      vector1[5] = por
      vector1[6] = afp_fut
      vector1[7] = fonv_fut
      vector1[8] = multas
     *vector1[9] = prest_caja
      vector1[9] = pulpe
      vector1[10] = otros

      IF !EVERYROW(forma1,vector1,.T.)
	 EXIT
      ENDIF
      SKIPLINE(1,.T.)
      vec_tot[2]=vec_tot[2]+vector[3]

      FOR i:=5 TO 13
	 vec_tot[i-5+3]=vec_tot[i-5+3]+vector[i]
      ENDFOR

      vec_tot1[1]=vec_tot1[1]+vector1[3]
      vec_tot1[2]=vec_tot1[2]+vector1[4]
      FOR i:=6 TO 10
	 vec_tot1[i-6+3]=vec_tot1[i-6+3]+vector1[i]
      ENDFOR

      IF ultima_linea = 3 .AND. !EOF()
	    vec_sub[1] = REPLICATE('-',222)
	    IF !EVERYROW(forma_,vec_sub,.T.)
	       EXIT
	    ENDIF
	ENDIF
     ENDIF
      SELECT 23
      //use baja index baja_ite
      SKIP

      SELECT 1
      //USE dat_empl index nitem,nombre,codigo,eventual,ci
      SEEK ALLTRIM(23->nitem)
      IF FOUND()
	  codi=codigo
      ENDIF

      SELECT 23
      //use baja index baja_ite
      IF EOF()
	sal = .T.
      ENDIF

ENDDO
     vec_sub[1] = REPLICATE('-',220)
     IF !EVERYROW(forma_,vec_sub,.T.)
	FINISHREPORT()
	RETURN
     ENDIF
     vec_tot[1] = ESPACIOS('Total General',30)
     IF !EVERYROW(form2,vec_tot,.T.)
	FINISHREPORT()
	RETURN
     ENDIF

     IF !EVERYROW(form3,vec_tot1,.T.)
	FINISHREPORT()
	RETURN
     ENDIF


     SKIPLINE(2,.T.)

     vec_tot[1] = numero_lite(vec_tot[11])
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
SELECT(area203)
//showTIME(2,70,.F.,"B/W")
RETURN

*****************************************************************************
* Cabecera de datos para la planilla de incapacidad                         *
*****************************************************************************
STATIC FUNCTION cab(seccion)
LOCAL pp[13]
VecEvHeader:={}
tituloscreen = 'PLANILLA DE PAGO POR INCAPACIDAD TEMPORAL Y ENFERMEDAD'
referescreen = ' Ref: Salarios del Mes de :'+lmeses

pp[1]= '<,057,04,'+la_empresa
pp[2]= '<,155,04,'+el_Lugar
pp[3]= 'pagino'
pp[4]= '^,185,00,'+tituloscreen
pp[5]= '^,000,00, ' +  "Cochabamba, " + FECHALIT(DATE(),3,3)
pp[6]= '<,000,00'
pp[7]= '<,009,5,'+ referescreen+SPACE(40)+_ci(155)+seccion
pp[8] = '<,004,5,====================================================================================================================================================================================================================='
pp[9] = '<,004,5, ITEM      APELLIDO Y NOMBRE                      SALARIO  MENS.   DIAS ENFER.    SAL. COT. ENFERM.  TOTAL SUBSI.  AFP BBV       F.SOL. BBV  ANTICIPOS    RETEN.JUD.  REFRIGERIO      TOTAL           LIQUIDO       '
pp[10]= '<,004,5, CARGO                           FECH. ING.       SAL. DIARIO                     SUBSIDIO DIAR.     POR INCAPAC.  AFP FUTURO    F.SOL. FUT. MULTAS       ANTICIPO_II OTROS         DESCUENTOS        PAGABLE       '
pp[11]= '<,004,5,====================================================================================================================================================================================================================='
EVERYHEADER (@pp,11)
RETURN


*****************************************************************************
* Vacia los meses activos a un array                                        *
*****************************************************************************
STATIC FUNCTION llenar_meses
LOCAL vec:={}
       area23:=SELECT()
       SELECT 39
       GO TOP
       DO WHILE !EOF()
	  AADD(vec,mes)
	  SKIP
       ENDDO
       SELECT(area23)
RETURN vec

STATIC FUNCTION LNEWPAGE()
    DO WHILE ultima_linea <> 0
       SKIPLINE(1,.T.)
    endDO
return


****f* PLANILLA/cabecera
*  FUNCION
*    establece la cabecera de la planilla para su impresion.
*  SINOPSIS
PROCEDURE cabecera(sub,act)
LOCAL pp[19]
VecEvHeader:={}

tituloscreen = 'PLANILLA DE PAGO DE HABERES DE LA '+la_empresa //CAJA PETROLERA DE SALUD'
referescreen = ' Ref: sueldos del Mes de: '+lmeses

pp[1]= '<,057,07,'+la_empresa
pp[2]= '<,155,07,'+el_Lugar
pp[3]= 'pagino'
pp[4]= '^,185,00,'+tituloscreen
pp[5]= '^,000,00, ' +  "Cochabamba, " + FECHALIT(cad_fecha,5,3)
pp[6]= '<,000,00'
pp[7]= '<,009,7,'+ referescreen

pp[8]= '<,009,7, PROGRAMA      01 PRESTACIONES DE SALUD'
pp[9]= '<,009,7, SUBPROGRAMA   '+sub
pp[10]='<,009,7, ACTIVIDAD     '+act
pp[11]='<,000,00'
pp[12]= '<,004,2,======================================================================================================================================================================================================================================'
pp[13]= '<,004,2,    I      CARGO                                    N DIAS SUE.MENS.   HRS. EXTRAS PRENATAL                AFP BBV     IVA         COL.MEDICO  AS.CIRUGIA  SIND.MEDICO PRO-DEPORTE MULTAS        REFRIGERIO                         '
pp[14]= '<,004,2,    T      APELLIDO Y NOMBRE                        I      BON.ANTIG.  SUPLENCIAS  NATALIDAD    T O T A L  AFP FUTURO  ANTICIPOS   COL.BIO.FA  AS.PEDIATR. SIND.ENFER. PTMO.COMPL. RETEN. JUD.   OTROS       T O T A L   L I Q U I D O'
pp[15]= '<,004,2,    E      CARNET DE IDENTIDAD                      V PORC.CAT.PROFE.  IND.EFECT.  DIF.ESCAL.  G A N A D O F.SOL. BBV  LACTANCIA   COL.ODONT.  SIND.ADMIN. F.S.T.S.S.S C.O.D.      ANTICIPO II               DESCUENTOS  P A G A B L E'
pp[16]= '<,004,2,    M      FECHA DE NACIMIENTO                      E      HRS.ADIC.   SUB.IRRAD.  BON.PROF.               F.SOL. FUT  PRENATAL    COL.ENFERM.                                                                                        '
pp[17]= '<,004,2,           FECH. ING.      FECH. REC.               L      HRS.NOCTUR. SER.EMERG.  ESCAL.ANT.                                                                                                                                         '
pp[18]= '<,004,2,======================================================================================================================================================================================================================================'
pp[19]= '<,000,00'
EVERYHEADER (@pp,19)
RETURN
******





STATIC FUNCTION _es(ite)
LOCAL resul:=.F.
       area103:=SELECT()

       SELECT 1
       USE dat_empl index nitem,nombre,codigo,eventual,ci
       SEEK ALLTRIM(ite)
       IF FOUND()
	 IF ALLTRIM(nombre)="ACEFALIA" .OR. ALLTRIM(nombre)="COMISION"
	   resul=.T.
	 ENDIF

/*       SELECT 32
	 USE personal  INDEX perso_it
	 SEEK alltrim(ite)
	 IF FOUND()
	    IF dias_trab=0;resul=.T.;ENDIF
	 ENDIF */
       ELSE
	 resul=.T.
       ENDIF
       SELECT(area103)
RETURN resul


STATIC FUNCTION aumentar()
LOCAL sn:='N',nombrec:=SPACE(50),sal:=0,elec,salut:=0,net:=0,fiva:=0

	are:=SELECT()
	pant_1:=OPEN_WINDOW(8,10,21,73,1,'Empleados con Saldo I.V.A.',"B/W",5,.T.)
	SET CURSOR ON
	SET COLOR TO "N/W"
	@ 9 ,12 SAY "Desea ingresar m s empleados S/N :"GET sn PICTURE "!A" COLOR "W/B" VALID sn $"SN"
	READ
	DO WHILE sn=="S"
	  @ 11,12 SAY "Apellidos y Nombres :" GET nombrec COLOR "W/B" PICTURE "@!S30X"
	  @ 13,12 SAY "Sueldo Neto         :" GET net   COLOR "W/B" PICTURE "999,999.99" VALID sal>=0
	  @ 15,12 SAY "Formulario 101      :" GET fiva COLOR "W/B" PICTURE "999,999.99" VALID salut>=0

	  @ 17,12 SAY "Saldo Anterior      :" GET sal   COLOR "W/B" PICTURE "999,999.99" VALID sal>=0
	  @ 19,12 SAY "Saldo Utilizado     :" GET salut COLOR "W/B" PICTURE "999,999.99" VALID salut>=0
	  READ
	  IF !LASTKEY()=27
	    elec=MULTIPLE({"Grabar","Salir sin Grabar"},2,21,23,.F.)
	    IF elec=1
	      // grabar
	      SELECT 50
	      USE aumento
	      APPEND BLANK
	      REPLACE nombre    WITH nombrec,;
		      saldo_ant WITH sal,;
		      saldo_uti WITH salut,;
		      neto      WITH net,;
		      f101      WITH fiva

	    MENSAJE("Grabando ..",12,17,'W/R+',.T.,.F.)
	    ENDIF
	 ELSE
	   EXIT
	  ENDIF
	  @ 9 ,12 SAY "Desea ingresar m s empleados S/N :"GET sn PICTURE "!A" COLOR "W/B" VALID sn $"SN"
	  READ
	 ENDDO
	CLOSE_WINDOW(pant_1)
	SELECT(are)
RETURN

/* Modificacion de Aumento 12 Mayo 1998 */

STATIC FUNCTION modif_neto()
LOCAL sn:='N',nombrec:=SPACE(50),sal:=0,elec,salut:=0,litem:=SPACE(10),sneto:=0

	are:=SELECT()
	pant_2:=OPEN_WINDOW(8,10,17,73,1,'Empleados con Saldo I.V.A.',"B/W",5,.T.)
	SET CURSOR ON


	    @ 11,23 SAY "No. DE ITEM :"

	    @ 11,37 GET litem PICTURE "@S40!"  COLOR "R/W"
	    READ
	    IF litem!=SPACE(10)
	       SELECT 1
	       use dat_empl index nitem,nombre,codigo,eventual,ci
	       SET ORDER TO 1
	       SEEK ALLTRIM(litem)
	       IF !FOUND()
		   MENSAJE("No existe ese ITEM  ...",12,17,'W/R+',.T.,.F.)
	       ELSE
		   lnom=rec_apell(litem,1)
		   @12,37 SAY lnom
		   SELECT 22
		    USE iva INDEX iva_item
				 SEEK alltrim(litem)
				 IF FOUND()
				    sneto = neto
				    @13,23 SAY "Sueldo Neto :" GET sneto
				    READ
				    REPLACE neto WITH sneto
				 ENDIF
	       ENDIF
	    ENDIF

	SET COLOR TO "N/W"
	CLOSE_WINDOW(pant_2)
	SELECT(are)
RETURN

FUNCTION rec_prof(nit)
LOCAL pp
	 ar:=SELECT()
	 SELECT 1
	 USE dat_empl index nitem,nombre,codigo,eventual,ci
	 SET ORDER TO 1

	 SEEK nit
	  IF FOUND()
	     pp=prof
	  END IF
	  SELECT (ar)
RETURN (pp)

FUNCTION repo_indice()
LOCAL COMP,k,nit
LOCAL ff:="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                          ###,###.##"
LOCAL vect[4],pp[20]
LOCAL niv,porcent

	area110:=SELECT()
	 IF !STARTREPORT(80,'O',4,5)
	    RETURN ' '
	 ENDIF

	 pp[1]= '<,057,04,'+la_empresa
	 pp[2]= '<,155,04,'+el_Lugar
	 pp[3]= 'pagino'
	 pp[4]= '^,185,00,'+'DETALLE CORRESPONDIENTE AL INDICE DE EFECTIVIDAD '
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
	IF !EVERYROW('INDICE',vect,.T.,9);FINISHREPORT();ENDIF
	SKIPLINE(1,.T.)
	vect[1]=''
	 IF !EVERYROW(REPLICATE("_",80),vect,.T.)
	   FINISHREPORT()
	   SELECT(area101)
	   return ''
	 ENDIF

	SELECT 1
	use dat_empl index nitem,nombre,codigo,eventual,ci
	SET ORDER TO 1
	GO TOP
	DO WHILE !EOF()
		    vect[1]=espacios(nombre,40)
		    vect[2]=ind_efec
		    vect[3]=ROUND(vect[3]+ind_efec,2)

		    IF !EVERYROW(ff,vect,.T.)
		      EXIT
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
	  vect[1]=espacios('TOTAL INDICE' ,40);vect[2]=vect[3]
	  IF !EVERYROW(ff,vect,.T.)
		 FINISHREPORT()
	  ENDIF

	 SELECT(area110)
RETURN

FUNCTION fneto(ite)
LOCAL valor:=0
	 area101:=SELECT()
	 SELECT 32
	 USE personal  INDEX perso_it,perso_nom
	 SET ORDER TO 1
	 SEEK alltrim(ite)
	 IF FOUND()
	   valor = bono
	 END IF
	 SELECT(area101)
RETURN valor

FUNCTION busdolar()
local do
	aa:= select()
	SELECT 44
	go 1
	  do = dolar
	SELECT(aa)                                      

RETURN  do


FUNCTION busdolar_ant()
LOCAL doa 
	aa:= select()
	

	select(aa)
RETURN doa 


