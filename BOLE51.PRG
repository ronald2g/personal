*****************************************************************************
*****************************************************************************
**                                                                          *
**  MODULO              :     BOLETA DE PAGO                                *
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

FUNCTION boleta()
LOCAL tituloscrren, referescreen, rec
LOCAL forma, forma1, forma2, forma3, forma4, formal, anterior, lcargo,formafir,tforma
LOCAL pp[15], vectora[20], vector[16], vector1[16], vector2[16], vector3[16], vector4[16]
LOCAL ante, sale,formaiva,vec_vacio[1],vec[10]                           //ESTE ULTIMO NOS DA EL TIPO DE LETRA
LOCAL forma5,forma6,forma7,forma8,forma9
* LOCAL  LETRAS:=CHR(27)+CHR(65)+CHR(6)+CHR(27)+'S'+CHR(1)+CHR(27)+CHR(33)
LOCAL  LETRAS:=CHR(27)+CHR(48)
*LOCAL HOJAS:=CHR(27)+"C"+CHR(0)+CHR(6)
LOCAL ec:={0.0,0.0},an,num_pag,band

PRIVATE c_petrol_s, a_p_a, fdo_labor, fdo_pen_ca, fdo_pen_in, col_econom, cta_person
PRIVATE fdo_vivien, ptmo_escol, gar_fdo_cps, c_s_t_s_S, cuota_mort
prepla("Boleta de Pago")

IF LASTKEY()=27;RETURN;ENDIF
ini_var()

SELECT 1
use dat_empl index nitem,nombre,codigo,eventual,ci
SET ORDER TO 1

IF LASTREC() = 0
   RETURN
ENDIF
			  //2,'82')
IF !STARTREPORT(80,'82',1,4)
   RETURN ' '
ENDIF
       forma0='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                                                                       XXXXXXXXXXXXXXXXXXX'
       forma ='XXXXXXXXXXXXXX        XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        XXXXXXXXXXXXXXXXXXXXXXXXXXXX'
       forma1='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX   XXXXXXXXXXXXXXXXXXXX         XXXXXXXXXXXXXXXXXXXXXXX'
       tforma=REPLICATE('X',25) + SPACE(15) + REPLICATE('X',17)    //SPACE(15)+REPLICATE('X',13)
       forma2='XXXXXXXXXXXXXXX: ##                                          XXXXXXXXXXXXXXX: #,###,###.##'
       forma3='XXXXXXXXXXXXXXX: ##,###,###.##                               XXXXXXXXXXXXXXX: #,###,###.##'
       forma4='XXXXXXXXXXXXXXX: ##,###,###.##     XXX ###                   XXXXXXXXXXXXXXX: #,###,###.##'
      forma41='XXXXXXXXXXXXXXX: ##,###,###.##'
       forma5='XXXXXXXXXXXXXXX: ##,###,###.##     XXXXXXXXX ###.##          XXXXXXXXXXXXXXX: #,###,###.##'
       forma6='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                             XXXXXXXXXXXXXXXXXX: #,###,###.##'
       forma7='XXXXXXXXXXXXXXXXXX  ##,###,###.##                               XXXXXXXXXXXXXXXXXX: #,###,###.##'
       forma8='                                                             XXXXXXXXXXXXXXX: #,###,###.##'
       forma9='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX          XXXXXXXXXXXXXXXXXX: #,###,###.##'

vec_vacio[1]=SPACE(130)
HEADERSCREEN(@vec_vacio,1)
   an=ALLTRIM(mes)+'/'+STR(anio)  //RIGHT(STR(YEAR(DATE())),2)
SELECT 1
use dat_empl index nitem,nombre,codigo,eventual,ci
SET ORDER TO 1
GO TOP
num_pag      = 0
band         =.T.
DO WHILE !EOF()
   anterior = ALLTRIM(codigo)

  IF !sale(nombre,nitem) .OR. tip_empl="E"
     SKIP
     LOOP
  ENDIF

 proceso2(ALLTRIM(nitem))
 ec=ex_no(nitem)

  vec_vacio[1] = ''
  vec[1]       = LETRAS

 IF band
     band=.F.
     num_pag++
    vec[2]       = ALLTRIM(STR(num_pag))
 ELSE
    vec[2]       = ''
    band= .T.
 ENDIF

 IF !EVERYROW(forma0,vec,.T.);EXIT;ENDIF

 vec[1]= _ci(28) + espacios(la_empresa,29) //'CAJA PETROLERA DE SALUD',29)
 vec[2]= _ci(5)  + DTOC(DATE())

 IF !EVERYROW(forma0,vec,.T.);EXIT;ENDIF
 vec[1]=_ci(12) + espacios('Regional Cochabamba',27)
 vec[2]=_ci(42) + espacios('BOLETA DE PAGO',14)
 IF !EVERYROW(tforma,vec,.T.);EXIT;ENDIF

 IF !EVERYROW(+_ci(25)+'                                  MES:'+an+SPACE(33)+_ci(5)+'      ',vec_vacio,.T.);EXIT;ENDIF

 vec[1]='';IF !EVERYROW('',vec,.T.);EXIT;ENDIF

 lcargo      = SUBSTR(anterior,9,3)

 vec[1]=espacios('ITEM:'+n_item,14);vec[2]=espAcios('CARGO:'+nom_car(lcargo),40);vec[3]=espacios('FECH. ING.:'+DTOC(fechita),21)
 IF !EVERYROW(forma,vec,.T.,9);EXIT;ENDIF
								//ALLTRIM(ci)+'-'+ALLTRIM(1->extension
 vec[1]=espacios('NOMBRE:'+nombre,40);vec[2]=espacios('C.I. :'+ci,18);vec[3]=espacios('FECH. NAC.:'+DTOC(fecha_nac),21)
 IF !EVERYROW(forma1,vec,.T.,9);EXIT;ENDIF

 vec[1]='';IF !EVERYROW(_ci(9)+'------------------------ INGRESOS ----------------- |------------- DESCUENTOS -----------|',vec,.T.);EXIT;ENDIF
// vec[1]='';IF !EVERYROW('',vec,.T.);EXIT;ENDIF
       //SUELDO MENSUAL
 vec[1]=espacios('NIVEL',15);vec[2]=nivel;   vec[3]=espacios('AFP BBV    ',15);vec[4]=afp_bbv
 IF !EVERYROW(forma2,vec,.T.);EXIT;ENDIF

 vec[1]=espacios('DIAS TRABAJ.',15);vec[2]=dias;vec[3]=espacios('AFP FUTURO ',15);vec[4]=afp_fut
 IF !EVERYROW(forma2,vec,.T.);EXIT;ENDIF

 vec[1]=espacios('SUELDO MENSUAL',15);vec[2]=sue_mens;vec[3]=espacios('F.SOL. BBV',15);vec[4]=fonv_bbv
 IF !EVERYROW(forma3,vec,.T.);EXIT;ENDIF

 vec[1]=espacios('BONO ANTIG.',15);vec[2]=bon_antig;vec[3]=espacios('F.SOL. FUT',15);vec[4]=fonv_fut
 IF !EVERYROW(forma3,vec,.T.);ENDIF

 vec[1]=espacios('CATEG.PROF.',15);vec[2]=cat_profe;vec[3]='% :';vec[4]=p_prof;vec[5]=espacios('C.O.D.',15);vec[6]=fdo_social
 IF !EVERYROW(forma4,vec,.T.);ENDIF

 // Escalafon medico
 vec[1]=espacios('ESCALAFON ',15);vec[2]=dos_por
 IF !EVERYROW(forma41,vec,.T.);ENDIF

//vec[1]='';IF !EVERYROW('',vec,.T.);EXIT;ENDIF blanco

 vec[1]=espacios('HORAS ADIC.',15);vec[2]=hrs_adic;vec[3]=espacios('IVA',15);vec[4]=renta
 IF !EVERYROW(forma3,vec,.T.);EXIT;ENDIF

 vec[1]=espacios('HORAS EXTRAS',15);vec[2]=hrs_extras;vec[3]='TOTAL HRS:';vec[4]=ec[1];vec[5]=espacios('ANTICIPOS',15);vec[6]=anticipos
 IF !EVERYROW(forma5,vec,.T.);EXIT;ENDIF

 vec[1]=espacios('HORAS NOCT.',15);vec[2]=recarg;vec[3]='TOTAL HRS:';vec[4]=ec[2];vec[5]=espacios('PREST.COMPL.',15);vec[6]=prest_caja
 IF !EVERYROW(forma5,vec,.T.);EXIT;ENDIF

 vec[1]='';IF !EVERYROW('',vec,.T.);EXIT;ENDIF //blanco

 vec[1]=espacios('INDICE EFEC.',15);vec[2]=indice;vec[3]=espacios('COL.MEDICO',15);vec[4]=col_medico
 IF !EVERYROW(forma3,vec,.T.);EXIT;ENDIF

 vec[1]=espacios('SUB.IRRADI.',15);vec[2]=sub_irrad;vec[3]=espacios('COL.BIOQ.FARM.',15);vec[4]=col_bio_fa
 IF !EVERYROW(forma3,vec,.T.);EXIT;ENDIF

 vec[1]=espacios('SERV.EMERG.',15);vec[2]=emerg;vec[3]=espacios('COL.ENFERM.',15);vec[4]=col_enferm
 IF !EVERYROW(forma3,vec,.T.);EXIT;ENDIF

 vec[1]='';IF !EVERYROW('',vec,.T.);EXIT;ENDIF //blanco

 //vec[1]=espacios('PRE NATAL',15);vec[2]=bon_nat[1];vec[3]=espacios('ASOC.A.ENF.',15);vec[4]=aso_aux_enf
 vec[1]=espacios('PRE NATAL',15);vec[2]=bon_nat[1];vec[3]=espacios('COL.ODONT.',15);vec[4]=aso_aux_enf
 IF !EVERYROW(forma3,vec,.T.);EXIT;ENDIF

 vec[1]=espacios('NATALIDAD',15);vec[2]=bon_nat[2];vec[3]=espacios('ASOC.CIRUGIA',15);vec[4]=aso_cir
 IF !EVERYROW(forma3,vec,.T.);EXIT;ENDIF

*vec[1]=espacios('LACTANCIA',15);vec[2]=bon_nat[3];vec[3]=espacios('ASOC.PEDIATR',15);vec[4]=aso_ped
 vec[1]=espacios('DIF.ESC.',15);vec[2]=dif_esc;vec[3]=espacios('ASOC.PEDIATR',15);vec[4]=aso_ped
 IF !EVERYROW(forma3,vec,.T.);EXIT;ENDIF

 vec[1]=espacios('BON.R.PROF.',15);vec[2]=bon_nat[4];vec[3]=espacios('SIND.ADMI',15);vec[4]=sindicatoA
 IF !EVERYROW(forma3,vec,.T.);EXIT;ENDIF

 vec[1]=espacios('SUPLENCIAS',15);vec[2]=0;vec[3]=espacios('SIND.MEDICO',15);vec[4]=sindicatoM
 IF !EVERYROW(forma3,vec,.T.);EXIT;ENDIF

 vec[1]=REPLICATE('-',35);vec[2]=espacios('SIND.ENFERM.',15);vec[3]=sindicatoE
 IF !EVERYROW(forma6,vec,.T.);EXIT;ENDIF

 vec[1]=_ci(9)+espacios('Total Ingresos',18);vec[2]=totalG ; vec[3]=_ci(1)+espacios('F.S.T.S.S.S.',18);vec[4]=sindiC
 IF !EVERYROW(forma7,vec,.T.);EXIT;ENDIF

 vec[1]=_ci(9)+espacios('Total Descuentos',18);vec[2]=totalD ;vec[3]=_ci(1)+espacios('LACTANCIA',18);vec[4]=bon_nat[3]
 IF !EVERYROW(forma7,vec,.T.);EXIT;ENDIF

 vec[1]=REPLICATE('-',35); vec[2]=espacios('PRE NATAL',15);vec[3]=bon_nat[1]
 IF !EVERYROW(forma6,vec,.T.);EXIT;ENDIF

 vec[1]=_ci(25)+espacios('LIQUIDO PAGABLE',18);vec[2]=totalG-totalD ; vec[3]=_ci(1)+espacios('MULTAS',18);vec[4]=multas
 IF !EVERYROW(forma7,vec,.T.);EXIT;ENDIF

// vec[1]='';IF !EVERYROW('',vec,.T.);EXIT;ENDIF
   vec[1]=espacios('PRO-DEP. ',15);vec[2]=el_fuerte
   IF !EVERYROW(forma8,vec,.T.);EXIT;ENDIF


 vec[1]=_ci(28)+espacios(numero_lite(totalG-totalD),80);vec[2]=_ci(1)+espacios('RET.JUDIC.',18);vec[3]=retencion
 IF !EVERYROW(forma9,vec,.T.);EXIT;ENDIF

 vec[1]=espacios('REFRIGERIO',15);vec[2]=refrige
 IF !EVERYROW(forma8,vec,.T.);EXIT;ENDIF
 
 vec[1]=espacios('Saldo IVA    ',15);vec[2]=saldo_iva(nitem);vec[3]=espacios('ANTIC.II',15);vec[4]=pulpe
*vec[1]=espacios('Saldo IVA    ',15);vec[2]=saldo_iva(nitem);vec[3]=espacios('PULPERIA',15);vec[4]=pulpe
 //vec[1]=espacios('             ',15);vec[2]=0
 //vec[1]=espacios('PULPERIA',15);vec[2]=pulpe
 // cambio de forma8 a forma3
 IF !EVERYROW(forma3,vec,.T.);EXIT;ENDIF

 vec[1]=espacios('OTROS',15);vec[2]=otros
 IF !EVERYROW(forma8,vec,.T.);EXIT;ENDIF

 vec[1]= espacios('FIRMA :_____________________',35);vec[2]=_ci(9)+espacios('TOTAL DESCUENTO',18);vec[3]=totalD
 IF !EVERYROW(forma6,vec,.T.);EXIT;ENDIF
 vec[1]='';IF !EVERYROW('',vec,.T.);EXIT;ENDIF
 vec[1]='';IF !EVERYROW('',vec,.T.);EXIT;ENDIF



   anterior = ALLTRIM(codigo)
   SELECT 1
  // use dat_empl index nitem,nombre,codigo,eventual,ci
  // set order to 1
   SKIP //1

 IF ultima_linea<=3
  NEWPAGE()
 ENDIF
ENDDO // fin while


FINISHREPORT()

RETURN

STATIC FUNCTION LNEWPAGE()
LOCAL a := ultima_linea
      SKIPLINE(a,.T.)
RETURN

FUNCTION proceso2(it)
LOCAL mul:=0,mul1:=0
   area0:=SELECT()
  //
    SELECT 1
    use dat_empl index nitem,nombre,codigo,eventual,ci
    set order to 1
    SEEK it

  //
       IF jor_trab=0.5
	  medio="1/2t"
       ELSE
	  medio="    "
       ENDIF
	 area100:=SELECT()
	 SELECT 32
	 USE personal INDEX perso_it
	 SEEK alltrim(it)
	 IF FOUND()
	    dias=dias_trab
            ldias_em = dias_em
	 ELSE
	    dias=30
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
	 //IF nitem = "CBB-001" .OR. nitem = "CBB-003"
	 //   indice = ind_efec
	 // ELSE

	    IF reincorp = "S"
		indice = efectivo("B",fechita,sue_mens)  // Fecha de ingreso institucional
	    ELSE
		indice = efectivo("B",fech_ing,sue_mens)
	    ENDIF

	  IF nitem = "CBB-001"
	     indice = 0           //gus
	  ENDIF

	 //ENDIF

	 bon_antig= bono_antig("B",fechita)*jor_trab
	 IF dias=0
	   limpiar()
	   RETURN
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

       por_prof = porce("B",fechita,CTOD("  /  /  "))        // porcentaje profesional
       por_irra = porce("R",fechita,CTOD("  /  /  "))

      *dos_por   = escalafon(prof,sue_mens,it)
  ***    IF conge[4]=0;hrs_adic = hrs_adicion(codigo,prof,sue_mens+dos_por);ELSE;hrs_adic=conge[4];ENDIF     // parametro it para octubre 1994

*      IF !EMPTY(fecp) .AND. fecp >= CTOD('01/01/2003')
*       cat_profe= categ_prof(sue_mens,p_prof,codigo,prof,hrs_adic)
*      ELSE
*       cat_profe= categ_prof(sue_mens+dos_por,p_prof,codigo,prof,hrs_adic)
*      ENDIF
       ******* em Congel.
       em1 = escalafo0(prof,sue_mens,nitem,0)   // esc. med congel. al 2002
       dos_por = em1
     * IF conge[4]=0;hrs_adic = hrs_adicion(codigo,prof,sue_mens+dos_por);ELSE;hrs_adic=conge[4];ENDIF     // parametro it para octubre 1994
       IF conge[4]=0;hrs_adic = hrs_adicion(codigo,prof,sue_mens);ELSE;hrs_adic=conge[4];ENDIF     // parametro it para octubre        1994
     * IF !EMPTY(fecp) .AND. fecp >= CTOD('01/01/2003')
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
      //IF !EMPTY(feemer) .AND. feemer >= CTOD('01/01/2003')
      // emerg    = servicio_em(serv_emerg,sueldo(nivel,1.0),0,0)
      //ELSE
      // emerg    = servicio_em(serv_emerg,sueldo(nivel,1.0)+em1,bon_antig,cat_profe)
      //ENDIF

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

       //v_h      = valor_hora(sexo,prof,sue_mens+dos_por,bon_antig,cat_profe,dias)
      *v_h      = valor_hora(sexo,prof,sueldo(nivel,1.0)+dos_por,bon_antig,cat_profe,dias)
      *v_h      = valor_hora(sexo,prof,sue_mens+dos_por,bon_antig,cat_profe,dias)
      *v_h      = valor_hora(sexo,prof,sue_mens+em1,bon_antig,cat_profe,dias)

       IF !EMPTY(fech_ing) .AND. fech_ing >= CTOD('01/01/2003')
        v_h  = valor_hora(sexo,prof,sue_mens,0,0,dias)
       ELSE
       // v_h  = valor_hora(sexo,prof,sue_mens+em1,bon_antig,cat_profe,dias)
        v_h  = valor_hora(sexo,prof,sue_mens,bon_antig,cat_profe,dias)
       ENDIF

       *IF nitem = "CBB-156" .OR. nitem = "CBB-159"
       *   v_h  = valor_hora(sexo,prof,sue_mens+em1,bon_antig,0,dias)
       *ENDIF

       IF ALLTRIM(nombre) = "MOREIRA PALENQUE,ALVARO" .OR. ALLTRIM(nombre) = "VILLARROEL RIVERO,JULIO"
         // v_h  = valor_hora(sexo,prof,sue_mens+em1,bon_antig,0,dias)
          v_h  = valor_hora(sexo,prof,sue_mens,bon_antig,0,dias)
       ENDIF

       IF ALLTRIM(nombre) = "DEHEZA UGARTE,RUTH PATRICIA"
          v_h  = valor_hora(sexo,prof,sue_mens,bon_antig,0,dias)
       ENDIF


       hrs_extras= hora_extra(it,v_h)
       if hrs_extras>=sue_mens .AND. sue_mens>0
       		hrs_extras = sue_mens-1.0
       endif

       
       recarg    = recarg_noct(it,v_h)
       bon_nat   = bonos_mater(it)

       *gus IF  p_prof > 0 ; bon_nat[4]=45*jor_trab ;else;bon_nat[4]=0;ENDIF
       *IF  p_prof > 0 ; bon_nat[4]=60*jor_trab ;else;bon_nat[4]=0;ENDIF
       *bon_nat[4]=60*jor_trab
       *bon_nat[4]=95*jor_trab

      IF dias<30
           bon_nat[4]= ROUND(ROUND(95/30,2)*dias*jor_trab,2)
      ELSE
          bon_nat[4]=95*jor_trab
      ENDIF
	

       // bono profesional no se paga a administradores
	   IF n_item = "CBB-001"
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

       desc_ley   = afp_bbv+afp_fut+desc_uno

       renta      = iva(nitem,TotalG,desc_ley)              //  IVA
       col_medico = general(asociacion,sue_mens,1,sue_mens)           //COLEGIO MEDICO
       col_bio_fa = general(asociacion,totalG,3,sue_mens)           //COLEGIO DE BIOQUIMICA
       col_enferm = general(asociacion,totalG,5,sue_mens)           //COLEGIO DE ENFERMERAS
       aso_aux_enf = general(asociacion,totalG,7,sue_mens)          //ASOCIACION DE ENFERMERAS


       aso_cir = general(asociacion,totaLG,9,sue_mens)          //ASOCIACION CIRUGIA Y PEDIATRIA
       aso_ped = general(asociacion,totaLG,11,sue_mens)

       sindicatoA  = general(asociacion,totalG,13,sue_mens)         //SINDICATO ADMINISTRATIVO
       sindicatoM  = general(asociacion,totalG,15,sue_mens)   //SINDICATO MEDICO antes sue_mens
       sindicatoE  = general(asociacion,totalG,17,sue_mens)  //SINDICATO DE ENFERMERAS
       sindiC      = general(asociacion,sue_mens,19,sue_mens)  //CONFEDERACION SIND. ADMI.
       el_fuerte   = general(asociacion,totalG,21,sue_mens)        //EL FUERTE

       anticipos  = anti_cipo(it)
       prest_caja = halla_pres(it,"01P")+halla_pres(it,"01H")
       otros= control_prest(it)


	fdo_social  = general(asociacion,totalG,23,sue_mens) //C.O.D.
	area25:=SELECT()
	 SELECT 32
	 USE personal  INDEX perso_it
	 set order to 1
	 SEEK alltrim(it)
	 IF FOUND()
	   multas     = multas(faltas,TotalG)+ROUND((TotalG - hrs_extras - emerg-recarg)/30,2)*falta_dia
	    temp      = ROUND((TotalG - hrs_extras - emerg-recarg)/30,2)*falta_dia
	    refrige   = refrigerio
	    retencion = reten_jud
	    pulpe     = pulperia
	    mul       = otra_multa
	    mul1      = mul_medico
	    dias      = dias_trab
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
totald = afp_bbv + afp_fut + desc_uno + renta + anticipos+col_medico+;
  col_bio_fa + col_enferm + aso_aux_enf + aso_cir+aso_ped + sindicatoA + sindicatoM+sindiC+sindicatoE+;
  el_fuerte +  multas + otros + prest_caja+fdo_social+retencion+pulpe+refrige
     totald = ROUND(totald,2)
   SELECT(area0)
RETURN

FUNCTION sale(nom,ite)
LOCAL ss:=.T.
      area45:=SELECT()

      IF (ALLTRIM(nombre)=="ACEFALIA" .OR. ALLTRIM(nombre)=="COMISION")
	ss=.F.
       // MENSAJE("No es posible generar la Boleta de Pago",12,17,'W/R+',.T.,.F.)
      ELSE

       SELECT 32
	 USE personal  INDEX perso_it
	 SEEK alltrim(ite)

	 IF FOUND()
	    IF dias_trab=0
	      ss=.F.
	      //MENSAJE("No es posible generar la Boleta de Pago, tiene cero dias trabajados",12,17,'W/R+',.T.,.F.)
	   ENDIF
	 ENDIF

     ENDIF
       SELECT(area45)
RETURN ss

STATIC FUNCTION saldo_iva(nit)
LOCAL xx:=0
       area105:=SELECT()
       SELECT 22
       USE  iva INDEX iva_item
       SEEK ALLTRIM(nit)
       IF FOUND()
	xx=sald_depen
       END
       SELECT(area105)
RETURN xx

FUNCTION ex_no(nite)
LOCAL en:={0.0,0.0}

	  area123:=SELECT()
	  SELECT 16
	  USE hrs_eme  INDEX extra_it
	  SEEK ALLTRIM(nite)
	  IF FOUND()
	     en[1]=hora_ext
	     en[2]=hora_noc
	  ENDIF
	 SELECT(area123)
RETURN en

******************************************************************************
* Imprime directamente la boleta de pago                                     *
******************************************************************************
FUNCTION direc(nitem,nomnb)
LOCAL forma, forma1, forma2, forma3, forma4, formal, anterior, lcargo,formafir,tforma
LOCAL pp[15], vectora[20], vector[16], vector1[16], vector2[16], vector3[16], vector4[16]
LOCAL ante, sale,formaiva,vec_vacio[1],vec[10]                           //ESTE ULTIMO NOS DA EL TIPO DE LETRA
LOCAL forma5,forma6,forma7,forma8,forma9

LOCAL LETRAS:=CHR(27)+CHR(48)
LOCAL ec:={0.0,0.0},an
prepla("Boleta de Pago")

      SELECT 1
      use dat_empl index nitem,nombre,codigo,eventual,ci
      SET ORDER TO 1
      SEEK nitem

      IF !FOUND()
	RETURN
      ENDIF

      IF !STARTREPORT(80,'82',1,4)
       RETURN ' '
      ENDIF

       forma0='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                                                                       XXXXXXXXXXXXXXXXXXX'
       forma ='XXXXXXXXXXXXXX        XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX         XXXXXXXXXXXXXXXXXXXXXXXXXXX'
       forma1='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        XXXXXXXXXXXXXXXXXXXX          XXXXXXXXXXXXXXXXXXXXXX'
       tforma=REPLICATE('X',25) + SPACE(15) + REPLICATE('X',17)
       forma2='XXXXXXXXXXXXXXX: ##                                          XXXXXXXXXXXXXXX: #,###,###.##'
       forma3='XXXXXXXXXXXXXXX: ##,###,###.##                               XXXXXXXXXXXXXXX: #,###,###.##'
       forma4='XXXXXXXXXXXXXXX: ##,###,###.##     XXX ###                   XXXXXXXXXXXXXXX: #,###,###.##'
      forma41='XXXXXXXXXXXXXXX: ##,###,###.##'
       forma5='XXXXXXXXXXXXXXX: ##,###,###.##     XXXXXXXXX ###.##          XXXXXXXXXXXXXXX: #,###,###.##'
       forma6='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                             XXXXXXXXXXXXXXXXXX: #,###,###.##'
       forma7='XXXXXXXXXXXXXXXXXX  ##,###,###.##                               XXXXXXXXXXXXXXXXXX: #,###,###.##'
       forma8='                                                             XXXXXXXXXXXXXXX: #,###,###.##'
       forma9='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX          XXXXXXXXXXXXXXXXXX: #,###,###.##'

vec_vacio[1]=SPACE(130)
HEADERSCREEN(@vec_vacio,1)

      ini_var()
     proceso2(ALLTRIM(nitem))
     IF dias=0
	MENSAJE("No es posible generar la Boleta de Pago, tiene cero dias trabajados",12,17,'W/R+',.T.,.F.)
	RETURN
     ENDIF
      ec=ex_no(nitem)
      vec_vacio[1]=''
      vec[1]=LETRAS
      vec[2]=SPACE(4)

      IF !EVERYROW(forma0,vec,.T.);ENDIF

      vec[1]= _ci(28) + espacios(la_empresa ,29) //'CAJA PETROLERA DE SALUD',29)
      vec[2]= _ci(5)  + DTOC(DATE())

      IF !EVERYROW(forma0,vec,.T.);ENDIF
      vec[1]=_ci(12) + espacios('Regional Cochabamba',27)
      vec[2]=_ci(42) + espacios('BOLETA DE PAGO',14)
      IF !EVERYROW(tforma,vec,.T.);ENDIF
      an=ALLTRIM(mes)+'/'+STR(anio) //RIGHT(STR(YEAR(DATE())),2)

      IF !EVERYROW(_ci(25)+'                                  MES:'+an+SPACE(35)+_ci(5),vec_vacio,.T.);ENDIF

      vec[1]='';IF !EVERYROW('',vec,.T.);ENDIF

      lcargo      = SUBSTR(codigo,9,3)

      vec[1]=espacios('ITEM:'+nitem,14);vec[2]=espAcios('CARGO:'+nom_car(lcargo),40);vec[3]=espacios('FECH. ING.:'+DTOC(fechita),21)
      IF !EVERYROW(forma,vec,.T.,9);ENDIF

      vec[1]=espacios('NOMBRE:'+nombre,40);vec[2]=espacios('C.I. :'+ci,18);vec[3]=espacios('FECH. NAC.:'+DTOC(fecha_nac),21)
      IF !EVERYROW(forma1,vec,.T.,9);ENDIF

      vec[1]='';IF !EVERYROW(_ci(9)+'------------------------ INGRESOS ----------------- |------------- DESCUENTOS -----------|',vec,.T.);ENDIF
      vec[1]=espacios('NIVEL',15);vec[2]=nivel;   vec[3]=espacios('AFP BBV    ',15);vec[4]=afp_bbv
      IF !EVERYROW(forma2,vec,.T.);ENDIF

      vec[1]=espacios('DIAS TRABAJ.',15);vec[2]=dias;vec[3]=espacios('AFP FUTURO ',15);vec[4]=afp_fut
      IF !EVERYROW(forma2,vec,.T.);ENDIF

      vec[1]=espacios('SUELDO MENSUAL',15);vec[2]=sue_mens;vec[3]=espacios('F.SOL. BBV',15);vec[4]=fonv_bbv
      IF !EVERYROW(forma3,vec,.T.);ENDIF

      vec[1]=espacios('BONO ANTIG.',15);vec[2]=bon_antig;vec[3]=espacios('F.SOL. FUT.',15);vec[4]=fonv_fut
      IF !EVERYROW(forma3,vec,.T.);ENDIF

      vec[1]=espacios('CATEG.PROF.',15);vec[2]=cat_profe;vec[3]='% :';vec[4]=p_prof;vec[5]=espacios('C.O.D.',15);vec[6]=fdo_social
      IF !EVERYROW(forma4,vec,.T.);ENDIF

      // Escalafon medico
      vec[1]=espacios('ESCALAFON ',15);vec[2]=dos_por
      IF !EVERYROW(forma41,vec,.T.);ENDIF

//    vec[1]='';IF !EVERYROW('',vec,.T.);ENDIF blanco

      vec[1]=espacios('HORAS ADIC.',15);vec[2]=hrs_adic;vec[3]=espacios('IVA',15);vec[4]=renta
      IF !EVERYROW(forma3,vec,.T.);ENDIF

      vec[1]=espacios('HORAS EXTRAS',15);vec[2]=hrs_extras;vec[3]='TOTAL HRS:';vec[4]=ec[1];vec[5]=espacios('ANTICIPOS',15);vec[6]=anticipos
      IF !EVERYROW(forma5,vec,.T.);ENDIF

      vec[1]=espacios('HORAS NOCT.',15);vec[2]=recarg;vec[3]='TOTAL HRS:';vec[4]=ec[2];vec[5]=espacios('PREST.COMPL.',15);vec[6]=prest_caja
      IF !EVERYROW(forma5,vec,.T.);ENDIF

      vec[1]='';IF !EVERYROW('',vec,.T.);ENDIF //blanco

      vec[1]=espacios('INDICE EFEC.',15);vec[2]=indice;vec[3]=espacios('COL.MEDICO',15);vec[4]=col_medico
      IF !EVERYROW(forma3,vec,.T.);ENDIF

      vec[1]=espacios('SUB.IRRADI.',15);vec[2]=sub_irrad;vec[3]=espacios('COL.BIOQ.FARM.',15);vec[4]=col_bio_fa
      IF !EVERYROW(forma3,vec,.T.);ENDIF

      vec[1]=espacios('SERV.EMERG.',15);vec[2]=emerg;vec[3]=espacios('COL.ENFERM.',15);vec[4]=col_enferm
      IF !EVERYROW(forma3,vec,.T.);ENDIF

      vec[1]='';IF !EVERYROW('',vec,.T.);ENDIF //blanco

      vec[1]=espacios('PRE NATAL',15);vec[2]=bon_nat[1];vec[3]=espacios('ASOC.A.ENF.',15);vec[4]=aso_aux_enf
      IF !EVERYROW(forma3,vec,.T.);ENDIF

    * vec[1]=espacios('NATALIDAD',15);vec[2]=bon_nat[2];vec[3]=espacios('ASOC.CIRUGIA',15);vec[4]=aso_cir
      vec[1]=espacios('DIF.ESC.',15);vec[2]=dif_esc;vec[3]=espacios('ASOC.CIRUGIA',15);vec[4]=aso_cir
      IF !EVERYROW(forma3,vec,.T.);ENDIF

     *vec[1]=espacios('LACTANCIA',15);vec[2]=bon_nat[3];vec[3]=espacios('ASOC.PEDIATR',15);vec[4]=aso_ped
      vec[1]=espacios('DIF.ESC.',15);vec[2]=dif_esc;vec[3]=espacios('ASOC.PEDIATR',15);vec[4]=aso_ped
      IF !EVERYROW(forma3,vec,.T.);ENDIF

      vec[1]=espacios('BON.R.PROF.',15);vec[2]=bon_nat[4];vec[3]=espacios('SIND.ADMI',15);vec[4]=sindicatoA
      IF !EVERYROW(forma3,vec,.T.);ENDIF

      vec[1]=espacios('SUPLENCIAS',15);vec[2]=0;vec[3]=espacios('SIND.MEDICO',15);vec[4]=sindicatoM
      IF !EVERYROW(forma3,vec,.T.);ENDIF

      vec[1]=REPLICATE('-',35);vec[2]=espacios('SIND.ENFERM.',15);vec[3]=sindicatoE
      IF !EVERYROW(forma6,vec,.T.);ENDIF

      vec[1]=_ci(9)+espacios('Total Ingresos',18);vec[2]=totalG ; vec[3]=_ci(1)+espacios('CONF.SIND.AD',18);vec[4]=sindiC
      IF !EVERYROW(forma7,vec,.T.);ENDIF

      vec[1]=_ci(9)+espacios('Total Descuentos',18);vec[2]=totalD ;vec[3]=_ci(1)+espacios('LACTANCIA',18);vec[4]=bon_nat[3]
      IF !EVERYROW(forma7,vec,.T.);ENDIF

      vec[1]=REPLICATE('-',35); vec[2]=espacios('PRE NATAL',15);vec[3]=bon_nat[1]
      IF !EVERYROW(forma6,vec,.T.);ENDIF
	
      vec[1]=_ci(25)+espacios('LIQUIDO PAGABLE',18);vec[2]=ROUND(totalG-totalD,2) ; vec[3]=_ci(1)+espacios('MULTAS',18);vec[4]=multas
      IF !EVERYROW(forma7,vec,.T.);ENDIF

     // vec[1]='';IF !EVERYROW('',vec,.T.);;ENDIF
	vec[1]=espacios('PRO-DEP. ',15);vec[2]=el_fuerte
	IF !EVERYROW(forma8,vec,.T.);ENDIF


      vec[1]=_ci(28)+espacios(numero_lite(totalG-totalD),80);vec[2]=_ci(1)+espacios('RET.JUDIC.',18);vec[3]=retencion
      IF !EVERYROW(forma9,vec,.T.);ENDIF

      vec[1]=espacios('REFRIGERIO',15);vec[2]=refrige
      IF !EVERYROW(forma8,vec,.T.);ENDIF

      vec[1]=espacios('Saldo IVA    ',15);vec[2]=saldo_iva(nitem);vec[3]=espacios('ANTIC.II',15);vec[4]=pulpe
      //vec[1]=espacios('             ',15);vec[2]=0
      //vec[1]=espacios('PULPERIA',15);vec[2]=pulpe
      IF !EVERYROW(forma3,vec,.T.);ENDIF

      vec[1]=espacios('OTROS',15);vec[2]=otros
      IF !EVERYROW(forma8,vec,.T.);ENDIF

      vec[1]= espacios('FIRMA :_____________________',35);vec[2]=_ci(9)+espacios('TOTAL DESCUENTO',18);vec[3]=totalD
      IF !EVERYROW(forma6,vec,.T.);ENDIF
      vec[1]='';IF !EVERYROW('',vec,.T.);ENDIF
     FINISHREPORT()
RETURN

FUNCTION ac_dir()
LOCAL pp,litem:=space(10),lnom:=space(40),ite

	    pp=OPEN_WINDOW(14,20,20,79,1,'',"B/N+",5,.T.)
	    SET COLOR TO "W/N+"
	    SET CURSOR ON

	    @ 16,23 SAY "No. DE ITEM :"
	    @ 18,23 SAY "APELLIDOS Y NOMBRES :"

	    @ 16,37 GET litem PICTURE "@S40!"  COLOR "R/W"
	    READ
	    IF LASTKEY()=27;CLOSE_WINDOW(pp);RETURN;ENDIF
	    IF litem!=SPACE(10) 
	       SELECT 1
	       use dat_empl index nitem,nombre,codigo,eventual,ci
	       SET ORDER TO 1
	       SEEK ALLTRIM(litem)
	       IF !FOUND()
		   MENSAJE("No existe ese ITEM  ...",12,17,'W/R+',.T.,.F.)
	       ELSE
		   lnom=rec_apell(litem,1)
	       ENDIF
	    ENDIF

	    @ 18,46 GET lnom  PICTURE "@S30!"  COLOR "R/W" VALID lnom!=SPACE(40)
	    READ
	    IF LASTKEY()=27;CLOSE_WINDOW(pp);RETURN;ENDIF
	    IF !(litem=SPACE(10) .AND. lnom=SPACE(40)) .AND. ALLTRIM(lnom)!="ACEFALIA" .AND. ALLTRIM(lnom)!="COMISION"
	       ite=rec_item(lnom)
	       IF ite!=SPACE(10) .AND. tip_empl!="E";direc(ite);ENDIF
	    ENDIF
	    CLOSE_WINDOW(pp)
RETURN

STATIC FUNCTION rec_item(nom)
LOCAL nn:=SPACE(10)
	   ar:=SELECT()
	    SELECT 1
	    use dat_empl index nitem,nombre,codigo,eventual,ci
	    SET ORDER TO 2
	    SEEK ALLTRIM(nom)
	    IF FOUND()
	       nn=nitem
	    ENDIF
	  SELECT (ar)
RETURN  nn


*****************************************************************************
* BOLETA DE PAGO AL PERSONAL EVENTUAL                                      *
*****************************************************************************

FUNCTION bol_eve()

LOCAL tituloscrren, referescreen, rec
LOCAL forma, forma1, forma2, forma3, forma4, formal, anterior, lcargo,formafir,tforma
LOCAL pp[15], vectora[20], vector[16], vector1[16], vector2[16], vector3[16], vector4[16]
LOCAL ante, sale,formaiva,vec_vacio[1],vec[10]                           //ESTE ULTIMO NOS DA EL TIPO DE LETRA
LOCAL forma5,forma6,forma7,forma8,forma9,forma52:=""
LOCAL LETRAS:=CHR(27)+CHR(48),num_pag,band
LOCAL ec:={0.0,0.0},carac,nit,an
are:=SELECT()
prepla("Boleta de Pago al Personal Eventual")
IF LASTKEY()=27;RETURN;ENDIF
ini_var()

SELECT 1
use dat_empl index nitem,nombre,codigo,eventual,ci
SET ORDER TO 4

IF LASTREC() = 0
   RETURN
ENDIF

IF !STARTREPORT(80,'82',1,4)
   RETURN ' '
ENDIF
       forma0='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                                                                         XXXXXXXXXXXXXXXXX'
       forma ='XXXXXXXXXXXXXX        XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX         XXXXXXXXXXXXXXXXXXXXXXXXX'
       forma1='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX   XXXXXXXXXXXXXXXXXXXX          XXXXXXXXXXXXXXXXXXXXX'
       tforma=REPLICATE('X',25) + SPACE(15) + REPLICATE('X',17)    //SPACE(15)+REPLICATE('X',13)
       forma2='XXXXXXXXXXXXXXX: ##                                          XXXXXXXXXXXXXXX: #,###,###.##'
       forma3='XXXXXXXXXXXXXXX: ##,###,###.##                               XXXXXXXXXXXXXXX: #,###,###.##'
       forma4='XXXXXXXXXXXXXXX: ##,###,###.##     XXX ###                   XXXXXXXXXXXXXXX: #,###,###.##'
       forma5='XXXXXXXXXXXXXXX: ##,###,###.##     XXXXXXXXX ###.##' //          XXXXXXXXXXXXXXX: #,###,###.##'
      forma51='XXXXXXXXXXXXXXX: ##,###,###.##     XXXXXXXXX ###.##          XXXXXXXXXXXXXXX: #,###,###.##'
	forma52='XXXXXXXXXXXXXXX: ##,###,###.## '//                              XXXXXXXXXXXXXXX: #,###,###.##'
       forma6='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                             XXXXXXXXXXXXXXXXXX: #,###,###.##'
       forma7='XXXXXXXXXXXXXXXXXX  ##,###,###.##                               XXXXXXXXXXXXXXXXXX: #,###,###.##'
       forma8='                                                             XXXXXXXXXXXXXXX: #,###,###.##'
       forma9='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'

vec_vacio[1]=SPACE(130)
HEADERSCREEN(@vec_vacio,1)
 an=ALLTRIM(mes)+'/'+STR(anio) //RIGHT(STR(YEAR(DATE())),2)
SELECT 1
use dat_empl index nitem,nombre,codigo,eventual,ci
SET ORDER TO 1 //4
num_pag      = 0
band         =.T.

GO TOP
/* DO WHILE tip_empl=" "
 SKIP
ENDDO */
carac=tip_empl

DO WHILE !EOF()  //carac="E"
   IF  tip_empl="E"
       anterior = ALLTRIM(codigo)
       IF 1->jor_trab=0.5
	  medio="1/2t"
       ELSE
	  medio="    "
       ENDIF
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

	 sue_mens=sueldo(1->nivel,1->jor_trab)
	 //sal_diar=sue_mens

	 IF dias=0
	   limpiar()
	   SELECT 2
	   //use temp_eve index enitem,enombre
	   SKIP
	   carac=tip_empl
	   nit=nitem
	   LOOP
	 ENDIF

	 IF anio_rec > 0 .OR. mes_rec > 0
	    fechita = halla_fecha(fech_ing,anio_rec,mes_rec,dia_rec)
	 ELSE
	    fechita = fech_ing
	 ENDIF

	bon_antig=bono_antig("B",fechita)*jor_trab


	IF dias<30
	   sue_mens     = ROUND(ROUND(sue_mens/30,2)*dias,2)
	  // sal_diar   = ROUND(ROUND(sue_mens/30,2)*dias,2)
	   bon_antig = ROUND(ROUND(bon_antig/30,2)*dias,2)

	ENDIF

       //hrs_adic  = hrs_adicion(codigo,prof,sue_mens)     // parametro it para octubre 1994
       conge = es_con(1->nitem)
       IF conge[4]=0;hrs_adic = hrs_adicion(codigo,prof,sue_mens);ELSE;hrs_adic=conge[4];ENDIF
       cat_profe = categ_prof(sue_mens,p_prof,codigo,prof,hrs_adic)
       emerg     = servicio_em(serv_emerg,sueldo(nivel,1.0),bon_antig,cat_profe)
       emerg     = ROUND(ROUND(emerg/30,2)*dias,2)
      *v_h       = valor_hora(sexo,prof,sueldo(nivel,1.0),bon_antig,cat_profe,30)

       IF !EMPTY(fech_ing) .AND. fech_ing >= CTOD('01/01/2003')
        v_h  = valor_hora(sexo,prof,sueldo(nivel,1.0),0,0,30)
       ELSE
        v_h  = valor_hora(sexo,prof,sueldo(nivel,1.0),bon_antig,cat_profe,30)
       ENDIF

       hrs_extras= hora_extra(1->nitem,v_h)
       recarg    = recarg_noct(1->nitem,v_h)

       totalg=sue_mens+hrs_extras+recarg+emerg+bon_antig

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




  /*    IF aporte_afp="S"
       fonvi      = fonvis(TotalG)
       if dat_empl->afp = "BBV"
	  if viejo(fecha_nac)
	  
	   afp_bbv = ROUND(TotalG*primaRiesgo,2)
	  else

	  afp_bbv = ROUND(TotalG*porc_afp,2)
	  endif
	  afp_fut = 0
	  fonv_bbv = fonvi
	  fonv_fut = 0
       else
	  if viejo(fecha_nac)
	  
	   afp_fut = ROUND(TotalG*primaRiesgo,2)
	  else

	  afp_fut = ROUND(TotalG*porc_afp,2)
	  endif
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
*/

       desc_ley   = afp_bbv+afp_fut+desc_uno
       renta      = iva_even(1->nitem,totalG,imp_iva+imp_t,desc_ley)

       //renta      = ROUND(((imp_iva+imp_t)/100)*TotalG,0)
       anticipos  = anti_cipo(1->nitem)

	area25:=SELECT()
	 SELECT 32
	 USE personal  INDEX perso_it
	 SEEK alltrim(1->nitem)
	 IF FOUND()
	    multas    = multas(faltas,TotalG)+ROUND((TotalG - hrs_extras - emerg-recarg)/30,2)*falta_dia
	    refrige   = refrigerio
	    retencion = reten_jud
	    pulpe     = pulperia
	 ELSE
	   multas    = 0
	   retencion = 0
	   refrige   = 0
	 ENDIF
	 SELECT(area25)

       otros      = control_prest(1->nitem)
       totaldesc  = afp_bbv+afp_fut+desc_uno+renta+anticipos+multas+retencion+pulpe+refrige+otros

       liq_pagable=totalg-totaldesc                             //entra

     IF !sale(nombre,1->nitem)
	 SELECT 1
	 use dat_empl index nitem,nombre,codigo,eventual,ci
	 SKIP
	 carac=tip_empl
	 LOOP
     ENDIF

     ec=ex_no(ALLTRIM(1->nitem))

     vec_vacio[1] = ''
     vec[1]       = LETRAS

      IF band
	  band=.F.
	  num_pag++
	 vec[2]       = ALLTRIM(STR(num_pag))
      ELSE
	 vec[2]       = ''
	 band= .T.
      ENDIF

      IF !EVERYROW(forma0,vec,.T.);EXIT;ENDIF

/*     vec_vacio[1]=''
     vec[1]=LETRAS
     vec[2]=SPACE(4)

     IF !EVERYROW(forma0,vec,.T.);EXIT;ENDIF */

       vec[1]= _ci(28) + espacios(la_empresa,29) //'CAJA PETROLERA DE SALUD',29)
       vec[2]= _ci(5)  + DTOC(DATE())

       IF !EVERYROW(forma0,vec,.T.);EXIT;ENDIF
       vec[1]=_ci(12) + espacios('Regional Cochabamba',27)
       vec[2]=_ci(42) + espacios('BOLETA DE PAGO',14)
       // vec[3]= CHR(27)+'S'+CHR(1)+'Soft. TECSYS'
       IF !EVERYROW(tforma,vec,.T.);EXIT;ENDIF

       IF !EVERYROW(+_ci(25)+'                                  MES:'+an+SPACE(35)+_ci(5),vec_vacio,.T.);EXIT;ENDIF

       vec[1]='';IF !EVERYROW('',vec,.T.);EXIT;ENDIF

       lcargo      = SUBSTR(anterior,9,3)

       vec[1]=espacios('ITEM:'+1->nitem,14);vec[2]=espAcios('CARGO:'+nom_car(lcargo),39);vec[3]=espacios('FECH. ING.:'+DTOC(fech_ing),21)
       IF !EVERYROW(forma,vec,.T.,9);EXIT;ENDIF

       vec[1]=espacios('NOMBRE:'+nombre,40);vec[2]=espacios('C.I. :'+ci+'-'+ALLTRIM(extension),17);vec[3]=espacios('FECH. NAC.:'+DTOC(fecha_nac),21)
       IF !EVERYROW(forma1,vec,.T.,9);EXIT;ENDIF

      vec[1]='';IF !EVERYROW(_ci(9)+'------------------------ INGRESOS ----------------- |------------- DESCUENTOS -----------|',vec,.T.);EXIT;ENDIF

      vec[1]=espacios('DIAS TRABAJ.',15);vec[2]=dias;vec[3]=espacios('AFP BBV.    ',15);vec[4]=afp_bbv
      IF !EVERYROW(forma2,vec,.T.);EXIT;ENDIF

      vec[1]=espacios('SUELDO MENSUAL',15);vec[2]=sue_mens;vec[3]=espacios('AFP FUTURO ',15);vec[4]= afp_fut
      IF !EVERYROW(forma3,vec,.T.);EXIT;ENDIF

      vec[1]='';IF !EVERYROW('',vec,.T.);EXIT;ENDIF //blanco

      vec[1]=espacios('HORAS EXTRAS',15);vec[2]=hrs_extras;vec[3]='TOTAL HRS:';vec[4]=ec[1];vec[5]=espacios('F.SOL. BBV',15);vec[6]=fonv_bbv
      IF !EVERYROW(forma51,vec,.T.);EXIT;ENDIF

      vec[1]=espacios('HORAS NOCT.',15);vec[2]=recarg;vec[3]='TOTAL HRS:';vec[4]=ec[2];vec[5]=espacios('F.SOL. FUT.',15);vec[6]=fonv_fut
      IF !EVERYROW(forma51,vec,.T.);EXIT;ENDIF

      vec[1]=espacios('SERV. EMERG.',15);vec[2]=emerg;vec[3]=espacios('I.V.A.',15);vec[4]= renta
      IF !EVERYROW(forma3,vec,.T.);EXIT;ENDIF

    //Aumento Nov 1999

      vec[1]=espacios('BONO ANTIG',15);vec[2]=bon_antig
      IF !EVERYROW(forma52,vec,.T.);EXIT;ENDIF

      totalG=sue_mens+hrs_extras+recarg+emerg+bon_antig

      vec[1]=REPLICATE('-',35);  vec[2]=_ci(1)+espacios('ANTICIPOS',15);vec[3]=anticipos
      IF !EVERYROW(forma6,vec,.T.);EXIT;ENDIF

      vec[1]=_ci(9)+espacios('Total Ingresos',18);vec[2]=totalG ;vec[3]=_ci(1)+espacios('MULTAS',18);vec[4]=multas
      IF !EVERYROW(forma7,vec,.T.);EXIT;ENDIF

      totald=afp_bbv+afp_fut+desc_uno+pulpe+multas+retencion+refrige+otros+renta+anticipos
      totald = ROUND(totald,2)

      vec[1]=_ci(9)+espacios('Total Descuentos',18);vec[2]=totalD;vec[3]=_ci(1)+ espacios('RET.JUDIC.',18);vec[4]=retencion
      IF !EVERYROW(forma7,vec,.T.);EXIT;ENDIF

      vec[1]=REPLICATE('-',35);vec[2]=espacios('PULPERIA',15);vec[3]=pulpe
      IF !EVERYROW(forma6,vec,.T.);EXIT;ENDIF

      vec[1]=_ci(25)+espacios('LIQUIDO PAGABLE',18);vec[2]=round(totalG-totalD,2);vec[3]=_ci(1)+espacios('REFRIGERIO',15);vec[4]=refrige
      IF !EVERYROW(forma7,vec,.T.);EXIT;ENDIF


      vec[1]=espacios('OTROS',15);vec[2]=otros
      IF !EVERYROW(forma8,vec,.T.);EXIT;ENDIF

      vec[1]=_ci(28)+espacios(numero_lite(totalG-totalD),80)
      IF !EVERYROW(forma9,vec,.T.);EXIT;ENDIF

      SKIPLINE(3,.T.)
      vec[1]= espacios('FIRMA :_____________________',35);vec[2]=_ci(9)+espacios('TOTAL DESCUENTO',18);vec[3]=totalD

      IF !EVERYROW(forma6,vec,.T.);EXIT;ENDIF
      vec[1]='';IF !EVERYROW('',vec,.T.);EXIT;ENDIF

    SKIPLINE(13,.T.)

   anterior = ALLTRIM(codigo)

   SELECT 1
   //use dat_empl index nitem,nombre,codigo,eventual,ci
   //SET ORDER TO  1 //4
   SKIP 1
   carac=tip_empl

   IF ultima_linea<=3
      NEWPAGE()
   ENDIF
 ELSE
  SKIP 1
 ENDIF

ENDDO // fin while
are:=SELECT()
FINISHREPORT()
RETURN



