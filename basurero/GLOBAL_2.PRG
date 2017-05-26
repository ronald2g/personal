#include "Directry.ch"
#include  "Inkey.ch"

****************************************************************************
*             RECUPERA EL NOMBRE DEL CARGO DE LA PERSONA                   *
****************************************************************************
FUNCTION const
LOCAL p1
      rr:=SELECT()
      SELECT 44
      USE constant
      IF LASTREC()>0
	      sal_minimo = minimo
	      imp_iva    = iva
	      ldol       = dolar
	      imp_t      = imp_tran
	      lab_fonvi  = fonvi_l
	      pat_fonvi  = fonvi_p
	      afp_bbv_   = fopeba_l
	      afp_futuro = fopeba_p
	      pat_cps    = cps_p
	      incre      = incremento
	      pat_bbv    = bbv_pat
	      pat_fut    = fut_pat
	      sal_min_iva= min_iva
	      tmayor65   = mayor65
      ENDIF

      SHOWGETS()
      SET COLOR TO "N/W+"
      pan1=OPEN_WINDOW(3,3,19,75,1,'Constantes ',"B/W+",5,.T.)
      SET CURSOR ON
      SET COLOR TO "B/W"
      @ 5,7 say "Salario Minimo (Bs.) :" GET sal_minimo PICTURE "999999.99"
      @ 5,40 SAY"Sal. Min. (I.V.A.) Bs." GET sal_min_iva PICTURE "999999.99"
      @ 6,7 SAY "Cambio del Dolar Bs. :" GET  ldol PICTURE "99999.99"
      @ 7,7 say "Impuesto I.V.A %     :" GET imp_iva  PICTURE "99.99"
      @ 8,7 SAY "Impuesto a las Trans.:" GET imp_t PICTURE "99.99"
      @ 9,7 SAY "Incremento Salarial (S. B sico) % :" GET incre PICTURE "99.99"

      @ 11,7 say "Aporte al fondo solidario    % :" get lab_fonvi  PICTURE "99.99"
      @ 12,7 say "Aporte Patronal al FONVI     % :" GET pat_fonvi      PICTURE "99.99"
      @ 13,7 SAY "A.F.P. PREVISION BBV    % :" get afp_bbv_   PICTURE "99.99"
      @ 14,7 SAY "Aporte Patronal  BBV    % :" get pat_bbv    PICTURE "99.99"
      @ 15,7 say "A.F.P. FUTURO           % :" get afp_futuro  PICTURE "99.99"
      @ 16,7 SAY "Aporte Patronal  FUTURO % :" get pat_fut    PICTURE "99.99"
      @ 17,7 say "Aporte Patronal C.P.S.  % :" get pat_cps    PICTURE "99.99"
      @ 18,7 say "Prima Riesgo Profesional % :" get tmayor65 PICTURE "99.99"
      READ
      IF LASTKEY()!=27
       elec=MULTIPLE({"Grabar","Cancelar"},2,17,23,.F.)
       IF elec=1
	 IF LASTREC()=0
	  APPEND BLANK
	 ENDIF
	 REPLACE minimo WITH sal_minimo,;
	      iva       WITH imp_iva,;
	      fopeba_l  WITH afp_bbv_,;
	      fopeba_p  WITH afp_futuro,;
	      fonvi_l   WITH lab_fonvi,;
	      fonvi_p   WITH pat_fonvi,;
	      cps_p     WITH pat_cps,;
	      dolar     WITH ldol,;
	      imp_tran  WITH imp_t,;
	      incremento WITH incre,;
	      bbv_pat   WITH pat_bbv,;
	      fut_pat   WITH pat_fut,;
	      min_iva   WITH sal_min_iva,;
	      mayor65  WITH tmayor65
       ENDIF
      ENDIF
      CLOSE_WINDOW(pan1)
      SELECT(rr)
RETURN

****************************************************************************
*                       Variables Globales                                 *
****************************************************************************
FUNCTION titulo
ee:=SELECT()
PUBLIC  sal_minimo:=0.0,mujer:=210,hombre:=240,medico:=180,camb_dolar:=0
PUBLIC  imp_iva:=13/100,dias_trab:=0.0,anio:=YEAR(DATE())
PUBLIC  afp_bbv_:=0.125,afp_futuro:=0.125,porc_afp:=0
PUBLIC  pat_cps:=0.02,f_vi:=0,imp_t:=0,incre:=0
PUBLIC  s1:=355,s2:=457,s3:=567,pat_bbv:=0,pat_fut:=0
PUBLIC  salADM:=355,salPARA:=457,salMED:=567,f_cps:=0,f_pm:=0,f_pb:=0,ldol:=0
PUBLIC  fechita,dos_por:=0,lab_fonvi,pat_fonvi, sal_min_iva,tmayor65:=0,primaRiesgo:=0
PUBLIC  desc_afp_a:=0,desc_afp_b:=0,desc_afp_c:=0,desc_afp_d:=0


SET SOFTSEEK ON
SET DATE TO french

SELECT(ee)
RETURN

*****************************************************************************
*                     Obtiene el cargo  de un empleado                      *
*****************************************************************************
FUNCTION nom_car(cod)
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

****************************************************************************
*                    RECUPERA LA ACTIVIDAD DEL CODIGO                      *
****************************************************************************
FUNCTION nom_act(cod)
LOCAL a, cad := ''

      a = SELECT()
      SELECT 11
      use (dr+"sector") index (dr+"sector")
      SEEK cod
      IF FOUND()
	 cad = nombre
      ENDIF
      SELECT(a)

RETURN cad

****************************************************************************
*                    RECUPERA LA ANTIGUEDAD EN PORCENTAJE                  *
****************************************************************************
FUNCTION bon_antig(tip,fech)
LOCAL  a
LOCAL  fech_act:=DATE(),rang:=CTOD("  /  /    "),r:=''

       a = SELECT()

       rang=YEAR(fech_act)-YEAR(CTOD(fech))
       IF rang<10
	  r='0'+ALLTRIM(STR(rang))
       ELSE
	  r=ALLTRIM(STR(rang))
       ENDIF
	  SELECT 14
	  use (dr+"bonos") index (dr+"bono_co")
	  SEEK tip+r
	  IF  (r<rango .AND. !FOUND()) .OR. EOF()
	     SKIP -1
	  ENDIF

	  SELECT (a)
RETURN porce

****************************************************************************
*                    ABRIR ARCHIVOS DE PLANILLAS                           *
****************************************************************************
FUNCTION abrir_tablas()

	SELECT 10
	USE (dr+"meses") INDEX (dr+"meses")
	SELECT 11
	USE (dr+"sector" )  INDEX (dr+"sector")
	SELECT 12
	USE (dr+"cargo")    INDEX (dr+"cargo_co")
	//SELECT 13
	//USE (dr+"nivel")    INDEX (dr+"nivel_co")
	//USE nivel INDEX nivel_co
	SELECT 14
	USE (dr+"bonos")    INDEX (dr+"bono_co")
	SELECT 18
	USE (dr+"bombom")
RETURN

****************************************************************************
*                           Recupera el apellido                           *
****************************************************************************
FUNCTION rec_apell(item,band)
LOCAL a,b,c, cad := ''

       a = SELECT()
       b = INDEXORD()
       c = RECNO()

       SELECT 1
       use dat_empl index nitem,nombre,codigo,eventual,ci
       SET ORDER TO 1

       SEEK item
       IF FOUND()
	 IF band=1
	   cad=ALLTRIM(nombre)
	 ELSE
	  cad =STRTRAN(nombre,","," ")
	ENDIF
       ENDIF

       SELECT (a)
       SET ORDER TO (b)
       GO c

RETURN cad

*****************************************************************************
*     EXTRAE EL NOMBRE DE LA UBICACION DE UN ITEM                           *
*****************************************************************************
FUNCTION nom_area(codi,pos)
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

****************************************************************************
*             Extrae el cargo de un Empleado                               *
****************************************************************************
FUNCTION bus_cargo(carg)
LOCAL car:=''
	 carg=substr(carg,9,3)
	 g:=SELECT()
	 SELECT 12
	 use (dr+"cargo") index (dr+"cargo_co")
	 SEEK carg
	 IF FOUND()
	       car=ALLTRIM(B)
	 ENDIF
	 SELECT(g)
RETURN car

****************************************************************************
*                       Dado el codigo obtiene su actividad                *
****************************************************************************
FUNCTION bus_sector(sec)
LOCAL nom:=''
	 sec=SUBST(sec,5,3)
	 c:=SELECT()
	 SELECT 11
	 use (dr+"sector") index (dr+"sector")
	 SEEK sec
	 IF FOUND()
	     nom=ALLTRIM(nombre)
	ENDIF
       SELECT(c)
RETURN nom

****************************************************************************
*                       Obtiene el Salario de un empleado                  *
****************************************************************************
FUNCTION sueldo(ni,JorTrab)
LOCAL su:=0.0
       a:=SELECT()
       SELECT 13
       //use (dr+"nivel") index (dr+"nivel_co")
       USE nivel INDEX nivel_co
       //SEEK ni
       SEEK ALLTRIM(ni)

       IF FOUND()
	       su= sueldo*JorTrab
	       su = su+(incre/100)*su
	ENDIF
	SELECT(a)

RETURN ROUND(su,2)

FUNCTION bono_antig(tipo,fech)
RETURN ROUND((porce(tipo,fech,CTOD("  /  /    "))*sal_minimo),2)

FUNCTION efectivo(tipo,fech,sueldoMensual)
RETURN ROUND((porce(tipo,fech,CTOD("  /  /    "))*sueldoMensual),2)

FUNCTION porce(tip,fech,fech_act)
LOCAL rang:=CTOD("  /  /    ")
LOCAL  r:='',pp:=0.0,dia1,dia2,mess,mes1,mes2

       IF fech_act=CTOD("  /  /    ")
	  fech_act=cad_fecha
       ENDIF

*       ? fech_act
*       ? fech
*       INKEY(0)

       rang=YEAR(fech_act)-YEAR(fech)
       mes1=MONTH(fech_act)
       mes2=MONTH(fech)
       mess=mes1-mes2
       dia1=DAY(fech_act)
       dia2=DAY(fech)

       b:=SELECT()

       IF mess<0
	   rang=rang-1
	ELSE
	   IF mess=0
	       IF dia2>dia1
		  rang=rang-1
	       ENDIF
	   ENDIF
	ENDIF
       /* Caso subsidio de irradiacion
       IF tip=="R"
	   IF dia2<dia1 .AND. mes2>=mes1
	     rang=rang+1
	   ENDIF
       ENDIF */

       IF rang=0
		RETURN 0
       ENDIF

       IF  rang<10
		r='0'+ALLTRIM(STR(rang))
       ELSE
	   r=ALLTRIM(STR(rang))
       ENDIF

       SELECT 14
       use (dr+"bonos") index (dr+"bono_co")
       SEEK tip+r

       IF (r<rango .AND. !FOUND()) .OR. (tip <> tipo)
	  skip -1
       endif
       pp=porce
       SELECT(b)

RETURN (pp/100)  // multiplicar por salario minimo para obtener
		 // bono de antiguedad

****************************************************************************
*                 C lculo de la Categoria Profesional                       *
*****************************************************************************
FUNCTION categ_prof(sue_mens,porce,codigo,prof,hr)
LOCAL ct:=0
	*IF LEFT(prof,6)=="MEDICO" .AND. SUBSTR(codigo,8,1)=="A"
	*  ct=(porce/100)*(sue_mens+hr)
	*ELSE
	*  ct=(porce/100)*sue_mens
	*ENDIF
        ct=(porce/100)*sue_mens
RETURN ROUND(ct,2)

*****************************************************************************
*             C lculo de las horas adicionales                              *
*****************************************************************************
FUNCTION hrs_adicion(codigo,prof,sue_mens) //,categ_pr)
LOCAL   hrad:=0.0
	IF LEFT(prof,6)=="MEDICO" .AND. SUBSTR(codigo,8,1)=="A"
	 *hrad=0.33*(sue_mens)            //+categ_pr)
         * hrad=0.20*(sue_mens)
	ENDIF
RETURN hrad

/*FUNCTION hrs_adicion(item)
LOCAL   hrad:=0.0
	IF item="CBB-001"
	   hrad=0.33*2957
	ENDIF
	IF item="CBB-022"
	   hrad=0.33*2497
	ENDIF
RETURN hrad mes de octubre  1994*/

*****************************************************************************
* Solo se ejecuta una sola vez cada 31 de diciembre                         *
* Indice de Efectividad                                                     *
*****************************************************************************

*****************************************************************************
FUNCTION indice_efectivo()
LOCAL sue_mens:=0.0,monto:=0.0,nivel:="   ",porcent:=0.0,fech_i:=DATE()
LOCAL pp11,lniv:=0,lbase:=0,lsal1:=0,lsal2:=0,lsal3:=0,fechita:=DATE()

       a:=SELECT()
       SELECT 50
       USE (dr+"ind_efec")
       IF LASTREC()>0
	 GO 1
	 lbase=sueld_base
	 sal1 =sueld1
	 sal2 =sueld2
	 sal3 =sueld3
      ENDIF

       pp11=OPEN_WINDOW(6,5,22,76,1,'Indice de Efectividad',"B/N+",5,.T.)
       SET COLOR TO "W/N+"
       SET CURSOR ON
       @ 8,10 SAY "Nivel del Salario Base :" GET lniv PICTURE "99" COLOR "B/W" VALID lniv>0
       @ 10,10 SAY "Salario Base           :" GET lbase PICTURE "999,999.99"  COLOR "B/W" VALID lniv>0
      /* @ 12,10 SAY "Salarios Siguientes al Base "
       @ 14,10 SAY "Salario 1 :"   GET  lsal1 PICTURE"999,999.99" COLOR "B/W" VALID lsal1>0
       @ 16,10 SAY "Salario 2 :"   GET  lsal2 PICTURE"999,999.99" COLOR "B/W" VALID lsal2>0
      */

      // @ 18,10 SAY "Salario 3 :"   GET  lsal3 PICTURE"999,999.99" COLOR "B/W" VALID lsal3>0
       @ 20,10 SAY "Fecha para realizar el C lculo :" GET fechita COLOR "B/W" VALID fechita!=CTOD("  /  /    ")
       READ
       IF LASTKEY()!=27
	 elec=MULTIPLE({"Grabar","Cancelar"},2,21,23,.F.)
	 IF elec=1
	    IF LASTREC()=0
	     APPEND BLANK
	   ENDIF

	   REPLACE sueld_base WITH lbase,;
		   sueld1     WITH sal1,;
		   sueld2     WITH sal2,;
		   sueld3     WITH sal3
	ENDIF
       //IF DAY(fech_i)=31 .AND. MONTH(fech_i)=12
       //ENDIF
       // SELECT 13
       // SEEK "15" // Busca el salario del nivel 15
       //sue_mens=sueldo
	MENSAJE("Calculando ....",12,17,'W/R+',.T.,.F.)
	SELECT 1
	use dat_empl index nitem,nombre,codigo,eventual,ci
	SET ORDER TO 1
	GO TOP
	DO WHILE !EOF()
	   niv=1->nivel

	   porcent=porce("B",1->fech_ing,fechita)

	   IF niv <= "19"               // Indice efectivo para los niveles 1 - 15
		monto = porcent * lbase //  sue_mens
	   ENDIF

	   // Calculo antiguo
	   /* ELSE                          Indice efectivo para niveles 16,17,18,19
		SELECT 13
		USE nivel INDEX nivel_co
		SEEK niv
		monto=porcent*13->sueldo

		DO CASE
		   CASE niv="18"
			monto = porcent * lsal1
		   CASE niv="19"
			monto = porcent * lsal2
		ENDCASE
	   ENDIF */

	   SELECT 1
	   //use dat_empl index nitem,nombre,codigo,eventual,ci


	   IF ALLTRIM(nombre)="ACEFALIA" .OR. ALLTRIM(nombre)="COMISION"
	     REPLACE 1->ind_efec WITH 0
	   ELSE
	     REPLACE 1->ind_efec WITH monto
	   ENDIF

	   SKIP
	ENDDO

	MENSAJE("Fin .......",12,17,'W/R+',.T.,.F.)
	ENDIF
	CLOSE_WINDOW(pp11)
	SELECT(a)
RETURN  ''

*****************************************************************************
* C lculo del subsidio de irradiaci¢n                                       *
*****************************************************************************
FUNCTION sub_irradia(codigo,fech,sue_mens)
LOCAL ultcar:=' ',subsi:=0.0

	 a:=SELECT()
	 ultcar=RIGHT(codigo,1)
	 IF ultcar=="R"
	    subsi=porce("R",fech,CTOD("  /  /    "))*sue_mens
	 ENDIF
	 SELECT(a)
RETURN ROUND( subsi,2)

*****************************************************************************
* Servicio de Emergencia solo M‚dicos                                       *
* excepto los odontologos                                                   *
* Realizar un control de quienes hacen servicios de emergencia o todos      *
* los m‚dicos                                                               *
*****************************************************************************
FUNCTION  servicio_em(serv_emerg,sueldo,bono_antig,categ_prof)
LOCAL se:=0.0
	IF serv_emerg='S'
	   se:=0.60*(sueldo+bono_antig+categ_prof)
	ENDIF
RETURN ROUND(se,2)


*****************************************************************************
*            C lculo del valor hora                                         *
*****************************************************************************
FUNCTION valor_hora(sexo,profe,sueldo,bon_antig,categ_pro,diasT)
LOCAL valor:=0.0

      IF LEFT(profe,6)=='MEDICO' .OR. LEFT(profe,12)="FARMACEUTICA" .OR. LEFT(profe,10)="BIOQUIMICA" .OR. LEFT(profe,10)="ODONTOLOGO"
	 valor=6*diasT
      ELSE
	 DO CASE
	    CASE ALLTRIM(sexo)=='F'
	     valor=7*diasT
	    CASE ALLTRIM(sexo)=='M'
	      valor=8*diasT
	 ENDCASE
      ENDIF

*       	   IF nitem = "CBB-122"
*	      ? "sueldo "+str(sueldo)       //gus
*              ? "bon_antig "+str(bon_antig)
*              ? "categ_pro "+str(categ_pro)
*              ? "valor "+str(valor)
*            * ? "hrs_extras "+str(hrs_extras)
*              inkey(0)
*	   ENDIF

//RETURN  ROUND((sueldo+bon_antig+categ_pro)/valor,2)
//RETURN  ROUND((sueldo+bon_antig)/valor,2)
RETURN  ROUND((sueldo)/valor,2)

*****************************************************************************
*          C lculo de las horas extras                                      *
*****************************************************************************
FUNCTION hora_extra(item,val_hora)
LOCAL he:=0.0
	 c:=SELECT()
	 SELECT 16
	 USE hrs_eme  INDEX extra_it
	 SEEK item
	 IF FOUND()
	     he=val_hora*ROUND(hora_ext*2,2)
	 ENDIF
	 SELECT(c)
RETURN ROUND(he,2)

// redondea al mayor
function redondeo2(valor)
local cortar
	cortar:=int(valor)
	if cortar < valor 
	    cortar=cortar+1
	endif
return cortar



FUNCTION hora2_extra(item,val_hora,resta)
	LOCAL he:=0.0
	if resta>0
		c:=SELECT()
		SELECT 16
		USE hrs_eme  INDEX extra_it
		SEEK item
		IF FOUND()
			he=val_hora*ROUND(hora_ext*2,2)
			if resta<he
				numero:=redondeo2(resta/val_hora/2)
				he=val_hora*ROUND(numero*2,2)
			endif
		ENDIF
		SELECT(c)
	endif
RETURN ROUND(he,2)


*****************************************************************************
*                     C lculo del Recargo nocturno                          *
*****************************************************************************
FUNCTION recarg_nocturno(item,valor_hora)
	LOCAL rn:=0.0
	d:=SELECT()
	SELECT 16
	USE hrs_eme  INDEX extra_it
	SEEK item
	IF FOUND()
	rn=valor_hora*ROUND(hora_noc*0.25,2)
	ENDIF
	SELECT(d)
RETURN ROUND(rn,2)


FUNCTION  recarg2_nocturno(item,valor_hora,resta)
LOCAL rn:=0.0
	if resta>0 
		d:=SELECT()
		SELECT 16
		USE hrs_eme  INDEX extra_it
		SEEK item
		IF FOUND()
			rn=valor_hora*ROUND(hora_noc*0.25,2)
			if resta<rn 
	       			numero:=redondeo2(resta/valor_hora/0.25)
	       			rn=valor_hora*ROUND(numero*0.25,2)
			endif
		ENDIF
		SELECT(d)
	endif
RETURN ROUND(rn,2)



*****************************************************************************
* Bonos prenatal,natalidad,lactancia y sepelio                              *
*****************************************************************************
FUNCTION Bonos_mater(item)
LOCAL ll:={0.0,0.0,0.0,0.0},fech_act:=DATE(),fech:=CTOD("  /  /    "),fech1:=CTOD("  /  /    ")

     g:=SELECT()
     SELECT 18
     USE nac_muer INDEX n_m_item
	SEEK item
	IF FOUND()
	    fech=fech_ini+365  // 1 a¤o
	    fech1=fech_ini+120 // 4 meses

	   IF SUBSTR(codigo,1,1)=="P" .AND. fech1>=fech_act
	     ll[1]=sal_minimo
	   ENDIF

	   IF SUBSTR(codigo,2,1)=="N"
	     ll[2]=sal_minimo
	   ENDIF

	   IF SUBSTR(codigo,3,1)=="L" .AND. fech>=fech_act
	     ll[3]=sal_minimo
	   ENDIF
       ENDIF

     IF SUBSTR(codigo,4,1)=="S"
	     ll[4]=sal_minimo
     ENDIF
     SELECT(g)
RETURN ll

*****************************************************************************
*                  Rellena con espacios una cadena                          *
*****************************************************************************
FUNCTION espacios(cadena, tam)
LOCAL cad := ''

      cad = LEFT(cadena,tam)
      cad = cad + SPACE(tam - LEN(cad))

RETURN cad

******************************************************************************
*                     Fondo Complementario de la CPS                         *
******************************************************************************
/*FUNCTION cps(profe,TotalG)
LOCAL cppp:=0.0
	 IF !(LEFT(profe,6)='MEDICO' .OR. LEFT(profe,12)="FARMACEUTICA" .OR. LEFT(profe,10)="BIOQUIMICA")
	    cppp=(lab_fccps/100)*TotalG
	 ENDIF
RETURN ROUND(cppp,2)
*/
*****************************************************************************
* Fondo de Pensiones Medicas                                                *
*****************************************************************************
/*FUNCTION Fpm(profe,TotalG)
LOCAL fp:=0.0
	 IF LEFT(profe,6)=='MEDICO' .OR. LEFT(profe,12)="FARMACEUTICA" .OR. LEFT(profe,10)="BIOQUIMICA"
	    fp=(f_pm/100)*TotalG
	 ENDIF
RETURN ROUND(fp,2)
*/

*****************************************************************************
* Fondo de pensiones B sicas                                                *
*****************************************************************************
//FUNCTION fpb(TotalG)
//RETURN ROUND((lab_fopeba/100)*TotalG,2)


****f* PLANILLA/solidario
* DESCRIPCION
*   calcula aporte al fondo solidario 
* SINOPSIS
FUNCTION fsolidario(TotalG)
******
	local descuento
	if TotalG > 13000
		descuento = ROUND((TotalG-13000)*lab_fonvi/100,2)
	else
		descuento = 0
	endif 
RETURN  descuento

****f* PLANILLA/fonvis
* DESCRIPCION
*   calcula aporte al fondo solidario 
* SINOPSIS
FUNCTION fonvis(TotalG)
******
	local descuento
	if TotalG > 13000
		descuento = ROUND((TotalG-13000)*lab_fonvi/100,2)
	else
		descuento = 0
	endif 
RETURN  descuento
****************************************************************************
* Busqueda de anticipos                                                    *
****************************************************************************
FUNCTION anti_cipo(nitem)
LOCAL an:=0.0
	 f:=SELECT()
	 SELECT 16
	 USE hrs_eme  INDEX extra_it
	 SEEK nitem
	 IF FOUND()
	       an=anticipo
	 ENDIF
	 SELECT(f)
RETURN  ROUND(an,2)

****************************************************************************
* Descuentos a Colegios y Asociaciones  obligatorios segun sectores        *
****************************************************************************
FUNCTION general(asoc,totalG,ini,basico)
LOCAL cole:=0.0,bus,lmes
SET SOFTSEEK OFF
	    g:=SELECT()
	    lmes=nu_mes(ALLTRIM(lmeses))
	    bus=ALLTRIM(SUBSTR(asoc,ini,2))
	    SELECT 19
	    USE impuesto INDEX impu_cod
	    SEEK bus
	    IF FOUND()
	     DO CASE
	       CASE bus=="03" .OR. bus=="10"
		  cole=(porcentaje/100)*basico

	       /* CASE (bus=="10") // Confederaci¢n de Administrativos
		IF (lmes=5 .OR. lmes=8 .OR. lmes=11 .OR. lmes=2) //cada tres meses
		    cole=(porcentaje/100)*basico
		 ENDIF
		*/
	       OTHERWISE
		   cole=(porcentaje/100)*totalG +monto
	       ENDCASE
	    ENDIF
	    SELECT(g)
SET SOFTSEEK ON
RETURN ROUND(cole,2)

****************************************************************************
* Prestamos efectuados por la CPS u otras instituciones                    *
****************************************************************************
FUNCTION control_prest(item)
LOCAL pres:=0
       area67:=SELECT()
       SELECT 48
       USE ACREEDOR INDEX acreedor
       rr=LASTREC()
	  FOR k:=2 TO rr
	     IF k<10
		pres=ROUND(pres+halla_pres(item,"0"+alltrim(str(k))),2)
	     ELSE
		pres=ROUND(pres+halla_pres(item,+alltrim(str(k))),2)
	    ENDIF
	   NEXT
       SELECT(area67)
RETURN pres

****************************************************************************
*                   Halla  el monto del pr‚stamo de un empleado            *
****************************************************************************
FUNCTION halla_pres(it,codi)
LOCAL sum:=0
	 area34:=SELECT()
	 SELECT 91
	 USE presta INDEX presta_it
	 SEEK ALLTRIM(it)+codi
	 IF FOUND()
	  IF codi="01P" .OR. codi="01H"
	    sum = monto * ldol
	  ELSE
	    sum:=monto
	  ENDIF
	 ENDIF
	 SELECT(area34)
RETURN ROUND(sum,2)

****************************************************************************
*                  Multas por atrasos                                      *
****************************************************************************
FUNCTION multas(tiempo,totalG)
LOCAL mul:=0.0
      DO  CASE
	  CASE tiempo>30 .AND. tiempo<=40
	    mul=0.5 // medio dia de haber del total ganado
	  CASE tiempo>40 .AND. tiempo<=60
	    mul=1   // un dia
	  CASE tiempo>60 .AND. tiempo<=90
	    mul=3
	  CASE tiempo>90
	    mul=3
      ENDCASE
RETURN ROUND(mul*(TotalG/30),2)

****************************************************************************
* Calculo del Impuesto al Valor Agregado                                   *
* sueld_neto = total ganado - Descuentos de ley                            *
****************************************************************************
FUNCTION iva(nite,totalg,desc_ley)
LOCAL valor:=0.0,aa:=0,bb:=0,cc:=0,dd:=0,ee:=0,ff:=0,gg:=0,hh:=0.0,ii:=0,jj:=0,kk:=0,ll:=0,mm:=0,nn:=0,lll:=0

    r:=SELECT()
    SELECT 22
    USE  iva INDEX iva_item
    SEEK ALLTRIM(nite)

    aa=ROUND(totalg-desc_ley+fneto(nite),0)
    IF totalg=0
       bb=0
     ELSE
       bb=ROUND(2*ROUND(sal_min_iva,2),0)              // m¡nimo no imponible
    ENDIF
     cc=ROUND(aa-bb,0)                     // diferencia sujeta a impuesto
     dd=ROUND((imp_iva/100)*cc,0)          // 13% impuesto liquidado
     ee=ROUND(formu101,0)                  // presentar formu101
     ff=ROUND((imp_iva/100)*bb,0)          // 13% de 2 salarios minimos
     gg=ROUND(dd-ff,0)                     // saldo a favor del fisco

     IF dd<ff
	  hh=(-1)*gg                      //Saldo a favor del dependiente
	  gg=0
     ENDIF

     ii=nada                             // SALDO anterior

     //jj=ROUND(mante_ant,0)               mantenimiento de valor respecto a i

     jj=ROUND(ii*ldolar_act/ldolar_ant - ii,0)

     kk=ROUND(ii+jj,0)                   // subtotal i+j

     IF ee>0
	  ll=ROUND(ee+kk+hh,0)             // Saldo utilizado
     ELSE                             // ll=ROUND(ee+hh+kk)
	  ll=ROUND(hh+kk+ee,0)
     END

     lll=gg                             //Saldo utilizado

     IF ll<lll
	 mm=ROUND(lll-ll,0)             //liquidacion de retenciones
	 nn=0                           // saldo a favor del dependiente al proxima mes
     ELSE
	 mm=0
	 nn=ROUND(abs(ll-gg),0)
     ENDIF                              //liquidacion de retenciones

/*      IF hh>0 .AND. ii=0
	 nn=hh
      ENDIF */

      IF !FOUND()
       APPEND BLANK
      ENDIF

      REPLACE         neto       WITH aa
      REPLACE         diferencia WITH cc
      REPLACE         impu_liqu  WITH dd
      REPLACE         formu101   WITH ee
      REPLACE         saldo_fisc WITH gg
      REPLACE         saldo_depe WITH hh
      REPLACE         saldo_ante WITH nada //ii
      REPLACE         mante_ant  WITH jj
      REPLACE         subtotal   WITH kk
      REPLACE         saldo_tot  WITH ll
      REPLACE         saldo_util WITH lll
      REPLACE         liquid_ret WITH mm
      REPLACE         sald_depen WITH nn
      SELECT(r)
RETURN ROUND(mm,0)

****************************************************************************
*                              Halla el literal de un n£mero               *
****************************************************************************
FUNCTION numero_lite(num)

  PRIVATE literal
  PRIVATE LIT1[20],LIT2[10],LIT3[10]
  PRIVATE H1,HH1,Q1,Q2,Q3,Q4,V10,V13,V14,V16
  LIT1={"UNO","DOS","TRES","CUATRO","CINCO","SEIS","SIETE","OCHO","NUEVE","DIEZ",;
      "ONCE","DOCE","TRECE","CATORCE","QUINCE","DIECISEIS","DIECISIETE",;
      "DIECIOCHO","DIECINUEVE"}

  LIT2={"CIENTO ","DOSCIENTOS ","TRESCIENTOS ","CUATROCIENTOS ","QUINIENTOS ",;
      "SEISCIENTOS ","SETECIENTOS ","OCHOCIENTOS ","NOVECIENTOS "}

  LIT3={" ","VEINTI","TREINTA","CUARENTA","CINCUENTA","SESENTA","SETENTA",;
      "OCHENTA","NOVENTA"}

  H1  = ABS(num)
  HH1 = H1
  DIGIT()

  IF HH1-INT(HH1)=0
     literal = "SON: " + V14+ " 00/100 BOLIVIANOS"
  ELSE
     literal = "SON: " + V14+" "+SUBS(LTRI(STR(HH1-int(HH1),10,2)),3,2)+"/100 BOLIVIANOS"
  ENDIF

RETURN  literal

STATIC FUNCTION DIGIT
  Q1=.F.
  Q2=.F.
  Q3=.F.
  Q4=.F.
  v14=""

  IF LEN(LTRIM(STR(INT(H1))))>10
     RETURN
  ENDIF

  h1 = LTRIM(str(INT(H1)))
  * literales de conversion maximo a 10 nros.
  h1 = SPACE( 12 - LEN(h1) ) + h1

  v16=1

  DO WHILE v16<5
  v13="q"+LTRIM(str(v16))
  &v13=subs(h1,(v16-1)*3+1,3)
  v16=v16+1
  ENDDO

  if val(q1)<>0
     v16=1
     number()
     v14=v14+" MIL "

     if val(q2)<>0
	v16=2
	number()
	v14=v14+" MILLONES "
      else

	 if val(q1)<>0
	    v14=v14+" MILLONES "
	 endi
     endiF
     else
	if val(q2)<>1
	    if val(q2)<>0
	       v16=2
	       number()
	       v14=v14+" MILLONES "
	     else
		 if val(q1)<>0
		    v14=v14+" MILLONES "
		 endiF
	    endiF
	  else
	  v14=v14+" UN MILLON "
	endiF
     endiF

     if val(q3)<>0
	v16=3
	number()
	v14=v14+" MIL "
     endiF
     if val(q4)<>0
	v16=4
	number()
     endiF
RETURN
*----------------------------------------------------------------

STATIC FUNCTION NUMBER
v13="q"+LTRIM(str(v16))
if val(&v13)=100
v14=v14+" CIEN "
retu
else
if val(subs(&v13,1,1))#0
 v10=lit2[val(subs(&v13,1,1))]
 v14=v14+v10
endi
endi
if subs(&v13,2,1)="1"
v10=lit1[val(subs(&v13,2,2))]
v14=v14+v10
retu
else
if subs(&v13,2,2)="20"
v14=v14+"VEINTE "
retu
else
if val(subs(&v13,2,1))#0
 v10=lit3[val(subs(&v13,2,1))]
 v14=v14+v10
endi
endi
endi
if val(subs(&v13,2,2))>20.and.val(subs(&v13,2,1))<>2.and.val(subs(&v13,3,1))<>0
v14=v14+" Y "
endi
if subs(&v13,3,1)="1".and.v16<>4
v14=v14+"UN "
retu
else
if val(subs(&v13,3,1))#0
v10=lit1[val(subs(&v13,3,1))]
v14=v14+v10
endi
endi
RETURN

FUNCTION boton(fil,col,cadena)
LOCAL parte:={},i:=0,car,aux:="",cc:=0,tam:=0
PRIVATE colo:="    "
       colo=SETCOLOR()
       ++i
       DO WHILE i<LEN(cadena)
	     car=SUBSTR(cadena,i,1)
	     IF car="<"
		   i++
		    car=SUBSTR(cadena,i,1)
		DO WHILE car!=">"
		    ++i
		    aux=aux+car
		    car=SUBSTR(cadena,i,1)
		ENDDO
		++cc
		AADD(parte,aux)
		aux=""
	    ENDIF
	++i
       ENDDO

       FOR i=1 TO cc
	  @ fil,col+tam+i*2 SAY "<" COLOR "R/BG+"
	  @ fil,col+tam+2 SAY parte[i] COLOR "B/BG+"
	  @ fil,col+tam+2 GET parte[i] COLOR "B/BG+"
	  tam=LEN(parte[i])
	  @ fil,col+tam+4 SAY ">"      COLOR "R/BG+"
       ENDFOR
       READ
       SETCOLOR("&colo")
RETURN

*****************************************************************************
*                   Almacena los datos finales de la planilla               *
*****************************************************************************
FUNCTION almacenar(v,v1,v2,v3,v4,ap,rc,ri,rc1,rc2,fcs,rc3,rc4,ap1)
       area100:=SELECT()
       SELECT 50
       USE total
	 IF LASTREC()=0
	   APPEND BLANK
	 ELSE
	   GO 1
	 ENDIF

	 REPLACE        iva      WITH v[6]
	 REPLACE        col_med  WITH v[7]
	 REPLACE        aso_cir  WITH v[8]
	 REPLACE        sind_medic  WITH v[9]
	 REPLACE        fuerte      WITH v[10]
	 REPLACE        multas      WITH v[11]
	 REPLACE        refrige     WITH v[12]

	 REPLACE        anticipo    WITH v1[5]
	 REPLACE        col_b_f     WITH v1[6]
	 REPLACE        aso_ped     WITH v1[7]
	 replace        sind_enfer  WITH v1[8]
	 replace        prest_comp  WITH v1[9]
	 REPLACE        retencion   WITH v1[10]
	 REPLACE        otros       WITH v1[11]

	 REPLACE        lactancia   WITH v2[6]
	 REPLACE        aso_aux_e   WITH v2[7]
	 REPLACE        sind_adm    WITH v2[8]
	 REPLACE        confe_s_ad  WITH v2[9]
	 REPLACE        fondo_soc   WITH v2[10]
	 REPLACE        pulperia    WITH v2[11]

	 REPLACE        prenatal    WITH v3[5]
	 REPLACE        col_enfer   WITH v3[6]

	 REPLACE        totalg      WITH v2[4]
	 REPLACE        totald      WITH v2[13]
	 REPLACE        totalp      WITH v2[14],;
			admin       WITH ap[1],;
			medico      WITH ap[2],;
			paramedico  WITH ap[3],;
			fonvAbbv    WITH rc[1],;
			fonvMbbv    WITH rc[2],;
			fonvPbbv    WITH rc[3],;
			fonvAfut    WITH ri[1],;
			fonvMfut    WITH ri[2],;
			fonvPfut    WITH ri[3],;
			fopebaA     WITH rc1[1],;
			fopebaM     WITH rc1[2],;
			fopebaP     WITH rc1[3],;
			fcpA        WITH rc2[1],;
			fcpM        WITH rc2[2],;
			fcpP        WITH rc2[3],;
			fcsmra      WITH fcs,;
			bbv_patA    WITH rc3[1],;
			bbv_patP    WITH rc3[3],;
			bbv_patM    WITH rc3[2],;
			fut_patA    WITH rc4[1],;
			fut_patP    WITH rc4[3],;
			fut_patM    WITH rc4[2],;
			admin1      WITH apo1[1],;
			para1       WITH apo1[3],;
			medico1     WITH apo1[2]
     SELECT(area100)
     //fopebaA es afp bbv de administrativos
     //fcpA   es afp futuro de    "
     //
RETURN

*****************************************************************************
*                     Dado el n£mero del mes devuelve su nombre             *
*****************************************************************************
FUNCTION mes(num)
LOCAL mm:={"ENERO","FEBRERO","MARZO","ABRIL","MAYO","JUNIO","JULIO","AGOSTO","SEPTIEMBRE","OCTUBRE","NOVIEMBRE","DICIEMBRE"}
RETURN mm[num]

*****************************************************************************
*                      Dado el nombre del mes devuelve el n£mero            *
*****************************************************************************
FUNCTION nu_mes(nom_mes)
LOCAL mm:={"ENERO","FEBRERO","MARZO","ABRIL","MAYO","JUNIO","JULIO","AGOSTO","SEPTIEMBRE","OCTUBRE","NOVIEMBRE","DICIEMBRE"},p
LOCAL no_mes:=0
   FOR p=1 TO 12
    IF ALLTRIM(nom_mes)==mm[p]
       no_mes=p
       EXIT
    ENDIF
   NEXT
RETURN no_mes

****************************************************************************
*                      INSERTA A UN ARRAY LOS DESCUENTOS                   *
****************************************************************************
FUNCTION cargar
LOCAL bb1:={}
      tt:=SELECT()
      SELECT 19
       USE impuesto INDEX impu_cod
      GO TOP
      DO WHILE !EOF()
       AADD(bb1,descripcio)
       SKIP
      ENDDO
      SELECT(tt)
RETURN bb1

*****************************************************************************
* Permite Cambiar el Mes de Transaccione, S¡ no Exite  crea uno nuevo       *
*****************************************************************************
FUNCTION preg_mes

LOCAL pant_1,lmeses:=mes(MONTH(DATE()))+"      ",director
LOCAL cop
LOCAL listad:={"baja.dbf",;
	      "hrs_eme.dbf",;
	      "incap.dbf",;
	      "iva.dbf",  ;
	      "nac_muer.dbf",;
	      "permiso.dbf",  ;
	      "personal.dbf",  ;
	      "presta.dbf", ;
	      "suplenci.dbf", ;
	      "total.dbf",;
	      "constant.dbf",;
	      "congelar.dbf",;
	      "acreedor.dbf",;
	      "temp_pre.dbf",;
	      "impuesto.dbf",;
	      "aumento.dbf",;
	      "acreedor.ntx",;
	      "suple_po.ntx", ;
	      "baja_ite.ntx", ;
	      "emp_ci.ntx",;
	      "extra_it.ntx",;
	      "hora_ite.ntx",;
	      "iva_item.ntx",;
	      "n_m_item.ntx",;
	      "perso_no.ntx",;
	      "perso_it.ntx",;
	      "presta_i.ntx",;
	      "suple_qu.ntx",;
	      "conge_it.ntx",;
	       "pnombre.ntx",;
	       "impu_cod.ntx",;
	      "dat_empl.dbf",;
	       "nitem.ntx",;
	       "nombre.ntx",;
	       "ci.ntx",;
	       "codigo.ntx",;
	       "eventual.ntx",;
	       "temp_eve.dbf",;
	       "enitem.ntx",;
	       "enombre.ntx"}

PUBLIC cop1,raiz,cerr_abi

    SET ESCAPE OFF
    area20:=SELECT()
/*    SELECT 18
    USE (dr+"bombom")
    GO TOP
    GO 1 */
    raiz=dr
    pant_1=OPEN_WINDOW(5,10,10,73,1,'',"W/B",5,.T.)
    SET CURSOR ON
    SET COLOR TO "W/B"
    @ 7,12 SAY "Transacciones Correspondientes al Mes de :" GET lmeses PICTURE "@!K" VALID lmeses!=SPACE(20) .AND. (AT(ALLTRIM(lmeses),"1,2,3,4,5,6,7,8,9,10,11,12")!=0 .OR. nu_mes(lmeses)!=0)
    @ 9,12 SAY "A¤o :" GET anio PICTURE"9999" VALID anio >0
    READ

    IF !LASTKEY()=27
	 IF AT(ALLTRIM(lmeses),"1,2,3,4,5,6,7,8,9,10,11,12")!=0
	    lmeses=mes(VAL(lmeses))
	 ELSE
	     READ
	 ENDIF
	 director=LEFT(lmeses,3)+"_"+ALLTRIM(STR(anio))
	 cop=raiz+"vacio\"
	 cop1=raiz+director+"\"
	 SELECT 10
	 use (dr+"meses") index (dr+"meses")
	 IF LASTREC()=0
	   prim:=.T.
	 ELSE
	   prim:=.F.
	 ENDIF
	 GO TOP
	 SEEK ALLTRIM(director)

	 IF !FOUND() // Creando un nuevo directorio de trabajo

	     APPEND BLANK
	     REPLACE mes WITH director,;
		     condicion WITH "A"
	     cerr_abi="A"

	     RUN ("MKDIR "+director)      // Crea directorios Historicos
	     FOR j=1 TO 40               // con el nombre de cada mes y a¤
	      COPY FILE (cop+listad[j]) TO (cop1+listad[j])
	     NEXT
	     SET DEFAULT TO (director)
	     SELECT 10
	     use (dr+"meses") index (dr+"meses")
	     IF !prim
	       copia_iva(ALLTRIM(lmeses),raiz,anio)  //copia iva del mes anterior
	       //copia_prestamo() //copia prestamo automatico
	       SELECT 22
	       USE  iva INDEX iva_item
	       REPLACE ALL formu101 WITH 0
	       REPLACE ALL nada WITH sald_depen

	       GO TOP 
	       DO WHILE !EOF()
			IF SUBSTR(nitem,8,1)= "E"
				DELETE
			END
			SKIP
	       ENDDO
		PACK
		CLOSE 22
	     ENDIF

	 ELSE //Revisar transacciones de anteriores meses
	    SET DEFAULT TO (director)
	    cerr_abi=condicion
	ENDIF
     ELSE
       lmeses="@"
     ENDIF
      CLOSE_WINDOW(pant_1)
      SELECT(area20)
      SET ESCAPE ON
RETURN lmeses

*****************************************************************************
* Permite Copiar el IVA correspondiente del anterior mes                    *
* y los datos correspondientes a los empleados                              *
*****************************************************************************
STATIC PROCEDURE copia_iva(dire,root,anio)
LOCAL val_ant:=0,aa_t:=0,mmes,cadena,cad1
      area50:=SELECT()

      aa_t=nu_mes(dire)
      IF aa_t=1
	 aa_t=13
	 --anio
      ENDIF
      mmes=mes(aa_t-1)
      cadena= root + LEFT(mmes,3)+"_"+ALLTRIM(STR(anio)) //anterior mes
      cad1  = root + LEFT(dire,3)+"_"+ALLTRIM(STR(YEAR(DATE()))) // mes actual

      SELECT 10
      use (dr+"meses") index (dr+"meses")
      SEEK LEFT(mmes,3)+"_"+ALLTRIM(STR(anio)) //anterior mes
      IF FOUND()
       COPY FILE (cadena+"\iva.dbf")      TO (cad1+"\iva.dbf")
       COPY FILE (cadena+"\iva_item.ntx") TO (cad1+"\iva_item.ntx")

       //Copia empleados de planta
       COPY FILE (cadena+"\dat_empl.dbf")      TO (cad1+"\dat_empl.dbf")
       COPY FILE (cadena+"\nitem.ntx")         TO (cad1+"\nitem.ntx")
       COPY FILE (cadena+"\nombre.ntx")        TO (cad1+"\nombre.ntx")
       COPY FILE (cadena+"\ci.ntx")            TO (cad1+"\ci.ntx")
       COPY FILE (cadena+"\codigo.ntx")        TO (cad1+"\codigo.ntx")
       COPY FILE (cadena+"\eventual.ntx")      TO (cad1+"\eventual.ntx")

       //Copia empleados eventuales
       COPY FILE (cadena+"\temp_eve.dbf")      TO (cad1+"\temp_eve.dbf")
       COPY FILE (cadena+"\enitem.ntx")         TO (cad1+"\enitem.ntx")
       COPY FILE (cadena+"\enombre.ntx")        TO (cad1+"\enombre.ntx")
       //copia congelados
       COPY FILE (cadena+"\congelar.dbf")        TO (cad1+"\congelar.dbf")
       COPY FILE (cadena+"\conge_it.ntx")        TO (cad1+"\conge_it.ntx")

       COPY FILE (cadena+"\nivel.dbf")        TO (cad1+"\nivel.dbf")
       COPY FILE (cadena+"\nivel_co.ntx")        TO (cad1+"\nivel_co.ntx")

      ENDIF

      SELECT(area50)

RETURN val_ant

FUNCTION auto
LOCAL pp12,jreg,jcon,jser,jserm,jadm,elec,creg,ccont,cser,cserm,cadm

       jreg:=jcon:=jser:=jserm:=jadm:=SPACE(50)
       creg :=ccont:=cser:=cserm:=cadm:= SPACE(30)
       area106:=SELECT()
       SELECT 100
       USE (dr+"firmas")
       IF LASTREC()>0
	    creg = cargo_reg
	    jreg = jefe_reg
	    ccon = cargo_cont
	    jcon = conta
	    cser = cargo_ser
	    jser = jefe_ser
	    cserm = cargo_med
	    jserm = jefe_serm
	    cadm  = cargo_adm
	    jadm = admi
       ENDIF
	      pp12=OPEN_WINDOW(7,5,19,76,1,' Autoridades ',"B/N+",5,.T.)
       SET COLOR TO "W/N+"
       SET CURSOR ON

       @  9,10 GET creg PICTURE "@!" COLOR "BG/N" VALID jreg!=SPACE(35)
       @  9,45 GET jreg PICTURE "@!S34" COLOR "BG/N" VALID jreg!=SPACE(50)
       @ 11,10 GET ccon PICTURE "@!" COLOR "BG/N" VALID jcon!=SPACE(35)
       @ 11,45 GET jcon PICTURE "@!S34" COLOR "BG/N" VALID jcon!=SPACE(50)
       @ 13,10 GET cser PICTURE "@!" COLOR "BG/N" VALID jser!=SPACE(35)
       @ 13,45 GET jser PICTURE "@!S34" COLOR "BG/N" VALID jser!=SPACE(50)
       @ 15,10 GET cserm PICTURE "@!" COLOR "BG/N" VALID jser!=SPACE(35)
       @ 15,45 GET jserm PICTURE "@!S34" COLOR "BG/N" VALID jser!=SPACE(50)
       @ 17,10 GET cadm  PICTURE "@!" COLOR "BG/N" VALID jser!=SPACE(35)
       @ 17,45 GET jadm PICTURE "@!S34" COLOR "BG/N" VALID jadm!=SPACE(50)


      /* @ 9, 10 SAY "JEFE REGIONAL DE PERSONAL   :" GET jreg PICTURE "@!S34" COLOR "BG/N" VALID jreg!=SPACE(54)
       @ 11,10 SAY "CONTADOR GENERAL            :" GET jcon PICTURE "@!S34" COLOR "BG/N" VALID jcon!=SPACE(54)
       @ 13,10 SAY "JEFE DE SERVICIOS GENERALES :" GET jser PICTURE "@!S34" COLOR "BG/N" VALID jser!=SPACE(54)
       @ 15,10 SAY "JEFE DE SERVICIOS DE SALUD  :" GET jserm PICTURE "@!S34" COLOR "BG/N" VALID jser!=SPACE(54)
       @ 17,10 SAY "ADMINISTRADOR REGIONAL      :" GET jadm PICTURE "@!S34" COLOR "BG/N" VALID jadm!=SPACE(54)
       */
       READ

       IF LASTKEY()!=27
	  elec=MULTIPLE({"Grabar","Salir sin Grabar"},2,21,23,.F.)
	  IF elec=1
	   SELECT 100
	   USE (dr+"firmas")
	   IF LASTREC()=0
	      APPEND BLANK
	   ENDIF

	   REPLACE jefe_reg  WITH jreg,;
		   conta     WITH jcon,;
		   jefe_ser  WITH jser,;
		   admi      WITH jadm,;
		   jefe_serm WITH jserm,;
		   cargo_reg WITH creg,;
		   cargo_cont WITH ccon,;
		   cargo_ser WITH cser,;
		   cargo_adm WITH cadm,;
		   cargo_med WITH cserm
	  ENDIF
       ENDIF
       CLOSE_WINDOW(pp12)
       SELECT(area106)
RETURN

*****************************************************************************
* Registra las firmas de las autoridades a cargo                            *
*****************************************************************************
FUNCTION quien_firma()
LOCAL vec_tot[1],fir[5],car[5]

ar:=SELECT()
SELECT 100
  USE (dr+"firmas")
  GO 1
      fir[1]=ALLTRIM(jefe_reg)
      fir[2]=ALLTRIM(conta)
      fir[3]=ALLTRIM(jefe_ser)
      fir[4]=ALLTRIM(admi)
      fir[5]=ALLTRIM(jefe_serm)
      car[1]=ALLTRIM(cargo_reg)
      car[2]=ALLTRIM(cargo_cont)
      car[3]=ALLTRIM(cargo_ser)
      car[4]=ALLTRIM(cargo_adm)
      car[5]=ALLTRIM(cargo_med)

IF !EVERYROW('               '+fir[1]+'                '+fir[2]+'       '+fir[3]+'          '+fir[5]+'           '+fir[4],vec_tot,.T.)
     FINISHREPORT()
     RETURN
ENDIF
vec_tot[1]=''
IF !EVERYROW('               '+car[1]+'                '+car[2]+'              '+car[3]+'          '+car[5]+'           '+car[4],vec_tot,.T.)
//IF !EVERYROW('               JEFE REGIONAL DE PERSONAL                 CONTADOR  REGIONAL         JEFE DE SERVICIOS GENERALES           JEFE DE SERVICIOS DE SALUD                    ADMINISTRADOR REGIONAL',vec_tot,.T.)
     FINISHREPORT()
     RETURN
ENDIF
SELECT(ar)
RETURN

*****************************************************************************
* Registra las firmas de las autoridades a cargo                            *
*****************************************************************************
FUNCTION jef_perso()
LOCAL vec_tot[1],fir[4]
SELECT 100
  USE (dr+"firmas")
  GO 1
      fir[1]=ALLTRIM(jefe_reg)
IF !EVERYROW('                                             '+fir[1],vec_tot,.T.)
     FINISHREPORT()
     RETURN
ENDIF

vec_tot[1]=''
IF !EVERYROW('                                            JEFE REGIONAL DE PERSONAL',vec_tot,.T.)
     FINISHREPORT()
     RETURN
ENDIF
RETURN

FUNCTION cambio_dol()
       SELECT 44
       USE constante
       GO 1
RETURN dolar

FUNCTION llen_acre
PUBLIC acre:={},acre_cod:={}

       area23:=SELECT()
       SELECT 48
       USE ACREEDOR INDEX Acreedor
       GO top
       SKIP
       DO WHILE !EOF()
	  AADD(acre,acreedor)
	  AADD(acre_cod,codigo)
	  SKIP
       ENDDO
       SELECT(area23)
RETURN  acre

*****************************************************************************
* Salida temporal al D.O.S.                                                *
*****************************************************************************
FUNCTION shell()
LOCAL pant
      SAVE SCREEN TO pant
      RUN CLS
      ?"Para retornar al menu Principal, escriba EXIT"
      RUN COMMAND
      SET DEFAULT TO (cop1)
      RESTORE SCREEN FROM pant
RETURN

*****************************************************************************
* Cerrar el mes de Transacciones                                           *
*****************************************************************************
FUNCTION cerrar()
LOCAL pant1,dire,lmeses:=mes(MONTH(DATE()))+"      ",anio:=YEAR(DATE()),elec

    area215:=SELECT()
    pant1=OPEN_WINDOW(10,15,19,73,1,'',"W/R",5,.T.)
    SET CURSOR ON
    SET COLOR TO "W/R"
    @ 12,17 SAY " ATENCION ! Esta opci¢n cerrar  todas las transacciones"
    @ 13,17 SAY " correspondientes al mes que seleccione."
    @ 14,17 SAY " El proceso ser  irreversible."
    @ 16,17 SAY " Ingrese el Nombre del Mes a Cerrar :" GET lmeses COLOR "N/W" PICTURE "@!K" VALID lmeses!=SPACE(20) .AND. (AT(ALLTRIM(lmeses),"1,2,3,4,5,6,7,8,9,10,11,12")!=0 .OR. nu_mes(lmeses)!=0)
    @ 17,17 SAY " A¤o :" GET anio COLOR "N/W" PICTURE"9999" VALID anio >0
    READ
     elec=MULTIPLE({"Aceptar","Cancelar"},2,21,23,.F.)
    IF LASTKEY()!=27  .AND. elec=1
      dire=LEFT(lmeses,3)+"_"+ALLTRIM(STR(anio))
      SELECT 10
      use (dr+"meses") index (dr+"meses")
      SEEK dire
      IF FOUND()
	REPLACE condicion WITH "C"
	cerr_abi="C"
	MENSAJE("Cerrando ....",12,17,'W/R+',.T.,.F.)
      ENDIF
    ENDIF
    CLOSE_WINDOW(pant1)
    SELECT(area215)
RETURN

*****************************************************************************
* Halla el Total Ganado de un Empleado                                     *
*****************************************************************************
FUNCTION total_ganado(nit)

     ar:=SELECT()
     SELECT 1
     use dat_empl index nitem,nombre,codigo,eventual,ci
     set order to 1
 SEEK nit
     IF FOUND()

       IF jor_trab=0.5
	  medio="1/2t"
       ELSE
	  medio="    "
       ENDIF

	 area100:=SELECT()
	 SELECT 32
	 USE personal  INDEX perso_it,perso_nom
         set order to 1
	 SEEK alltrim(nit)
	 IF FOUND()
	    dias=dias_trab
	 ELSE
	  dias=30
	 ENDIF

	 SELECT(area100)

	 IF anio_rec > 0 .OR. mes_rec > 0       // 2da.
	    fechita = halla_fecha(1->fech_ing,anio_rec,mes_rec,dia_rec)
	 ELSE
	   fechita  = 1->fech_ing
	 ENDIF

	 sue_mens=sueldo(nivel,jor_trab)
	//indice=ind_efec
	IF reincorp ="S"
	 indice = efectivo("B",fechita,sue_mens)  // Fecha de ingreso institucional
	ELSE
	 indice = efectivo("B",1->fech_ing,sue_mens)
	ENDIF

	 IF nit = "CBB-001" 
	    indice = 0   //gus
	 ENDIF

	 bon_antig= bono_antig("B",fechita)*jor_trab

	IF dias<30
	   sue_mens  = ROUND(ROUND(sue_mens/30,2)*dias,2)
	   bon_antig = ROUND(ROUND(bon_antig/30,2)*dias,2)
	   indice    = ROUND(ROUND(ind_efec/30,2)*dias,2)
       ENDIF
       conge := es_con(nit)   // ingresos congelados
	 IF conge[2]!=0;indice=conge[2];ENDIF
	 IF conge[1]!=0;bon_antig=conge[1];ENDIF

       por_prof = porce("B",fechita,CTOD("  /  /    "))        // porcentaje profesional
       por_irra = porce("R",fechita,CTOD("  /  /    "))

       dos_por   = escalafon(prof,sue_mens,nit)
       IF conge[4]=0;hrs_adic = hrs_adicion(codigo,prof,sue_mens+dos_por);ELSE;hrs_adic=conge[4];ENDIF     // parametro it para octubre        1994

      ***** em Congel.
      em1 = escalafo0(prof,sue_mens,nitem,0)   // esc. med congel. al 2002
      IF !EMPTY(fecp) .AND. fecp >= CTOD('01/01/2003')
       cat_profe = categ_prof(sue_mens,p_prof,codigo,prof,hrs_adic)
      ELSE
      *cat_profe = categ_prof(sue_mens+dos_por,p_prof,codigo,prof,hrs_adic)
       cat_profe = categ_prof(sue_mens+em1,p_prof,codigo,prof,hrs_adic)
      ENDIF
       dif_esc = escalafo0(prof,sue_mens,nitem,1)   // difer.escal. actual y congel.
      *****

       IF conge[3]=0;sub_irrad= sub_irradia(codigo,fechita,sue_mens);ELSE;sub_irrad=conge[3];ENDIF

     * emerg    = servicio_em(serv_emerg,sueldo(nivel,1.0)+dos_por,bon_antig,cat_profe)
      *emerg    = servicio_em(serv_emerg,sueldo(nivel,1.0)+em1,bon_antig,cat_profe)
      IF !EMPTY(feemer) .AND. feemer >= CTOD('01/01/2003')
       emerg    = servicio_em(serv_emerg,sueldo(nivel,1.0),0,0)
      ELSE
       emerg    = servicio_em(serv_emerg,sueldo(nivel,1.0)+em1,bon_antig,cat_profe)
      ENDIF

       //v_h      = valor_hora(sexo,prof,sue_mens+dos_por,bon_antig,cat_profe,dias)
      *v_h      = valor_hora(sexo,prof,sueldo(nivel,1.0)+dos_por,bon_antig,cat_profe,dias)
      *v_h      = valor_hora(sexo,prof,sue_mens+em1,bon_antig,cat_profe,dias)

       IF !EMPTY(fech_ing) .AND. fech_ing >= CTOD('01/01/2003')
        v_h  = valor_hora(sexo,prof,sue_mens,0,0,dias)
       ELSE
        v_h  = valor_hora(sexo,prof,sue_mens+em1,bon_antig,cat_profe,dias)
       ENDIF

       IF nitem = "CBB-156" .OR. nitem = "CBB-159"
          v_h  = valor_hora(sexo,prof,sue_mens+em1,bon_antig,0,dias)
       ENDIF

       hrs_extras= hora_extra(nitem,v_h)
       recarg    = recarg_noct(nitem,v_h)
       bon_nat   = bonos_mater(nitem)

       *IF p_prof >0
	   *bon_nat[4]=45*jor_trab
           bon_nat[4]=60*jor_trab
       *ELSE
	*  bon_nat[4]=0
       *ENDIF

*      TotalG    = sue_mens+bon_antig+indice+cat_profe+hrs_adic+sub_irrad+emerg+hrs_extras+recarg+bon_nat[1]+;
*		bon_nat[2]+bon_nat[3]+bon_nat[4]+dos_por

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

     ENDIF
     SELECT(ar)
RETURN totalg

****************************************************************************
* Reindexando los indices de las bases de datos                            *
****************************************************************************
FUNCTION mante()
LOCAL elec
      // Base de Datos constantes
      elec=MULTIPLE({"Aceptar","Cancelar"},2,21,23,.F.)
      IF elec=1
	pp:=MENSAJE("Procesando ....",12,17,'W/R+',.F.,.T.)
	SELECT 10
	USE (dr+"meses") INDEX (dr+"meses")
	REINDEX
	SELECT 11
	USE (dr+"sector")  INDEX (dr+"sector")
	REINDEX
	SELECT 12
	USE (dr+"cargo")    INDEX (dr+"cargo_co")
	REINDEX
	//SELECT 13
	//USE (dr+"nivel")    INDEX (dr+"nivel_co")
       // USE   nivel INDEX nivel_co
	REINDEX
	SELECT 14
	USE (dr+"bonos")    INDEX (dr+"bono_co")
	REINDEX
	SELECT 21
	USE (dr+"prestamo") INDEX (dr+"prest_it")
	REINDEX

	SELECT 38
	USE  (dr+"vacacion.dbf") INDEX (dr+"vaca_ite")
	REINDEX
	CLOSE ALL
	// Base de datos variables
	SELECT 1
	USE dat_empl INDEX nitem,nombre,codigo,eventual,ci,orden //personal
	REINDEX
	SELECT 2
	USE temp_eve INDEX enitem,enombre
	REINDEX
	SELECT 23
	USE  baja.dbf     INDEX       baja_ite.ntx
	REINDEX
	SELECT 16
	USE   hrs_eme.dbf INDEX  extra_it.ntx
	REINDEX
	SELECT 22
	USE       iva.dbf      INDEX  iva_item.ntx
	REINDEX
	SELECT 18
	USE  nac_muer.dbf INDEX n_m_item.ntx
	REINDEX
	SELECT 32
	USE       personal.dbf INDEX perso_no,perso_it
	REINDEX

	SELECT 91
	USE       presta.dbf   INDEX  presta_i
	REINDEX
	SELECT 17
	USE       suplencia.dbf INDEX  suple_po.ntx,suple_qu
	REINDEX
	SET ORDER TO 1
	
	SELECT 45
	USE       congelar INDEX conge_it
	REINDEX
	SELECT 48
	USE       acreedor INDEX acreedor
	REINDEX
	SELECT 19
	USE impuesto INDEX impu_cod
	REINDEX
	SELECT 13
	USE   nivel INDEX nivel_co
	REINDEX
	CLOSE_WINDOW(pp)
	CLOSE ALL
      ENDIF
RETURN

*****************************************************************************
* Seleci¢n de meses historicos                                             *
*****************************************************************************
FUNCTION histo()

LOCAL hh:={},se,pan
LOCAL mm:={"ENERO","FEBRERO","MARZO","ABRIL","MAYO","JUNIO","JULIO","AGOSTO","SEPTIEMBRE","OCTUBRE","NOVIEMBRE","DICIEMBRE"}
     tt:=SELECT()

      SELECT 10
      USE (dr+"meses") index (dr+"meses")
      GO TOP
      DO WHILE !EOF()
       IF condicion="C"
	 AADD(hh,mes)
       ENDIF
       SKIP
      ENDDO
      pan=OPEN_WINDOW(9,30,17,50,1,"Datos Hist¢ricos","B/N+",5,.T.)
      SET COLOR TO "R/W"
      se=ACHOICE(10,31,16,49,hh,.T.)

      IF se!=0
       //CAMBIAR DE MES
       SET DEFAULT TO (hh[se])

	FOR i:=1 TO 12
	 IF hh[se]=SUBSTR(mm[i],1,3)
	   EXIT
	 ENDIF
       NEXT
       mes     = mm[i]
       cerr_abi= "C"
       anio    = VAL(RIGHT(ALLTRIM(hh[se]),4))
      ENDIF
      CLOSE_WINDOW(pan)
      SELECT(tt)

RETURN

*****************************************************************************
*            Informaci¢n acerca del programa                                *
*****************************************************************************
FUNCTION about()
LOCAL pan,rr
      ar:=select()
      pan=OPEN_WINDOW(8,14,22,75,2,'',"W/W+",5,.T.)
      SET COLOR TO "N/W"
      @ 9,29 SAY sistema
      @ 11,16 SAY RIGHT(derechos,13)
      @ 10,29 SAY "Versi¢n 1.0"
      @ 12,29 SAY "DERECHOS RESERVADOS 1995"
      @ 15,29 SAY la_empresa+' C.P.S.'
      @ 16,29 SAY el_lugar
      @ 19,29 SAY "Desarrollado por: " + hecho
      INKEY(0)
      CLOSE_WINDOW(pan)
      select(ar)
RETURN

*****************************************************************************
*  Copias de seguridad                                                      *
*****************************************************************************
FUNCTION copia()
LOCAL pan,des:="A:",me:=mes(MONTH(DATE()))+"       ",an:=YEAR(DATE())
LOCAL direc,capacidad:=250000,listad:={}
      pan=OPEN_WINDOW(9,20,15,60,1,'Copia de Respaldo',"N/W+",5,.T.)
      SET COLOR TO "N/W"

      @ 11,22 SAY "Unidad destino :" GET des COLOR "N/BG"
      @ 12,22 SAY "Mes            :" GET me COLOR "N/BG"  PICTURE "@!k" VALID me!=SPACE(20) .AND. (AT(ALLTRIM(me),"1,2,3,4,5,6,7,8,9,10,11,12")!=0 .OR. nu_mes(me)!=0)
      @ 13,22 SAY "A¤o            :" GET an  COLOR "N/BG" PICTURE "9999" VALID anio >0
      READ

      IF LASTKEY()!=27
	IF DISKFREE(des)>capacidad
	 IF AT(ALLTRIM(me),"1,2,3,4,5,6,7,8,9,10,11,12")!=0
	     me=mes(VAL(me))
	 ENDIF

	 origen = dr+LEFT(me,3)+"_"+ALLTRIM(STR(an,4))+"\"
	 IF DIRCHANGE(LEFT(me,3)+"_"+ALLTRIM(STR(an,4)))==-3
	     MENSAJE("Error No existe ese mes ....",12,17,'W/R+',.T.,.F.)
	     CLOSE_WINDOW(pan)
	     SET DEFAULT TO (origen)
	     RETURN
	 ENDIF
	 RUN ("CD ..")
	 RUN ("CD "+LEFT(me,3)+"_"+ALLTRIM(STR(an,4)))
	 listad=DIRECTORY("*.*")

	 direc = des+LEFT(me,3)+"_"+ALLTRIM(STR(an,4))+".RES"

	 IF DIRCHANGE(direc)==-3
	    RUN ("CD "+des+"\")
	    DIRMAKE(direc)
	    //Copiar directorio
	    direc=direc+"\"

	    FOR j=1 TO LEN(listad)          // con el nombre de cada mes y a¤o
		COPY FILE (origen+listad[j,1]) TO (direc+listad[j,1])
	    NEXT

	 ELSE
	    MENSAJE("Error directorio ya existe ....",12,17,'W/R+',.T.,.F.)
	 ENDIF

	 ELSE
	    MENSAJE("Error Insuficiente espacio en el disco ....",12,17,'W/R+',.T.,.F.)
	 ENDIF
	DIRCHANGE( LEFT(dr,LEN(dr)-1) )
	SET DEFAULT TO (origen)
      ENDIF
      CLOSE_WINDOW(pan)
RETURN

FUNCTION rec_nom(nt)
LOCAL name:=SPACE(40)

	 ar:=SELECT()

	 SELECT 1
	 USE dat_empl INDEX nitem,nombre,codigo,eventual,ci

	 SET ORDER TO 1
	 SEEK ALLTRIM(nt)
	 IF FOUND()
	  name = nombre
	 ENDIF
	 SELECT(ar)
RETURN   name

FUNCTION copia_prestamo()
LOCAL cc,ss
	 are:=SELECT()
	 SELECT 91
	 USE    presta.dbf   INDEX  presta_i

	 IF LASTREC()<>0
	   MENSAJE("El Registro de Prestamos ya fue Procesado",12,17,'W/R+',.T.,.F.)
	   SELECT(are)
	   RETURN
	 ENDIF

	   ss= MULTI_OPCION('Registro Automatico de Prestamos, Desea continuar',{'Si','No'},2,18,.T.)
	IF ss = 1

	 SELECT 21
	 USE (dr+"prestamo") INDEX (dr+"prest_it")

	 SET ORDER TO 1
	 DBGOTOP()
	 DO WHILE !EOF()
	    cc = 21->cuota + 1
	    SELECT 91
	    APPEND BLANK
	    REPLACE 91->nitem WITH 21->nitem,;
		    91->monto WITH 21->monto_mes,;
		    91->cuota WITH (21->cuota+1)

	    SELECT 21
	    cc = 21->cuota + 1
	    IF cc < plazo
	       REPLACE 21->cuota WITH cc
	    ELSE
		SELECT 21
		MENSAJE("Es la Ultima cuota de "+rec_nom(LEFT(21->nitem,7))+".Su pr‚stamo ser  anulado",12,17,'W/R+',.T.,.F.)
		DELETE
	    ENDIF

	    SELECT 21
	    SKIP
	 END DO
	 MENSAJE("Procesando..",12,17,'W/R+',.T.,.F.)
	 SELECT 21
	 PACK
	ENDIF
	 SELECT(are)
RETURN

****************************************************************************
*  C lculo del escalafon Medico                                            *
****************************************************************************

FUNCTION escalafon(profe,sue,nt)
LOCAL esc:=0
	ar5:=SELECT()
	SELECT 1
	SET ORDER TO 1
	use dat_empl index nitem,nombre,codigo,eventual,ci
	SEEK(nt)

	// Correcion del escalafon 19/07/2001
	//IF (LEFT(profe,6)=='MEDICO' .OR. LEFT(profe,12)="FARMACEUTICA" .OR. LEFT(profe,10)="BIOQUIMICA" ).AND. (AT("ODONTOLOGO",profe)=0)
	   //esc = ROUND((2/100)*sue,2)
	esc = ROUND((esc_med/100)*sue,2)
	//ENDIF

	SELECT(ar5)
RETURN esc

****************************************************************************
*  C lculo del escalafon Medico Congelado al 2002                          *
****************************************************************************

FUNCTION escalafo0(profe,sue,nt,ind)
LOCAL esc:=0
	ar5:=SELECT()
	SELECT 1
	SET ORDER TO 1
	use dat_empl index nitem,nombre,codigo,eventual,ci
	SEEK(nt)

	// Correcion del escalafon 20/09/2004  G
        IF ind = 0
	   esc = ROUND((esc_me0/100)*sue,2)
        ELSE
           IF esc_med > 0
              esc = ROUND(((esc_med-esc_me0)/100)*sue,2)
           ENDIF
        ENDIF
	SELECT(ar5)
RETURN esc

** Calculo IVA a eventuales ******

FUNCTION iva_even(nt,sg,imp,des)
LOCAL res:=0
	 ar4:= SELECT()
	  SELECT 22
	  USE  iva INDEX iva_item
	  SEEK ALLTRIM(nt)

	  IF FOUND()
	       res = iva(nt,sg,des)
	  ELSE
	       res = ROUND(((imp_iva+imp_t)/100)*TotalG,0)
	  ENDIF
	 SELECT (ar4)

RETURN   res

**************************************************************************
*   Selecciona AFP
**************************************************************************
FUNCTION cual_afp(fn)
	LOCAL resp := .F.  	
	IF dat_empl->afp = "BBV"
		resp = .T.
	ENDIF

	// LOCAL dia,mes,par
	// dia = DAY(fn)
	// mes = MONTH(fn)
	// par = dia % 2
	// IF  par == 0 .OR. (dia = 1 .AND. mes=1) .OR. (dia = 3 .AND. mes=1) .OR. (dia = 5 .AND. mes=1)
	// 	resp = .T. //AFP BBV
	// END IF
	
RETURN resp




FUNCTION total_afp(nit,tg)
 LOCAL ax[2],an:=0,me:=0

     ax[1]:=0
     ax[2]:=0

     ar:=SELECT()
     SELECT 1
     use dat_empl index nitem,nombre,codigo,eventual,ci
     set order to 1

     SEEK nit

     IF FOUND()
	 an = YEAR(DATE()) - YEAR(fecha_nac)
	 //?MONTH(DATE()),MONTH(fecha_nac)
	 
	IF aporte_afp ="S" 
	if dat_empl->afp = "BBV"
	   if  an >= 65  .AND. (MONTH(fecha_nac) > MONTH(DATE())) .AND. (DAY(fecha_nac) > DAY(DATE()))
	   
	      ax[1] = ROUND(TG*primaRiesgo,2)
	   else
	      ax[1] = ROUND(TG*porc_afp,2)
	      ax[2] = 0
	   endif
	else
	   if an >= 65  .AND. (MONTH(fecha_nac) > MONTH(DATE())) .AND. (DAY(fecha_nac) > DAY(DATE()))
	   
	      ax[1] = ROUND(TG*primaRiesgo,2)
	   else
	       ax[1] = ROUND(TG*porc_afp,2)
	       ax[2] = 0
	   endif
       end if
      ELSE
	ax[1] = 0
	ax[2] = 0

      ENDIF            ^

      ENDIF
      SELECT(ar)
 RETURN ax

/* C lcula la antiguedad */

FUNCTION fecha_rec()
LOCAL aa:=0,mm:=0,dd:=0,lcalc:=DATE()

      pp81=OPEN_WINDOW(6,5,19,65,1,'C lculo de la Antiguedad',"B/N+",5,.T.)
      SET COLOR TO "W/N+"
      SET CURSOR ON
      @8,9 SAY "Fecha de C lculo :" GET lcalc COLOR "B/W" VALID   lcalc>CTOD("  /  /    ")
      @10,9  SAY "A¤os :" GET aa COLOR "B/W" PICTURE "#####"
      @12,9 SAY "Meses:" GET mm COLOR "B/W" PICTURE "##"
      @14,9 SAY "Dias :" GET dd COLOR "B/W" PICTURE "##"
      READ

      lcalc=Halla_fecha(lcalc,aa,mm,dd)

      @16,9 SAY "Fecha:"
      @16,16 SAY lcalc
      @18,8 SAY "Presione ESC para salir"
      WHILE INKEY()!=27
      ENDDO

      CLOSE_WINDOW(pp81)
RETURN

FUNCTION halla_fecha(ff,anio,mes,dia)
local v_meses:={31,28,31,30,31,30,31,31,30,31,30,31}
local an,me,di,new_fecha

      an = YEAR(ff) - anio
      me = MONTH(ff)- mes
      di = DAY(ff)  - dia

      IF me <= 0
         *IF anio = 0
         *   an = YEAR(ff) - 1
         *ELSE
         *   an = an - 1
         *ENDIF
         an = an - 1
	 me = me + 12
      ENDIF

      IF di <=0

	 IF me == 2 .AND. (an % 4) == 0
	   di = di + 29
	 ELSE
	  di = di + v_meses[me]
	 ENDIF

      ENDIF

      new_fecha=STR(di)+"/"+STR(me)+"/"+STR(an)
      new_fecha=CTOD(new_fecha)

RETURN new_fecha

FUNCTION anios(fe)
RETURN (YEAR(DATE()) -YEAR(fe))



FUNCTION viejo(fe)
	LOCAL v,res
	v = YEAR(DATE()) -YEAR(fe)
 	IF v > 65
		res =.T.
	ELSE
		IF v == 65  .AND. ( MONTH(fe) <= MONTH(DATE()) )
			res = .T.
		ELSE
			res = .F.
		ENDIF
	ENDIF
RETURN res
