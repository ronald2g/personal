*****************************************************************************
// MODULO            : Menu Principal 
// PARTE DEL SISTEMA : Menus
// PROPIETARIO       : Caja Petrolera de Salud
// VERSION           : 1.00
// CREACION          : 4  de Julio de 1994
// ULTIMA REVISION   : 4 de Enero de 1998
// AUTOR             : Richard Jim‚nez Velasco
*****************************************************************************

CLS
SET CENTURY ON

*****s* ESTRUCTURA/CARGO
*  DATOS
*    veop1 : Lista de Programas
*       01 : "SERVICIO DE ADMINISTRACION"
*       02 : "SERVICIOS DE SALUD"
*    veop2 : Lista de Subprogramas
*       01 : "APOYO ADMINISTRATIVO"
*       02 : "ATENCION HOSPITALARIA"
*       03 : "CONSULTA EXTERNA"
*    veop3 : Lista de Actividades
*       001 : "MEDICO"
*	002 : "PARAMEDICO"
*	003 : "ADMINISTRATIVO"
*  FUENTE
PUBLIC veop1:={"SERVICIO DE ADMINISTRACION","SERVICIOS DE SALUD"}
PUBLIC vere1:={"01","02"}
PUBLIC veop2:={"APOYO ADMINISTRATIVO","ATENCION HOSPITALARIA","CONSULTA EXTERNA"}
PUBLIC vere2:={"01","02","03"}
PUBLIC veop3:={"MEDICO","PARAMEDICO","ADMINISTRATIVO"}
PUBLIC vere3:={"001","002","003"}
******



PUBLIC mes_act:=MONTH(DATE()),dr
PUBLIC lmeses:=mes(MONTH(DATE()))+"      "
PUBLIC cad_fecha:=date()
PUBLIC la_empresa,el_lugar,hecho,temp:=0
PUBLIC ldolar_act:=0,ldolar_ant:=0

SET KEY 301 TO rein()
SET KEY 286 TO man_transa()
SET KEY 289 TO man_finan()
SET KEY 300 TO calculadora()
SET KEY -9 TO

*****************************************************************************
*       Variables de Instalaci¢n                                            *
*****************************************************************************
SELECT 18
USE  bombom
GO 1
dr         = "C:\PERSO.NAL\"
el_lugar   = "REGIONAL COCHABAMBA"
la_empresa = "CAJA PETROLERA DE SALUD"
sistema    = "SISTEMA DE PERSONAL"
derechos   = "DERECHOS RESERVADOS TECSYS S.R.L."
hecho      = "Richard Jimenez Velasco"


PARAMETERS inpapa
//SET KEY -1 TO write_help            && F2
//SET KEY 28 TO ON_LINE_HELP          && F1
IF inpapa = NIL
   inpapa = 'a'
ENDIF

titulo()
abrir_tablas()

*---------------------------------------------------------*
STARTMVGLIB()           && INICIO DE LIBRERIAS
*---------------------------------------------------------*
IF UPPER(inpapa) <> 'CLAVE'
   STARTMENU(.T.)
   SET KEY -9 TO SCREEN_LOCK()                && F10
ENDIF

*----------------------------------------------------------------------------

SET SOFTSEEK ON
//SET DELETED ON
SET CONFIRM ON
SET MESSAGE TO 24

?"paso 3"
******************************************************************************
* Menu Principal                                                            *
******************************************************************************

DECLARE menu_empl1[2]
menu_empl1[1]="De Planta,1,Transacciones de empleados de Planta,manejo_perman()"
menu_empl1[2]="Eventuales ,1,Transacciones de empleados Eventuales ,manejo_eventu()"

NEWMENU("menu_empl1")

DECLARE menu_transa[2]
menu_transa[1]="Administrativas,1,Registro de todas las transacciones administrativas,man_transa()"
menu_transa[2]="Financieras,1,Registro de todas las transacciones financieras,man_finan()"
NEWMENU("menu_transa")

DECLARE menu_plan[8]
menu_plan[1]="Planillas,2,Reportes de las Planillas,menu_report"
menu_plan[2]="Empleados,2,Reportes de Empleados,menu_empl"
menu_plan[3]="Descuentos,2,Reportes de Descuentos a Colegios, Asociaciones y Sindicatos,menu_des"
menu_plan[4]="Boleta de Pago,2,Reporte de la Papeleta de Pago a todo el Personal,menu_bol"
menu_plan[5]="~Res£men de Descuentos,1,Res£men de Descuentos Totales de la Planilla de Salarios,resu_tot()"
menu_plan[6]="~Distrib. Horas M‚dico,1,Reporte de Distribuci¢n de horas en Policlinico y Hospital,dist_med()"
menu_plan[7]="~FONVI BBV,1, Reporte de Descuentos AFP BBV,fonv_rep(.T.)"
menu_plan[8]="~FONVI AFP FUTURO,1, Reporte de Descuentos AFP BBV,fonv_rep(.F.)"
//menu_plan[7]="~Descuento AFP BBV,1, Reporte de Descuentos AFP BBV,afp_rep(.T.)"
//menu_plan[8]="~Descuento AFP FUTURO,1, Reporte de Descuentos AFP BBV,afp_rep(.F.)"
NEWMENU("menu_plan")

DECLARE menu_bol[2]
menu_bol[1]="Planta,1,Genera las boletas de Pago para todo el Personal,boleta()"
menu_bol[2]="Eventuales,1,Genera las boletas de pago para el Personal Eventual,bol_eve()"
NEWMENU("menu_bol")

DECLARE menu_des[12]
menu_des[1]="Colegio M‚dico,1,Detalle de descuentos al Colegio M‚dico,repo_desc(1)"
menu_des[2]="Colegio de Bioqu¡mica y Farmacia,1,Detalle de descuentos al Colegio de Bioqu¡mica,repo_desc(3)"
menu_des[3]="Colegio de Enfermeras,1,Detalle de descuentos al Colegio De Enfermeras,repo_desc(5)"
menu_des[4]="Colegio de Odont¢logos,1,Detalle de descuentos para Colegio de Odont¢logos,repo_desc(7)"
menu_des[5]="Aso. de Cirugia,1,Detalle de descuentos para la  Aso. de Cirugia,repo_desc(9)"
menu_des[6]="Aso. de Pediatria,1,Detalle de descuentos para la Aso. de Pediatria,repo_desc(11)"
menu_des[7]="Sind. Administrativo,1,Detalle de descuentos para el Sind. Administrativo,repo_desc(13)"
menu_des[8]="Sind. M‚dico,1,Detalle de descuentos para el Sindicato M‚dico,repo_desc(15)"
menu_des[9]="Sind. de Enfermeras,1,Detalle de descuentos para el Sindicato de Enfermeras,repo_desc(17)"
menu_des[10]="Conf. Sind. Administrativo,1,Detalle de descuentos para la Conf. Sind. Administrativo,repo_desc(19)"
menu_des[11]="Central Obrera Deptal.,1,Detalle de descuentos para la C.O.D.,repo_desc(23)"
menu_des[12]="Pro Deportes,1,Detalle de descuentos para Pro Deportes,repo_desc(21)"
NEWMENU("menu_des")

DECLARE menu_empl[2]
menu_empl[1]="Item,1,Reporte de empleados ordenado por item,Ite_emp()"
menu_empl[2]="Apellido,1,Reporte de empleados en orden alfab‚tico,ape_emp()"
NEWMENU("menu_empl")
//menu_plan[4]="Liquidaciones,1,Reporte de liquidaciones,liqui()"

DECLARE menu_report[6]
menu_report[1]="Planilla de Sueldos,1,Reporte de la Planilla de sueldos y salarios,planilla()"
menu_report[2]="Planilla I.V.A.,1,Reporte de la Planilla  IVA,list_iva()"
menu_report[3]="Planilla de Aportes,1,Reporte de la Planilla de Aportes laborales y patronales,lab_pat()"
menu_report[4]="Planilla de Eventuales,1,Reporte de la Planilla de Sueldos y Salarios al Personal Eventual,list_eventu()"
menu_report[5]="Planilla de Incapacidad,1,Reporte de la Planilla de Incapacidad,list_inca()"
menu_report[6]="Planilla de Suplencias,1,Planilla de Suplencias,list_suple()"
NEWMENU("menu_report")

DECLARE menu_est[9]

menu_est[1]="Trabajo,2,Consultas de dias, horas y atrasos del personal,menu_t"
menu_est[2]="Descuentos,2,Consultas sobre anticipo II, refrigerio, anticipos y Otros,menu_d"
menu_est[3]="~Pr‚stamos,1,Listado de Pr‚stamos Hipotecarios y Personales,prest_h_p()"
menu_est[4]=" Total Pr‚stamos,1,Listado de Pr‚stamos Hipotecarios y Personales Totales,prestamos()"
menu_est[5]="Saldo anterior  I.V.A.,1,Listado del Saldo anterior y Formulario 101   I.V.A.,saldo_iva()"
menu_est[6]="Indice de Efectividad,1,Reporte General para todo el Personal de su Indice de Efectividad,repo_indice()"
menu_est[7]="Vacaci¢n,1,Consulta sobre el control de vacaciones,vaca()"
menu_est[8]="Consulta Boleta de Pago,1,Consulta acerca de la boleta de pago de un empleado,ac_dir()"
menu_est[9]="Hist¢ricos,1, Consultas sobre anteriores meses, histo()"
NEWMENU("menu_est")

DECLARE menu_t[4]
menu_t[1]="Horas Extras y Nocturnas,1,Detalle de las horas Extras y Nocturnas del personal,hextra()"
menu_t[2]="D¡as y Horas Trabajadas,1,Detalle de dias y horas trabajadas,jor_trab()"
menu_t[3]="Retrasos,1,Detalle de retrasos del personal,retraso()"
menu_t[4]="Faltas,1,Detalle de Faltas del personal,faltas()"
NEWMENU("menu_t")

DECLARE menu_d[4]
menu_d[1]="Anticipos,1,Listado de Anticipos,anti()"
menu_d[2]="Anticipo II,1,Listado de empleados con Pulperia,pulp()"
menu_d[3]="Otros,1,Listado de empleados que tienen Cr‚ditos con Instituciones y/o Personas Particulares,otro_p()"
menu_d[4]="Refrigerio,1,Listado de empleados que tienen Refrigerios,refri()"
NEWMENU("menu_d")

DECLARE Tablas[9]
 Tablas[1] = "Tablas,2,Permite la Edici¢n de Tablas,Tabla_1"
 Tablas[2] = "Constantes,1,Permite la Configuraci¢n de Constantes,const()"
 Tablas[3] = "Responsables,1,Autoridades que dan el Vo.Bo.,auto()"
 Tablas[4] = "C lculo del Indice de Efectividad,1,Realiza el C lculo del Indice de Efectividad,indice_efectivo()"
 Tablas[5] = "C lculo de la Fecha Reconocida,1,Realiza el C lculo de la Fecha de Antiguedad Reconocida,fecha_rec()"
 Tablas[6] = "Cambiar Mes,1,Permite Cambiar el mes de las Transacciones,preg_ax()"
 Tablas[7] = "~CERRAR,1,Cancela todas las Transacciones de un determinado Mes,cerrar()"
 Tablas[8] = "Mantenimiento de la Base de Datos,1,Realiza el mantenimiento de la Base de Datos,mante()"
 Tablas[9] = "Copias de Seguridad,1,Realiza una copia de seguridad de la base de datos,copia()"
NEWMENU("Tablas")

DECLARE Tabla_1[8]
 Tabla_1[1] = 'Departamentos,1, Permite la Edici¢n de la Tabla de Departamentos, Entrada_22( 1 )  '
 Tabla_1[2] = 'Cargos      ,1, Permite la Edici¢n de la Tabla de Cargos      , Entrada_22( 2 )  '
 Tabla_1[3] = 'Niveles     ,1, Permite la Edici¢n de la Tabla de Niveles     , Entrada_22( 3 )  '
 Tabla_1[4] = 'Bono de Antiguedad,1,Permite la edici¢n de la Tabla del Bono de Antiguedad,antig()'   //Entrada_33( 2 )  '
 Tabla_1[5] = 'Bono de Radiaci¢n,1,Permite la edici¢n de la Tabla del Bono de Radiaci¢n, Entrada_22( 6 )  '   //Entrada_33( 2 )  '
 Tabla_1[6] = 'Descuentos ,1, Permite la Edici¢n de Descuentos a Colegios Sindicatos etc, Entra_44( 2 ) '
 Tabla_1[7] = "Lista de Acreedores,1,Permite la Edici¢n de Acreedores,Entrada_22(7)"
 Tabla_1[8] = "Bono Profesional,1,Bono Profesional,Entrada_22(8)"

NEWMENU("Tabla_1")


DECLARE salidaa[4]
salidaa[1]="Salir del Sistema,1, Salir del sistema al menu principal,rein()"
salidaa[2]="Calculadora,1,Calculadora,calculadora()"
salidaa[3]="Salir y Regresar,1,Sale al Sistema Operativo temporalmente,shell()"
salidaa[4]="?,1,about()"
NEWMENU("salidaa")

menu:={"00,Empleados,menu_empl1","14,Transacciones,menu_transa","33,Reportes,menu_plan","46,Consultas,menu_est","60,Configurar,Tablas","73,Salir,salidaa"}
IF UPPER(inpapa)='CLAVE'
     CLEAR
     MEN_PASS("menu")
     CLEAR
     QUIT
ENDIF

MENUDT(@menu,'Î',"B/BG+",.f.,'presentacion()',5)

******************************************************************************
* Presentaci¢n del Sistema                                                   *
******************************************************************************
FUNCTION presentacion
PUBLIC mes
	 MESSBAR_ON('<ALT-X> Salir','N/W','R/W')
	 SILUETA (0,0,4,78,24,79,"GR+/R+",1)
	 OPEN_WINDOW(0,0,4,78,1,'',"B/W",1,.F.)
	 //SHOWTIME(2,70,.F.,"B/W")
	 @ 1,27 SAY sistema    COLOR "B/W"
	 @ 2,25 SAY la_empresa COLOR "B/W"
	 @ 3,21 SAY derechos  COLOR "B/W"
	 @ 3,68 SAY  DATE() COLOR "R/W"
	 mes=preg_mes()
	 abre_arch()
RETURN ''

******************************************************************************
* Salida del Sistema de Personal
******************************************************************************
FUNCTION rein
	 salir = MULTI_OPCION('Desea salir del sistema, y retornar al menu principal',{'Si','No'},2,18,.T.)
	 IF salir=1
	    SETCOLOR("W/N")
	    CLEAR
	    CLOSE ALL
	    //SHOWTIME()
	    QUIT
	 ENDIF
RETURN ''

*****************************************************************************
*   recupera constantes                                       *
*****************************************************************************
FUNCTION abre_arch()
      SELECT 44
      USE constant
       GO 1
		sal_minimo = minimo
		imp_iva    = iva
		lab_fonvi  = fonvi_l
		pat_fonvi  = fonvi_p
		afp_bbv_   = fopeba_l
		afp_futuro = fopeba_p
		pat_cps    = cps_p
		ldol       = dolar
		imp_t      = imp_tran
		pat_bbv    = bbv_pat
		pat_fut    = fut_pat
		incre      = incremento
		sal_min_iva = min_iva
                porc_afp = afp_bbv_ /100
		tmayor65  = mayor65/100
		//primaRiesgo = (afp_bbv_ - mayor65)/100
		
		cot_mensual      = 10.0 /100
		pri_riesgo       = 1.71 /100
		comision         = 0.5  /100 
		aporte_solidario = 0.5  /100
		
		desc_afp_a      = cot_mensual + pri_riesgo + comision + aporte_solidario
	        desc_afp_b      = cot_mensual + comision + aporte_solidario
	        desc_afp_c      = pri_riesgo + comision + aporte_solidario
	        desc_afp_d      = comision + aporte_solidario
	        

	        
      SELECT 1
       USE dat_empl INDEX nitem,nombre,codigo,eventual,ci //personal
      SELECT 2
       USE temp_eve INDEX enitem,enombre

RETURN

*****************************************************************************
* Selecci¢n del Mes y A¤o para las distintas tareas                         *
*****************************************************************************
FUNCTION pre_pla(cadena,par)
PUBLIC pant_1,directorio,hoy[1]

  ini_var()
  lmeses=mes
  pant_1=OPEN_WINDOW(7,10,15,60,1,'',"B/N",5,.T.)
  SET CURSOR ON
  SET COLOR TO "W/N"
  @ 9,12 SAY cadena
  @ 10,12 SAY "Correspondiente al Mes de :" GET lmeses PICTURE "@!k" VALID lmeses!=SPACE(20) .AND. (AT(ALLTRIM(lmeses),"1,2,3,4,5,6,7,8,9,10,11,12")!=0 .OR. nu_mes(lmeses)!=0)
  @ 12,12 SAY "A¤o :" GET anio PICTURE "9999" VALID anio >0
  @ 14,12 SAY "Fecha de extensi¢n :" GET cad_fecha VALID cad_fecha>CTOD("  /  /    ")
  READ
  IF !LASTKEY()=27
       IF AT(ALLTRIM(lmeses),"1,2,3,4,5,6,7,8,9,10,11,12")!=0
	  lmeses=mes(VAL(lmeses))
       ENDIF
       directorio=LEFT(ALLTRIM(lmeses),3)+"_"+ALLTRIM(STR(anio))
       SELECT 10
       USE (dr+"meses") INDEX (dr+"meses")
       SEEK directorio
       IF FOUND()
	 SET DEFAULT TO (directorio)
       ELSE
	  CLOSE_WINDOW(pant_1)
	  MENSAJE("No existen Datos para ese MES",12,17,'W/R+',.T.,.F.)
	  RETURN
       ENDIF
  ELSE
     CLOSE_WINDOW(pant_1)
     RETURN
  ENDIF
  CLOSE_WINDOW(pant_1)
RETURN

*****************************************************************************
*                        Consulta el mes                                    *
*****************************************************************************
FUNCTION preg_ax()
  mes=preg_mes()
  abre_arch()
RETURN

*****************************************************************************
*  INTRODUCE SEGURIDAD A UN ARCHIVO                                         *
*****************************************************************************
FUNCTION caract(nombre)
	 LOCAL cont:= 0, cadena := ''
	 #INCLUDE "fileio.ch"
	 #DEFINE   f_block  1

	 tam_buffer := SPACE(f_block)
	 entrada := FOPEN(nombre,FO_READ)
	 acabar := .F.

	 DO WHILE !acabar
	    nbytes_read = FREAD(entrada,@tam_buffer,f_block)
	    IF asc(tam_buffer) = 13
	       cont = cont + 1
	    ENDIF

	 DO CASE
	    CASE cont = 2 .AND. ASC(tam_buffer) <> 13
		 cadena = cadena + tam_buffer
	    CASE cont = 3
		 acabar = .t.
	 ENDCASE
	 ENDDO

	 FCLOSE(entrada)
RETURN cadena

****f* PLANILLA/directorio
*  FUNCION
*    Selecciona el directorio del mes para procesar datos [ meses.dbf ]
*  SINOPSIS
FUNCTION prepla(cadena,par)
*  FUENTE	
PUBLIC pant_1,directorio,hoy[1]
  ini_var()
  lmeses=mes
  pant_1=OPEN_WINDOW(7,10,20,60,1,'',"B/N",5,.T.)
  SET CURSOR ON
  SET COLOR TO "W/N"
  @ 9,12 SAY cadena
  @ 10,12 SAY "Correspondiente al Mes de :" GET lmeses PICTURE "@!k" VALID lmeses!=SPACE(20) .AND. (AT(ALLTRIM(lmeses),"1,2,3,4,5,6,7,8,9,10,11,12")!=0 .OR. nu_mes(lmeses)!=0)
  @ 12,12 SAY "A¤o :" GET anio PICTURE "9999" VALID anio >0
  @ 14,12 SAY "Fecha de extensi¢n :" GET cad_fecha VALID cad_fecha>CTOD("  /  /    ")
  @ 16,12 SAY "Dolar Actual :" GET ldolar_act PICTURE "999.99"
  @ 18,12 SAY "Dolar Anterior Mes :" GET ldolar_ant PICTURE "999.99"

  READ
  IF !LASTKEY()=27
       IF AT(ALLTRIM(lmeses),"1,2,3,4,5,6,7,8,9,10,11,12")!=0
	  lmeses=mes(VAL(lmeses))
       ENDIF
       directorio=LEFT(ALLTRIM(lmeses),3)+"_"+ALLTRIM(STR(anio))
       SELECT 10
       USE (dr+"meses") INDEX (dr+"meses")
       SEEK directorio
       IF FOUND()
	 SET DEFAULT TO (directorio)
       ELSE
	  CLOSE_WINDOW(pant_1)
	  MENSAJE("No existen Datos para ese MES",12,17,'W/R+',.T.,.F.)
	  RETURN
       ENDIF
  ELSE
     CLOSE_WINDOW(pant_1)
     RETURN
  ENDIF
  CLOSE_WINDOW(pant_1)
RETURN
******
