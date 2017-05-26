**************************************************************************
* modulo que hace las entradas de las tablas de planillas que tengan DOS columnas
* fecha de creaci¢n :                 4 de agosto de 1994
* responzable       :                 Fernando Valdivia L.
*************************************************************************

*****************************************************************************
* esta funcion hace la llamada para el TBrowse()
*****************************************************************************
FUNCTION Entrada_22( accion )
   LOCAL   ctaven,venta_total,pantalla1,titulo,cod,detal:={"   ","                             ",0}
   PRIVATE fil1,fil2,col1,col2,tabla
   PRIVATE area,indi,namecampo1,namecampo2,titucampo1,titucampo2,pictcampo1,pictcampo2

   
   DO CASE
      CASE  accion = 1

            area        := 11
            indi        := 1
            titucampo1  := "CODIGO"                     && obtengo el titulo del campo1
            titucampo2  := "DETALLE"                    && obtengo el titulo del campo2
            pictcampo1  := "!!!"                        && obtengo el picture del campo1
            pictcampo2  := "@S50!"                      && obtengo el picture del campo2
            tabla       := "DEPARTAMENTOS"

      CASE  accion = 2
            area        := 12
            indi        := 1
            titucampo1  := "CODIGO"                     && obtengo el titulo del campo1
            titucampo2  := "DETALLE"                    && obtengo el titulo del campo2
            pictcampo1  := "!!!"                        && obtengo el picture del campo1
            pictcampo2  := "@S50!"                      && obtengo el picture del campo2
            tabla       := "CARGOS"

      CASE  accion = 3
            SELECT 13
            USE nivel INDEX nivel_co
            area        := 13
            indi        := 1
            titucampo1  := "CODIGO"                     && obtengo el titulo del campo1
            titucampo2  := "SUELDO"                     && obtengo el titulo del campo2
            pictcampo1  := "!!!"                         && obtengo el picture del campo1
            pictcampo2  := "999,999.99"                 && obtengo el picture del campo2
            tabla       := "NIVELES"

      CASE  accion = 4
            area        := 20
            indi        := 1
            titucampo1  := "CODIGO"                     && obtengo el titulo del campo1
            titucampo2  := "PORCENTAJE"                 && obtengo el titulo del campo2
            pictcampo1  := "!"                          && obtengo el picture del campo1
            pictcampo2  := "99"                        && obtengo el picture del campo2
            tabla       := "PROFESIONES"

      CASE accion = 5
            SELECT 42
            USE (dr+"temp_bon")
            area        := 42
            indi        := 1
            titucampo1  := "RANGO (A§os)"                     && obtengo el titulo del campo1
            titucampo2  := "PORCENTAJE (%)"                 && obtengo el titulo del campo2
            pictcampo1  := "!!!!!!!"                     && obtengo el picture del campo1
            pictcampo2  := "99.99"                        && obtengo el picture del campo2
            tabla       := "Bono de Antiguedad"

      CASE accion = 6
             SELECT 43
             USE (dr+"temp_rad")
            area        := 43
            indi        := 1
            titucampo1  := "RANGO (A§os)"                     && obtengo el titulo del campo1
            titucampo2  := "PORCENTAJE (%)"                 && obtengo el titulo del campo2
            pictcampo1  := "!!!!!!!!!!"                     && obtengo el picture del campo1
            pictcampo2  := "99.99"                        && obtengo el picture del campo2
            tabla       := "Bono de Radiaci¢n"

      CASE  accion = 7
            SELECT 48
            //USE (dr+"ACREEDOR") INDEX (dr+"acreedor")
            USE acreedor INDEX acreedor
            area        := 48
            indi        := 1
            titucampo1  := "CODIGO"                     && obtengo el titulo del campo1
            titucampo2  := "ACREEDOR"                    && obtengo el titulo del campo2
            pictcampo1  := "!!"                        && obtengo el picture del campo1
            pictcampo2  := "@S30!"                      && obtengo el picture del campo2
            tabla       := "ACREEDORES"

   ENDCASE

   SET SCOREBOARD OFF
   venta_total = SAVES( 0,0,24,79 )
   //CLS
   fil1 := 4
   fil2 := 19
   col1 := 6  //10
   col2 := 73 //63
   titulo      = 'EDICION TABLA DE ' + tabla
   ctaven      = OPEN_WINDOW(fil1-1,col1-1,fil2+1,col2+1,'€€€ﬁﬂﬂﬂ› ',titulo,'R/B,N/BG',5,.T.)
   pantalla1   = OPEN_WINDOW(fil2 + 2 ,col1 - 1,fil2 + 4,col2 + 1,1,'',"R/BG",2,.T.)
   @ fil2+3,45 SAY 'ENTER = Seleccionar'    COLOR "GR+/WR+"

  // Abrir_Tablas()                             && abre las tablas DBFs

   SELECT(area)                               && pongo el area de trabajo par actual
   SET ORDER TO indi                          && activo el indice adecuado
   GO TOP
   detal := MyBrowse_22( fil1,col1,fil2-1,col2 )

   CLOSE_WINDOW( pantalla1 )
   CLOSE_WINDOW( ctaven )
   CLOSE_WINDOW( venta_total )
   //DBCLOSEALL()
RETURN detal
*------------------------------------------------------------------------

*****************************************************************************
*  hace todo el TBrowse
***************************************************************************

STATIC PROCEDURE MyBrowse_22(nTop, nLeft, nBottom, nRight)
  #include "Common.ch"
  #include "Inkey.ch"
  #include "Setcurs.ch"
  #include "Error.ch"
  #define APP_MODE_ON( b )      ( b:cargo := TRUE  )
  #define APP_MODE_OFF( b )     ( b:cargo := FALSE )
  #define APP_MODE_ACTIVE( b )  ( b:cargo )
  #define MY_HEADSEP      "Õ—Õ"
  #define MY_COLSEP       " ≥ "

   LOCAL    oBrowse                          // The TBrowse object
   LOCAL    nKey,cod                         // Keystroke
   LOCAL    detal:={"   ","                             ",0}
   LOCAL    Salir := TRUE                    // Loop control
   PRIVATE  foot1[3],Num_Reg:=0

*****************************************************************************
* inicializacion de  mensajes de los campos para las cuentas
*****************************************************************************
    foot1[1]  = "Codigo de la Tabla        "
    foot1[2]  = "Valor que tomara el Codigo"
*****************************************************************************
*****************************************************************************

   oBrowse           := AumentaColBrowse( nTop, nLeft, nBottom, nRight )
   APP_MODE_OFF( oBrowse )
   oBrowse:skipBlock := { |x| Skipper( x, oBrowse ) }
   oBrowse:headSep   := MY_HEADSEP
   oBrowse:colSep    := MY_COLSEP
   SETCURSOR(0)
   oBrowse:forceStable()
   mensaje_22(oBrowse:ColPos())

   DO WHILE Salir

      oBrowse:forceStable()

      IF ( oBrowse:hitBottom .and. !APP_MODE_ACTIVE( oBrowse ) )
         APP_MODE_ON( oBrowse )
         nKey := K_DOWN
       ELSE
         IF ( oBrowse:hitTop .or. oBrowse:hitBottom )
            TONE( 125, 0 )
         ENDIF

         oBrowse:refreshCurrent():forceStable()
         nKey := inkey( 0 )
      ENDIF

      DO CASE
         CASE  nKey == K_ENTER
               detal[1]:=ALLTRIM(Fieldget(1))
               detal[2]:=ALLTRIM(FieldGet(2))
               detal[3]:=RECNO()
               Salir := .F.
         CASE nkey ==K_ESC
               salir:=.F.
         OTHERWISE
               ApplyKey_3(oBrowse, nKey)
      ENDCASE

   ENDDO

   MESSBAR_OFF()
   SETCURSOR( 0 )
RETURN detal
*---------------------------------------------------------------------------

STATIC FUNCTION mensaje_22(Ind)
***    Despliega al pie un mensaje por cada campo que se edita

        IF Ind < 3 .AND. Ind > 0
           @ fil2,col1 + 10 SAY foot1[Ind]  COLOR 'GR+/B'
        ENDIF
RETURN ' '
*----------------------------------------------------------------------------

****************************************************************************
*     Ejecuta operaciones segun la tecla que se presiono
***************************************************************************

STATIC PROCEDURE ApplyKey_3( oBrowse, nKey )

  DO CASE

   CASE nKey == K_DEL
        IF EMPTY( FIELDGET( 1 ) ) = .F.
           Borrar_Reg(oBrowse,RECNO())
        ENDIF

   CASE nKey == K_DOWN
        IF RECNO() == LASTREC() + 1
           oBrowse:panHome()
        ELSE
           oBrowse:down()
        ENDIF

   CASE nKey == K_PGDN
        oBrowse:pageDown()

   CASE nKey == K_CTRL_PGDN
        oBrowse:goBottom()
        APP_MODE_OFF( oBrowse )

   CASE nKey == K_UP
        oBrowse:up()
        if APP_MODE_ACTIVE( oBrowse )
           APP_MODE_OFF( oBrowse )
           oBrowse:refreshAll()
        endif

   CASE nKey == K_PGUP
        oBrowse:pageUp()
        if APP_MODE_ACTIVE( oBrowse )
           APP_MODE_OFF( oBrowse )
           oBrowse:refreshAll()
        endif

   CASE nKey == K_CTRL_PGUP
        oBrowse:goTop()
        APP_MODE_OFF( oBrowse )

   CASE nKey == K_RIGHT
        oBrowse:right()

   CASE nKey == K_LEFT
        oBrowse:left()

   CASE nKey == K_HOME
        oBrowse:home()

   CASE nKey == K_END
        oBrowse:end()

   CASE nKey == K_CTRL_LEFT
        oBrowse:panLeft()

   CASE nKey == K_CTRL_RIGHT
        oBrowse:panRight()

   CASE nKey == K_CTRL_HOME
        oBrowse:panHome()

   CASE nKey == K_CTRL_END
        oBrowse:panEnd()
   CASE (nKey >= 48 .AND. nKey <= 122) .OR. nKey == K_RETURN
        entrada_3(oBrowse,nKey,.F.)             && lee datos por teclado
        oBrowse:ForceStable()

   OTHERWISE
        TONE(1000,0)
  ENDCASE
  mensaje_22(oBrowse:ColPos())
RETURN ' '
*----------------------------------------------------------------------------

****************************************************************************
* Permite la entrada de los datos para todas las columnas
***************************************************************************

STATIC FUNCTION entrada_3( oBrowse,nKey,inserto )
LOCAL  cod := SPACE(15)

 DO CASE
    CASE oBrowse:colPos() == 1
         IF nkey <> K_RETURN
            KEYBOARD chr( nKey )
         ENDIF

         SETCURSOR(1)
         cod := FIELDGET( oBrowse:Colpos() )
         @ ROW(),COL()  GET cod    COLOR "GR+/W" PICTURE pictcampo1
         READ

         IF LASTKEY() <> K_ESC  .AND.  cod <> FIELDGET( oBrowse:Colpos() )
            //Rellena_Caracter(@cod,'0',LEN(pictcampo1))
            //Rellena_Caracter(@cod,' ',LEN(pictcampo1))
            IF No_Existe(oBrowse,cod,RECNO()) = .T.
               Inserta_Reg(oBrowse,cod)
             ENDIF
         ENDIF

         SETCURSOR(0)

    CASE oBrowse:colPos() == 2
         IF nkey <> K_RETURN
            KEYBOARD chr( nKey )
         ENDIF
         DoGet( oBrowse )

 ENDCASE
RETURN ' '
*---------------------------------------------------------------------

***************************************************************************
* HACE EL GET DE LA ENTRADA DE DATOS
**************************************************************************

FUNCTION DoGet( oBrowse )
   LOCAL oCol
   LOCAL GetList
   LOCAL nKey
   LOCAL nLen
   LOCAL lAppEND
   LOCAL bSavIns
   LOCAL nSavRecNo := RECNO()
   LOCAL xNewKey
   LOCAL xSavKey

   IF EOF()
      lAppEND := APP_MODE_ON( oBrowse )
   else
      lAppEND := APP_MODE_ACTIVE( oBrowse )
   ENDIF

   oBrowse:forceStable()

   IF  RECNO() == LASTREC()+1
       KEYBOARD CHR(K_UP)
       RETURN
   ENDIF

   IF ( lAppend .and. ( RECNO() == LASTREC() + 1 ) )
      DBAPPEND()
   ENDIF


   xSavKey := IIF( EMPTY( INDEXKEY() ), NIL, &( INDEXKEY() ) )

   oCol := oBrowse:getColumn( oBrowse:colPos )
   nLen := oBrowse:colWidth( oBrowse:colPos )
   GetList := { getnew( row(), col(),     ;
                        oCol:block,       ;
                        oCol:heading,     ;
                        oCol:picture,     ;
                        oBrowse:colorSpec ) }
   bSavIns := setkey( K_INS, { || InsToggle() } )
   setcursor( iIF( ReadInsert(), SC_INSERT, SC_NORMAL ) )

   READ
   SETCURSOR( SC_NONE )
   SETKEY( K_INS, bSavIns )

   APP_MODE_OFF( oBrowse )
   xNewKey := IF( empty( indexkey() ), NIL, &( indexkey() ) )
   oBrowse:inValidate()
   oBrowse:refreshAll():forceStable()

   IF !( xNewKey == xSavKey ) .or. ( lAppEND .and. xNewKey != NIL )
      oBrowse:refreshAll():forceStable()
      while &( indexkey() ) > xNewKey .and. !oBrowse:hitTop()
         oBrowse:up():forceStable()
      ENDDO
   ENDIF

   nKey := lastkey()
   IF nKey == K_UP   .or. nKey == K_DOWN .or. ;
      nKey == K_PGUP .or. nKey == K_PGDN
      keyboard( chr( nKey ) )
   ENDIF

RETURN
*-----------------------------------------------------------------

****************************************************************************
*   Aumenta Columnas al TBrowse()
****************************************************************************
STATIC FUNCTION AumentaColBrowse( nTop, nLeft, nBottom, nRight )
   LOCAL oBrowse,n,oColumn

   oBrowse := TBrowseDB( nTop, nLeft, nBottom, nRight )
   oBrowse:colorSpec := "BG+/B,GR+/BG,W+/B,N/BG,R/G,BG+/BG,R/W,W+/R"

    n := 1
    namecampo1 := FIELDNAME( n )               && obtengo el nombre del campo1
    oColumn    := TBColumnNew( titucampo1,FieldWBlock( field( n ), select() ) )
    oBrowse:addColumn( oColumn )
    oColumn:picture   = pictcampo1
    oColumn:defColor  = { 3,4 }

    n := 2
    namecampo2  := FIELDNAME( n )               && obtengo el nombre del campo2
    oColumn     := TBColumnNew( titucampo2,FieldWBlock( field( n ), select() ) )
    oBrowse:addColumn( oColumn )
    oColumn:picture    := pictcampo2
    oColumn:defColor   := { 3,7 }

RETURN oBrowse
*---------------------------------------------------------------------

***************************************************************************
***    borra un registro del archivo
**************************************************************************

FUNCTION Borrar_Reg(oBrowse,reg_nro)
LOCAL verifica := 0,opciones[2],panta,nreg

verifica = MULTI_OPCION("Esta Seguro de Borrar Registro...!",{'No','Si'},2,11,.T.)

DO CASE
 CASE verifica == 1
      RETURN .F.

 CASE verifica == 2
      panta := MENSAJE('Borrando Registro....  Espere Por Favor ',10,17,'W*/R+',.F.,.T.)
      nreg  := RECNO()
      DELETE
      PACK
      GO nreg
      CLOSE_WINDOW(panta)
      oBrowse:ForceStable():refreshAll()
      RETURN .T.
 ENDCASE

RETURN .F.
*-------------------------------------------------------------------------

*************************************************************************
*   programa donde se encontraran todas las funciones comunes
*************************************************************************

FUNCTION Skipper( nSkip, oBrowse )

  LOCAL lAppEND := APP_MODE_ACTIVE( oBrowse )
  LOCAL i := 0

   DO CASE
    CASE ( nSkip == 0 .or. lastrec() == 0 )
      dbSkip( 0 )
    CASE ( nSkip > 0 .and. !eof() )
      while ( i < nSkip )           // Skip Foward
         dbskip( 1 )
         IF eof()
            iIF( lAppEND, i++, dbskip( -1 ) )
            exit
         ENDIF
         i++
      ENDDO
   CASE ( nSkip < 0 )
      while ( i > nSkip )           // Skip backward
         dbskip( -1 )
         IF bof()
            exit
         ENDIF
         i--
      ENDDO
   ENDCASE
RETURN i
*---------------------------------------------------------------------------

FUNCTION InsToggle()
***     pone el modo del cursor

   IF readinsert()
      readinsert( .F. )
      setcursor( 1 )
   else
      readinsert( .T. )
      setcursor( 2 )
   ENDIF
RETURN
*---------------------------------------------------------------------------
***********************************************************************
* inserta un registro
***********************************************************************

FUNCTION Inserta_Reg(oBrowse,cod)

   LOCAL nKey
   LOCAL lAppEND
   LOCAL bSavIns
   LOCAL nSavRecNo := recno()
   LOCAL xNewKey
   LOCAL xSavKey

   IF EOF()
      lAppEND := APP_MODE_ON( oBrowse )
   else
      lAppEND := APP_MODE_ACTIVE( oBrowse )
   ENDIF

   oBrowse:forceStable()
   IF ( lAppEND .AND. ( RECNO() = LASTREC() + 1 ) )
      DBAPPEND()
      
   ENDIF
   xSavKey := IIF( EMPTY( INDEXKEY() ), NIL, &( INDEXKEY() ) )

   REPLACE &namecampo1  WITH cod

   bSavIns := setkey( K_INS, { || InsToggle() } )
   setcursor( iIF( ReadInsert(), SC_INSERT, SC_NORMAL ) )
   setcursor( SC_NONE )
   setkey( K_INS, bSavIns )
   APP_MODE_OFF( oBrowse )
   xNewKey := IF( empty( indexkey() ), NIL, &( indexkey() ) )
   oBrowse:refreshAll():forceStable()

   IF !( xNewKey == xSavKey ) .or. ( lAppEND .and. xNewKey != NIL )
      oBrowse:refreshAll():forceStable()
      while &( indexkey() ) > xNewKey .and. !oBrowse:hitTop()
         oBrowse:up():forceStable()
      ENDDO
   ENDIF
   nKey := lastkey()
   IF nKey == K_UP   .or. nKey == K_DOWN .or. ;
      nKey == K_PGUP .or. nKey == K_PGDN
      keyboard( chr( nKey ) )
   ENDIF
RETURN
*---------------------------------------------------------------------------

***************************************************************************
* busca si un codigo  ya Existe
***************************************************************************

FUNCTION No_Existe(oBrowse,cod,nreg)

  SEEK cod
  IF FOUND()
     MENSAJE("El C¢digo Ya Existe... Corrija por Favor !",fil2-3,col1+2,'W+/R',.T.,.F.)
     GO nreg
     oBrowse:ForceStable()
     RETURN .F.
  ENDIF
  GO nreg
  oBrowse:ForceStable()
RETURN .T.
*-------------------------------------------------------------------------

***************************************************************************
* RELLENA DE CEROS LA PARTE IZQUIERDA DEL CODIGO DEL ARCHIVO
***************************************************************************

FUNCTION Rellena_Caracter(cod,car,tamano)

  cod := ALLTRIM(cod)

  DO WHILE LEN(cod) < tamano
     cod := car + cod
  ENDDO
RETURN .T.
***************************************************************************

FUNCTION antig
LOCAL aa

         aa=entrada_22(5)
/*         ?aa[3]
         SELECT 14
         GO aa[3]
         @23,4 SAY  porce
         @23,7 say aa[3]
         REPLACE porce WITH aa[2]*/
RETURN