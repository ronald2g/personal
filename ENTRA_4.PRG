
**************************************************************************
* modulo que hace las entradas de todas las tablas de planillas
* fecha de cracion:                 4 de agosto de 1994
* responzable     :                 Fernando Valdivia L.
*************************************************************************

*****************************************************************************
* esta funcion hace la llamada para el TBrowse()
*****************************************************************************

FUNCTION Entra_44( accion )
   LOCAL   ctaven,venta_total,pantalla1,titulo
   PRIVATE fil1,fil2,col1,col2,tabla
   PRIVATE area,indi,namecampo1,namecampo2,namecampo3,namecampo4
   PRIVATE titucampo1,titucampo2,titucampo3,titucampo4
   PRIVATE pictcampo1,pictcampo2,pictcampo3,pictcampo4,pic_item:="!AAA-999"

   DO CASE
      CASE  accion = 1
            area        := 16
            indi        := 1
            titucampo1  := "No ITEM"                     && obtengo el titulo del campo1
            titucampo2  := "HORAS EXTRAS"                    && obtengo el titulo del campo2
            titucampo3  := "HORAS NOCTURNAS"
            titucampo4  := "ANTICIPO"
            pictcampo1  := "@!"                       && obtengo el picture del campo1
            pictcampo2  := "999.99"                      && obtengo el picture del campo2
            pictcampo3  := "999.99"
            pictcampo4  := "999,999.99"
            tabla       := "HORAS EMERGENCIA"
            SELECT( area )
	    USE hrs_eme   INDEX hora_ite

      CASE  accion = 2
            area        := 19
            indi        := 1
            titucampo1  := "CODIGO"                     && obtengo el titulo del campo1
            titucampo2  := "DESCRIPCION"                    && obtengo el titulo del campo2
            titucampo3  := "PORCENTAJE"
            titucampo4  := "MONTO"
            pictcampo1  := "@!"                       && obtengo el picture del campo1
            pictcampo2  := "@!"                     && obtengo el picture del campo2
            pictcampo3  := "99.99"
            pictcampo4  := "999,999.99"
            tabla       := "IMPUESTOS"
	    SELECT( area )
	    USE impuesto  INDEX impu_cod
/*
      CASE  accion = 3
            area        := 17
            indi        := 1
            titucampo1  := "QUIEN"                     && obtengo el titulo del campo1
            titucampo2  := "POR QUIEN"                    && obtengo el titulo del campo2
            titucampo3  := "DIAS SUPLE"
            titucampo4  := "LIQ PAGABLE"
            pictcampo1  := "@!"                       && obtengo el picture del campo1
            pictcampo2  := "@!"                     && obtengo el picture del campo2
            pictcampo3  := "999"
            pictcampo4  := "999,999.99"
            tabla       := "SUPLENCIAS"
	    SELECT( area )
	    USE suplencia INDEX suple_qu
*/
   ENDCASE

   SET SCOREBOARD OFF
   venta_total = SAVES( 0,0,24,79 )
   fil1 := 5
   fil2 := 19
   col1 := 5
   col2 := 75
   titulo      = '' &&EDICION TABLA DE ' + 'HORAS Y ANTICIPOS'
   ctaven      = OPEN_WINDOW(fil1-1,col1-1,fil2+1,col2+1,'ÛÛÛÞßßßÝ ',titulo,'R/B,N/BG',5,.T.)
   pantalla1   = OPEN_WINDOW(fil2 + 2 ,col1 - 1,fil2 + 4,col2 + 1,1,'',"N/BG",2,.T.)
   @ fil2+3,45 SAY 'ESC Salir'    COLOR "GR+/WR+"

   MyBrowse_44( fil1,col1,fil2-1,col2 )

   CLOSE_WINDOW( pantalla1 )
   CLOSE_WINDOW( ctaven )
   CLOSE_WINDOW( venta_total )
//   DBCLOSEALL()
RETURN .T.
*------------------------------------------------------------------------

*****************************************************************************
*  hace todo el TBrowse
***************************************************************************

STATIC PROCEDURE MyBrowse_44(nTop, nLeft, nBottom, nRight)
  #include "Common.ch"
  #include "Inkey.ch"
  #include "Setcurs.ch"
  #include "Error.ch"
  #define APP_MODE_ON( b )      ( b:cargo := TRUE  )
  #define APP_MODE_OFF( b )     ( b:cargo := FALSE )
  #define APP_MODE_ACTIVE( b )  ( b:cargo )
  #define MY_HEADSEP      "ÍÑÍ"
  #define MY_COLSEP       " ³ "

   LOCAL oBrowse                          // The TBrowse object
   LOCAL nKey                             // Keystroke
   LOCAL Salir := TRUE                    // Loop control
   PRIVATE  foot1[4],Num_Reg:=0

*****************************************************************************
* inicializacion de  mensajes de los campos para las cuentas
*****************************************************************************
    foot1[1]  = "" &&Numero de Item               "
    foot1[2]  = "" &&Horas Extras que Realizo     "
    foot1[3]  = "" &&Horas Ncturnas que Realizo   "
    foot1[4]  = "" &&Anticipos que Recibio        "
*****************************************************************************
*****************************************************************************

   oBrowse           := AumentaColBrowse( nTop, nLeft, nBottom, nRight )
   APP_MODE_OFF( oBrowse )
   oBrowse:skipBlock := { |x| Skipper( x, oBrowse ) }
   oBrowse:headSep   := MY_HEADSEP
   oBrowse:colSep    := MY_COLSEP
   SETCURSOR(0)
   oBrowse:forceStable()
   mensaje_44(oBrowse:ColPos())

   DO WHILE Salir
      IF (oBrowse:colPos >= 2 )
        oBrowse:freeze := 1
       ELSE
         oBrowse:freeze :=0
      ENDIF

      IF ( oBrowse:colPos <= oBrowse:freeze )
         oBrowse:colPos := ( oBrowse:freeze + 1 )
      ENDIF

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

      IF ( nKey == 27 )
         Salir := .F.
       ELSE
         ApplyKey_3(oBrowse, nKey)
      ENDIF

   ENDDO

   MESSBAR_OFF()
   SETCURSOR( 0 )
RETURN
*---------------------------------------------------------------------------

STATIC FUNCTION mensaje_44(Ind)
***    Despliega al pie un mensaje por cada campo que se edita

        IF Ind < 5 .AND. Ind > 0
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
  mensaje_44(oBrowse:ColPos())
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
         @ ROW(),COL()  GET cod    COLOR "GR+/W" PICTURE pic_item
         READ

         IF LASTKEY() <> K_ESC  .AND.  cod <> FIELDGET( oBrowse:Colpos() )
            Rellena_Caracter(@cod,'0',LEN(pic_item))
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

    CASE oBrowse:colPos() == 3
         IF nkey <> K_RETURN
            KEYBOARD chr( nKey )
         ENDIF
         DoGet( oBrowse )

    CASE oBrowse:colPos() == 4
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

STATIC FUNCTION DoGet( oBrowse )
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
    namecampo2 := FIELDNAME( n )               && obtengo el nombre del campo1
    oColumn    := TBColumnNew( titucampo2,FieldWBlock( field( n ), select() ) )
    oBrowse:addColumn( oColumn )
    oColumn:picture   = pictcampo2
    oColumn:defColor  = { 3,4 }

    n := 3
    namecampo3 := FIELDNAME( n )               && obtengo el nombre del campo1
    oColumn    := TBColumnNew( titucampo3,FieldWBlock( field( n ), select() ) )
    oBrowse:addColumn( oColumn )
    oColumn:picture   = pictcampo3
    oColumn:defColor  = { 3,4 }

    n := 4
    namecampo4 := FIELDNAME( n )               && obtengo el nombre del campo1
    oColumn    := TBColumnNew( titucampo4,FieldWBlock( field( n ), select() ) )
    oBrowse:addColumn( oColumn )
    oColumn:picture   = pictcampo4
    oColumn:defColor  = { 3,4 }

RETURN oBrowse
*---------------------------------------------------------------------

