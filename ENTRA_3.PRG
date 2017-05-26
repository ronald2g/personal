**************************************************************************
* modulo que hace las entradas de las tablas de planillas que tengan DOS columnas
* fecha de cracion:                 4 de agosto de 1994
* responzable     :                 Fernando Valdivia L.
*************************************************************************

*****************************************************************************
* esta funcion hace la llamada para el TBrowse()
*****************************************************************************

FUNCTION Entrada_33( accion )
   LOCAL   ctaven,venta_total,pantalla1,titulo
   PRIVATE fil1,fil2,col1,col2,tabla
   PRIVATE area,indi,namecampo1,namecampo2,name_compo3,titucampo1,titucampo2,titucampo3,pictcampo1,pictcampo2,pictcampo3,pic_item:="!AAA-999"

   DO CASE

      CASE  accion = 1
            area        := 10
            indi        := 1
            titucampo1  := "CODIGO"                       && obtengo el titulo del campo1
            titucampo2  := "NOMBRE1"                      && obtengo el titulo del campo2
            titucampo3  := "NOMBRE2"
            pictcampo1  := "!!!"                         && obtengo el picture del campo1
            pictcampo2  := "@S20!"                        && obtengo el picture del campo2
            pictcampo3  := "@S20!"
            tabla       := "AREAS"
  	    SELECT( area )
	    USE area     INDEX area_co


      CASE  accion = 2
            area        := 14
            indi        := 1
            titucampo1  := "TIPO"                       && obtengo el titulo del campo1
            titucampo2  := "RANGO"                      && obtengo el titulo del campo2
            titucampo3  := "PORCENTAJE"
            pictcampo1  := "!"                         && obtengo el picture del campo1
            pictcampo2  := "!!"                        && obtengo el picture del campo2
            pictcampo3  := "99"
            tabla       := "BONOS"
	    SELECT( area )
            USE bonos     INDEX bono_co

      CASE  accion = 3
            area        := 18
            indi        := 1
            titucampo1  := "No ITEM"                       && obtengo el titulo del campo1
            titucampo2  := "CODIGO"                      && obtengo el titulo del campo2
            titucampo3  := "FECHA INICIAL"
            pictcampo1  := pic_item                       && obtengo el picture del campo1
            pictcampo2  := "!!!!!"                        && obtengo el picture del campo2
            pictcampo3  := "DD/MM/YY"
            tabla       := "SEPELIOS"
  	    SELECT( area )
	    USE nac_muer  INDEX n_m_item

      CASE  accion = 4
            area        := 17
            indi        := 1
            titucampo1  := "QUIEN"                     && obtengo el titulo del campo1
            titucampo2  := "POR QUIEN"                    && obtengo el titulo del campo2
            titucampo3  := "DIAS SUPLE"
            pictcampo1  := "@!"                       && obtengo el picture del campo1
            pictcampo2  := "@!"                     && obtengo el picture del campo2
            pictcampo3  := "999"
            tabla       := "SUPLENCIAS"
	    SELECT( area )
	    USE suplencia INDEX suple_qu

   ENDCASE

   SET SCOREBOARD OFF
   venta_total = SAVES( 0,0,24,79 )
   fil1 := 7
   fil2 := 18
   col1 := 10
   col2 := 63
   titulo      = 'EDICION TABLA DE ' + tabla
   ctaven      = OPEN_WINDOW(fil1-1,col1-1,fil2+1,col2+1,'€€€ﬁﬂﬂﬂ› ',titulo,'R/B,N/BG',5,.T.)
   pantalla1   = OPEN_WINDOW(fil2 + 2 ,col1 - 1,fil2 + 4,col2 + 1,1,'',"R/BG",2,.T.)
   @ fil2+3,45 SAY 'F10 Salir'    COLOR "GR+/WR+"

   SELECT(area)                               && pongo el area de trabajo par actual
   SET ORDER TO indi                          && activo el indice adecuado

   MyBrowse_33( fil1,col1,fil2-1,col2 )

   CLOSE_WINDOW( pantalla1 )
   CLOSE_WINDOW( ctaven )
   CLOSE_WINDOW( venta_total )
//   DBCLOSEALL()
RETURN .T.
*------------------------------------------------------------------------

*****************************************************************************
*  hace todo el TBrowse
***************************************************************************

STATIC PROCEDURE MyBrowse_33(nTop, nLeft, nBottom, nRight)
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
   LOCAL    nKey                             // Keystroke
   LOCAL    Salir := TRUE                    // Loop control
   PRIVATE  foot1[3],Num_Reg:=0

*****************************************************************************
* inicializacion de  mensajes de los campos para las cuentas
*****************************************************************************
    foot1[1]  = "Codigo de la Tabla        "
    foot1[2]  = "Valor que tomara el Codigo"
    foot1[3]  = "bla bla bla..."
*****************************************************************************
*****************************************************************************

   oBrowse           := AumentaColBrowse( nTop, nLeft, nBottom, nRight )
   APP_MODE_OFF( oBrowse )
   oBrowse:skipBlock := { |x| Skipper( x, oBrowse ) }
   oBrowse:headSep   := MY_HEADSEP
   oBrowse:colSep    := MY_COLSEP
   SETCURSOR(0)
   oBrowse:forceStable()
   mensaje_33(oBrowse:ColPos())

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

      IF ( nKey == K_F10 )
         Salir := .F.
       ELSE
         ApplyKey_3(oBrowse, nKey)
      ENDIF

   ENDDO

   MESSBAR_OFF()
   SETCURSOR( 0 )
RETURN
*---------------------------------------------------------------------------

STATIC FUNCTION mensaje_33(Ind)
***    Despliega al pie un mensaje por cada campo que se edita

        IF Ind < 4 .AND. Ind > 0
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
  mensaje_33(oBrowse:ColPos())
RETURN ' '
*----------------------------------------------------------------------------

****************************************************************************
* Permite la entrada de los datos para todas las columnas
***************************************************************************

STATIC FUNCTION entrada_3( oBrowse,nKey,inserto )
LOCAL  cod := SPACE(15),car := ' '

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
            Rellena_Caracter(@cod,'0',LEN(pictcampo1))
            IF No_Existe(oBrowse,cod,RECNO()) = .T.
               Inserta_Reg(oBrowse,cod)
             ENDIF
         ENDIF

         SETCURSOR(0)

    CASE oBrowse:colPos() == 2
         IF nkey <> K_RETURN
            KEYBOARD chr( nKey )
         ENDIF

        IF area = 18                  && para archivo nac_muer.dbf
           IF !( UPPER( CHR(nkey) ) $ 'PNLS' )
              car := MENUV(15,50,{"PRENATAL","NATALIDAD","LACTANCIA","SEPELIO"},{"P","N","L","S"},'W+/R',.T.)
              IF LASTKEY() <> K_ESC
                 REPLACE &namecampo2   WITH car
              ENDIF
           ELSE
              REPLACE &namecampo2   WITH CHR(nKey)
              CLEAR TYPEAHEAD
           ENDIF
           RETURN .T.
        ENDIF

        IF area = 14                  && para archivo bonos.dbf
           CLEAR TYPEAHEAD
           car :=  MENUV(15,54,{"0 - 1","2 - 3","4 - 5","5 - 6","7 - 8","9 - ->"},{"1","2","3","4","5","6"},'W+/R',.T.)
           IF LASTKEY() <> K_ESC
              REPLACE &namecampo2 WITH '0' + car
           ENDIF
           RETURN .T.
        ENDIF

        DoGet( oBrowse )

   CASE oBrowse:colPos() == 3
        IF nkey <> K_RETURN
           KEYBOARD chr( nKey )
        ENDIF
        DoGet( oBrowse )

 ENDCASE
RETURN .T.
*---------------------------------------------------------------------

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

    n := 3
    namecampo3         := FIELDNAME( n )               && obtengo el nombre del campo2
    oColumn            := TBColumnNew( titucampo3,FieldWBlock( field( n ), select() ) )
    oBrowse:addColumn( oColumn )
    oColumn:picture    := pictcampo3
    oColumn:defColor   := { 3,7 }

RETURN oBrowse
*---------------------------------------------------------------------

