&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-win 
/*------------------------------------------------------------------------

  File: DatePicker.w

  Description: DatePicker to use in Progress ABL applications

  Input Parameters: Can use Input Parameters or global variable
      <none>

  Output Parameters: Can use Output Parameters or global variable

  Author: Erick Oliveira Marcolino - erick_marcolino@yahoo.com.br

  Created: 06/08/2021
*/

CREATE WIDGET-POOL.

//DEF INPUT-OUTPUT PARAM DataOriginal AS DATE.
DEFINE NEW GLOBAL SHARED VARIABLE DataOriginal AS DATE NO-UNDO.

DEF VAR DataInicial   AS DATE NO-UNDO.
DEF VAR DataAtual     AS DATE NO-UNDO.
DEF VAR Ano           AS INT  NO-UNDO.
DEF VAR Mes           AS INT  NO-UNDO.
DEF VAR Dia           AS INT  NO-UNDO.
DEF VAR iContador     AS INT  NO-UNDO.
DEF VAR cTexto        AS CHAR NO-UNDO.
DEF VAR cSeparador    AS CHAR NO-UNDO.

IF DataOriginal = ? THEN
    DataOriginal = TODAY.

IF YEAR(DataOriginal) < YEAR(TODAY) - 300 THEN
    DataOriginal = TODAY.

IF YEAR(DataOriginal) > YEAR(TODAY) + 300 THEN
    DataOriginal = TODAY.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BoxMes btnDom btnMesPrev btnMesNext txtAno ~
btnAnoPrev btnAnoNext btnHoje D1 D2 D3 D4 D5 D6 D7 D8 D9 D10 btnQua D11 D12 ~
D13 D14 D15 btnQui D16 D17 D19 D20 D21 D22 D23 btnSab D24 D25 D26 D27 D28 ~
D29 D30 D31 D32 D33 D34 D35 btnSeg D36 D37 D38 D39 D40 btnSex D41 D42 ~
btnTer 
&Scoped-Define DISPLAYED-OBJECTS BoxMes txtAno 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FindWidget w-win 
FUNCTION FindWidget RETURNS WIDGET-HANDLE
  ( ObjetoInicial AS WIDGET-HANDLE,
    Nome AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAnoNext 
     LABEL ">" 
     SIZE 2.29 BY 1.

DEFINE BUTTON btnAnoPrev 
     LABEL "<" 
     SIZE 2 BY 1.

DEFINE BUTTON btnDom  NO-FOCUS FLAT-BUTTON
     LABEL "Dom" 
     SIZE 5 BY 1.13
     BGCOLOR 18 .

DEFINE BUTTON btnHoje 
     LABEL "Hoje" 
     SIZE 10 BY 1
     BGCOLOR 18 .

DEFINE BUTTON btnMesNext 
     LABEL ">" 
     SIZE 2.29 BY 1.

DEFINE BUTTON btnMesPrev 
     LABEL "<" 
     SIZE 2 BY 1.

DEFINE BUTTON btnQua  NO-FOCUS FLAT-BUTTON
     LABEL "Qua" 
     SIZE 5 BY 1.13
     BGCOLOR 18 .

DEFINE BUTTON btnQui  NO-FOCUS FLAT-BUTTON
     LABEL "Qui" 
     SIZE 5 BY 1.13
     BGCOLOR 18 .

DEFINE BUTTON btnSab  NO-FOCUS FLAT-BUTTON
     LABEL "Sab" 
     SIZE 5 BY 1.13
     BGCOLOR 18 .

DEFINE BUTTON btnSeg  NO-FOCUS FLAT-BUTTON
     LABEL "Seg" 
     SIZE 5 BY 1.13
     BGCOLOR 18 .

DEFINE BUTTON btnSex  NO-FOCUS FLAT-BUTTON
     LABEL "Sex" 
     SIZE 5 BY 1.13
     BGCOLOR 18 .

DEFINE BUTTON btnTer  NO-FOCUS FLAT-BUTTON
     LABEL "Ter" 
     SIZE 5 BY 1.13
     BGCOLOR 18 .

DEFINE BUTTON D1 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D10 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D11 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D12 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D13 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D14 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D15 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D16 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D17 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D18 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D19 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D2 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D20 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D21 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D22 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D23 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D24 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D25 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D26 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D27 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D28 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D29 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D3 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D30 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D31 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D32 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D33 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D34 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D35 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D36 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D37 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D38 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D39 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D4 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D40 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D41 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D42 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D5 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D6 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D7 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D8 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE BUTTON D9 
     LABEL "-" 
     SIZE 5 BY 1.13
     BGCOLOR 17 .

DEFINE VARIABLE BoxMes AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEM-PAIRS "Janeiro",1,
                     "Fevereiro",2,
                     "Mar‡o",3,
                     "Abril",4,
                     "Maio",5,
                     "Junho",6,
                     "Julho",7,
                     "Agosto",8,
                     "Setembro",9,
                     "Outubro",10,
                     "Novembro",11,
                     "Dezembro",12
     DROP-DOWN-LIST
     SIZE 14.72 BY 1 NO-UNDO.

DEFINE VARIABLE txtAno AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Ano" 
     VIEW-AS FILL-IN 
     SIZE 6.29 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmMain
     BoxMes AT ROW 1.29 COL 2.29 NO-LABEL WIDGET-ID 4
     btnDom AT ROW 3.5 COL 2 WIDGET-ID 18
     btnMesPrev AT ROW 1.29 COL 17 WIDGET-ID 6
     btnMesNext AT ROW 1.29 COL 18.86 WIDGET-ID 8
     txtAno AT ROW 1.29 COL 23.86 COLON-ALIGNED WIDGET-ID 114
     btnAnoPrev AT ROW 1.29 COL 32.14 WIDGET-ID 14
     btnAnoNext AT ROW 1.29 COL 34 WIDGET-ID 12
     btnHoje AT ROW 2.38 COL 14 WIDGET-ID 116
     D1 AT ROW 4.58 COL 2 WIDGET-ID 16
     D2 AT ROW 4.58 COL 6.86 WIDGET-ID 32
     D3 AT ROW 4.58 COL 11.72 WIDGET-ID 64
     D4 AT ROW 4.58 COL 16.57 WIDGET-ID 62
     D5 AT ROW 4.58 COL 21.43 WIDGET-ID 60
     D6 AT ROW 4.58 COL 26.29 WIDGET-ID 58
     D7 AT ROW 4.58 COL 31.14 WIDGET-ID 56
     D8 AT ROW 5.67 COL 2 WIDGET-ID 54
     D9 AT ROW 5.67 COL 6.86 WIDGET-ID 52
     D10 AT ROW 5.67 COL 11.72 WIDGET-ID 50
     btnQua AT ROW 3.5 COL 16.57 WIDGET-ID 24
     D11 AT ROW 5.67 COL 16.57 WIDGET-ID 48
     D12 AT ROW 5.67 COL 21.43 WIDGET-ID 46
     D13 AT ROW 5.67 COL 26.29 WIDGET-ID 44
     D14 AT ROW 5.67 COL 31.14 WIDGET-ID 42
     D15 AT ROW 6.75 COL 2 WIDGET-ID 36
     btnQui AT ROW 3.5 COL 21.43 WIDGET-ID 26
     D16 AT ROW 6.75 COL 6.86 WIDGET-ID 76
     D17 AT ROW 6.75 COL 11.72 WIDGET-ID 34
     D18 AT ROW 6.75 COL 16.57 WIDGET-ID 86
     D19 AT ROW 6.75 COL 21.43 WIDGET-ID 84
     D20 AT ROW 6.75 COL 26.29 WIDGET-ID 78
     D21 AT ROW 6.75 COL 31.14 WIDGET-ID 38
     D22 AT ROW 7.83 COL 2 WIDGET-ID 80
     D23 AT ROW 7.83 COL 6.86 WIDGET-ID 74
     btnSab AT ROW 3.5 COL 31.14 WIDGET-ID 30
     D24 AT ROW 7.83 COL 11.72 WIDGET-ID 72
     D25 AT ROW 7.83 COL 16.57 WIDGET-ID 82
     D26 AT ROW 7.83 COL 21.43 WIDGET-ID 70
     D27 AT ROW 7.83 COL 26.29 WIDGET-ID 68
     D28 AT ROW 7.83 COL 31.14 WIDGET-ID 66
     D29 AT ROW 8.92 COL 2 WIDGET-ID 112
     D30 AT ROW 8.92 COL 6.86 WIDGET-ID 110
     D31 AT ROW 8.92 COL 11.72 WIDGET-ID 108
     D32 AT ROW 8.92 COL 16.57 WIDGET-ID 106
     D33 AT ROW 8.92 COL 21.43 WIDGET-ID 104
     D34 AT ROW 8.92 COL 26.29 WIDGET-ID 102
     D35 AT ROW 8.92 COL 31.14 WIDGET-ID 100
     btnSeg AT ROW 3.5 COL 6.86 WIDGET-ID 20
     D36 AT ROW 10 COL 2 WIDGET-ID 98
     D37 AT ROW 10 COL 6.86 WIDGET-ID 96
     D38 AT ROW 10 COL 11.72 WIDGET-ID 94
     D39 AT ROW 10 COL 16.57 WIDGET-ID 92
     D40 AT ROW 10 COL 21.43 WIDGET-ID 90
     btnSex AT ROW 3.5 COL 26.29 WIDGET-ID 28
     D41 AT ROW 10 COL 26.29 WIDGET-ID 88
     D42 AT ROW 10 COL 31.14 WIDGET-ID 40
     btnTer AT ROW 3.5 COL 11.72 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 36 BY 10.38 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-win ASSIGN
         HIDDEN             = YES
         TITLE              = "DatePicker"
         HEIGHT             = 10.38
         WIDTH              = 36
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME frmMain
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX BoxMes IN FRAME frmMain
   ALIGN-L                                                              */
/* SETTINGS FOR BUTTON D18 IN FRAME frmMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-win)
THEN w-win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-win w-win
ON END-ERROR OF w-win /* DatePicker */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-win w-win
ON WINDOW-CLOSE OF w-win /* DatePicker */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BoxMes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BoxMes w-win
ON VALUE-CHANGED OF BoxMes IN FRAME frmMain
DO:
    RUN DiasDoMes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAnoNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAnoNext w-win
ON CHOOSE OF btnAnoNext IN FRAME frmMain /* > */
DO:
    IF INPUT FRAME {&FRAME-NAME} txtAno < 9999 THEN
        INPUT FRAME {&FRAME-NAME} txtAno:SCREEN-VALUE = STRING(INPUT FRAME {&FRAME-NAME} txtAno + 1).

    RUN DiasDoMes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAnoPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAnoPrev w-win
ON CHOOSE OF btnAnoPrev IN FRAME frmMain /* < */
DO:
    IF INPUT FRAME {&FRAME-NAME} txtAno > 0 THEN
        INPUT FRAME {&FRAME-NAME} txtAno:SCREEN-VALUE = STRING(INPUT FRAME {&FRAME-NAME} txtAno - 1).

    RUN DiasDoMes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHoje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHoje w-win
ON CHOOSE OF btnHoje IN FRAME frmMain /* Hoje */
DO:
    DataOriginal = TODAY.

    RUN Inicializa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMesNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMesNext w-win
ON CHOOSE OF btnMesNext IN FRAME frmMain /* > */
DO:
    IF INPUT FRAME {&FRAME-NAME} BoxMes < 12 THEN
        INPUT FRAME {&FRAME-NAME} BoxMes:SCREEN-VALUE = STRING(INPUT FRAME {&FRAME-NAME} BoxMes + 1).
    ELSE IF INPUT FRAME {&FRAME-NAME} BoxMes = 12 AND INPUT FRAME {&FRAME-NAME} txtAno < 9999 THEN
        DO:
            INPUT FRAME {&FRAME-NAME} BoxMes:SCREEN-VALUE = STRING(1).
            INPUT FRAME {&FRAME-NAME} txtAno:SCREEN-VALUE = STRING(INPUT FRAME {&FRAME-NAME} txtAno + 1).
        END.

    RUN DiasDoMes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMesPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMesPrev w-win
ON CHOOSE OF btnMesPrev IN FRAME frmMain /* < */
DO:
    IF INPUT FRAME {&FRAME-NAME} BoxMes > 1 THEN
        INPUT FRAME {&FRAME-NAME} BoxMes:SCREEN-VALUE = STRING(INPUT FRAME {&FRAME-NAME} BoxMes - 1).
    ELSE IF INPUT FRAME {&FRAME-NAME} BoxMes = 1 AND INPUT FRAME {&FRAME-NAME} txtAno > 0 THEN DO:
        INPUT FRAME {&FRAME-NAME} BoxMes:SCREEN-VALUE = STRING(12).
        INPUT FRAME {&FRAME-NAME} txtAno:SCREEN-VALUE = STRING(INPUT FRAME {&FRAME-NAME} txtAno - 1).
    END.

    RUN DiasDoMes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D1 w-win
ON CHOOSE OF D1 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D10 w-win
ON CHOOSE OF D10 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D11 w-win
ON CHOOSE OF D11 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D12 w-win
ON CHOOSE OF D12 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D13 w-win
ON CHOOSE OF D13 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D14 w-win
ON CHOOSE OF D14 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D15 w-win
ON CHOOSE OF D15 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D16 w-win
ON CHOOSE OF D16 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D17
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D17 w-win
ON CHOOSE OF D17 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D18 w-win
ON CHOOSE OF D18 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D19 w-win
ON CHOOSE OF D19 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D2 w-win
ON CHOOSE OF D2 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D20 w-win
ON CHOOSE OF D20 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D21 w-win
ON CHOOSE OF D21 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D22
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D22 w-win
ON CHOOSE OF D22 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D23
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D23 w-win
ON CHOOSE OF D23 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D24
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D24 w-win
ON CHOOSE OF D24 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D25
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D25 w-win
ON CHOOSE OF D25 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D26
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D26 w-win
ON CHOOSE OF D26 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D27
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D27 w-win
ON CHOOSE OF D27 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D28
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D28 w-win
ON CHOOSE OF D28 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D29
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D29 w-win
ON CHOOSE OF D29 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D3 w-win
ON CHOOSE OF D3 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D30
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D30 w-win
ON CHOOSE OF D30 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D31
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D31 w-win
ON CHOOSE OF D31 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D32
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D32 w-win
ON CHOOSE OF D32 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D33
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D33 w-win
ON CHOOSE OF D33 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D34
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D34 w-win
ON CHOOSE OF D34 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D35
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D35 w-win
ON CHOOSE OF D35 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D36
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D36 w-win
ON CHOOSE OF D36 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D37
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D37 w-win
ON CHOOSE OF D37 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D38
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D38 w-win
ON CHOOSE OF D38 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D39
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D39 w-win
ON CHOOSE OF D39 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D4 w-win
ON CHOOSE OF D4 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D40
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D40 w-win
ON CHOOSE OF D40 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D41
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D41 w-win
ON CHOOSE OF D41 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D42
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D42 w-win
ON CHOOSE OF D42 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D5 w-win
ON CHOOSE OF D5 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D6 w-win
ON CHOOSE OF D6 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D7 w-win
ON CHOOSE OF D7 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D8 w-win
ON CHOOSE OF D8 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME D9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D9 w-win
ON CHOOSE OF D9 IN FRAME frmMain /* - */
DO:
    RUN Finaliza (INPUT SELF:LABEL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txtAno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtAno w-win
ON LEAVE OF txtAno IN FRAME frmMain /* Ano */
DO:
    //RUN DiasDoMes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtAno w-win
ON VALUE-CHANGED OF txtAno IN FRAME frmMain /* Ano */
DO:
    RUN DiasDoMes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.

    RUN inicializa.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DiasDoMes w-win 
PROCEDURE DiasDoMes :
DataInicial = DATE(INPUT FRAME {&FRAME-NAME} BoxMes, 1, INPUT FRAME {&FRAME-NAME} txtAno).

DEFINE VAR Objeto AS WIDGET-HANDLE NO-UNDO.

/*MESSAGE STRING(DataInicial) VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

DO iContador = 1 TO 42:
    IF iContador = 1 THEN
        DataAtual = DataInicial - WEEKDAY(DataInicial) + 1.
    ELSE
        DataAtual = DataAtual + 1.

    Objeto = FindWidget(FRAME frmMain:HANDLE, "D" + STRING(iContador)).

    IF DataAtual = DataOriginal THEN DO:
        Objeto:LABEL = "[" + STRING(Day(DataAtual)) + "]".
        Objeto:FGCOLOR = 12. //Not working
    END.
    ELSE
        Objeto:LABEL = STRING(Day(DataAtual)).
    
    IF MONTH(DataAtual) = MONTH(DataInicial) THEN
       Objeto:SENSITIVE = YES.
    ELSE
       Objeto:SENSITIVE = FALSE.

    VIEW
        D1
        D2
        D3
        D4
        D5
        D6
        D7
        D35
        D36
        D37
        D38
        D39
        D40
        D41
        D42
    .
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-win)
  THEN DELETE WIDGET w-win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY BoxMes txtAno 
      WITH FRAME frmMain IN WINDOW w-win.
  ENABLE BoxMes btnDom btnMesPrev btnMesNext txtAno btnAnoPrev btnAnoNext 
         btnHoje D1 D2 D3 D4 D5 D6 D7 D8 D9 D10 btnQua D11 D12 D13 D14 D15 
         btnQui D16 D17 D19 D20 D21 D22 D23 btnSab D24 D25 D26 D27 D28 D29 D30 
         D31 D32 D33 D34 D35 btnSeg D36 D37 D38 D39 D40 btnSex D41 D42 btnTer 
      WITH FRAME frmMain IN WINDOW w-win.
  {&OPEN-BROWSERS-IN-QUERY-frmMain}
  VIEW w-win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Finaliza w-win 
PROCEDURE Finaliza :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM p-dia AS CHAR.

    ASSIGN 
        Dia = INT(REPLACE(REPLACE(p-dia, "[", ""), "]", ""))
        DataOriginal = DATE(INPUT FRAME {&FRAME-NAME} BoxMes, Dia, INPUT FRAME {&FRAME-NAME} txtAno)
    .

    //MESSAGE DataOriginal VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    
    APPLY "END-ERROR":U TO SELF.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializa w-win 
PROCEDURE Inicializa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN 
        Ano         = YEAR(DataOriginal)
        Mes         = MONTH(DataOriginal)
        Dia         = DAY(DataOriginal)
    
        cTexto      = ""
    .
    
    ASSIGN
        INPUT FRAME {&FRAME-NAME} txtAno:SCREEN-VALUE = STRING(Ano).
        INPUT FRAME {&FRAME-NAME} BoxMes:SCREEN-VALUE = STRING(Mes).
    .
    
    ASSIGN
        BoxMes
        txtAno
    .
    
    RUN DiasDoMes.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FindWidget w-win 
FUNCTION FindWidget RETURNS WIDGET-HANDLE
  ( ObjetoInicial AS WIDGET-HANDLE,
    Nome AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VAR ObjetoEncontrado                 AS WIDGET-HANDLE NO-UNDO.
    DEFINE VAR ObjetoTeste                      AS WIDGET-HANDLE NO-UNDO.

    ObjetoTeste = ObjetoInicial:FIRST-CHILD NO-ERROR.
    DO WHILE (ObjetoTeste <> ?):

        IF ObjetoTeste:NAME = Nome THEN
            RETURN ObjetoTeste.

        IF ObjetoEncontrado = ? THEN
        DO:

            ObjetoEncontrado = FindWidget (ObjetoTeste, Nome).
            IF ObjetoEncontrado <> ? THEN
                RETURN ObjetoEncontrado.
        END.

        ObjetoTeste = ObjetoTeste:NEXT-SIBLING.
    END. /* do while (ObjetoTeste <> ?) */

    ObjetoInicial = ObjetoInicial:NEXT-SIBLING.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

