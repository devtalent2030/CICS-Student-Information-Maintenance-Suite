       IDENTIFICATION DIVISION.
       PROGRAM-ID. UUMENF2A.
       AUTHOR. TALENT NYOTA.
       DATE-WRITTEN. 21-11-2024.
      *PROGRAM DESCRIPTION:
      *
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
       01 WS-COMMUNICATION-AREA             PIC X VALUE SPACES.
      *
       01 WS-CNSTS.
           05 WS-TRUE-CNST                  PIC X VALUE 'Y'.
           05 WS-FALSE-CNST                 PIC X VALUE 'N'.
           05 WS-YES-CNST                   PIC X VALUE 'Y'.
           05 WS-NO-CNST                    PIC X VALUE 'N'.
           05 WS-OPEN-CNST                  PIC X VALUE 'Y'.
           05 WS-CLOSED-CNST                PIC X VALUE 'N'.
      *
       01 WS-CICS-CNSTS.
           05 WS-MEN-TRANSID-CNST           PIC X(04) VALUE 'UF2A'.
           05 WS-MEN-MAPSET-CNST            PIC X(08) VALUE 'MENSF2A '.
           05 WS-MEN-MAP-CNST               PIC X(08) VALUE 'MENMF2A '.
      *
       01 WS-FILE-CNSTS.
           05 WS-CMF-FILE-NAME-CNST         PIC X(08) VALUE 'CMFF2A  '.
      *
       01 WS-FLAGS.
           05 WS-VALID-DATA-FLAG            PIC X VALUE 'Y'.
               88 WS-VALID-DATA-88                VALUE 'Y'.
           05 WS-SEND-FLAG                  PIC X.
               88  WS-SEND-ERASE-88               VALUE '1'.
               88  WS-SEND-DATAONLY-88            VALUE '2'.
               88  WS-SEND-DATAONLY-ALARM-88      VALUE '3'.
      *
       01 WS-RESPONSE-CODES.
           05 WS-RESPONSE-CODE              PIC S9(8)  COMP VALUE 0.
           05 WS-RESPONSE-CODE2             PIC S9(8)  COMP VALUE 0.
      *
       01 WS-END-OF-SESSION-MESSAGE         PIC X(13)
           VALUE 'Session ended'.
      *
       01 WS-USER-MESSAGES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-KEY-INVALID.
               10 FILLER                    PIC X(40)
                   VALUE 'Invalid key pressed.                   '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-ENTER-VALID-ACTION.
               10 FILLER                    PIC X(40)
                   VALUE 'You must enter valid action.           '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-PROGRAM-NOT-AVAILABLE.
               10 FILLER                    PIC X(40)
                   VALUE 'That program is not available.         '.
               10 FILLER                    PIC X(08)
                   VALUE 'PROGRAM='.
               10 WS-PROGRAM-NAME-NOT-AVAIL PIC X(08)
                   VALUE SPACES.
               10 FILLER                    PIC X(23)
                   VALUE SPACES.
      *
       01 WS-PROGRAM-LIST-TABLE.
           05 WS-PROGRAM-SUB                PIC 9.
               88 WS-PROGRAM-SUB-VALID-88          VALUE 1, 2, 3.
           05 WS-PROGRAM-LIST-CNSTS.
               10 WS-PROGRAM-1-CNST         PIC X(8) VALUE 'CMINQF2A'.
               10 WS-PROGRAM-2-CNST         PIC X(8) VALUE 'CMMNTF2A'.
               10 WS-PROGRAM-3-CNST         PIC X(8) VALUE 'ORDERF2A'.
           05 WS-PROGRAM-NAME REDEFINES
              WS-PROGRAM-LIST-CNSTS         PIC X(8) OCCURS 3 TIMES.
      *
       COPY ATTR.
      *
       COPY DFHAID.
      *
       COPY ERRORSWS.
      *
       COPY MENSF2A.
      *
       LINKAGE SECTION.
      *
       01 DFHCOMMAREA                       PIC X.
      *
       PROCEDURE DIVISION.
      *
       0000-PROCESS-MASTER-MENU.
      *
           MOVE SPACES                      TO WS-HA-EXEC-TEXT.
           MOVE ALL '*'                     TO WS-HA-EXEC-TEXT-T1.
           MOVE WS-HA-UNEXPECTED-ABEND      TO WS-HA-EXEC-TEXT-T2.
           MOVE ALL '*'                     TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               HANDLE ABEND LABEL(9900-HANDLE-ABEND)
           END-EXEC.
      *
           EVALUATE TRUE
      *
               WHEN EIBCALEN = ZERO
                   MOVE LOW-VALUE           TO MENMF2AO
                   MOVE WS-MEN-TRANSID-CNST TO TRANIDO
                   SET  WS-SEND-ERASE-88    TO TRUE
                   PERFORM 1400-SEND-MENU-MAP
      *
               WHEN EIBAID = DFHCLEAR
                   MOVE LOW-VALUE           TO MENMF2AO
                   MOVE WS-MEN-TRANSID-CNST TO TRANIDO
                   SET  WS-SEND-ERASE-88    TO TRUE
                   PERFORM 1400-SEND-MENU-MAP
      *
               WHEN EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
                   CONTINUE
      *
               WHEN EIBAID = DFHPF3 OR DFHPF12
                   PERFORM 9800-SEND-TERMINATION-MESSAGE
                   EXEC CICS
                       RETURN
                   END-EXEC
      *
               WHEN EIBAID = DFHENTER
                   PERFORM 1000-PROCESS-MENU-MAP
      *
               WHEN OTHER
                   MOVE WS-KEY-INVALID      TO MESSAGEO
                   SET WS-SEND-DATAONLY-ALARM-88
                     TO TRUE
                   PERFORM 1400-SEND-MENU-MAP
      *
           END-EVALUATE.
      *
           MOVE SPACES                      TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                     TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                 TO WS-HA-EXEC-TEXT-T3.
           MOVE 'INQUIRE TRANSACTION'       TO WS-HA-EXEC-TEXT-T4.
           MOVE WS-MEN-TRANSID-CNST         TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                      TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-TRANSIDERR)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               INQUIRE TRANSACTION(WS-MEN-TRANSID-CNST)
                       RESP       (WS-RESPONSE-CODE)
                       RESP2      (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               CONTINUE
           ELSE
               MOVE 'INQUIRE TRANSACTION'   TO WS-RT-MSG-HEADER
               MOVE 'ERROR              '   TO WS-RT-MSG-NORMAL
               MOVE WS-MEN-TRANSID-CNST     TO WS-RT-MSG-OTHER
               PERFORM 9700-RESPTEXT
           END-IF.
      *
           MOVE SPACES                      TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                     TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                 TO WS-HA-EXEC-TEXT-T3.
           MOVE 'RETURN TRANSID'            TO WS-HA-EXEC-TEXT-T4.
           MOVE WS-MEN-TRANSID-CNST         TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                      TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-TRANSIDERR)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               RETURN TRANSID (WS-MEN-TRANSID-CNST)
                      COMMAREA(WS-COMMUNICATION-AREA)
                      RESP    (WS-RESPONSE-CODE)
                      RESP2   (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               CONTINUE
           ELSE
               MOVE 'RETURN TRANSID'        TO WS-RT-MSG-HEADER
               MOVE 'ERROR         '        TO WS-RT-MSG-NORMAL
               MOVE WS-MEN-TRANSID-CNST     TO WS-RT-MSG-OTHER
               PERFORM 9700-RESPTEXT
           END-IF.
      *
       1000-PROCESS-MENU-MAP.
      *
           PERFORM 1100-RECEIVE-MENU-MAP.
           MOVE ACTIONI                     TO WS-PROGRAM-SUB.
           PERFORM 1200-EDIT-MENU-DATA.
           IF WS-VALID-DATA-88
               PERFORM 1300-BRANCH-TO-PROGRAM
           END-IF.
           SET WS-SEND-DATAONLY-ALARM-88    TO TRUE.
           PERFORM 1400-SEND-MENU-MAP.
      *
       1100-RECEIVE-MENU-MAP.
      *
           MOVE SPACES                      TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                     TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                 TO WS-HA-EXEC-TEXT-T3.
           MOVE 'RECEIVE MAP - MAPSET/MAP'  TO WS-HA-EXEC-TEXT-T4.
           MOVE WS-MEN-MAPSET-CNST          TO WS-HA-EXEC-TEXT-T5.
           MOVE WS-MEN-MAP-CNST             TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               RECEIVE MAP   (WS-MEN-MAP-CNST)
                       MAPSET(WS-MEN-MAPSET-CNST)
                       INTO  (MENMF2AI)
                       RESP  (WS-RESPONSE-CODE)
                       RESP2 (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               CONTINUE
           ELSE
               MOVE 'RECEIVE MAP - MAPSET/MAP'
                 TO WS-RT-MSG-HEADER
               MOVE WS-MEN-MAPSET-CNST      TO WS-RT-MSG-NORMAL
               MOVE WS-MEN-MAP-CNST         TO WS-RT-MSG-OTHER
               PERFORM 9700-RESPTEXT
           END-IF.
      *
       1200-EDIT-MENU-DATA.
      *
           IF NOT WS-PROGRAM-SUB-VALID-88
               MOVE ATTR-REVERSE            TO ACTIONH
               MOVE WS-ENTER-VALID-ACTION   TO MESSAGEO
               MOVE WS-FALSE-CNST           TO WS-VALID-DATA-FLAG
           END-IF.
      *
       1300-BRANCH-TO-PROGRAM.
      *
           MOVE SPACES                      TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                     TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                 TO WS-HA-EXEC-TEXT-T3.
           MOVE 'XCTL PROGRAM - NBR/NAME'   TO WS-HA-EXEC-TEXT-T4.
           MOVE WS-PROGRAM-SUB              TO WS-HA-EXEC-TEXT-T5.
           MOVE WS-PROGRAM-NAME(WS-PROGRAM-SUB)
             TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-PGMIDERR)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               XCTL PROGRAM(WS-PROGRAM-NAME(WS-PROGRAM-SUB))
                    RESP   (WS-RESPONSE-CODE)
                    RESP2  (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           MOVE WS-PROGRAM-NAME(WS-PROGRAM-SUB)
             TO WS-PROGRAM-NAME-NOT-AVAIL.
           MOVE WS-PROGRAM-NOT-AVAILABLE    TO MESSAGEO.
      *
       1400-SEND-MENU-MAP.
      *
           MOVE WS-MEN-TRANSID-CNST         TO TRANIDO.
           EVALUATE TRUE
               WHEN WS-SEND-ERASE-88
                   MOVE SPACES              TO WS-HA-EXEC-TEXT
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T1
                   MOVE 'ABEND'             TO WS-HA-EXEC-TEXT-T2
                   MOVE 'EVALUATE WHEN WS-SEND-ERASE-88'
                     TO WS-HA-EXEC-TEXT-T3
                   MOVE 'EXEC CICS SEND MAP - MAPSET/MAP'
                     TO WS-HA-EXEC-TEXT-T4
                   MOVE WS-MEN-MAPSET-CNST  TO WS-HA-EXEC-TEXT-T5
                   MOVE WS-MEN-MAP-CNST     TO WS-HA-EXEC-TEXT-T6
                   MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
                     TO WS-HA-EXEC-TEXT-T7
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T8
      *
                   EXEC CICS
                       SEND MAP   (WS-MEN-MAP-CNST)
                            MAPSET(WS-MEN-MAPSET-CNST)
                            FROM  (MENMF2AO)
                            ERASE
                            RESP  (WS-RESPONSE-CODE)
                            RESP2 (WS-RESPONSE-CODE2)
                   END-EXEC
      *
                   IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                       CONTINUE
                   ELSE
                       MOVE 'SEND MAP     ' TO WS-RT-MSG-HEADER
                       MOVE 'ERROR        ' TO WS-RT-MSG-NORMAL
                       MOVE 'ERASE        ' TO WS-RT-MSG-OTHER
                       PERFORM 9700-RESPTEXT
                   END-IF
      *
               WHEN WS-SEND-DATAONLY-88
                   MOVE SPACES              TO WS-HA-EXEC-TEXT
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T1
                   MOVE 'ABEND'             TO WS-HA-EXEC-TEXT-T2
                   MOVE 'EVALUATE WHEN WS-SEND-DATAONLY-88'
                     TO WS-HA-EXEC-TEXT-T3
                   MOVE 'EXEC CICS SEND MAP - MAPSET/MAP'
                     TO WS-HA-EXEC-TEXT-T4
                   MOVE WS-MEN-MAPSET-CNST  TO WS-HA-EXEC-TEXT-T5
                   MOVE WS-MEN-MAP-CNST     TO WS-HA-EXEC-TEXT-T6
                   MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
                     TO WS-HA-EXEC-TEXT-T7
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T8
      *
                   EXEC CICS
                       SEND MAP   (WS-MEN-MAP-CNST)
                            MAPSET(WS-MEN-MAPSET-CNST)
                            FROM  (MENMF2AO)
                            DATAONLY
                            RESP  (WS-RESPONSE-CODE)
                            RESP2 (WS-RESPONSE-CODE2)
                   END-EXEC
      *
                   IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                       CONTINUE
                   ELSE
                       MOVE 'SEND MAP     ' TO WS-RT-MSG-HEADER
                       MOVE 'ERROR        ' TO WS-RT-MSG-NORMAL
                       MOVE 'DATAONLY     ' TO WS-RT-MSG-OTHER
                       PERFORM 9700-RESPTEXT
                   END-IF
      *
               WHEN WS-SEND-DATAONLY-ALARM-88
                   MOVE SPACES              TO WS-HA-EXEC-TEXT
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T1
                   MOVE 'ABEND'             TO WS-HA-EXEC-TEXT-T2
                   MOVE 'EVALUATE WHEN WS-SEND-DATAONLY-ALARM-88'
                     TO WS-HA-EXEC-TEXT-T3
                   MOVE 'EXEC CICS SEND MAP - MAPSET/MAP'
                     TO WS-HA-EXEC-TEXT-T4
                   MOVE WS-MEN-MAPSET-CNST  TO WS-HA-EXEC-TEXT-T5
                   MOVE WS-MEN-MAPSET-CNST  TO WS-HA-EXEC-TEXT-T6
                   MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
                     TO WS-HA-EXEC-TEXT-T7
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T8
      *
                   EXEC CICS
                       SEND MAP   (WS-MEN-MAP-CNST)
                            MAPSET(WS-MEN-MAPSET-CNST)
                            FROM  (MENMF2AO)
                            DATAONLY
                            ALARM
                            RESP  (WS-RESPONSE-CODE)
                            RESP2 (WS-RESPONSE-CODE2)
                   END-EXEC
      *
                   IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                       CONTINUE
                   ELSE
                       MOVE 'SEND MAP     ' TO WS-RT-MSG-HEADER
                       MOVE 'ERROR        ' TO WS-RT-MSG-NORMAL
                       MOVE 'DATAONLY-ALARM'
                         TO WS-RT-MSG-OTHER
                       PERFORM 9700-RESPTEXT
                   END-IF
      *
           END-EVALUATE.
      *
      ******************************************************************
      * ADD PROCEDURE DIVISION COPY STATEMENTS HERE

      *
      ******************************************************************
      *
       COPY RESPTEXT.
      *
       COPY TERMMSG.
      *
       COPY HDLABEND.
      *