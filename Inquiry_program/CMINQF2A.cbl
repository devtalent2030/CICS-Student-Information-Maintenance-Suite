       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CMINQF2A.
       AUTHOR. TALENT NYOTA.
       DATE-WRITTEN. 11-11-2024.
      *PROGRAM DESCRIPTION: COBOL source file for CMINQ program
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
           05 WS-MEN-PROGRAM-CNST           PIC X(08) VALUE 'UUMENYYY'.
           05 WS-INQ-TRANSID-CNST           PIC X(04) VALUE 'IF2A'.
           05 WS-INQ-MAPSET-CNST            PIC X(08) VALUE 'INQSF2A '.
           05 WS-INQ-MAP-CNST               PIC X(08) VALUE 'INQMF2A '.
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
           05 WS-ENTER-CUST-NBR.
               10 FILLER                    PIC X(40)
                   VALUE 'You must enter a customer number.      '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-CUST-DOES-NOT-EXIST.
               10 FILLER                    PIC X(40)
                   VALUE 'That customer does not exist.          '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *
       COPY ATTR.
      *
       COPY DFHAID.
      *
       COPY ERRORSWS.
      *
       COPY INQSF2A.
      *
      ******************************************************************
      * ADD WORKING_STORAGE COPY STATEMENTS HERE
       COPY CMFWSREC.
      *
       COPY CMFWSSTS.
      *
      ******************************************************************
      *
       LINKAGE SECTION.
      *
       01 DFHCOMMAREA                       PIC X.
      *
       PROCEDURE DIVISION.
      *
       0000-PROCESS-CUSTOMER-INQUIRY.
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
                   MOVE LOW-VALUE           TO INQMF2AO
                   MOVE WS-INQ-TRANSID-CNST TO TRANIDO
                   SET WS-SEND-ERASE-88     TO TRUE
                   PERFORM 1400-SEND-CUSTOMER-MAP
      *
               WHEN EIBAID = DFHCLEAR
                   MOVE LOW-VALUE           TO INQMF2AO
                   MOVE WS-INQ-TRANSID-CNST TO TRANIDO
                   SET WS-SEND-ERASE-88     TO TRUE
                   PERFORM 1400-SEND-CUSTOMER-MAP
      *
               WHEN EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
                   CONTINUE
      *
               WHEN EIBAID = DFHPF3 OR DFHPF12
                   PERFORM 8200-CMF-CLOSE

                   PERFORM 9800-SEND-TERMINATION-MESSAGE

                   EXEC CICS
                         RETURN
                   END-EXEC
      *
               WHEN EIBAID = DFHENTER
                   PERFORM 1000-PROCESS-CUSTOMER-MAP
      *
               WHEN OTHER
                   MOVE LOW-VALUE           TO INQMF2AO
                   MOVE WS-KEY-INVALID      TO MESSAGEO
                   SET WS-SEND-DATAONLY-ALARM-88
                    TO TRUE
                   PERFORM 1400-SEND-CUSTOMER-MAP



      *
           END-EVALUATE.
      *
           MOVE SPACES                      TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                     TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                 TO WS-HA-EXEC-TEXT-T3.
           MOVE 'INQUIRE TRANSACTION'       TO WS-HA-EXEC-TEXT-T4.
           MOVE WS-INQ-TRANSID-CNST         TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                      TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-TRANSIDERR)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               INQUIRE TRANSACTION(WS-INQ-TRANSID-CNST)
                       RESP       (WS-RESPONSE-CODE)
                       RESP2      (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               CONTINUE
           ELSE
               MOVE 'INQUIRE TRANSACTION'   TO WS-RT-MSG-HEADER
               MOVE 'ERROR              '   TO WS-RT-MSG-NORMAL
               MOVE WS-INQ-TRANSID-CNST     TO WS-RT-MSG-OTHER
               PERFORM 9700-RESPTEXT
           END-IF.
      *
           MOVE SPACES                      TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                     TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                 TO WS-HA-EXEC-TEXT-T3.
           MOVE 'RETURN TRANSID'            TO WS-HA-EXEC-TEXT-T4.
           MOVE WS-INQ-TRANSID-CNST         TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                      TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-TRANSIDERR)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               RETURN TRANSID (WS-INQ-TRANSID-CNST)
                      COMMAREA(WS-COMMUNICATION-AREA)
                      RESP    (WS-RESPONSE-CODE)
                      RESP2   (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               CONTINUE
           ELSE
               MOVE 'RETURN TRANSID '       TO WS-RT-MSG-HEADER
               MOVE 'ERROR          '       TO WS-RT-MSG-NORMAL
               MOVE WS-INQ-TRANSID-CNST     TO WS-RT-MSG-OTHER
               PERFORM 9700-RESPTEXT
           END-IF.
      *
       1000-PROCESS-CUSTOMER-MAP.
      *
           PERFORM 1100-RECEIVE-CUSTOMER-MAP.
           PERFORM 1200-EDIT-CUSTOMER-DATA.
      *
           IF WS-VALID-DATA-88
               PERFORM 1300-GET-CUSTOMER-RECORD
               SET WS-SEND-DATAONLY-88
                TO TRUE
               PERFORM 1400-SEND-CUSTOMER-MAP
           ELSE
               SET WS-SEND-DATAONLY-ALARM-88
                TO TRUE
               PERFORM 1400-SEND-CUSTOMER-MAP
           END-IF.
      *
       1100-RECEIVE-CUSTOMER-MAP.
      *
           MOVE SPACES                      TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                     TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                 TO WS-HA-EXEC-TEXT-T3.
           MOVE 'RECEIVE MAP - MAPSET/MAP'  TO WS-HA-EXEC-TEXT-T4.
           MOVE WS-INQ-MAPSET-CNST          TO WS-HA-EXEC-TEXT-T5.
           MOVE WS-INQ-MAP-CNST             TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               RECEIVE MAP   (WS-INQ-MAP-CNST)
                       MAPSET(WS-INQ-MAPSET-CNST)
                       INTO  (INQMF2AI)
                       RESP  (WS-RESPONSE-CODE)
                       RESP2 (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               CONTINUE
           ELSE
               MOVE 'RECEIVE MAP - MAPSET/MAP'
                 TO WS-RT-MSG-HEADER
               MOVE WS-INQ-MAPSET-CNST      TO WS-RT-MSG-NORMAL
               MOVE WS-INQ-MAP-CNST         TO WS-RT-MSG-OTHER
               PERFORM 9700-RESPTEXT
           END-IF.
      *
           INSPECT INQMF2AI
               REPLACING ALL '_' BY SPACE.
      *
       1200-EDIT-CUSTOMER-DATA.
      *
           IF       CUSTNOL = ZERO
                 OR CUSTNOI = SPACE
               MOVE WS-FALSE-CNST           TO WS-VALID-DATA-FLAG
               MOVE WS-ENTER-CUST-NBR       TO MESSAGEO
           END-IF.
      *
       1300-GET-CUSTOMER-RECORD.
           PERFORM 8100-CMF-OPEN.

      *
           MOVE SPACES                      TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                     TO WS-HA-EXEC-TEXT-T2.
           MOVE 'READ FILE'
             TO WS-HA-EXEC-TEXT-T3.
           MOVE 'EXEC CICS READ FILE'
             TO WS-HA-EXEC-TEXT-T4.
           MOVE WS-CMF-FILE-NAME-CNST       TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                      TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-FILENOTFOUND)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               READ FILE  (WS-CMF-FILE-NAME-CNST)
                    INTO  (WS-CUSTOMER-MASTER-RECORD)
                    RIDFLD(CUSTNOI)
                    RESP  (WS-RESPONSE-CODE)
                    RESP2 (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               MOVE SPACES                  TO MESSAGEO
               MOVE WS-CM-LAST-NAME         TO LNAMEO
               MOVE WS-CM-FIRST-NAME        TO FNAMEO
               MOVE WS-CM-ADDRESS           TO ADDRO
               MOVE WS-CM-CITY              TO CITYO
               MOVE WS-CM-STATE             TO STATEO
               MOVE WS-CM-ZIP-CODE          TO ZIPCODEO
           ELSE IF WS-RESPONSE-CODE = DFHRESP(NOTFND)
               MOVE WS-FALSE-CNST           TO WS-VALID-DATA-FLAG
               MOVE WS-CUST-DOES-NOT-EXIST  TO MESSAGEO
               MOVE SPACES                  TO LNAMEO
                                               FNAMEO
                                               ADDRO
                                               CITYO
                                               STATEO
                                               ZIPCODEO
           ELSE
               MOVE 'READ FILE      '       TO WS-RT-MSG-HEADER
               MOVE 'ERROR          '       TO WS-RT-MSG-NORMAL
               MOVE WS-CMF-FILE-NAME-CNST   TO WS-RT-MSG-OTHER
               PERFORM 9700-RESPTEXT
           END-IF.

           PERFORM 8200-CMF-CLOSE.



      *
       1400-SEND-CUSTOMER-MAP.
      *
           EVALUATE TRUE
               WHEN WS-SEND-ERASE-88
                   MOVE SPACES              TO WS-HA-EXEC-TEXT
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T1
                   MOVE 'ABEND'             TO WS-HA-EXEC-TEXT-T2
                   MOVE 'EVALUATE WHEN WS-SEND-ERASE-88'
                     TO WS-HA-EXEC-TEXT-T3
                   MOVE 'EXEC CICS SEND MAP - MAPSET/MAP'
                     TO WS-HA-EXEC-TEXT-T4
                   MOVE WS-INQ-MAPSET-CNST  TO WS-HA-EXEC-TEXT-T5
                   MOVE WS-INQ-MAP-CNST     TO WS-HA-EXEC-TEXT-T6
                   MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
                     TO WS-HA-EXEC-TEXT-T7
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T8
      *
                   EXEC CICS
                       SEND MAP   (WS-INQ-MAP-CNST)
                            MAPSET(WS-INQ-MAPSET-CNST)
                            FROM  (INQMF2AO)
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
                   MOVE WS-INQ-MAPSET-CNST  TO WS-HA-EXEC-TEXT-T5
                   MOVE WS-INQ-MAP-CNST     TO WS-HA-EXEC-TEXT-T6
                   MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
                     TO WS-HA-EXEC-TEXT-T7
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T8
      *
                   EXEC CICS
                       SEND MAP   (WS-INQ-MAP-CNST)
                            MAPSET(WS-INQ-MAPSET-CNST)
                            FROM  (INQMF2AO)
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
                   MOVE WS-INQ-MAPSET-CNST  TO WS-HA-EXEC-TEXT-T5
                   MOVE WS-INQ-MAP-CNST     TO WS-HA-EXEC-TEXT-T6
                   MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
                     TO WS-HA-EXEC-TEXT-T7
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T8
      *
                   EXEC CICS
                       SEND MAP   (WS-INQ-MAP-CNST)
                            MAPSET(WS-INQ-MAPSET-CNST)
                            FROM  (INQMF2AO)
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
       COPY CMFOPEN.
      *
       COPY CMFCLOSE.
      *
      ******************************************************************
      *
       COPY RESPTEXT.
      *
       COPY TERMMSG.
      *
       COPY HDLABEND.
      *