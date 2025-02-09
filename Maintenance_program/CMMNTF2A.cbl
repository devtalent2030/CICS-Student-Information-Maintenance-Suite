       IDENTIFICATION DIVISION.
       PROGRAM-ID. CMMNTF2A.
       AUTHOR. TALENT NYOTA.
       DATE-WRITTEN. 22-11-2024.
      *PROGRAM DESCRIPTION: COBOL source for MAINT program
      *
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
       01 WS-COMMUNICATION-AREA.
           05 WS-CA-CONTEXT-FLAG            PIC X(01).
               88 WS-CA-PROCESS-KEY-MAP-88        VALUE '1'.
               88 WS-CA-PROCESS-ADD-CUST-88       VALUE '2'.
               88 WS-CA-PROCESS-CHG-CUST-88       VALUE '3'.
               88 WS-CA-PROCESS-DEL-CUST-88       VALUE '4'.
           05 WS-CA-CUSTOMER-RECORD.
               10 WS-CA-CUSTOMER-NUMBER     PIC X(06).
               10 FILLER                    PIC X(112).
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
           05 WS-MEN-PROGRAM-CNST           PIC X(08) VALUE 'UUMENF2A'.
           05 WS-MNT-TRANSID-CNST           PIC X(04) VALUE 'VF2A'.
           05 WS-MNT-MAPSET-CNST            PIC X(07) VALUE 'MNTSF2A'.
           05 WS-MNT-MAP1-CNST              PIC X(07) VALUE 'MNT1F2A'.
           05 WS-MNT-MAP2-CNST              PIC X(07) VALUE 'MNT2F2A'.
      *
       01 WS-FILE-CNSTS.
           05 WS-CMF-FILE-NAME-CNST         PIC X(08) VALUE 'CMFF2A  '.
      *
       01 WS-FLAGS.
           05 WS-VALID-DATA-FLAG            PIC X(01) VALUE 'Y'.
               88 WS-VALID-DATA-88                    VALUE 'Y'.
           05 WS-SEND-FLAG                  PIC X(01).
               88 WS-SEND-ERASE-88                VALUE '1'.
               88 WS-SEND-ERASE-ALARM-88          VALUE '2'.
               88 WS-SEND-DATAONLY-88             VALUE '3'.
               88 WS-SEND-DATAONLY-ALARM-88       VALUE '4'.
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
           05 WS-ADD.
               10 FILLER                    PIC X(40)
                   VALUE 'Type information for new customer. Then'.
               10 FILLER                    PIC X(39)
                   VALUE ' Press Enter.                         '.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-CHG.
               10 FILLER                    PIC X(40)
                   VALUE 'Type changes.  Then press Enter.       '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-DEL.
               10 FILLER                    PIC X(40)
                   VALUE 'Press Enter to delete this customer or '.
               10 FILLER                    PIC X(39)
                   VALUE 'press F12                             '.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-KEY-UNASSIGNED.
               10 FILLER                    PIC X(40)
                   VALUE 'That key is unassigned.                '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-ACTION-INVALID.
               10 FILLER                    PIC X(40)
                   VALUE 'Action must be 1, 2, or 3.             '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-ENTER-CUST-NBR.
               10 FILLER                    PIC X(40)
                   VALUE 'You must enter a customer number.      '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-CUST-EXISTS.
               10 FILLER                    PIC X(40)
                   VALUE 'That customer already exists.          '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-CUST-DOES-NOT-EXIST.
               10 FILLER                    PIC X(40)
                   VALUE 'That customer does not exist.          '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-CUST-ADDED.
               10 FILLER                    PIC X(40)
                   VALUE 'Customer record added.                 '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-CUST-ADDED-BY-ANOTHER.
               10 FILLER                    PIC X(40)
                   VALUE 'Another user has added a record with th'.
               10 FILLER                    PIC X(39)
                   VALUE 'at customer number.                   '.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-CUST-UPDATED.
               10 FILLER                    PIC X(40)
                   VALUE 'Customer record updated.               '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-CUST-UPDATED-BY-ANOTHER.
               10 FILLER                    PIC X(40)
                   VALUE 'Another user has updated the record. Tr'.
               10 FILLER                    PIC X(39)
                   VALUE 'y again.                              '.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-CUST-DELETED.
               10 FILLER                    PIC X(40)
                   VALUE 'Customer record deleted.               '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-CUST-DELETED-BY-ANOTHER.
               10 FILLER                    PIC X(40)
                   VALUE 'Another user has deleted the record. Tr'.
               10 FILLER                    PIC X(39)
                   VALUE 'y again.                              '.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-ENTER-ZIP-CODE.
               10 FILLER                    PIC X(40)
                   VALUE 'You must enter a zip code.             '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-ENTER-STATE.
               10 FILLER                    PIC X(40)
                   VALUE 'You must enter a state.                '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-ENTER-CITY.
               10 FILLER                    PIC X(40)
                   VALUE 'You must enter a city.                 '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-ENTER-ADDRESS.
               10 FILLER                    PIC X(40)
                   VALUE 'You must enter a address.              '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-ENTER-FIRST-NAME.
               10 FILLER                    PIC X(40)
                   VALUE 'You must enter a first name.           '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *                   ----+----1----+----2----+----3----+----4
           05 WS-ENTER-LAST-NAME.
               10 FILLER                    PIC X(40)
                   VALUE 'You must enter a last name.            '.
               10 FILLER                    PIC X(39)
                   VALUE SPACES.
      *
       COPY ATTR.
      *
       COPY DFHAID.
      *
       COPY ERRORSWS.
      *
       COPY MNTSF2A.
      *
      ******************************************************************
      * ADD WORKING_STORAGE COPY STATEMENTS HERE

       COPY CMFWSSTS.
      *
       COPY CMFWSREC.
      *
      ******************************************************************
      *
       LINKAGE SECTION.
      *
       01 DFHCOMMAREA                       PIC X(119).
      *
       PROCEDURE DIVISION.
      *
       0000-PROCESS-CUSTOMER-MAINT.
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
           IF EIBCALEN > ZERO
               MOVE DFHCOMMAREA             TO WS-COMMUNICATION-AREA
           END-IF.
      *
           EVALUATE TRUE
      *
               WHEN EIBCALEN = ZERO
                   MOVE LOW-VALUE           TO MNT1F2AO
                   MOVE -1                  TO CUSTNO1L
                   SET WS-SEND-ERASE-88     TO TRUE
                   PERFORM 1500-SEND-KEY-MAP
                   SET WS-CA-PROCESS-KEY-MAP-88
                      TO TRUE
      *
               WHEN EIBAID = DFHCLEAR
                   IF WS-CA-PROCESS-KEY-MAP-88
                       MOVE LOW-VALUE       TO MNT1F2AO
                       MOVE -1              TO CUSTNO1L
                       SET WS-SEND-ERASE-88 TO TRUE
                       PERFORM 1500-SEND-KEY-MAP
                   ELSE
                       MOVE LOW-VALUE       TO MNT2F2AO
                       MOVE WS-CA-CUSTOMER-NUMBER
                         TO CUSTNO2O
                       EVALUATE TRUE
                           WHEN WS-CA-PROCESS-ADD-CUST-88
                               MOVE WS-ADD  TO INSTR2O
                           WHEN WS-CA-PROCESS-CHG-CUST-88
                               MOVE WS-CHG  TO INSTR2O
                           WHEN WS-CA-PROCESS-DEL-CUST-88
                               MOVE WS-DEL  TO INSTR2O
                       END-EVALUATE
                       MOVE -1              TO LNAMEL
                       SET WS-SEND-ERASE-88 TO TRUE
                       PERFORM 1400-SEND-DATA-MAP
                   END-IF
      *
               WHEN EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
                   CONTINUE
      *
               WHEN EIBAID = DFHPF3
                   PERFORM 8200-CMF-CLOSE      *> Close VSAM before exit

                   MOVE SPACES              TO WS-HA-EXEC-TEXT
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T1
                   MOVE 'ABEND DFHPF3'      TO WS-HA-EXEC-TEXT-T2
                   MOVE 'EXEC CICS'         TO WS-HA-EXEC-TEXT-T3
                   MOVE 'XCTL PROGRAM'      TO WS-HA-EXEC-TEXT-T4
                   MOVE WS-MEN-PROGRAM-CNST TO WS-HA-EXEC-TEXT-T5
                   MOVE SPACES              TO WS-HA-EXEC-TEXT-T6
                   MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-PGMIDERR)
                     TO WS-HA-EXEC-TEXT-T7
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T8

                   EXEC CICS
                       XCTL PROGRAM(WS-MEN-PROGRAM-CNST)
                            RESP   (WS-RESPONSE-CODE)
                            RESP2  (WS-RESPONSE-CODE2)
                   END-EXEC

                   IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                       EXEC CICS
                           RETURN
                       END-EXEC
                   ELSE
                       MOVE 'XCTL PROGRAM'  TO WS-RT-MSG-HEADER
                       MOVE 'ERROR DFHPF3 ' TO WS-RT-MSG-NORMAL
                       MOVE WS-MEN-PROGRAM-CNST
                         TO WS-RT-MSG-OTHER
                       PERFORM 9700-RESPTEXT
                   END-IF
      *
               WHEN EIBAID = DFHPF12
                   IF WS-CA-PROCESS-KEY-MAP-88
                       PERFORM 8200-CMF-CLOSE  *> Close VSAM

                       MOVE SPACES          TO WS-HA-EXEC-TEXT
                       MOVE ALL '='         TO WS-HA-EXEC-TEXT-T1
                       MOVE 'ABEND DFHPF12' TO WS-HA-EXEC-TEXT-T2
                       MOVE 'EXEC CICS'     TO WS-HA-EXEC-TEXT-T3
                       MOVE 'XCTL PROGRAM'  TO WS-HA-EXEC-TEXT-T4
                       MOVE WS-MEN-PROGRAM-CNST
                         TO WS-HA-EXEC-TEXT-T5
                       MOVE SPACES          TO WS-HA-EXEC-TEXT-T6
                       MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-PGMIDERR)
                         TO WS-HA-EXEC-TEXT-T7
                       MOVE ALL '='         TO WS-HA-EXEC-TEXT-T8

                       EXEC CICS
                           XCTL PROGRAM(WS-MEN-PROGRAM-CNST)
                                RESP   (WS-RESPONSE-CODE)
                                RESP2  (WS-RESPONSE-CODE2)
                       END-EXEC

                       IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                           EXEC CICS
                               RETURN
                           END-EXEC
                       ELSE
                           MOVE 'XCTL PROGRAM'
                             TO WS-RT-MSG-HEADER
                           MOVE 'ERROR DFHPF12'
                             TO WS-RT-MSG-NORMAL
                           MOVE WS-MEN-PROGRAM-CNST
                             TO WS-RT-MSG-OTHER
                           PERFORM 9700-RESPTEXT
                       END-IF
                   ELSE
                       MOVE LOW-VALUE       TO MNT1F2AO
                       MOVE -1              TO CUSTNO1L
                       SET WS-SEND-ERASE-88 TO TRUE
                       PERFORM 1500-SEND-KEY-MAP
                       SET WS-CA-PROCESS-KEY-MAP-88
                          TO TRUE
                   END-IF
      *
               WHEN EIBAID = DFHENTER
                   PERFORM 8100-CMF-OPEN      *> Open VSAM
                   EVALUATE TRUE
                       WHEN WS-CA-PROCESS-KEY-MAP-88
                           PERFORM 1000-PROCESS-KEY-MAP
                       WHEN WS-CA-PROCESS-ADD-CUST-88
                           PERFORM 2000-PROCESS-ADD-CUSTOMER
                       WHEN WS-CA-PROCESS-CHG-CUST-88
                           PERFORM 3000-PROCESS-CHANGE-CUSTOMER
                       WHEN WS-CA-PROCESS-DEL-CUST-88
                           PERFORM 4000-PROCESS-DELETE-CUSTOMER
                   END-EVALUATE
                   PERFORM 8200-CMF-CLOSE
      *
               WHEN OTHER
                   IF WS-CA-PROCESS-KEY-MAP-88
                       MOVE LOW-VALUE       TO MNT1F2AO
                       MOVE WS-KEY-UNASSIGNED
                          TO MSG1O
                       MOVE -1              TO CUSTNO1L
                       SET WS-SEND-DATAONLY-ALARM-88
                           TO TRUE
                       PERFORM 1500-SEND-KEY-MAP
                   ELSE
                       MOVE LOW-VALUE       TO MNT2F2AO
                       MOVE WS-KEY-UNASSIGNED
                         TO MSG2O
                       MOVE -1              TO LNAMEL
                       SET WS-SEND-DATAONLY-ALARM-88
                           TO TRUE
                       PERFORM 1400-SEND-DATA-MAP
                   END-IF
      *
           END-EVALUATE.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                       TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                   TO WS-HA-EXEC-TEXT-T3.
           MOVE 'INQUIRE TRANSACTION'         TO WS-HA-EXEC-TEXT-T4.
           MOVE WS-MNT-TRANSID-CNST           TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                        TO WS-HA-EXEC-TEXT-T6.

           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-TRANSIDERR)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               INQUIRE TRANSACTION(WS-MNT-TRANSID-CNST)
                       RESP       (WS-RESPONSE-CODE)
                       RESP2      (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               CONTINUE
           ELSE
               MOVE 'INQUIRE TRANSACTION'     TO WS-RT-MSG-HEADER
               MOVE 'ERROR              '     TO WS-RT-MSG-NORMAL
               MOVE WS-MNT-TRANSID-CNST       TO WS-RT-MSG-OTHER
               PERFORM 9700-RESPTEXT
           END-IF.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                       TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                   TO WS-HA-EXEC-TEXT-T3.
           MOVE 'RETURN TRANSID'              TO WS-HA-EXEC-TEXT-T4.
           MOVE WS-MNT-TRANSID-CNST           TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                        TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-TRANSIDERR)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               RETURN TRANSID (WS-MNT-TRANSID-CNST)
                      COMMAREA(WS-COMMUNICATION-AREA)
                      RESP    (WS-RESPONSE-CODE)
                      RESP2   (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               CONTINUE
           ELSE
               MOVE 'RETURN TRANSID '         TO WS-RT-MSG-HEADER
               MOVE 'ERROR          '         TO WS-RT-MSG-NORMAL
               MOVE WS-MNT-TRANSID-CNST       TO WS-RT-MSG-OTHER
               PERFORM 9700-RESPTEXT
           END-IF.
      *
       1000-PROCESS-KEY-MAP.
      *
           PERFORM 1100-RECEIVE-KEY-MAP.
           PERFORM 1200-EDIT-KEY-DATA.
           IF WS-VALID-DATA-88
               IF NOT WS-CA-PROCESS-DEL-CUST-88
                   INSPECT WS-CUSTOMER-MASTER-RECORD
                       REPLACING ALL SPACE BY '_'
               END-IF
               MOVE CUSTNO1I                TO CUSTNO2O
               MOVE WS-CM-LAST-NAME         TO LNAMEO
               MOVE WS-CM-FIRST-NAME        TO FNAMEO
               MOVE WS-CM-ADDRESS           TO ADDRO
               MOVE WS-CM-CITY              TO CITYO
               MOVE WS-CM-STATE             TO STATEO
               MOVE WS-CM-ZIP-CODE          TO ZIPCODEO
               MOVE -1                      TO LNAMEL
               SET WS-SEND-ERASE-88         TO TRUE
               PERFORM 1400-SEND-DATA-MAP
           ELSE
               MOVE LOW-VALUE               TO CUSTNO1O
                                               ACTIONO
               SET WS-SEND-DATAONLY-ALARM-88
                   TO TRUE
               PERFORM 1500-SEND-KEY-MAP
           END-IF.
      *
       1100-RECEIVE-KEY-MAP.
      *
           MOVE SPACES                        TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                       TO WS-HA-EXEC-TEXT-T2.
           MOVE 'EXEC CICS'                   TO WS-HA-EXEC-TEXT-T3.
           MOVE 'RECEIVE MAP - MAPSET/MAP'    TO WS-HA-EXEC-TEXT-T4.
           MOVE WS-MNT-MAPSET-CNST            TO WS-HA-EXEC-TEXT-T5.
           MOVE WS-MNT-MAP1-CNST              TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                       TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               RECEIVE MAP   (WS-MNT-MAP1-CNST)
                       MAPSET(WS-MNT-MAPSET-CNST)
                       INTO  (MNT1F2AI)
                       RESP  (WS-RESPONSE-CODE)
                       RESP2 (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               MOVE 'RECEIVE MAP - MAPSET/MAP'
                 TO WS-RT-MSG-HEADER
               MOVE WS-MNT-MAP1-CNST          TO WS-RT-MSG-NORMAL
               MOVE WS-MNT-MAPSET-CNST        TO WS-RT-MSG-OTHER
               PERFORM 9700-RESPTEXT
           END-IF.
      *
           INSPECT MNT1F2AI
               REPLACING ALL '_' BY SPACE.
      *
       1200-EDIT-KEY-DATA.
      *
           MOVE ATTR-NO-HIGHLIGHT           TO ACTIONH
                                               CUSTNO1H.
      *
           IF ACTIONI NOT = '1' AND '2' AND '3'
               MOVE ATTR-REVERSE            TO ACTIONH
               MOVE -1                      TO ACTIONL
               MOVE WS-ACTION-INVALID       TO MSG1O
               MOVE WS-FALSE-CNST           TO WS-VALID-DATA-FLAG
           END-IF.
      *
           IF       CUSTNO1L = ZERO
                 OR CUSTNO1I = SPACE
               MOVE ATTR-REVERSE            TO CUSTNO1H
               MOVE -1                      TO CUSTNO1L
               MOVE WS-ENTER-CUST-NBR       TO MSG1O
               MOVE WS-FALSE-CNST           TO WS-VALID-DATA-FLAG
           END-IF.
      *
           IF WS-VALID-DATA-88
               MOVE LOW-VALUE               TO MNT2F2AO
               EVALUATE ACTIONI
                   WHEN '1'
                       PERFORM 1300-READ-CUSTOMER-RECORD
                       IF WS-RESPONSE-CODE = DFHRESP(NOTFND)
                           MOVE WS-ADD      TO INSTR2O
                           SET WS-CA-PROCESS-ADD-CUST-88
                             TO TRUE
                           MOVE SPACE       TO WS-CUSTOMER-MASTER-RECORD
                       ELSE
                           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                               MOVE WS-CUST-EXISTS
                                 TO MSG1O
                               MOVE WS-FALSE-CNST
                                 TO WS-VALID-DATA-FLAG
                           END-IF
                       END-IF

                   WHEN '2'
                       PERFORM 1300-READ-CUSTOMER-RECORD
                       IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                           MOVE WS-CUSTOMER-MASTER-RECORD
                             TO WS-CA-CUSTOMER-RECORD
                           MOVE WS-CHG      TO INSTR2O
                           SET WS-CA-PROCESS-CHG-CUST-88
                             TO TRUE
                       ELSE
                           IF WS-RESPONSE-CODE = DFHRESP(NOTFND)
                               MOVE WS-CUST-DOES-NOT-EXIST
                                 TO MSG1O
                               MOVE WS-FALSE-CNST
                                 TO WS-VALID-DATA-FLAG
                           END-IF
                       END-IF

                   WHEN '3'
                       PERFORM 1300-READ-CUSTOMER-RECORD
                       IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                           MOVE WS-CUSTOMER-MASTER-RECORD
                             TO WS-CA-CUSTOMER-RECORD
                           MOVE WS-DEL      TO INSTR2O
                           SET WS-CA-PROCESS-DEL-CUST-88
                             TO TRUE
                           MOVE ATTR-PROT   TO LNAMEA
                                               FNAMEA
                                               ADDRA
                                               CITYA
                                               STATEA
                                               ZIPCODEA
                       ELSE
                           IF WS-RESPONSE-CODE = DFHRESP(NOTFND)
                               MOVE WS-CUST-DOES-NOT-EXIST
                                 TO MSG1O
                               MOVE WS-FALSE-CNST
                                 TO WS-VALID-DATA-FLAG
                           END-IF
                       END-IF
               END-EVALUATE.
      *
       1300-READ-CUSTOMER-RECORD.
           PERFORM 8100-CMF-OPEN.
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
                    RIDFLD(CUSTNO1I)
                    RESP  (WS-RESPONSE-CODE)
                    RESP2 (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF     WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
              AND WS-RESPONSE-CODE NOT = DFHRESP(NOTFND)
               MOVE 'READ FILE      '       TO WS-RT-MSG-HEADER
               MOVE 'ERROR          '       TO WS-RT-MSG-NORMAL
               MOVE WS-CMF-FILE-NAME-CNST   TO WS-RT-MSG-OTHER
               PERFORM 9700-RESPTEXT
           END-IF.
      *
       1400-SEND-DATA-MAP.
      *
           MOVE WS-MNT-TRANSID-CNST         TO TRANID2O.
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
                   MOVE WS-MNT-MAPSET-CNST  TO WS-HA-EXEC-TEXT-T5
                   MOVE WS-MNT-MAP2-CNST    TO WS-HA-EXEC-TEXT-T6
                   MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
                     TO WS-HA-EXEC-TEXT-T7
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T8
      *
                   EXEC CICS
                       SEND MAP   (WS-MNT-MAP2-CNST)
                            MAPSET(WS-MNT-MAPSET-CNST)
                            FROM  (MNT2F2AO)
                            ERASE
                            CURSOR
                            RESP  (WS-RESPONSE-CODE)
                            RESP2 (WS-RESPONSE-CODE2)
                   END-EXEC
      *
                   IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                       CONTINUE
                   ELSE
                       MOVE 'SEND MAP2    ' TO WS-RT-MSG-HEADER
                       MOVE 'ERROR        ' TO WS-RT-MSG-NORMAL
                       MOVE 'ERASE        ' TO WS-RT-MSG-OTHER
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
                   MOVE WS-MNT-MAPSET-CNST  TO WS-HA-EXEC-TEXT-T5
                   MOVE WS-MNT-MAP2-CNST    TO WS-HA-EXEC-TEXT-T6
                   MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
                     TO WS-HA-EXEC-TEXT-T7
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T8
      *
                   EXEC CICS
                       SEND MAP   (WS-MNT-MAP2-CNST)
                            MAPSET(WS-MNT-MAPSET-CNST)
                            FROM  (MNT2F2AO)
                            DATAONLY
                            ALARM
                            CURSOR
                            RESP  (WS-RESPONSE-CODE)
                            RESP2 (WS-RESPONSE-CODE2)
                   END-EXEC
      *
                   IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                       CONTINUE
                   ELSE
                       MOVE 'SEND MAP2    ' TO WS-RT-MSG-HEADER
                       MOVE 'ERROR        ' TO WS-RT-MSG-NORMAL
                       MOVE 'DATAONLY-ALARM'
                         TO WS-RT-MSG-OTHER
                       PERFORM 9700-RESPTEXT
                   END-IF
      *
           END-EVALUATE.
      *
       1500-SEND-KEY-MAP.
      *
           MOVE WS-MNT-TRANSID-CNST         TO TRANID1O.
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
                   MOVE WS-MNT-MAPSET-CNST  TO WS-HA-EXEC-TEXT-T5
                   MOVE WS-MNT-MAP1-CNST    TO WS-HA-EXEC-TEXT-T6
                   MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
                     TO WS-HA-EXEC-TEXT-T7
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T8
      *
                   EXEC CICS
                       SEND MAP   (WS-MNT-MAP1-CNST)
                            MAPSET(WS-MNT-MAPSET-CNST)
                            FROM  (MNT1F2AO)
                            ERASE
                            CURSOR
                            RESP  (WS-RESPONSE-CODE)
                            RESP2 (WS-RESPONSE-CODE2)
                   END-EXEC
      *
                   IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                       CONTINUE
                   ELSE
                       MOVE 'SEND MAP1    ' TO WS-RT-MSG-HEADER
                       MOVE 'ERROR        ' TO WS-RT-MSG-NORMAL
                       MOVE 'ERASE        ' TO WS-RT-MSG-OTHER
                       PERFORM 9700-RESPTEXT
                   END-IF
      *
               WHEN WS-SEND-ERASE-ALARM-88
                   MOVE SPACES              TO WS-HA-EXEC-TEXT
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T1
                   MOVE 'ABEND'             TO WS-HA-EXEC-TEXT-T2
                   MOVE 'EVALUATE WHEN WS-SEND-ERASE-ALARM-88'
                     TO WS-HA-EXEC-TEXT-T3
                   MOVE 'EXEC CICS SEND MAP - MAPSET/MAP'
                     TO WS-HA-EXEC-TEXT-T4
                   MOVE WS-MNT-MAPSET-CNST  TO WS-HA-EXEC-TEXT-T5
                   MOVE WS-MNT-MAP1-CNST    TO WS-HA-EXEC-TEXT-T6
                   MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
                     TO WS-HA-EXEC-TEXT-T7
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T8
      *
                   EXEC CICS
                       SEND MAP   (WS-MNT-MAP1-CNST)
                            MAPSET(WS-MNT-MAPSET-CNST)
                            FROM  (MNT1F2AO)
                            ERASE
                            ALARM
                            CURSOR
                            RESP  (WS-RESPONSE-CODE)
                            RESP2 (WS-RESPONSE-CODE2)
                   END-EXEC
      *
                   IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                       CONTINUE
                   ELSE
                       MOVE 'SEND MAP1    ' TO WS-RT-MSG-HEADER
                       MOVE 'ERROR        ' TO WS-RT-MSG-NORMAL
                       MOVE 'ERASE ALARM  ' TO WS-RT-MSG-OTHER
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
                   MOVE WS-MNT-MAPSET-CNST  TO WS-HA-EXEC-TEXT-T5
                   MOVE WS-MNT-MAP1-CNST    TO WS-HA-EXEC-TEXT-T6
                   MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
                     TO WS-HA-EXEC-TEXT-T7
                   MOVE ALL '='             TO WS-HA-EXEC-TEXT-T8
      *
                   EXEC CICS
                       SEND MAP   (WS-MNT-MAP1-CNST)
                            MAPSET(WS-MNT-MAPSET-CNST)
                            FROM  (MNT1F2AO)
                            DATAONLY
                            ALARM
                            CURSOR
                            RESP  (WS-RESPONSE-CODE)
                            RESP2 (WS-RESPONSE-CODE2)
                   END-EXEC
      *
                   IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                       CONTINUE
                   ELSE
                       MOVE 'SEND MAP1    ' TO WS-RT-MSG-HEADER
                       MOVE 'ERROR        ' TO WS-RT-MSG-NORMAL
                       MOVE 'DATAONLY ALARM'
                         TO WS-RT-MSG-OTHER
                       PERFORM 9700-RESPTEXT
                   END-IF
      *
           END-EVALUATE.
      *
       2000-PROCESS-ADD-CUSTOMER.
      *
           PERFORM 2100-RECEIVE-DATA-MAP.
           PERFORM 2200-EDIT-CUSTOMER-DATA.
           IF WS-VALID-DATA-88
               PERFORM 2300-WRITE-CUSTOMER-RECORD
               IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                   MOVE WS-CUST-ADDED       TO MSG1O
                   SET WS-SEND-ERASE-88     TO TRUE
               ELSE
                   IF WS-RESPONSE-CODE = DFHRESP(DUPREC)
                       MOVE WS-CUST-ADDED-BY-ANOTHER
                         TO MSG1O
                       SET WS-SEND-ERASE-ALARM-88
                           TO TRUE
                   END-IF
               END-IF
               MOVE -1                      TO CUSTNO1L
               PERFORM 1500-SEND-KEY-MAP
               SET WS-CA-PROCESS-KEY-MAP-88 TO TRUE
           ELSE
               MOVE LOW-VALUE               TO LNAMEO
                                               FNAMEO
                                               ADDRO
                                               CITYO
                                               STATEO
                                               ZIPCODEO
               SET WS-SEND-DATAONLY-ALARM-88
                   TO TRUE
               PERFORM 1400-SEND-DATA-MAP
           END-IF.
      *
       2100-RECEIVE-DATA-MAP.
      *
           MOVE SPACES                      TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                     TO WS-HA-EXEC-TEXT-T2.
           MOVE 'RECEIVE DATA MAP'
             TO WS-HA-EXEC-TEXT-T3.
           MOVE 'EXEC CICS RECEIVE MAP - MAPSET/MAP'
             TO WS-HA-EXEC-TEXT-T4.
           MOVE WS-MNT-MAPSET-CNST          TO WS-HA-EXEC-TEXT-T5.
           MOVE WS-MNT-MAP2-CNST            TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-MAPFAIL)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               RECEIVE MAP   (WS-MNT-MAP2-CNST)
                       MAPSET(WS-MNT-MAPSET-CNST)
                       INTO  (MNT2F2AI)
                       RESP  (WS-RESPONSE-CODE)
                       RESP2 (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               CONTINUE
           ELSE
               MOVE 'SEND MAP2    ' TO WS-RT-MSG-HEADER
               MOVE 'ERROR        ' TO WS-RT-MSG-NORMAL
               MOVE 'RECEIVE DATA MAP'
                 TO WS-RT-MSG-OTHER
               PERFORM 9700-RESPTEXT
           END-IF.
      *
           INSPECT MNT2F2AI
               REPLACING ALL '_' BY SPACE.
      *
       2200-EDIT-CUSTOMER-DATA.
      *
           MOVE ATTR-NO-HIGHLIGHT           TO ZIPCODEH
                                               STATEH
                                               CITYH
                                               ADDRH
                                               FNAMEH
                                               LNAMEH.
      *
           IF       ZIPCODEI = SPACE
                 OR ZIPCODEL = ZERO
               MOVE ATTR-REVERSE            TO ZIPCODEH
               MOVE -1                      TO ZIPCODEL
               MOVE WS-ENTER-ZIP-CODE       TO MSG2O
               MOVE WS-FALSE-CNST           TO WS-VALID-DATA-FLAG
           END-IF.

           IF       STATEI = SPACE
                 OR STATEL = ZERO
               MOVE ATTR-REVERSE            TO STATEH
               MOVE -1                      TO STATEL
               MOVE WS-ENTER-STATE          TO MSG2O
               MOVE WS-FALSE-CNST           TO WS-VALID-DATA-FLAG
           END-IF.

           IF       CITYI = SPACE
                 OR CITYL = ZERO
               MOVE ATTR-REVERSE            TO CITYH
               MOVE -1                      TO CITYL
               MOVE WS-ENTER-CITY           TO MSG2O
               MOVE WS-FALSE-CNST           TO WS-VALID-DATA-FLAG
           END-IF.

           IF       ADDRI = SPACE
                 OR ADDRL = ZERO
               MOVE ATTR-REVERSE            TO ADDRH
               MOVE -1                      TO ADDRL
               MOVE WS-ENTER-ADDRESS        TO MSG2O
               MOVE WS-FALSE-CNST           TO WS-VALID-DATA-FLAG
           END-IF.

           IF       FNAMEI = SPACE
                 OR FNAMEL = ZERO
               MOVE ATTR-REVERSE            TO FNAMEH
               MOVE -1                      TO FNAMEL
               MOVE WS-ENTER-FIRST-NAME     TO MSG2O
               MOVE WS-FALSE-CNST           TO WS-VALID-DATA-FLAG
           END-IF.

           IF       LNAMEI = SPACE
                 OR LNAMEL = ZERO
               MOVE ATTR-REVERSE            TO LNAMEH
               MOVE -1                      TO LNAMEL
               MOVE WS-ENTER-LAST-NAME      TO MSG2O
               MOVE WS-FALSE-CNST           TO WS-VALID-DATA-FLAG
           END-IF.
      *
       2300-WRITE-CUSTOMER-RECORD.
      *
           PERFORM 8100-CMF-OPEN.
           MOVE CUSTNO2I                    TO WS-CM-CUSTOMER-NUMBER.
           MOVE LNAMEI                      TO WS-CM-LAST-NAME.
           MOVE FNAMEI                      TO WS-CM-FIRST-NAME.
           MOVE ADDRI                       TO WS-CM-ADDRESS.
           MOVE CITYI                       TO WS-CM-CITY.
           MOVE STATEI                      TO WS-CM-STATE.
           MOVE ZIPCODEI                    TO WS-CM-ZIP-CODE.
      *
           MOVE SPACES                      TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                     TO WS-HA-EXEC-TEXT-T2.
           MOVE 'WRITE FILE'
             TO WS-HA-EXEC-TEXT-T3.
           MOVE 'EXEC CICS WRITE FILE'
             TO WS-HA-EXEC-TEXT-T4.
           MOVE WS-CMF-FILE-NAME-CNST       TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                      TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-FILENOTFOUND)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               WRITE FILE  (WS-CMF-FILE-NAME-CNST)
                     FROM  (WS-CUSTOMER-MASTER-RECORD)
                     RIDFLD(WS-CM-CUSTOMER-NUMBER)
                     RESP  (WS-RESPONSE-CODE)
                     RESP2 (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF      WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               AND WS-RESPONSE-CODE NOT = DFHRESP(DUPREC)
               PERFORM 9700-RESPTEXT
           END-IF.
      *
       3000-PROCESS-CHANGE-CUSTOMER.
      *
           PERFORM 2100-RECEIVE-DATA-MAP.
           PERFORM 2200-EDIT-CUSTOMER-DATA.
           IF WS-VALID-DATA-88
               MOVE CUSTNO2I                TO WS-CM-CUSTOMER-NUMBER
               PERFORM 3100-READ-CUSTOMER-FOR-UPDATE
               IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
                   IF WS-CUSTOMER-MASTER-RECORD = WS-CA-CUSTOMER-RECORD
                       PERFORM 3200-REWRITE-CUSTOMER-RECORD
                       MOVE WS-CUST-UPDATED TO MSG1O
                       SET WS-SEND-ERASE-88 TO TRUE
                   ELSE
                       MOVE WS-CUST-UPDATED-BY-ANOTHER
                         TO MSG1O
                       SET WS-SEND-ERASE-ALARM-88
                           TO TRUE
                   END-IF
               ELSE
                   IF WS-RESPONSE-CODE = DFHRESP(NOTFND)
                       MOVE WS-CUST-DELETED-BY-ANOTHER
                         TO MSG1O
                       SET WS-SEND-ERASE-ALARM-88
                           TO TRUE
                   END-IF
               END-IF
               MOVE -1                      TO CUSTNO1L
               PERFORM 1500-SEND-KEY-MAP
               SET WS-CA-PROCESS-KEY-MAP-88 TO TRUE
           ELSE
               MOVE LOW-VALUE               TO LNAMEO
                                               FNAMEO
                                               ADDRO
                                               CITYO
                                               STATEO
                                               ZIPCODEO
               SET WS-SEND-DATAONLY-ALARM-88
                   TO TRUE
               PERFORM 1400-SEND-DATA-MAP
           END-IF.
      *
       3100-READ-CUSTOMER-FOR-UPDATE.
      *
           PERFORM 8100-CMF-OPEN.
           MOVE SPACES                      TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                     TO WS-HA-EXEC-TEXT-T2.
           MOVE 'READ FILE UPDATE'
             TO WS-HA-EXEC-TEXT-T3.
           MOVE 'EXEC CICS READ FILE UPDATE'
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
                    RIDFLD(WS-CM-CUSTOMER-NUMBER)
                    UPDATE
                    RESP  (WS-RESPONSE-CODE)
                    RESP2 (WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF      WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               AND WS-RESPONSE-CODE NOT = DFHRESP(NOTFND)
               PERFORM 9700-RESPTEXT
           END-IF.
      *
       3200-REWRITE-CUSTOMER-RECORD.
      *
           PERFORM 8100-CMF-OPEN.
           MOVE LNAMEI                      TO WS-CM-LAST-NAME.
           MOVE FNAMEI                      TO WS-CM-FIRST-NAME.
           MOVE ADDRI                       TO WS-CM-ADDRESS.
           MOVE CITYI                       TO WS-CM-CITY.
           MOVE STATEI                      TO WS-CM-STATE.
           MOVE ZIPCODEI                    TO WS-CM-ZIP-CODE.
      *
           MOVE SPACES                      TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                     TO WS-HA-EXEC-TEXT-T2.
           MOVE 'REWRITE FILE'
             TO WS-HA-EXEC-TEXT-T3.
           MOVE 'EXEC CICS REWRITE FILE'
             TO WS-HA-EXEC-TEXT-T4.
           MOVE WS-CMF-FILE-NAME-CNST       TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                      TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-FILENOTFOUND)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               REWRITE FILE (WS-CMF-FILE-NAME-CNST)
                       FROM (WS-CUSTOMER-MASTER-RECORD)
                       RESP (WS-RESPONSE-CODE)
                       RESP2(WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM 9700-RESPTEXT
           END-IF.
      *
       4000-PROCESS-DELETE-CUSTOMER.
      *
           MOVE WS-CA-CUSTOMER-NUMBER       TO WS-CM-CUSTOMER-NUMBER.
           PERFORM 3100-READ-CUSTOMER-FOR-UPDATE.
           IF WS-RESPONSE-CODE = DFHRESP(NORMAL)
               IF WS-CUSTOMER-MASTER-RECORD = WS-CA-CUSTOMER-RECORD
                   PERFORM 4100-DELETE-CUSTOMER-RECORD
                   MOVE 'Customer deleted.' TO MSG1O
                   SET WS-SEND-ERASE-88     TO TRUE
               ELSE
                   MOVE WS-CUST-UPDATED-BY-ANOTHER
                     TO MSG1O
                   SET WS-SEND-ERASE-ALARM-88
                       TO TRUE
               END-IF
           ELSE
               IF WS-RESPONSE-CODE = DFHRESP(NOTFND)
                   MOVE WS-CUST-DELETED-BY-ANOTHER
                     TO MSG1O
                   SET WS-SEND-ERASE-ALARM-88
                       TO TRUE
               END-IF
           END-IF.
           MOVE -1                          TO CUSTNO1L.
           PERFORM 1500-SEND-KEY-MAP.
           SET WS-CA-PROCESS-KEY-MAP-88     TO TRUE.
      *
       4100-DELETE-CUSTOMER-RECORD.
      *
           PERFORM 8100-CMF-OPEN.

           MOVE SPACES                      TO WS-HA-EXEC-TEXT.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T1.
           MOVE 'ABEND'                     TO WS-HA-EXEC-TEXT-T2.
           MOVE 'DELETE FILE'
             TO WS-HA-EXEC-TEXT-T3.
           MOVE 'EXEC CICS DELETE FILE'
             TO WS-HA-EXEC-TEXT-T4.
           MOVE WS-CMF-FILE-NAME-CNST       TO WS-HA-EXEC-TEXT-T5.
           MOVE SPACES                      TO WS-HA-EXEC-TEXT-T6.
           MOVE WS-RT-MSG-NBR-TEXT(WS-HA-ERR-MSG-FILENOTFOUND)
             TO WS-HA-EXEC-TEXT-T7.
           MOVE ALL '='                     TO WS-HA-EXEC-TEXT-T8.
      *
           EXEC CICS
               DELETE FILE (WS-CMF-FILE-NAME-CNST)
                      RESP (WS-RESPONSE-CODE)
                      RESP2(WS-RESPONSE-CODE2)
           END-EXEC.
      *
           IF  WS-RESPONSE-CODE NOT = DFHRESP(NORMAL)
               PERFORM 9700-RESPTEXT
           END-IF.
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

       COPY CMFOPEN.
      *
       COPY CMFCLOSE.
      *