       IDENTIFICATION DIVISION.
       PROGRAM-ID. CMFLOF2A.
       AUTHOR. TALENT NYOTA.
       DATE-WRITTEN. 11-11-2024.
      *PROGRAM DESCRIPTION: VSAM data load program
      *
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
      *
       FILE-CONTROL.
           SELECT CMF-INPUT ASSIGN TO CMFDATA
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-INFILE-SW.

           SELECT CMF-FILE ASSIGN TO OUTFILE
               RECORD KEY IS CMF-FILE-KEY
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-OUTFILE-SW.
      *
       DATA DIVISION.
       FILE SECTION.
       FD CMF-INPUT
           RECORDING MODE IS F
           RECORD CONTAINS 118 CHARACTERS
           DATA RECORD IS CMF-INPUT-RECORD.
       01 CMF-INPUT-RECORD.
           05 CMF-INPUT-KEY.
               10 CMF-INPUT-NUMBER          PIC X(6).
           05 FILLER                        PIC X(112).

       FD CMF-FILE
           RECORD CONTAINS 118 CHARACTERS
           DATA RECORD IS CMF-FILE-RECORD.
       01 CMF-FILE-RECORD.
           05 CMF-FILE-KEY.
               10 CMF-FILE-NUMBER           PIC X(6).
           05 FILLER                        PIC X(112).
      *
       WORKING-STORAGE SECTION.
       01 WS-INFILE-SW                      PIC X(02)  VALUE SPACES.
           88  WS-INFILE-SUCCESS                VALUE '00'.
           88  WS-INFILE-EOF                    VALUE '10'.
       01 WS-OUTFILE-SW                     PIC X(02)  VALUE SPACES.
           88  WS-OUTFILE-SUCCESS               VALUE '00'.
           88  WS-OUTFILE-IOERROR               VALUE '37'.
           88  WS-OUTFILE-EOF                   VALUE '10'.
       01 WS-CMF-REC                        PIC X(118).
       01 WS-EOF-SW-IN                      PIC X(01)  VALUE 'N'.
           88  WS-EOF-IN-NO                     VALUE 'N'.
           88  WS-EOF-IN-YES                    VALUE 'Y'.
       01 WS-EOF-SW-OUT                     PIC X(01)  VALUE 'N'.
           88  WS-EOF-OUT-NO                    VALUE 'N'.
           88  WS-EOF-OUT-YES                   VALUE 'Y'.
       01 WS-NBR                            PIC 9      VALUE 0.
      *
       PROCEDURE DIVISION.
       000-MAIN-PARA.
           MOVE SPACE TO WS-INFILE-SW
                         WS-OUTFILE-SW
                         WS-CMF-REC.
      *
           OPEN INPUT CMF-INPUT.
           IF WS-INFILE-SUCCESS
               DISPLAY "CMF-INPUT OPEN SUCCESSFUL"
           ELSE
               DISPLAY "WS-INFILE-SW=" WS-INFILE-SW
               DISPLAY "CMF-INPUT OPEN ERROR"
               PERFORM 800-PROGRAM-FAILED
           END-IF.
      *
           OPEN OUTPUT CMF-FILE.
           IF WS-OUTFILE-SUCCESS
               DISPLAY "CMF-FILE OPEN SUCCESSFUL"
           ELSE IF WS-OUTFILE-IOERROR
               DISPLAY "WS-OUTFILE-SW=" WS-OUTFILE-SW
               DISPLAY "CMF-FILE IOERROR - OPEN OUTPUT SHOULD BE "
                       "OPEN INPUT OR I-O OR EXTEND  "
               DISPLAY "- DATA MAY ALREADY HAVE BEEN LOADED PREVIOUSLY"
               DISPLAY "- DELETE AND INITIALIZE FILE TO RELOAD DATA"
               PERFORM 800-PROGRAM-FAILED
           ELSE
               DISPLAY "WS-OUTFILE-SW=" WS-OUTFILE-SW
               DISPLAY "CMF-FILE OPENING ERROR"
               PERFORM 800-PROGRAM-FAILED
           END-IF.
      *
           READ CMF-INPUT
               AT END MOVE 'Y' TO WS-EOF-SW-IN.
      *
           IF WS-EOF-IN-NO THEN
               DISPLAY "1ST READ CMF-INPUT-RECORD=" CMF-INPUT-RECORD
      *
               PERFORM 100-LOAD-PARA
                   UNTIL WS-EOF-IN-YES
           ELSE
               DISPLAY "NO DATA IN CMF-INPUT"
               PERFORM 800-PROGRAM-FAILED
           END-IF.
      *
           PERFORM 900-COMPLETED-OK.
      *
           STOP RUN.
      *
       100-LOAD-PARA.
      *
           WRITE CMF-FILE-RECORD FROM CMF-INPUT-RECORD.
           DISPLAY "WRITE TO CMF-FILE".
      *
           READ CMF-INPUT
               AT END MOVE 'Y' TO WS-EOF-SW-IN.
      *
           IF WS-EOF-IN-NO THEN
               DISPLAY "NTH READ CMF-INPUT-RECORD=" CMF-INPUT-RECORD
           ELSE
               DISPLAY "EOF CMF-INPUT"
           END-IF.
      *
       800-PROGRAM-FAILED.
           DISPLAY "PROGRAM TERMINATED WITH DIVIDE BY ZERO!".
           DISPLAY "CHECK ERROR MESSAGES IN SYSOUT PART OF JOB SUMMARY".
           COMPUTE WS-NBR = WS-NBR / WS-NBR.
      *
       900-COMPLETED-OK.
           DISPLAY "PROGRAM COMPLETED OK".
      *