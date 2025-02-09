//CMINQF2A JOB
//*
//CICSPROC JCLLIB ORDER=(KC03F2A.DCMAFD02.PROCLIB)
//*
//*********************************************************************
//*           TRANSLATE COMPILE
//*           AND LINK A CICS COBOL PROGRAM
//*********************************************************************
//COMPLI   EXEC DFHZITCL,
//             PROGLIB=TSOECCC.CICSTS12.STUDENT.LOADLIB,
//             PROGSRC=KC03F2A.DCMAFD02.A5P1.COBOL,
//             PROGMBR=CMINQF2A,
//             CPYLIBWS=KC03F2A.DCMAFD02.COPYBOOK.WS.COBOL,
//             CPYLIBPR=KC03F2A.DCMAFD02.COPYBOOK.PR.COBOL
//* NAME OF PROGRAM AND MEMBER TO TRANSLATE/COMPILE/LKED
//COBOL.SYSIN DD DSN=&PROGSRC(&PROGMBR),DISP=SHR
/*