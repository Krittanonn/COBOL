      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATEIOX.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO "employees.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT IDX-FILE ASSIGN TO "employees.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS IDX-ID.

       DATA DIVISION.
       FILE SECTION.
       FD  SEQ-FILE.
       01  SEQ-REC       PIC X(40).
       FD  IDX-FILE.
       01  IDX-REC.
           05 IDX-ID     PIC X(4).
           05 IDX-NAME   PIC X(15).
           05 IDX-DEPT   PIC X(10).
           05 IDX-SALARY PIC 9(8)V99.
       WORKING-STORAGE SECTION.
       01  WS-EOF-FLAG     PIC X.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN INPUT SEQ-FILE.
            OPEN OUTPUT IDX-FILE.

            PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ SEQ-FILE AT END MOVE 'Y' TO WS-EOF-FLAG
               NOT AT END
                   MOVE SEQ-REC TO IDX-REC
                   WRITE IDX-REC INVALID KEY DISPLAY "WRITE ERROR!!"
               END-READ
            END-PERFORM
            CLOSE SEQ-FILE,IDX-FILE
            STOP RUN.
       END PROGRAM  CREATEIOX.
