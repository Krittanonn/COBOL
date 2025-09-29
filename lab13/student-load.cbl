      ******************************************************************
      * Author: krittanon Museesut
      * Date: 09/29/2025
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE ASSIGN TO 'students.dat'
               ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.

       FILE SECTION.

       FD STUDENT-FILE.
       01 STUDENT-RECORD.
           05 STU-ID    PIC X(4).
           05 STU-NAME  PIC X(20).


       WORKING-STORAGE SECTION.

       01 WS-INPUT-ID       PIC X(4).
       01 WS-FOUND-FLAG     PIC X(1) VALUE 'N'.
       01 WS-EOF-FLAG       PIC X(1) VALUE 'N'.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           DISPLAY "ENTER STUDENT ID (4 DIGIT): ".
           ACCEPT WS-INPUT-ID.


           OPEN INPUT STUDENT-FILE.

           PERFORM UNTIL WS-EOF-FLAG = 'Y' OR WS-FOUND-FLAG = 'Y'
              READ STUDENT-FILE
                  AT END
                      MOVE 'Y' TO WS-EOF-FLAG
                      NOT AT END
                          IF STU-ID = WS-INPUT-ID
                              DISPLAY "FOUND STUDENT NAME IS" STU-NAME
                              MOVE 'Y' TO WS-FOUND-FLAG
                          END-IF
               END-READ
           END-PERFORM.

           IF WS-FOUND-FLAG NOT = 'Y'
               DISPLAY "STUDENT ID NOT FOUND."
           END-IF.

           CLOSE STUDENT-FILE.
       END PROGRAM YOUR-PROGRAM-NAME.
