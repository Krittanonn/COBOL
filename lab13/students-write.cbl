      ******************************************************************
      * Author: KRITTANON
      * Date: 09/29/2025
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENTS-FILE ASSIGN TO 'students.dat'
               ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.

       FILE SECTION.

       FD STUDENTS-FILE.
       01 STUDENT-RECORD.
           05 STU-ID    PIC X(4).
           05 STU-NAME  PIC X(20).
           
       WORKING-STORAGE SECTION.
       
       01 WS-INPUT-ID       PIC X(4).
       01 WS-FOUND-FLAG     PIC X(1) VALUE 'N'.
       01 WS-EOF-FLAG       PIC X(1) VALUE 'N'.
       
       01 WS-NEW-STUDENTS.
           05 WS-NEW-ID PIC X(4).
           05 WS-NEW-NAME PIC X(20).
           
       01 STUDENTS-RECORD-OUT.
           05 OUT-STU-ID       PIC X(4).
           05 OUT-STU-NAME     PIC X(20).
           
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           
           DISPLAY "ENTER NEW STUDENT ID( 4 DIGITS): "
           ACCEPT WS-NEW-ID.
           
           DISPLAY "ENTER NEW STUDENTS NAME (20 CAHR): "
           ACCEPT WS-NEW-STUDENTS.
           
           OPEN EXTEND STUDENTS-FILE.
           
           MOVE WS-NEW-ID TO OUT-STU-ID
           MOVE WS-NEW-NAME TO OUT-STU-NAME
           
           WRITE STUDENT-RECORD FROM STUDENTS-RECORD-OUT.
           
           CLOSE STUDENTS-FILE.
           
           DISPLAY "NEW STUDENT HAS DEEN ADDED.".
            
       END PROGRAM YOUR-PROGRAM-NAME.

