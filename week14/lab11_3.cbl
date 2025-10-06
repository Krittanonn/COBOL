      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN to "employees.txt"
           ORGANIZATION is LINE SEQUENTIAL.
           SELECt IDX-FILE ASSIGN to "employees.dat"
           ORGANIZATION is INDEXED
           ACCESS mode is SEQUENTIAL
           record key is IDX-ID.
       DATA DIVISION.
       FILE SECTION.
       FD SEQ-FILE.
       01 SEQ-rec pic x(80).
       FD IDX-file.
       01 idx-rec.
           05 idx-id pic x(4).
           05 idx-name pic x(15).
           05 idx-dept pic x(10).
           05 idx-salary pic 9(8)v99.
       WORKING-STORAGE SECTION.
       01 ws-eof-flag pic x.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           open input SEQ-FILE.
           open output idx-file.

           perform until ws-eof-flag = 'Y'
               read seq-file
                   at end
                       move 'Y' to ws-eof-flag
                   not at end
                       move SEQ-rec to idx-rec
                       write idx-rec INVALID key DISPLAY "WRITE ERROR!"
              end-read
           END-PERFORM.
           close SEQ-FILE , IDX-file.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
