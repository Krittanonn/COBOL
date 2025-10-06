      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. READIDX.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN to "employees.dat"
           ORGANIZATION is INDEXED
           access mode is SEQUENTIAL
           record key is EMP-ID.
       DATA DIVISION.
       FILE SECTION.
       fd employee-file.
       01 employee-record.
           05 emp-id pic x(4).
           05 emp-name pic x(15).
           05 emp-dept pic x(10).
           05 emp-salary pic 9(8)v99.
       WORKING-STORAGE SECTION.
       01 ws-eof-flag pic x value 'N'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           open input employee-file
           display "========= EMPLOYEE DATA ==========".

           PERFORM UNTIL ws-eof-flag = 'Y'
           read employee-file next RECORD
               at end
               move 'Y' to ws-eof-flag
               not at end
               display "ID: " emp-id " , NAME: "emp-name
               " , DEPT: " emp-dept " , SALARY: " emp-salary
            END-READ
           END-PERFORM.
                display "==================================".
           close employee-file.
           STOP RUN.
       END PROGRAM READIDX.
