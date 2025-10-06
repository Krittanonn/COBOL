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
       SELECT EMPLOYEE-FILE ASSIGN to "employees.dat"
           ORGANIZATION is INDEXED
           access mode is RANDOM
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
       01 WS-DISPLAY-LINE pic x(40).
       01 ws-input-id pic x(4).
       01 ws-new-dept pic x(30).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            open I-O employee-file
           display "Enter Employee ID to update: ".
           accept ws-input-id.
           move ws-input-id to emp-id
           read employee-file
               invalid KEY
                   display "Employee not found."
               not INVALID KEY
                   display "Current Dept for " EMP-NAME " is " emp-dept
                   display "Enter new Department: "
                   accept ws-new-dept
                   move ws-new-dept to emp-dept
                   rewrite employee-record
                       INVALID KEY display "update failed"
                   END-REWRITE
                   display "Update Successful"
           close employee-file
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
