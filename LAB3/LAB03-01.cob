      ******************************************************************
      * Author: KRITTANON
      * Date: 7/14/2025
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-RECORD.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 EMPLOYEE-RECORD.
               05  EMP-ID              PIC 9(5) VALUE 12345.
               05  EMP-TITLE           PIC X(10) VALUE "Mr.".
               05  EMP-FNAME           PIC X(25) VALUE "John".
               05  EMP-LNAME           PIC X(25) VALUE "Kim".
               05  EMP-DEPARTMENT      PIC X(20) VALUE "IT DEPARTMENT".
               05  EMP-SALARY          PIC 9(7)V99 VALUE 12345.67.

       PROCEDURE DIVISION.

       DISPLAY "======================================".
       DISPLAY "          Employee INFORMATION        ".
       DISPLAY "======================================".
       DISPLAY "Employee ID: " EMP-ID.
       DISPLAY "Title: " EMP-TITLE.
       DISPLAY "First Name: " EMP-FNAME.
       DISPLAY "Last Name: " EMP-LNAME.
       DISPLAY "Department: " EMP-DEPARTMENT.
       DISPLAY "Salary: " EMP-SALARY.
       DISPLAY "======================================".

       STOP RUN.
