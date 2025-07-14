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

       01  EMPLOYEE-RECORD.
           05  EMP-ID              PIC 9(5).
           05  EMP-TITLE           PIC X(10).
           05  EMP-FNAME           PIC X(25).
           05  EMP-LNAME           PIC X(25).
           05  EMP-DEPARTMENT      PIC X(20) VALUE "IT DEPARTMENT".
           05  EMP-SALARY          PIC 9(7)V99.

       PROCEDURE DIVISION.

       DISPLAY "Enter Employee ID (5 digits): ".
       ACCEPT EMP-ID.

       DISPLAY "Enter Employee Title (10 characters): ".
       ACCEPT EMP-TITLE.

       DISPLAY "Enter Employee First Name (25 characters): ".
       ACCEPT EMP-FNAME.

       DISPLAY "Enter Employee Last Name (25 characters): ".
       ACCEPT EMP-LNAME.

       DISPLAY "Enter Employee Salary (7 digits with 2 decimals): ".
       ACCEPT EMP-SALARY.
       
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
