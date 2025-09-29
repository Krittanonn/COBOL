      ******************************************************************
      * Author: KRITTANON
      * Date: 09/29/2025
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEREST.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS ASSIGN TO "ACCOUNTS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCOUNTS-NEW ASSIGN TO "ACCOUNTS_NEW.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD ACCOUNTS.
       01 ACCOUNT-REC.
          05 ACC-NO       PIC X(10).
          05 ACC-PIN      PIC X(4).
          05 ACC-NAME     PIC X(20).
          05 ACC-BALANCE  PIC 9(8)V99.

       FD ACCOUNTS-NEW.
       01 ACCOUNT-REC-NEW.
          05 NEW-ACC-NO       PIC X(10).
          05 NEW-ACC-PIN      PIC X(4).
          05 NEW-ACC-NAME     PIC X(20).
          05 NEW-ACC-BALANCE  PIC 9(8)V99.

       WORKING-STORAGE SECTION.

       01 EOF-FLAG      PIC X VALUE "N".
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN INPUT ACCOUNTS
           OPEN OUTPUT ACCOUNTS-NEW

           PERFORM UNTIL EOF-FLAG = "Y"
               READ ACCOUNTS
                  AT END MOVE "Y" TO EOF-FLAG
                  NOT AT END
                     MOVE ACC-NO TO NEW-ACC-NO
                     MOVE ACC-PIN TO NEW-ACC-PIN
                     MOVE ACC-NAME TO NEW-ACC-NAME
                     COMPUTE NEW-ACC-BALANCE = ACC-BALANCE * 1.015
                     WRITE ACCOUNT-REC-NEW
               END-READ
           END-PERFORM

           CLOSE ACCOUNTS
           CLOSE ACCOUNTS-NEW

           DISPLAY "Interest calculation completed."

           STOP RUN.
       END PROGRAM INTEREST.
