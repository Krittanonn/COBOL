      ******************************************************************
      * Author: KRITTANON
      * Date: 09/29/2025
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDUSER.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS ASSIGN TO "ACCOUNTS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
               
       DATA DIVISION.
       FILE SECTION.
       
       FD ACCOUNTS.
       01 ACCOUNT-REC.
           05 ACC-NO       PIC X(10).
           05 ACC-PIN      PIC X(4).
           05 ACC-NAME     PIC X(20).
           05 ACC-BALANCE  PIC 9(8)V99.
           
       WORKING-STORAGE SECTION.
       
       01 WS-NEW-ACC-NO       PIC X(10).
       01 WS-NEW-ACC-PIN      PIC X(4).
       01 WS-NEW-ACC-NAME     PIC X(20).
       01 WS-NEW-ACC-BALANCE  PIC 9(8)V99.

       01 WS-EOF-FLAG         PIC X VALUE "N".
       01 WS-DUP-FLAG         PIC X VALUE "N".

       01 WS-ACCOUNT-REC.
           05 WS-ACC-NO       PIC X(10).
           05 WS-ACC-PIN      PIC X(4).
           05 WS-ACC-NAME     PIC X(20).
           05 WS-ACC-BALANCE  PIC 9(8)V99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "===== ADD NEW USER TO ACCOUNTS ====="

           DISPLAY "ENTER NEW ACCOUNT NUMBER (10 CHAR): "
           ACCEPT WS-NEW-ACC-NO

           DISPLAY "ENTER PIN (4 DIGITS): "
           ACCEPT WS-NEW-ACC-PIN

           DISPLAY "ENTER NAME (20 CHAR): "
           ACCEPT WS-NEW-ACC-NAME

           DISPLAY "ENTER INITIAL BALANCE (NUMERIC): "
           ACCEPT WS-NEW-ACC-BALANCE

           PERFORM CHECK-DUPLICATE

           IF WS-DUP-FLAG = "Y"
               DISPLAY "ERROR: ACCOUNT NUMBER ALREADY EXISTS!"
           ELSE
              MOVE WS-NEW-ACC-NO      TO ACC-NO
               MOVE WS-NEW-ACC-PIN     TO ACC-PIN
               MOVE WS-NEW-ACC-NAME    TO ACC-NAME
               MOVE WS-NEW-ACC-BALANCE TO ACC-BALANCE

               OPEN EXTEND ACCOUNTS
               WRITE ACCOUNT-REC
               CLOSE ACCOUNTS

               DISPLAY "NEW USER HAS BEEN ADDED SUCCESSFULLY."
           END-IF
           
           STOP RUN.
           
           CHECK-DUPLICATE.
           MOVE "N" TO WS-DUP-FLAG
           MOVE "N" TO WS-EOF-FLAG

           OPEN INPUT ACCOUNTS

           PERFORM UNTIL WS-EOF-FLAG = "Y" OR WS-DUP-FLAG = "Y"
               READ ACCOUNTS
                   AT END
                       MOVE "Y" TO WS-EOF-FLAG
                   NOT AT END
                       MOVE ACCOUNT-REC TO WS-ACCOUNT-REC
                       IF WS-ACC-NO = WS-NEW-ACC-NO
                           MOVE "Y" TO WS-DUP-FLAG
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE ACCOUNTS.
           
       END PROGRAM ADDUSER.

