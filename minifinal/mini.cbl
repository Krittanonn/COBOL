       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBRARY-MANAGEMENT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MEMBER-FILE ASSIGN TO "MEMBERS.TXT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS MEMBER-ID-KEY
              FILE STATUS IS WS-FS-MEMBER.

           SELECT BOOK-FILE ASSIGN TO "BOOKS.TXT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS BOOK-ISBN-KEY
              FILE STATUS IS WS-FS-BOOK.

           SELECT LOAN-FILE ASSIGN TO "LOANS.TXT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS LOAN-ID-KEY
              FILE STATUS IS WS-FS-LOAN.

           SELECT FINE-FILE ASSIGN TO "FINES.TXT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS FINE-ID-KEY
              FILE STATUS IS WS-FS-FINE.

       DATA DIVISION.
       FILE SECTION.

       FD MEMBER-FILE.
       01 MEMBER-RECORD.
          05 MEMBER-ID-KEY      PIC X(12).
          05 MEMBER-NAME        PIC X(40).
          05 MEMBER-PASSWORD    PIC X(20).
          05 MEMBER-EMAIL       PIC X(50).
          05 MEMBER-PHONE       PIC X(20).
          05 MEMBER-STATUS      PIC X(10).

       FD BOOK-FILE.
       01 BOOK-RECORD.
          05 BOOK-ISBN-KEY      PIC X(13).
          05 BOOK-TITLE         PIC X(60).
          05 BOOK-AUTHOR        PIC X(40).
          05 BOOK-PUBLISHER     PIC X(40).
          05 BOOK-YEAR          PIC 9(4).
          05 BOOK-CALLNUMBER    PIC X(20).
          05 BOOK-CATEGORY      PIC X(10).
          05 BOOK-STATUS        PIC X(10).

       FD LOAN-FILE.
       01 LOAN-RECORD.
          05 LOAN-ID-KEY        PIC X(12).
          05 LOAN-MEMBER-ID     PIC X(12).
          05 LOAN-ISBN          PIC X(13).
          05 LOAN-DATE          PIC 9(8).
          05 LOAN-DUE-DATE      PIC 9(8).
          05 LOAN-RETURN-DATE   PIC 9(8).
          05 LOAN-STATUS        PIC X(10).
          05 LOAN-RENEW-COUNT   PIC 9(2).

       FD FINE-FILE.
       01 FINE-RECORD.
          05 FINE-ID-KEY        PIC X(12).
          05 FINE-LOAN-ID       PIC X(12).
          05 FINE-MEMBER-ID     PIC X(12).
          05 FINE-AMOUNT        PIC 9(6)V99.
          05 FINE-PAID-FLAG     PIC X(3).
          05 FINE-DATE          PIC 9(8).

       WORKING-STORAGE SECTION.
       77 WS-OPTION             PIC 9 VALUE 0.
       77 WS-USER-ID            PIC X(12).
       77 WS-PASS               PIC X(20).
       77 WS-FS-MEMBER          PIC XX.
       77 WS-FS-BOOK            PIC XX.
       77 WS-FS-LOAN            PIC XX.
       77 WS-FS-FINE            PIC XX.
       77 WS-LOAN-SEQ           PIC 9(9) VALUE 1000.
       77 WS-FINE-SEQ           PIC 9(9) VALUE 5000.
       77 WS-LOAN-PERIOD-DAYS   PIC 9(3) VALUE 14.
       77 WS-MAX-RENEWALS       PIC 9(2) VALUE 2.
       77 WS-FINE-RATE          PIC 9(3) VALUE 5.
       77 WS-DAYS-LATE          PIC 9(3).
       77 WS-CURRENT-DATE       PIC 9(8).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "=== LIBRARY MANAGEMENT SYSTEM ===".
           PERFORM OPEN-FILES.
           PERFORM UNTIL WS-OPTION = 9
               MOVE 0 TO WS-OPTION
               PERFORM DISPLAY-MAIN-MENU
               PERFORM HANDLE-MENU-OPTION
           END-PERFORM.
           PERFORM CLOSE-FILES.
           DISPLAY "Goodbye.".
           STOP RUN.

       OPEN-FILES.
           OPEN I-O MEMBER-FILE
           IF WS-FS-MEMBER NOT = "00"
               DISPLAY "MEMBER file not found. Creating..."
               OPEN OUTPUT MEMBER-FILE
               CLOSE MEMBER-FILE
               OPEN I-O MEMBER-FILE
           END-IF

           OPEN I-O BOOK-FILE
           IF WS-FS-BOOK NOT = "00"
               DISPLAY "BOOK file not found. Creating..."
               OPEN OUTPUT BOOK-FILE
               CLOSE BOOK-FILE
               OPEN I-O BOOK-FILE
           END-IF

           OPEN I-O LOAN-FILE
           IF WS-FS-LOAN NOT = "00"
               DISPLAY "LOAN file not found. Creating..."
               OPEN OUTPUT LOAN-FILE
               CLOSE LOAN-FILE
               OPEN I-O LOAN-FILE
           END-IF

           OPEN I-O FINE-FILE
           IF WS-FS-FINE NOT = "00"
               DISPLAY "FINE file not found. Creating..."
               OPEN OUTPUT FINE-FILE
               CLOSE FINE-FILE
               OPEN I-O FINE-FILE
           END-IF.

       CLOSE-FILES.
           CLOSE MEMBER-FILE BOOK-FILE LOAN-FILE FINE-FILE.

       DISPLAY-MAIN-MENU.
           DISPLAY " ".
           DISPLAY "1. Register Member".
           DISPLAY "2. Login".
           DISPLAY "3. Book Management".
           DISPLAY "4. Borrow Book".
           DISPLAY "5. Return Book".
           DISPLAY "6. Renew Loan".
           DISPLAY "7. Search Book".
           DISPLAY "8. Reports".
           DISPLAY "9. Exit".
           DISPLAY "Select option (1-9): " WITH NO ADVANCING
           ACCEPT WS-OPTION.

       HANDLE-MENU-OPTION.
           EVALUATE WS-OPTION
              WHEN 1 PERFORM REGISTER-MEMBER
              WHEN 2 PERFORM LOGIN-PROMPT
              WHEN 3 PERFORM BOOK-MENU
              WHEN 4 PERFORM BORROW-BOOK
              WHEN 5 PERFORM RETURN-BOOK
              WHEN 6 PERFORM RENEW-LOAN
              WHEN 7 PERFORM SEARCH-BOOK
              WHEN 8 PERFORM REPORT-MENU
              WHEN 9 CONTINUE
              WHEN OTHER DISPLAY "Invalid option."
           END-EVALUATE.


       REGISTER-MEMBER.
           DISPLAY "== Register Member ==".
           DISPLAY "Enter Member ID: "
           ACCEPT MEMBER-ID-KEY
           READ MEMBER-FILE KEY IS MEMBER-ID-KEY
              INVALID KEY
                 DISPLAY "Name: "
                 ACCEPT MEMBER-NAME
                 DISPLAY "Password: "
                 ACCEPT MEMBER-PASSWORD
                 MOVE "ACTIVE" TO MEMBER-STATUS
                 WRITE MEMBER-RECORD
              NOT INVALID KEY
                 DISPLAY "Member already exists."
           END-READ.

       LOGIN-PROMPT.
           DISPLAY "== Login ==".
           DISPLAY "Member ID: "
           ACCEPT WS-USER-ID
           DISPLAY "Password: "
           ACCEPT WS-PASS
           MOVE WS-USER-ID TO MEMBER-ID-KEY
           READ MEMBER-FILE KEY IS MEMBER-ID-KEY
              INVALID KEY DISPLAY "No such member."
              NOT INVALID KEY
                 IF MEMBER-PASSWORD = WS-PASS
                    DISPLAY "Login successful."
                 ELSE
                    DISPLAY "Wrong password."
                 END-IF
           END-READ.


       BOOK-MENU.
           DISPLAY "== Book Menu ==".
           DISPLAY "1. Add Book".
           DISPLAY "2. Update Book Status".
           DISPLAY "3. Show Book".
           DISPLAY "4. Back".
           DISPLAY "Select option (1-4): "
           ACCEPT WS-OPTION
           EVALUATE WS-OPTION
             WHEN 1 PERFORM ADD-BOOK
             WHEN 2 PERFORM UPDATE-BOOK-STATUS
             WHEN 3 PERFORM SHOW-BOOK
             WHEN OTHER CONTINUE
           END-EVALUATE.

       ADD-BOOK.
           DISPLAY "Enter ISBN: "
           ACCEPT BOOK-ISBN-KEY
           DISPLAY "Enter Title: "
           ACCEPT BOOK-TITLE
           DISPLAY "Enter Author: "
           ACCEPT BOOK-AUTHOR
           DISPLAY "Enter Publisher: "
           ACCEPT BOOK-PUBLISHER
           DISPLAY "Enter Year: "
           ACCEPT BOOK-YEAR
           MOVE "AVAILABLE" TO BOOK-STATUS
           WRITE BOOK-RECORD INVALID KEY
               DISPLAY "Book already exists."
           END-WRITE.

       UPDATE-BOOK-STATUS.
           DISPLAY "Enter ISBN: "
           ACCEPT BOOK-ISBN-KEY
           READ BOOK-FILE KEY IS BOOK-ISBN-KEY
              INVALID KEY DISPLAY "Book not found."
              NOT INVALID KEY
                 DISPLAY "New Status: "
                 ACCEPT BOOK-STATUS
                 REWRITE BOOK-RECORD
           END-READ.

       SHOW-BOOK.
           DISPLAY "Enter ISBN: "
           ACCEPT BOOK-ISBN-KEY
           READ BOOK-FILE KEY IS BOOK-ISBN-KEY
              INVALID KEY DISPLAY "Book not found."
              NOT INVALID KEY
                 DISPLAY "Title: " BOOK-TITLE
                 DISPLAY "Author: " BOOK-AUTHOR
                 DISPLAY "Status: " BOOK-STATUS
           END-READ.


       BORROW-BOOK.
           DISPLAY "Enter Member ID: "
           ACCEPT LOAN-MEMBER-ID
           DISPLAY "Enter ISBN: "
           ACCEPT LOAN-ISBN
           READ BOOK-FILE KEY IS LOAN-ISBN
              INVALID KEY DISPLAY "Book not found."
              NOT INVALID KEY
                 MOVE "OUT" TO BOOK-STATUS
                 REWRITE BOOK-RECORD
                 ADD 1 TO WS-LOAN-SEQ
                 MOVE WS-LOAN-SEQ TO LOAN-ID-KEY
                 MOVE FUNCTION CURRENT-DATE(1:8) TO LOAN-DATE
                 COMPUTE LOAN-DUE-DATE = FUNCTION
                 INTEGER-OF-DATE(LOAN-DATE) + WS-LOAN-PERIOD-DAYS
                 MOVE "OUT" TO LOAN-STATUS
                 MOVE 0 TO LOAN-RENEW-COUNT
                 WRITE LOAN-RECORD
           END-READ.

       RETURN-BOOK.
           DISPLAY "Enter Loan ID: "
           ACCEPT LOAN-ID-KEY
           READ LOAN-FILE KEY IS LOAN-ID-KEY
              INVALID KEY DISPLAY "Loan not found."
              NOT INVALID KEY
                 MOVE FUNCTION CURRENT-DATE(1:8) TO LOAN-RETURN-DATE
                 MOVE "RETURNED" TO LOAN-STATUS
                 REWRITE LOAN-RECORD
                 PERFORM CALCULATE-FINE
           END-READ.

       RENEW-LOAN.
           DISPLAY "Enter Loan ID: "
           ACCEPT LOAN-ID-KEY
           READ LOAN-FILE KEY IS LOAN-ID-KEY
              INVALID KEY DISPLAY "Loan not found."
              NOT INVALID KEY
                 IF LOAN-RENEW-COUNT < WS-MAX-RENEWALS
                    ADD 1 TO LOAN-RENEW-COUNT
                    REWRITE LOAN-RECORD
                    DISPLAY "Loan renewed successfully."
                 ELSE
                    DISPLAY "Maximum renewals reached."
                 END-IF
           END-READ.

       SEARCH-BOOK.
           DISPLAY "Enter ISBN to search: "
           ACCEPT BOOK-ISBN-KEY
           READ BOOK-FILE KEY IS BOOK-ISBN-KEY
              INVALID KEY DISPLAY "Book not found."
              NOT INVALID KEY
                 DISPLAY "Title: " BOOK-TITLE
                 DISPLAY "Author: " BOOK-AUTHOR
                 DISPLAY "Status: " BOOK-STATUS
           END-READ.


       CALCULATE-FINE.
           COMPUTE WS-DAYS-LATE = FUNCTION
           INTEGER-OF-DATE(LOAN-RETURN-DATE)
                                 - FUNCTION
                                 INTEGER-OF-DATE(LOAN-DUE-DATE)
           IF WS-DAYS-LATE > 0
               ADD 1 TO WS-FINE-SEQ
               MOVE WS-FINE-SEQ TO FINE-ID-KEY
               MOVE LOAN-ID-KEY TO FINE-LOAN-ID
               MOVE LOAN-MEMBER-ID TO FINE-MEMBER-ID
               COMPUTE FINE-AMOUNT = WS-DAYS-LATE * WS-FINE-RATE
               MOVE "NO" TO FINE-PAID-FLAG
               MOVE LOAN-RETURN-DATE TO FINE-DATE
               WRITE FINE-RECORD
               DISPLAY "Late by " WS-DAYS-LATE " days. Fine = "
               FINE-AMOUNT
           ELSE
               DISPLAY "No fine."
           END-IF.


       REPORT-MENU.
           DISPLAY "== Reports ==".
           DISPLAY "1. All Books".
           DISPLAY "2. All Loans".
           DISPLAY "3. All Fines".
           DISPLAY "4. Back".
           DISPLAY "Select report (1-4): "
           ACCEPT WS-OPTION
           EVALUATE WS-OPTION
             WHEN 1 PERFORM REPORT-ALL-BOOKS
             WHEN 2 PERFORM REPORT-ALL-LOANS
             WHEN 3 PERFORM REPORT-ALL-FINES
             WHEN OTHER CONTINUE
           END-EVALUATE.

       REPORT-ALL-BOOKS.
           DISPLAY "== All Books ==".
           MOVE "00" TO WS-FS-BOOK
           PERFORM UNTIL WS-FS-BOOK = "10"
               READ BOOK-FILE NEXT
                   AT END MOVE "10" TO WS-FS-BOOK
                   NOT AT END
                       DISPLAY "ISBN: " BOOK-ISBN-KEY
                       DISPLAY "Title: " BOOK-TITLE
                       DISPLAY "Author: " BOOK-AUTHOR
                       DISPLAY "Status: " BOOK-STATUS
               END-READ
           END-PERFORM.

       REPORT-ALL-LOANS.
           DISPLAY "== All Loans ==".
           MOVE "00" TO WS-FS-LOAN
           PERFORM UNTIL WS-FS-LOAN = "10"
               READ LOAN-FILE NEXT
                   AT END MOVE "10" TO WS-FS-LOAN
                   NOT AT END
                       DISPLAY "Loan ID: " LOAN-ID-KEY
                       DISPLAY "Member ID: " LOAN-MEMBER-ID
                       DISPLAY "Book ISBN: " LOAN-ISBN
                       DISPLAY "Status: " LOAN-STATUS
               END-READ
           END-PERFORM.

       REPORT-ALL-FINES.
           DISPLAY "== All Fines ==".
           MOVE "00" TO WS-FS-FINE
           PERFORM UNTIL WS-FS-FINE = "10"
               READ FINE-FILE NEXT
                   AT END MOVE "10" TO WS-FS-FINE
                   NOT AT END
                       DISPLAY "Fine ID: " FINE-ID-KEY
                       DISPLAY "Loan ID: " FINE-LOAN-ID
                       DISPLAY "Member ID: " FINE-MEMBER-ID
                       DISPLAY "Amount: " FINE-AMOUNT
                       DISPLAY "Paid: " FINE-PAID-FLAG
               END-READ
           END-PERFORM.
