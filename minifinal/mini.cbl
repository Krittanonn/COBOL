       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBRARY-BASIC.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MEMBER-FILE ASSIGN TO "MEMBERS.TXT"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BOOK-FILE ASSIGN TO "BOOKS.TXT"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT LOAN-FILE ASSIGN TO "LOANS.TXT"
              ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TEMP-FILE ASSIGN TO "TEMP.TXT"
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD MEMBER-FILE.
       01 MEMBER-RECORD.
          05 MEMBER-ID      PIC X(12).
          05 MEMBER-PASSWORD PIC X(20).

       FD BOOK-FILE.
       01 BOOK-RECORD.
          05 BOOK-ISBN      PIC X(13).
          05 BOOK-TITLE     PIC X(60).
          05 BOOK-STATUS    PIC X(10).

       FD LOAN-FILE.
       01 LOAN-RECORD.
          05 LOAN-ID        PIC 9(6).
          05 LOAN-MEMBER-ID PIC X(12).
          05 LOAN-ISBN      PIC X(13).
          05 LOAN-DATE      PIC 9(8).
          05 RETURN-DATE    PIC 9(8).
          05 LOAN-STATUS    PIC X(10).

       FD TEMP-FILE.
       01 TEMP-RECORD      PIC X(200).

       WORKING-STORAGE SECTION.
       77 WS-OPTION        PIC 9 VALUE 0.
       77 WS-USER-ID       PIC X(12).
       77 WS-PASS          PIC X(20).
       77 WS-FOUND         PIC X VALUE 'N'.
       77 WS-LOAN-SEQ      PIC 9(6) VALUE 1000.
       77 WS-DAYS-LATE     PIC 9(3) VALUE 0.
       77 WS-FINE          PIC 9(5) VALUE 0.
       77 WS-ISBN          PIC X(13).
       77 WS-LOAN-ID       PIC 9(6).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "=== LIBRARY SYSTEM BASIC ===".
           PERFORM UNTIL WS-OPTION = 6
               DISPLAY "1. Register Member"
               DISPLAY "2. Login"
               DISPLAY "3. Search Book"
               DISPLAY "4. Borrow Book"
               DISPLAY "5. Return Book"
               DISPLAY "6. Exit"
               DISPLAY "Select option (1-6): "
               ACCEPT WS-OPTION
               EVALUATE WS-OPTION
                  WHEN 1 PERFORM REGISTER-MEMBER
                  WHEN 2 PERFORM LOGIN
                  WHEN 3 PERFORM SEARCH-BOOK
                  WHEN 4 PERFORM BORROW-BOOK
                  WHEN 5 PERFORM RETURN-BOOK
                  WHEN 6 CONTINUE
                  WHEN OTHER DISPLAY "Invalid option."
               END-EVALUATE
           END-PERFORM
           DISPLAY "Goodbye."
           STOP RUN.

       REGISTER-MEMBER.
           DISPLAY "== Register Member =="
           DISPLAY "Enter Member ID: "
           ACCEPT MEMBER-ID
           DISPLAY "Enter Password: "
           ACCEPT MEMBER-PASSWORD
           OPEN OUTPUT MEMBER-FILE
           WRITE MEMBER-RECORD
           CLOSE MEMBER-FILE
           DISPLAY "Registration completed."
           .

       LOGIN.
           DISPLAY "== Login =="
           DISPLAY "Member ID: "
           ACCEPT WS-USER-ID
           DISPLAY "Password: "
           ACCEPT WS-PASS
           MOVE 'N' TO WS-FOUND
           OPEN INPUT MEMBER-FILE
           PERFORM UNTIL WS-FOUND = 'Y'
               READ MEMBER-FILE
                   AT END
                       DISPLAY "No such member."
                       MOVE 'Y' TO WS-FOUND
                   NOT AT END
                       IF FUNCTION TRIM(MEMBER-ID) = FUNCTION
                           TRIM(WS-USER-ID)
                          IF FUNCTION TRIM(MEMBER-PASSWORD) =
                              FUNCTION TRIM(WS-PASS)
                             DISPLAY "Login successful."
                          ELSE
                             DISPLAY "Wrong password."
                          END-IF
                          MOVE 'Y' TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM
           CLOSE MEMBER-FILE
           .

       SEARCH-BOOK.
           DISPLAY "Enter ISBN to search: "
           ACCEPT WS-ISBN
           MOVE 'N' TO WS-FOUND
           OPEN INPUT BOOK-FILE
           PERFORM UNTIL WS-FOUND = 'Y'
               READ BOOK-FILE
                   AT END MOVE 'Y' TO WS-FOUND
                   NOT AT END
                       IF FUNCTION TRIM(BOOK-ISBN) = FUNCTION
                           TRIM(WS-ISBN)
                          DISPLAY "Title: " BOOK-TITLE
                          DISPLAY "Status: " BOOK-STATUS
                          MOVE 'Y' TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM
           CLOSE BOOK-FILE
           .

       BORROW-BOOK.
           DISPLAY "Enter Member ID: "
           ACCEPT WS-USER-ID
           DISPLAY "Enter ISBN: "
           ACCEPT WS-ISBN
           MOVE 'N' TO WS-FOUND
           OPEN INPUT BOOK-FILE
           OPEN OUTPUT TEMP-FILE
           PERFORM UNTIL WS-FOUND = 'Y'
               READ BOOK-FILE
                   AT END MOVE 'Y' TO WS-FOUND
                   NOT AT END
                       IF FUNCTION TRIM(BOOK-ISBN) = FUNCTION
                           TRIM(WS-ISBN)
                          IF FUNCTION TRIM(BOOK-STATUS) = "AVAILABLE"
                             MOVE "OUT" TO BOOK-STATUS
                             ADD 1 TO WS-LOAN-SEQ
                             MOVE WS-LOAN-SEQ TO LOAN-ID
                             MOVE WS-USER-ID TO LOAN-MEMBER-ID
                             MOVE WS-ISBN TO LOAN-ISBN
                             MOVE FUNCTION CURRENT-DATE TO LOAN-DATE
                             MOVE 0 TO RETURN-DATE
                             MOVE "OUT" TO LOAN-STATUS
                             OPEN OUTPUT LOAN-FILE
                             WRITE LOAN-RECORD
                             CLOSE LOAN-FILE
                             DISPLAY "Book borrowed successfully."
                             MOVE 'Y' TO WS-FOUND
                          ELSE
                             DISPLAY "Book not available."
                             MOVE 'Y' TO WS-FOUND
                          END-IF
                       END-IF
                       WRITE TEMP-RECORD FROM BOOK-RECORD
               END-READ
           END-PERFORM
           CLOSE BOOK-FILE TEMP-FILE
           DISPLAY "Book file updated."
           .

       RETURN-BOOK.
           DISPLAY "Enter Loan ID: "
           ACCEPT WS-LOAN-ID
           MOVE 'N' TO WS-FOUND
           OPEN INPUT LOAN-FILE
           OPEN OUTPUT TEMP-FILE
           PERFORM UNTIL WS-FOUND = 'Y'
               READ LOAN-FILE
                   AT END MOVE 'Y' TO WS-FOUND
                   NOT AT END
                       IF LOAN-ID = WS-LOAN-ID
                          MOVE FUNCTION CURRENT-DATE TO RETURN-DATE
                          MOVE "RETURNED" TO LOAN-STATUS
                          COMPUTE WS-DAYS-LATE = RETURN-DATE - LOAN-DATE
                          IF WS-DAYS-LATE > 14
                             COMPUTE WS-FINE = (WS-DAYS-LATE - 14) * 5
                             DISPLAY "Late by " WS-DAYS-LATE
                             " days. Fine: " WS-FINE
                          ELSE
                             DISPLAY "Returned on time."
                          END-IF
                          MOVE 'Y' TO WS-FOUND
                       END-IF
                       WRITE TEMP-RECORD FROM LOAN-RECORD
               END-READ
           END-PERFORM
           CLOSE LOAN-FILE TEMP-FILE
           DISPLAY "Loan file updated."
           .
