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

       DATA DIVISION.
       FILE SECTION.
       FD MEMBER-FILE.
       01 MEMBER-RECORD.
           05 MEMBER-ID       PIC X(12).
           05 MEMBER-PASSWORD PIC X(20).

       FD BOOK-FILE.
       01 BOOK-RECORD.
           05 BOOK-ISBN    PIC X(13).
           05 BOOK-TITLE   PIC X(60).
           05 BOOK-STATUS  PIC X(10).

       FD LOAN-FILE.
       01 LOAN-RECORD.
           05 LOAN-ID        PIC 9(6).
           05 LOAN-MEMBER-ID PIC X(12).
           05 LOAN-ISBN      PIC X(13).
           05 LOAN-DATE      PIC 9(8).
           05 RETURN-DATE    PIC 9(8).
           05 LOAN-STATUS    PIC X(10).

       WORKING-STORAGE SECTION.
       77 WS-OPTION          PIC 9 VALUE 0.
       77 WS-USER-ID         PIC X(12).
       77 WS-PASS            PIC X(20).
       77 WS-SEARCH-ISBN     PIC X(13).
       77 WS-BORROW-ISBN     PIC X(13).
       77 WS-FOUND           PIC X VALUE 'N'.
       77 WS-LOAN-SEQ        PIC 9(6) VALUE 1000.
       77 WS-DAYS-LATE       PIC 9(3) VALUE 0.
       77 WS-FINE            PIC 9(5) VALUE 0.
       77 WS-LOAN-ID         PIC 9(6).
       77 WS-EOF-FLAG        PIC X VALUE 'N'.
       77 WS-LOGIN-SUCCESS   PIC X VALUE 'N'.
       77 WS-CURRENT-DATE    PIC 9(8) VALUE 20250915.
       77 WS-RETURN-DATE     PIC 9(8).
       77 WS-STATUS-CHECK    PIC X(10).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "=== LIBRARY MANAGEMENT SYSTEM ===".
           PERFORM UNTIL WS-OPTION = 6
               DISPLAY " "
               DISPLAY "MAIN MENU:"
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
                  WHEN 2 PERFORM LOGIN-PARAGRAPH
                  WHEN 3 PERFORM SEARCH-BOOK
                  WHEN 4 PERFORM BORROW-BOOK
                  WHEN 5 PERFORM RETURN-BOOK
                  WHEN 6 DISPLAY "Exiting system..."
                  WHEN OTHER DISPLAY "Invalid option. Please try again."
               END-EVALUATE
           END-PERFORM
           DISPLAY "Thank you for using the Library System!"
           STOP RUN.

       REGISTER-MEMBER.
           DISPLAY " "
           DISPLAY "=== REGISTER NEW MEMBER ==="
           DISPLAY "Enter Member ID (max 12 chars): "
           ACCEPT MEMBER-ID
           DISPLAY "Enter Password (max 20 chars): "
           ACCEPT MEMBER-PASSWORD

           OPEN EXTEND MEMBER-FILE
           WRITE MEMBER-RECORD
           CLOSE MEMBER-FILE

           DISPLAY "Registration completed successfully!"
           DISPLAY "Member ID: " MEMBER-ID
           DISPLAY " ".

       LOGIN-PARAGRAPH.
           DISPLAY " "
           DISPLAY "=== MEMBER LOGIN ==="
           DISPLAY "Enter Member ID: "
           ACCEPT WS-USER-ID
           DISPLAY "Enter Password: "
           ACCEPT WS-PASS

           MOVE 'N' TO WS-FOUND
           MOVE 'N' TO WS-EOF-FLAG
           MOVE 'N' TO WS-LOGIN-SUCCESS

           OPEN INPUT MEMBER-FILE
           PERFORM UNTIL WS-EOF-FLAG = 'Y' OR WS-FOUND = 'Y'
               READ MEMBER-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       IF MEMBER-ID = WS-USER-ID
                          IF MEMBER-PASSWORD = WS-PASS
                             DISPLAY
                             "Login successful! Welcome " WS-USER-ID
                             MOVE 'Y' TO WS-FOUND
                             MOVE 'Y' TO WS-LOGIN-SUCCESS
                          ELSE
                             DISPLAY
                             "Incorrect password for " WS-USER-ID
                             MOVE 'Y' TO WS-FOUND
                          END-IF
                       END-IF
               END-READ
           END-PERFORM

           IF WS-LOGIN-SUCCESS = 'N' AND WS-EOF-FLAG = 'Y'
               DISPLAY "Member ID " WS-USER-ID " not found."
               DISPLAY "Please register first."
           END-IF

           CLOSE MEMBER-FILE
           DISPLAY " ".

       SEARCH-BOOK.
           DISPLAY " "
           DISPLAY "=== SEARCH BOOK ==="
           DISPLAY "Enter ISBN to search (13 digits): "
           ACCEPT WS-SEARCH-ISBN

           MOVE 'N' TO WS-FOUND
           MOVE 'N' TO WS-EOF-FLAG

           OPEN INPUT BOOK-FILE
           PERFORM UNTIL WS-EOF-FLAG = 'Y' OR WS-FOUND = 'Y'
               READ BOOK-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       IF BOOK-ISBN = WS-SEARCH-ISBN
                          DISPLAY "=== BOOK FOUND ==="
                          DISPLAY "ISBN: " BOOK-ISBN
                          DISPLAY "Title: " BOOK-TITLE
                          DISPLAY "Status: " BOOK-STATUS
                          MOVE 'Y' TO WS-FOUND
                       END-IF
               END-READ
           END-PERFORM

           IF WS-FOUND = 'N'
               DISPLAY "Book with ISBN " WS-SEARCH-ISBN " not found."
               DISPLAY "Please check the ISBN and try again."
           END-IF

           CLOSE BOOK-FILE
           DISPLAY " ".

       BORROW-BOOK.
           DISPLAY " "
           DISPLAY "=== BORROW BOOK ==="
           DISPLAY "Enter your Member ID: "
           ACCEPT WS-USER-ID
           DISPLAY "Enter Book ISBN: "
           ACCEPT WS-BORROW-ISBN

           MOVE 'N' TO WS-FOUND
           MOVE 'N' TO WS-EOF-FLAG

           OPEN INPUT BOOK-FILE
           PERFORM UNTIL WS-EOF-FLAG = 'Y' OR WS-FOUND = 'Y'
               READ BOOK-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       IF BOOK-ISBN = WS-BORROW-ISBN
                          MOVE BOOK-STATUS TO WS-STATUS-CHECK
                          IF WS-STATUS-CHECK = "AVAILABLE "
                             ADD 1 TO WS-LOAN-SEQ
                             MOVE WS-LOAN-SEQ TO LOAN-ID
                             MOVE WS-USER-ID TO LOAN-MEMBER-ID
                             MOVE WS-BORROW-ISBN TO LOAN-ISBN
                             MOVE WS-CURRENT-DATE TO LOAN-DATE
                             MOVE 0 TO RETURN-DATE
                             MOVE "OUT" TO LOAN-STATUS

                             OPEN EXTEND LOAN-FILE
                             WRITE LOAN-RECORD
                             CLOSE LOAN-FILE

                             DISPLAY
                             "=== BOOK BORROWED SUCCESSFULLY ==="
                             DISPLAY "Book: " BOOK-TITLE
                             DISPLAY "Loan ID: " WS-LOAN-SEQ
                             DISPLAY "Borrowed by: " WS-USER-ID
                             DISPLAY "Loan Date: " WS-CURRENT-DATE
                             DISPLAY "Due Date: 14 days from loan date"
                             DISPLAY
                             "Please keep your Loan ID for returns!"
                             MOVE 'Y' TO WS-FOUND
                          ELSE
                             DISPLAY "Sorry, this book is currently "
                                 BOOK-STATUS
                             DISPLAY "Please try another book."
                             MOVE 'Y' TO WS-FOUND
                          END-IF
                       END-IF
               END-READ
           END-PERFORM

           IF WS-FOUND = 'N'
               DISPLAY "Book with ISBN " WS-BORROW-ISBN " not found."
               DISPLAY "Please check the ISBN and try again."
           END-IF

           CLOSE BOOK-FILE
           DISPLAY " ".

       RETURN-BOOK.
           DISPLAY " "
           DISPLAY "=== RETURN BOOK ==="
           DISPLAY "Enter Loan ID: "
           ACCEPT WS-LOAN-ID

           MOVE 'N' TO WS-FOUND
           MOVE 'N' TO WS-EOF-FLAG
           MOVE 20250930 TO WS-RETURN-DATE

           OPEN INPUT LOAN-FILE
           PERFORM UNTIL WS-EOF-FLAG = 'Y' OR WS-FOUND = 'Y'
               READ LOAN-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       IF LOAN-ID = WS-LOAN-ID
                          IF LOAN-STATUS = "OUT"
                             COMPUTE WS-DAYS-LATE =
                             WS-RETURN-DATE - LOAN-DATE
                             DISPLAY "=== BOOK RETURN PROCESSED ==="
                             DISPLAY "Loan ID: " WS-LOAN-ID
                             DISPLAY "Member: " LOAN-MEMBER-ID
                             DISPLAY "Book ISBN: " LOAN-ISBN
                             DISPLAY "Loan Date: " LOAN-DATE
                             DISPLAY "Return Date: " WS-RETURN-DATE
                             DISPLAY "Days borrowed: " WS-DAYS-LATE

                             IF WS-DAYS-LATE > 14
                                COMPUTE WS-FINE =
                                (WS-DAYS-LATE - 14) * 5
                                DISPLAY "*** OVERDUE NOTICE ***"

                                DISPLAY "Fine amount: $" WS-FINE
                                DISPLAY "Please pay at the front desk."
                             ELSE
                                DISPLAY "Book returned on time."
                                DISPLAY "Thank you!"
                             END-IF
                             MOVE 'Y' TO WS-FOUND
                          ELSE
                             DISPLAY
                             "This book has already been returned."
                             DISPLAY "Loan Status: " LOAN-STATUS
                             MOVE 'Y' TO WS-FOUND
                          END-IF
                       END-IF
               END-READ
           END-PERFORM

           IF WS-FOUND = 'N'
               DISPLAY "Loan ID " WS-LOAN-ID " not found."
               DISPLAY "Please check your Loan ID and try again."
           END-IF

           CLOSE LOAN-FILE
           DISPLAY " ".
