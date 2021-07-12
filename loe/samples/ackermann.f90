  RECURSIVE FUNCTION Ackermann(m, n) RESULT(ack)
    INTEGER :: ack, m, n
 
    IF (m == 0) THEN
      ack = n + 1
    ELSE IF (n == 0) THEN
      ack = Ackermann(m - 1, 1)
    ELSE
      ack = Ackermann(m - 1, Ackermann(m, n - 1))
    END IF
  END FUNCTION Ackermann
