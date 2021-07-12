MODULE qsort_mod
 
  IMPLICIT NONE
 
  TYPE group
     INTEGER :: order
     REAL    :: VALUE
  END TYPE group
 
CONTAINS
 
  RECURSIVE SUBROUTINE QSort(a,na)
 
    INTEGER, INTENT(in) :: nA
    TYPE (group), DIMENSION(nA), INTENT(in out) :: A
 
    INTEGER :: left, right
    REAL :: random
    REAL :: pivot
    TYPE (group) :: temp
    INTEGER :: marker
 
    IF (nA > 1) THEN
 
       CALL random_NUMBER(random)
       pivot = A(INT(random*REAL(nA-1))+1)%VALUE
       left = 1
       right = nA
       DO
          IF (left >= right) EXIT
          DO
             IF (A(right)%VALUE <= pivot) EXIT
             right = right - 1
          END DO
          DO
             IF (A(left)%VALUE >= pivot) EXIT
             left = left + 1
          END DO
          IF (left < right) THEN
             temp = A(left)
             A(left) = A(right)
             A(right) = temp
          END IF
       END DO
 
       IF (left == right) THEN
          marker = left + 1
       ELSE
          marker = left
       END IF
 
       CALL QSort(A(:marker-1),marker-1)
       CALL QSort(A(marker:),nA-marker+1)
 
    END IF
 
  END SUBROUTINE QSort
 
END MODULE qsort_mod
