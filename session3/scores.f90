program scores
    implicit none
    integer, parameter :: n_stud = 3, n_test = 4
    integer, dimension (n_stud, n_test) :: score_table
    real, dimension(n_stud) :: mean_scores
    real :: total_avg

    intrinsic :: reshape
    score_table = reshape([85, 71, 66, 76, 45, 45, 90, 50, 21, 60, 80, 55], &
            shape=[n_stud, n_test])

    ! Average score for each test, across all students
    print *, 'Average per test : ', sum(score_table, dim=1)/real(n_stud)

    ! sum above total average
    total_avg = sum(score_table)/real(n_stud*n_test)
    print *, 'Number above total average : ', count( score_table > total_avg)

    ! avg abs deviation per student 
    mean_scores = (sum(score_table, dim=2) - maxval(score_table, dim=2) &
    - minval(score_table, dim=2)) / 2.0
    
    print *, 'Average abs. deviation per student : ', & 
            abs(sum(transpose(score_table) - spread(mean_scores, 2, n_test), dim = 2)/ real(n_test))

    print *, 'Always scored above overall average:', &
            all(score_table > total_avg, dim = 2)

end program scores