module Parameter
    use, intrinsic :: iso_fortran_env
    implicit none


    real(real64), parameter :: atm = 101.325d3, &! [Pa], [N/m2]
                               Rgas = 8.31446261815324d0  ! [J/(mol * K)]
end module
