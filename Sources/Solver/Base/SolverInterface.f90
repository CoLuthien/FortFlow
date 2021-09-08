module SolverInterface
    use, intrinsic :: iso_fortran_env
    use :: Grid
    implicit none

    type, abstract :: Solver3
        type(Grid3) :: m_grid 
    end type


end module 