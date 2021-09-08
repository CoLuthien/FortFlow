module SolverLegacy
    use, intrinsic :: iso_fortran_env
    use :: SolverInterface
    use :: CellMetrics
    use :: Grid
    implicit none

    type, extends(Solver3) :: LegacySolver3
        type(CellMetric3) :: m_metrics
    end type
end module 