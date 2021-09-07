module Globals
    use, intrinsic :: iso_fortran_env
    use :: Metrics
    implicit none

    type(FlowMetrics) :: inflow_state, inflow_state_nd ! add as you need
    type(ChemicalMetrics) :: chem_state

    ! Flow Field datas that are identical for all flow field in same case
    real(real64) :: length_scale, cdl, ctk, ctw, kappa

    ! non dimensional constants
    real(real64) :: Re, Sc
    real(real64) :: Rea1 ! Re @ M = 1

    ! non-dimensionalizing constants that has to be calculated in runtime
    real(real64) :: tau_flow

    ! non-d chemistry

    ! constants for reaction jacobian calculation
    !Ak, Bk, Ck
    real(real64), allocatable :: reaction_nd_coef(:), reaction_jaco_nd_coef_B(:, :), reaction_jaco_nd_coef_C(:)
end module 