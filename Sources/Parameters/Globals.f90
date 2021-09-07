module Globals
    use, intrinsic :: iso_fortran_env
    use :: Metrics
    implicit none

    type :: ControlInputs
        character(len=:), allocatable :: case_name
        character(len=:), allocatable :: grid_file
        integer :: iter_end, write_interval
        integer :: n_spc
        integer :: nx, ny, nz ! redundant but for convenience
        logical :: restart = .false.
    end type

    type :: NumericalInputs
        real(real64) :: CFL, machine_epsilon = 1.d-12, rel_eps, abs_eps
        ! scheme dependent variables
        real(real64) :: eps, theta
    end type

    type :: ChemistryInputs
        character(len=:), allocatable :: thermo_table, trans_table, reaction_table ! name of input data files
    end type

    type :: FlowMetricsInputs
        real(real64) :: mach, p, t
        character(len=10), allocatable :: name(:)
        real(real64), allocatable :: value(:)
        real(real64) :: length_scale, cdl, ctk, ctw, kappa
    end type


    type(FlowMetrics) :: inflow_state, inflow_state_nd ! add as you need
    type(ChemicalMetrics) :: chem_state

    type(ControlInputs) :: ctrl_var
    type(NumericalInputs) :: num_var


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