module Metrics
    use, intrinsic:: iso_fortran_env
    use :: Vector
    implicit none

    ! WARNING: This module only defines metrics and other things
    ! so DO NOT attempt to initialize something in this module

    type :: ControlVars
        character(len=20) :: case_name, chem_model
        character(len=20) :: grid_file
        integer :: iter_end, write_interval
        integer :: n_spc
        integer :: nx, ny, nz ! redundant but for convenience
        logical :: restart = .false.
    end type

    type :: NumericalVars
        real(real64) :: CFL, machine_epsilon = 1.d-12, rel_eps, abs_eps
    end type

    type :: FlowMetricsCommon
        real(real64) :: length_scale, cdl, ctk, ctw, kappa
    end type

    type :: FlowMetrics
        logical :: init = .false. !at reference state
        real(real64) :: pressure, temperature, sound_speed, mach_number
        real(real64) :: density, total_energy
        real(real64) :: tk, tw ! turbulent kinetic energy, specific disspation rate
        type(Vector3) :: speed
        real(real64), allocatable :: spcs_density(:)
    end type

    type :: FlowMetricsRaw
        real(real64) :: mach, p, t
        character(len=10), allocatable :: name(:)
        real(real64), allocatable :: value(:)
    end type

    type :: ChemicalMetrics
        logical :: init = .false.
        real(real64) :: mean_molar_weight
        real(real64), allocatable :: ref_molar_concentration(:) ! [mol / m3], reference state molar concentration
    end type

end module
