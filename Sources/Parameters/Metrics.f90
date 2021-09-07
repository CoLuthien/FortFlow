module Metrics
    use, intrinsic:: iso_fortran_env
    use :: Vector
    implicit none

    ! WARNING: This module only defines metrics and other things
    ! so DO NOT attempt to initialize something in this module



    type :: FlowMetrics
        logical :: init = .false. !at reference state
        real(real64) :: pressure, temperature, sound_speed, mach_number
        real(real64) :: density, total_energy
        real(real64) :: tk, tw ! turbulent kinetic energy, specific disspation rate
        type(Vector3) :: speed
        real(real64), allocatable :: spcs_density(:)
    end type


    type :: ChemicalMetrics
        logical :: init = .false.
        real(real64) :: mean_molar_weight
        real(real64), allocatable :: ref_molar_concentration(:) ! [mol / m3], reference state molar concentration
    end type

end module
